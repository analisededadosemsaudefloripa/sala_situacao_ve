#################################################################################
#Leitura de dados
#################################################################################
#'O banco deve conter as seguintes colunas: DT_TRI, Names com os nomes dos CS, as colunas dos indicadores em formato largo 
#################################################################################
#Bibliotecas
#################################################################################
library(shiny)
library(tidyverse)
library(sp)
library(leaflet)
library(DT)
library(plotly)
library(shinyWidgets)
#################################################################################
#UI
#################################################################################
#A função UI deve entra como argumento de um tabPanel

mapa_UI <- function(id, input_dados,banco){
        ns <- NS(id)
        
        tagList(
                 fluidPage(
                   # Barra de navegação 
                   sidebarLayout(
                      sidebarPanel(
                        #INPUT para entrada de dados
                        input_dados,
                         #Selecionando Indicadores
                         selectInput(inputId = ns("indicador"), 
                                     label = "Selecione um indicador:",
                                     choices = sort(unique(colnames(banco))),
                                     selected = NULL),
                         #Selicionando Trimestre
                         sliderTextInput(inputId =ns("data"), 
                                     label = "Selecione um trimestre:", 
                                     choices = sort(unique(banco$DT_TRI)), 
                                     selected = "2013 Q1",
                                     animate = animationOptions(interval = 2000, 
                                                                loop = FALSE, 
                                                                playButton = NULL,
                                                                pauseButton = NULL)),
                         #Selecionando se dados aparecerão ou não
                         checkboxInput(inputId =  ns("dados"), 
                                    label = "Mostrar os Dados", 
                                    value = TRUE),
                         #Tabela
                         DT::dataTableOutput(outputId = ns("table"))
                      ),
                      
                      # Mapa
                      mainPanel(
                         #Mapa
                         leafletOutput(outputId=ns("map"), width = "100%", height = 660),
                         splitLayout(
                                 plotlyOutput(outputId = ns("serie")),
                                 plotlyOutput(outputId = ns("densidade"))
                                )
                        )
                )
        )
)


}



#################################################################################
#Server
#################################################################################

mapa <- function(input, output,session, banco_preparado){

   
#Indicador
indicador <- reactive({
        req(input$indicador)
        input$indicador
})


banco_prev <- reactive({
        Cod_Unidades_VE <- read_csv("dados_cs/Cod_Unidades_VE.csv", 
        col_types = cols(COD_VE = col_integer()))
        merge(banco_preparado(), Cod_Unidades_VE, by.x = "UNIDADE",by.y = "COD_VE", all.x = TRUE)
})



#Mapa 
cs_select <- reactive({
        sp::merge(x = abrangencia_cs, 
         y = banco_prev()[,c(which(names(banco_prev())) == indicador(),  (which(colnames(banco_prev()) == "DT_TRI")), (which(colnames(banco_prev()) == "Name")))],  
         by = c("COD"),duplicateGeoms = T, na.rm = F)
})

  
        
    
data_cs_select <- reactive({
        req(input$data)
        subset(cs_select(),cs_select()@data$DT_TRI == input$data) 
})
        
        
colorpal <- reactive({
colorNumeric("YlOrRd", domain = banco_preparado[ ,c((which(colnames(banco_preparado) == indicador())))]) #feito com banco_preparado completo, para pegar todos os valores da série temporal, permitindo a comparação entre os períodos
})



output$map <- renderLeaflet({
        pal <- colorpal() 
        
        labels <- sprintf(
        "<strong>%s</strong><br/>Valor: %g",
        data_cs_select()@data$Name, data_cs_select()@data$indicador()
        ) %>% lapply(htmltools::HTML)

leaflet(data = data_cs_select()) %>% 
        addProviderTiles("Esri.WorldImagery")%>% 
        setView(lng =-48.47 , lat=-27.6,zoom=10.5)%>%
        addPolygons(fillColor = ~pal(data_cs_select()@data$indicador()),
             weight = 2,
             opacity = 1,
             color = "white",
             dashArray = "3",
             fillOpacity = 0.7,
             popup = data_cs_select()@data$Name,
             highlight = highlightOptions(
                            weight = 5,
                            color = "#666",
                            dashArray = "",
                            fillOpacity = 0.7,
                            bringToFront = TRUE),
             label = labels,
                     labelOptions = labelOptions(
                     style = list("font-weight" = "normal", padding = "3px 8px"),
                     textsize = "15px",
                     direction = "auto"))%>%
              addLegend(pal = pal, values = ~data_cs_select()@data$indicador(), opacity = 0.7, title = NULL,
position = "bottomright") 
})


observe({
        
        pal <- colorpal() 
        
        labels <- sprintf(
        "<strong>%s</strong><br/>Valor: %g",
        data_cs_select()@data$Name, data_cs_select()@data$indicador()
        ) %>% lapply(htmltools::HTML)

        leafletProxy("map", data = data_cs_select()) %>%
        clearShapes() %>%
        addPolygons(fillColor = ~pal(data_cs_select()@data$indicador()),
             weight = 2,
             opacity = 1,
             color = "white",
             dashArray = "3",
             fillOpacity = 0.7,
             popup = data_cs_select()@data$Name,
             highlight = highlightOptions(
                            weight = 5,
                            color = "#666",
                            dashArray = "",
                            fillOpacity = 0.7,
                            bringToFront = TRUE),
             label = labels,
                     labelOptions = labelOptions(
                     style = list("font-weight" = "normal", padding = "3px 8px"),
                     textsize = "15px",
                     direction = "auto"))%>%
              addLegend(pal = pal, values = ~data_cs_select()@data$indicador(), opacity = 0.7, title = NULL,
position = "bottomright")
})



#Gráfico com densidade

max_valor <- reactive({
        a <- banco_preparado[ ,c((which(colnames(banco_preparado) == indicador())))]
        b <-as.numeric(max(a$indicador(), na.rm = T))
        b
})


output$densidade <- renderPlotly({

c <- ggplot(data_cs_select()@data)+
        geom_density(aes(data_cs_select()@data$indicador()),fill = "red", color = "red", alpha = 0.5,position = "identity",inherit.aes = F)+
        scale_x_continuous(limits = c(0, max_valor()), na.value = F)+
        xlab(" ") +
        ylab("Densidade") +
        ggtitle("Densidade - Trimestral")
ggplotly(c)
})




#Gráfico com série temporal
#floripa_select <- reactive({
#        req(input$indicador)
#        subset(banco_florianopolis, TIPO == input$indicador)
#})
        
#data_floripa_select <- reactive({
#        req(input$data)
#        subset(floripa_select(),floripa_select()$DT_TRI == input$data)  
#})

#output$serie <- renderPlotly({
        
#d <-ggplot(floripa_select())+
#        geom_line(aes(TRIMESTRE, VALOR, group = TIPO))+
#        geom_point(aes(data_floripa_select()$DT_TRI, data_floripa_select()$VALOR), size = 5, fill = "red", color = "red")+        
#        ylab("Valor")+
#        xlab(" ")+
#        ggtitle("Série Temporal - Trimestral")+
#        theme(axis.text.x = element_text(angle = 90, hjust = 1))

#ggplotly(d, xaxis = list(automargin=TRUE))

#})



#Tabela de dados
output$table <- DT::renderDataTable({
        
     if(input$dados){
                 
     dados_organizados <- data_cs_select()@data#[,c(2,5,6,7)]
     
     DT::datatable(data = dados_organizados,
             rownames = FALSE,
             editable = FALSE,
             options = list(lengthMenu = c(10,20, 40, 60, 80, 100), pageLength = 20))}
})


          
}
