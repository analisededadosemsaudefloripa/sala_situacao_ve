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
                         #Selicionando Trimestre
                         sliderTextInput(inputId =ns("data"), 
                                     label = "Selecione um trimestre:", 
                                     choices = sort(unique(banco$DT_TRI)), 
                                     selected = sort(banco$DT_TRI)[1],
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

banco <- reactive({
        banco_preparado()
})
   
banco_prim<- reactive({
        a <- banco()[,c("DT_TRI", "UNIDADE", "VALOR")]
        a <- aggregate(a$VALOR, by = list(a$DT_TRI, a$UNIDADE), FUN = sum)
        names(a) <- c("DT_TRI", "UNIDADE", "VALOR")
        a
})        
        
   

banco_prev <- reactive({
        Cod_Unidades_VE <- read_csv("dados_cs/Cod_Unidades_VE.csv", 
        col_types = cols(COD_VE = col_integer()))
        a <- merge(banco_prim(), Cod_Unidades_VE, by.x = "UNIDADE", by.y = "COD_VE", all.x = TRUE) 
        a <- a[, c("DT_TRI", "COD", "VALOR")]
        a
})



cs_select <- reactive({
        sp::merge(x = abrangencia_cs, 
         y = banco_prev(),  
         by = c("COD"),duplicateGeoms = T, na.rm = F)
})

  
        
    
data_cs_select <- reactive({
        req(input$data)
        subset(cs_select(),cs_select()@data$DT_TRI == input$data) 
})

colorpal <- reactive({
colorNumeric("YlOrRd", domain = cs_select()$VALOR) #feito com banco_prim completo, para pegar todos os valores da série temporal, permitindo a comparação entre os períodos
})



output$map <- renderLeaflet({
        pal <- colorpal() 
        
        labels <- sprintf(
        "<strong>%s</strong><br/>Valor: %g",
        data_cs_select()@data$Name, data_cs_select()@data$VALOR
        ) %>% lapply(htmltools::HTML)

leaflet(data = data_cs_select()) %>% 
        addProviderTiles("Esri.WorldImagery")%>% 
        setView(lng =-48.47 , lat=-27.6,zoom=10.5)%>%
        addPolygons(fillColor = ~pal(data_cs_select()@data$VALOR),
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
              addLegend(pal = pal, values = ~cs_select()$VALOR, opacity = 0.7, title = NULL,
position = "bottomright") 
})


#Gráfico com densidade

output$densidade <- renderPlotly({
        VALOR1 <- na.omit(data_cs_select()@data$VALOR)
        VALOR2 <- na.omit(cs_select()$VALOR)

c <- ggplot(data_cs_select()@data)+
        geom_density(aes(VALOR1),fill = "red", color = "red", alpha = 0.5,position = "identity",inherit.aes = F)+
        scale_x_continuous(limits = c(0, max(VALOR2)), na.value = F)+
        scale_y_continuous(limits = c(0, (max(density(VALOR2)$y)*3)), na.value = F)+
        xlab(" ") +
        ylab("Densidade") +
        ggtitle("Densidade - Trimestral")
ggplotly(c)
})




#Gráfico com série temporal
floripa_select <- reactive({
        a <- banco_prim()[,c("DT_TRI", "VALOR")]
        a <- aggregate(a$VALOR, by = list(a$DT_TRI), FUN = sum)
        names(a) <- c("DT_TRI", "VALOR")
        a

})
        
data_floripa_select <- reactive({
        req(input$data)
        subset(floripa_select(),floripa_select()$DT_TRI == input$data)

})

output$serie <- renderPlotly({
        
d <-ggplot(floripa_select())+
        geom_line(aes(DT_TRI, as.numeric(VALOR), group = 1))+
        geom_point(aes(data_floripa_select()$DT_TRI, data_floripa_select()$VALOR), size = 5, fill = "red", color = "red")+        
        ylab("Valor")+
        xlab(" ")+
        ggtitle("Série Temporal - Trimestral")+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplotly(d, xaxis = list(automargin=TRUE))

})



#Tabela de dados
output$table <- DT::renderDataTable({
        
     if(input$dados){
         
     dados_organizados <- data_cs_select()@data[,c(2,4,5)]
    
     DT::datatable(data = dados_organizados,
             rownames = FALSE,
             editable = FALSE,
             options = list(lengthMenu = c(10,20, 40, 60, 80, 100), pageLength = 20))}
})



          
}
