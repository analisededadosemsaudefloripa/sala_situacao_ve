#################################################################################
#Leitura de dados
#################################################################################
#O banco geral deve conter um banco com dados por cs e um banco com dados de florianópolis
#'O banco_cs deve conter as seguintes colunas, na seguinte ordem: TRIMESTRE, TIPO, COD, DISTRITO, VALOR. TIPO refere-se ao indicador, e COD ao código do CS
#'O banco_florianopolis deve conter as seguintes colunas, na seguinte ordem: TRIMESTRE, TIPO, VALOR
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

mapa_tab_dens_serie_UI <- function(id, banco_geral, banco_cs){
        ns <- NS(id)
        
        tagList(
                 fluidPage(
                   # Barra de navegação 
                   sidebarLayout(
                      sidebarPanel(
                         #Selecionando Indicadores
                         selectInput(inputId = ns("indicador"), 
                                     label = "Selecione um indicador:",
                                     choices = sort(unique(banco_cs$TIPO)),
                                     selected = NULL),
                         #Selicionando Trimestre
                         sliderTextInput(inputId =ns("data"), 
                                     label = "Selecione um trimestre:", 
                                     choices = sort(unique(banco_cs$TRIMESTRE)), 
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

mapa_tab_dens_serie <- function(input, output,session, banco_geral, banco_cs, banco_florianopolis){

#Mapa sifilis
cs_select <- reactive({
        req(input$indicador)
        sp::merge(x = abrangencia_cs, 
         y = (subset(banco_cs, TIPO == input$indicador)),  
         by = c("COD"),duplicateGeoms = T, na.rm = F)
})
        
data_cs_select <- reactive({
        req(input$data)
        subset(cs_select(),cs_select()@data$TRIMESTRE == input$data) 
})
        
        
colorpal <- reactive({
colorNumeric("YlOrRd", domain = cs_select()@data$VALOR) #feito com banco_sífilis, para pegar todos os valores da série temporal, permitindo a comparação entre os períodos
})



output$map <- renderLeaflet({
        pal <- colorpal() 
        
        labels <- sprintf(
        "<strong>%s</strong><br/>Valor: %g",
        data_cs_select()@data$Name, data_cs_select()@data$VALOR
        ) %>% lapply(htmltools::HTML)

leaflet(data = data_cs_select()) %>% 
        addTiles()%>% 
        addPolygons(fillColor = ~pal(data_cs_select()@data$VALOR),
             weight = 2,
             opacity = 1,
             color = "grey",
             dashArray = "3",
             fillOpacity = 0.9,
             popup = data_cs_select()@data$Name,
             highlight = highlightOptions(
                            weight = 5,
                            color = "#666",
                            dashArray = "",
                            fillOpacity = 0.9,
                            bringToFront = TRUE),
             label = labels,
                     labelOptions = labelOptions(
                     style = list("font-weight" = "normal", padding = "3px 8px"),
                     textsize = "15px",
                     direction = "auto"))%>%
              addLegend(pal = pal, values = ~cs_select()@data$VALOR, opacity = 0.7, title = NULL,
position = "bottomright") 
})



#Gráfico com densidade

max_valor <- reactive({
        a <- subset(banco_cs, TIPO == input$indicador)
        b <-as.numeric(max(a$VALOR, na.rm = T))
        b
})


output$densidade <- renderPlotly({
        VALOR2 <- density(na.omit(cs_select()@data$VALOR))$y 
        
c <- ggplot(data_cs_select()@data)+
        geom_density(aes(data_cs_select()@data$VALOR),fill = "red", color = "red", alpha = 0.5,position = "identity",inherit.aes = F)+
        scale_x_continuous(limits = c(0, max_valor()), na.value = F)+
        scale_y_continuous(limits = c(0, (max(VALOR2)*3)), na.value = F)+
        xlab(" ") +
        ylab("Densidade") +
        ggtitle("Densidade - Trimestral")
ggplotly(c)
})




#Gráfico com série temporal


#Mapa sifilis
floripa_select <- reactive({
        req(input$indicador)
        subset(banco_florianopolis, TIPO == input$indicador)
})
        
data_floripa_select <- reactive({
        req(input$data)
        subset(floripa_select(),floripa_select()$TRIMESTRE == input$data)  
})

output$serie <- renderPlotly({
        
d <-ggplot(floripa_select())+
        geom_line(aes(TRIMESTRE, VALOR, group = TIPO))+
        geom_point(aes(data_floripa_select()$TRIMESTRE, data_floripa_select()$VALOR), size = 5, fill = "red", color = "red")+        
        ylab("Valor")+
        xlab(" ")+
        ggtitle("Série Temporal - Trimestral")+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplotly(d, xaxis = list(automargin=TRUE))

})



#Tabela de dados
output$table <- DT::renderDataTable({
        
     if(input$dados){
                 
     dados_organizados <- data_cs_select()@data[,c(2,5,6,7)]
     
     DT::datatable(data = dados_organizados,
             rownames = FALSE,
             editable = FALSE,
             options = list(lengthMenu = c(10,20, 40, 60, 80, 100), pageLength = 20))}
})


          
}
