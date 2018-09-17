#################################################################################
#Leitura de dados
#################################################################################
#O banco geral deve conter um banco com dados por cs de apenas um período
#'O banco deve conter as seguintes colunas, na seguinte ordem: TRIMESTRE, TIPO, VALOR
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

mapa_simples_UI <- function(id, banco){
        ns <- NS(id)
        
        tagList(
                 fluidPage(
                   # Barra de navegação 
                   sidebarLayout(
                      sidebarPanel(
                         #Selecionando Indicadores
                         selectInput(inputId = ns("indicador"), 
                                     label = "Selecione um indicador:",
                                     choices = sort(unique(banco$TIPO)),
                                     selected = NULL),
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
                         leafletOutput(outputId=ns("map"), width = "100%", height = 660)
                        )
                )
        )
)


}



#################################################################################
#Server
#################################################################################

mapa_simples <- function(input, output,session, banco_geral, banco, banco_florianopolis){

#Mapa sifilis
cs_select <- reactive({
        req(input$indicador)
        sp::merge(x = abrangencia_cs, 
         y = (subset(banco, TIPO == input$indicador)),  
         by = c("COD"),duplicateGeoms = T, na.rm = F)
})
        

        
        
colorpal <- reactive({
colorNumeric("YlOrRd", domain = cs_select()@data$VALOR) 
})



output$map <- renderLeaflet({
        pal <- colorpal() 
        
        labels <- sprintf(
        "<strong>%s</strong><br/>Valor: %g",
        cs_select()@data$Name, cs_select()@data$VALOR
        ) %>% lapply(htmltools::HTML)

leaflet(data = cs_select()) %>% 
        addTiles()%>% 
        addPolygons(fillColor = ~pal(cs_select()@data$VALOR),
             weight = 2,
             opacity = 1,
             color = "grey",
             dashArray = "3",
             fillOpacity = 0.9,
             popup = cs_select()@data$Name,
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


#Tabela de dados
output$table <- DT::renderDataTable({
        
     if(input$dados){
                 
     dados_organizados <- cs_select()@data
     
     DT::datatable(data = dados_organizados[,c(2,17,18)],
             rownames = FALSE,
             editable = FALSE,
             options = list(lengthMenu = c(10,20, 40, 60, 80, 100), pageLength = 20))}
})


          
}
