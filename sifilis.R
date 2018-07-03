# Sala de Situação da Vigilância Epidemiológica de Florianópolis
options("scipen" = 100, "digits"=4) #Não colocar notação científica
#######################################################################
#Bibliotecas
#######################################################################
library(shiny)
library(googlesheets)
library(stringr)
library(tidyverse)
library(reshape2)
library(plotly)
library(leaflet)
library(magrittr)
library(htmltools)
library(readr)
library(stringr)
library(dplyr)
library(DT)
library(shinythemes)
library(readxl)
library(readr)
library(leaflet)
library(rgdal)#para ler kml
library(maptools)
library(ggmap)
library(plotKML)
library(sp)
library(tidykml)
library(RColorBrewer)
library(rgeos)
library(plyr)
library(ISOweek)
library(zoo)
library(fpp2)
library(shinyWidgets)

#######################################################################
#Bases de dados
#######################################################################
#Abrangência CS
#ogrListLayers("dados_cs/Abrangencia_Centros_de_Saude.kml") #Lê as layer do kml
abrangencia_cs <- readOGR(dsn = "dados_cs/Abrangencia_Centros_de_Saude.kml",layer =  "Abrangencia_Centros_de_Saude", encoding="UTF-8")
abrangencia_cs <- spTransform(abrangencia_cs, CRS("+proj=longlat +ellps=GRS80"))

abrangencia_cs$Name <- c("CS Centro",                  "CS Trindade",                "CS Monte Cristo",                      
                      "CS Capoeiras",               "CS Vila Aparecida",          "CS Abraao", 
                      "CS Barra da Lagoa",          "CS Canto da Lagoa",          "CS Costa da Lagoa",  
                      "CS Joao Paulo",              "CS Lagoa da Conceicao",      "CS Pantanal",
                      "CS Monte Serrat",            "CS Saco Grande",             "CS Cachoeira do Bom Jesus",
                      "CS Canasvieiras",            "CS Santinho",                "CS Jurere", 
                      "CS Ponta das Canas",         "CS Ratones",                 "CS Rio Vermelho",
                      "CS Santo Antonio de Lisboa", "CS Vargem Grande",           "CS Prainha",
                      "CS Vargem Pequena",          "CS Alto Ribeirao",           "CS Armacao",  
                      "CS Caieira da Barra do Sul", "CS Campeche",                "CS Carianos", 
                      "CS Costeira do Pirajubae",   "CS Fazenda do Rio Tavares",  "CS Morro das Pedras",
                      "CS Pantano do Sul",          "CS Agronomica",              "CS Ribeirao da Ilha", 
                      "CS Saco dos Limoes",         "CS Tapera",                  "CS Rio Tavares", 
                      "CS Corrego Grande",          "CS Ingleses",                "CS Novo Continente",
                      "CS Itacorubi ",              "CS Coqueiros",               "CS Jardim Atlantico",
                      "CS Sape",                    "CS Coloninha",               "CS Balneario",  
                      "CS Estreito")



abrangencia_cs$COD <- c("cs.14","cs.46","cs.28","cs.12","cs.49","cs.01","cs.06","cs.11",
                        "cs.18","cs.25","cs.27","cs.32","cs.29","cs.41","cs.07","cs.10",
                        "cs.42","cs.26","cs.34","cs.36","cs.39","cs.43","cs.47","cs.35",
                        "cs.48","cs.03","cs.04","cs.08","cs.09","cs.13","cs.19","cs.21",
                        "cs.30","cs.33","cs.02","cs.37","cs.40","cs.45","cs.38","cs.17",
                        "cs.22","cs.31","cs.23","cs.16","cs.24","cs.44","cs.15","cs.05",
                        "cs.20") #O código foi colocado em ordem alfabética


#Tabela de notificações, tratamento e teste rápido
sifilis_cs <- read_csv("sifilis/bases/transformadas/sifilis_cs.csv")
sifilis_cs <- sifilis_cs[order(sifilis_cs$UNIDADE),]
unidades <- sifilis_cs[,2] %>% unique()
unidades$COD <- c("cs.01","cs.02","cs.03","cs.04","cs.05","cs.06","cs.07","cs.08",
                  "cs.09","cs.10","cs.11","cs.12","cs.13","cs.14","cs.15","cs.16",
                  "cs.17","cs.18","cs.19","cs.20","cs.21","cs.22","cs.23","cs.24",
                  "cs.25","cs.26","cs.27","cs.28","cs.29","cs.30","cs.31","cs.32",
                  "cs.33","cs.34","cs.35","cs.36","cs.37","cs.38","cs.39","cs.40",
                  "cs.41","cs.42","cs.43","cs.44","cs.45","cs.46","cs.47","cs.48",
                  "cs.49") #O código foi colocado em ordem alfabética
sifilis_cs <- merge(sifilis_cs, unidades, by = "UNIDADE", all.x = T)
sifilis_cs$UNIDADE <- NULL
testes_rapidos_cs <- read_csv("sifilis/bases/transformadas/testes_rapidos_cs.csv")
testes_rapidos_cs <- subset(testes_rapidos_cs, testes_rapidos_cs$PROCEDIMENTO == "TESTE RÁPIDO PARA SÍFILIS")
colnames(testes_rapidos_cs)[4] <- "TIPO"
#Como o teste rápido começou a ser feito em 2014 Q4, inseri linhas para os outros períodos, para que o slider funcione
testes_rapidos_cs1 <-c("Continente","CS JARDIM ATLÂNTICO","2013 Q1","TESTE RÁPIDO PARA SÍFILIS",NA)
testes_rapidos_cs2 <-c("Continente","CS JARDIM ATLÂNTICO","2013 Q2","TESTE RÁPIDO PARA SÍFILIS",NA)
testes_rapidos_cs3 <-c("Continente","CS JARDIM ATLÂNTICO","2013 Q3","TESTE RÁPIDO PARA SÍFILIS",NA)
testes_rapidos_cs4 <-c("Continente","CS JARDIM ATLÂNTICO","2013 Q4","TESTE RÁPIDO PARA SÍFILIS",NA)
testes_rapidos_cs5 <-c("Continente","CS JARDIM ATLÂNTICO","2014 Q1","TESTE RÁPIDO PARA SÍFILIS",NA)
testes_rapidos_cs6 <-c("Continente","CS JARDIM ATLÂNTICO","2014 Q2","TESTE RÁPIDO PARA SÍFILIS",NA)
testes_rapidos_cs7 <-c("Continente","CS JARDIM ATLÂNTICO","2014 Q3","TESTE RÁPIDO PARA SÍFILIS",NA)
testes_rapidos_csa<- rbind(testes_rapidos_cs1,testes_rapidos_cs2,testes_rapidos_cs3,testes_rapidos_cs4,
                           testes_rapidos_cs5,testes_rapidos_cs6,testes_rapidos_cs7) %>% as.data.frame(row.names = F)
names(testes_rapidos_csa) <- names(testes_rapidos_cs)
testes_rapidos_cs<-rbind(testes_rapidos_cs,testes_rapidos_csa)
testes_rapidos_cs <- merge(testes_rapidos_cs, unidades,  by = c("UNIDADE"),all.x = T)
testes_rapidos_cs$UNIDADE <- NULL
banco_siflis_cs <-rbind(sifilis_cs,testes_rapidos_cs)
banco_siflis_cs$TRIMESTRE <- as.character(banco_siflis_cs$TRIMESTRE) 
banco_siflis_cs$VALOR <- as.numeric(banco_siflis_cs$VALOR) 

sifilis_florianopolis <- read_csv("sifilis/bases/transformadas/sifilis_florianopolis.csv")
testes_rapidos_florianopolis <- read_csv("sifilis/bases/transformadas/testes_rapidos_florianopolis.csv")
testes_rapidos_florianopolis <- subset(testes_rapidos_florianopolis, testes_rapidos_florianopolis$PROCEDIMENTO == "TESTE RÁPIDO PARA SÍFILIS")
colnames(testes_rapidos_florianopolis)[2] <- "TIPO"
banco_siflis_florianopolis <- rbind(sifilis_florianopolis,testes_rapidos_florianopolis)


#######################################################################
##Análise dos casos de sífilis
#######################################################################
#######################################################################
#User Interface
#######################################################################
# Define UI for application that draws a histogram
ui <- shinyUI(
        navbarPage(title = "Sífilis",
#######################################################################
##Qualidade da água
#######################################################################
    tabPanel("Sífilis",
                  fluidPage(
                   # Barra de navegação 
                   sidebarLayout(
                      sidebarPanel(
                         #Selecionando Indicadores
                         selectInput(inputId = "sifilis_indicador", 
                                     label = "Selecione um indicadores:",
                                     choices = sort(unique(banco_siflis_cs$TIPO)),
                                     selected = "Aguardando_Investigacao"),
                         #Selicionando Trimestre
                         sliderTextInput("sifilis_data", 
                                     label = "Selecione um trimestre:", 
                                     choices = sort(unique(banco_siflis_cs$TRIMESTRE)), 
                                     selected = "2013 Q1",
                                     animate = animationOptions(interval = 1000, 
                                                                loop = FALSE, 
                                                                playButton = NULL,
                                                                pauseButton = NULL)),
                         #Selecionando se dados aparecerão ou não
                         checkboxInput(inputId =  "sifilis_dados", 
                                    label = "Mostrar os Dados:", 
                                    value = TRUE),
                         #Tabela
                         DT::dataTableOutput(outputId = "sifilis_table")
                      ),
                      
                      # Mapa
                      mainPanel(
                         #Mapa
                         leafletOutput(outputId="sifilis_map", width = "100%", height = 660))
             )
          )
     )
  )
)
#############################################################################
#Server
#############################################################################
server <- function(input, output) {
#############################################################################
#Mapa sifilis
sifilis_select <- reactive({
sp::merge(x = abrangencia_cs, 
      y = (subset(banco_siflis_cs, TIPO == input$sifilis_indicador)),  
      by = "COD",duplicateGeoms = T)
})
        
data_select <- reactive({
        req(input$sifilis_data)
        subset(sifilis_select(),sifilis_select()$TRIMESTRE == input$sifilis_data) %>% na.omit() 
})
        
        
colorpal <- reactive({
colorNumeric("YlOrRd", domain = sifilis_select()$VALOR) #feito com banco_sífilis, para pegar todos os valores da série temporal, permitindo a comparação entre os períodos
})



output$sifilis_map <- renderLeaflet({


leaflet(data_select()) %>% 
         addProviderTiles("Esri.WorldImagery")%>% 
         setView(lng =-48.47 , lat=-27.6,zoom=10.5) 
})


observe({
        
        pal <- colorpal() 
        
        labels <- sprintf(
        "<strong>%s</strong><br/>Valor: %g",
        data_select()@data$Name, data_select()@data$VALOR
        ) %>% lapply(htmltools::HTML)

        leafletProxy("sifilis_map", data = data_select()) %>%
        clearShapes() %>%
        addPolygons(fillColor = ~pal(data_select()@data$VALOR),
             weight = 2,
             opacity = 1,
             color = "white",
             dashArray = "3",
             fillOpacity = 0.7,
             popup = data_select()@data$Name,
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
              addLegend(pal = pal, values = ~data_select()@data$VALOR, opacity = 0.7, title = NULL,
position = "bottomright")
})


output$sifilis_table <- DT::renderDataTable({
     if(input$sifilis_dados){
     DT::datatable(data = data_select()@data[,c(2,5,6,7)],
             rownames = FALSE,
             editable = FALSE,
             options = list(lengthMenu = c(10,20, 40, 60, 80, 100), pageLength = 10))}
})

}

# Run the application 
shinyApp(ui = ui, server = server)