# Sala de Situação da Vigilância Epidemiológica de Florianópolis
options("scipen" = 100, "digits"=4) #Não colocar notação científica
#######################################################################
#Bibliotecas
#######################################################################
library(shiny)
library(tidyverse)
library(reshape2)
library(plotly)
library(leaflet)
library(readr)
library(DT)
library(maptools)
library(ggmap)
library(plotKML)
library(sp)
library(tidykml)
library(RColorBrewer)
library(rgdal)
library(rgeos)
library(zoo)
library(fpp2)
library(htmltools)


#######################################################################
#Bases de dados
#######################################################################
#Abrangência CS
#ogrListLayers("dados_cs/Abrangencia_Centros_de_Saude.kml") #Lê as layer do kml
#abrangencia <- readOGR(dsn = "dados_cs/Abrangencia_Centros_de_Saude.kml",layer =  "Abrangencia_Centros_de_Saude", encoding="UTF-8")
#abrangencia <- spTransform(abrangencia, CRS("+proj=longlat +ellps=GRS80"))

#abrangencia_cs$Name <- c("CS Centro",                  "CS Trindade",                "CS Monte Cristo",                      "CS Capoeiras",               "CS Vila Aparecida",          "CS Abraao", 
#                      "CS Barra da Lagoa",          "CS Canto da Lagoa",          "CS Costa da Lagoa",  
#                      "CS Joao Paulo",              "CS Lagoa da Conceicao",      "CS Pantanal",
#                      "CS Monte Serrat",            "CS Saco Grande",             "CS Cachoeira do Bom Jesus",
#                      "CS Canasvieiras",            "CS Santinho",                "CS Jurere", 
#                      "CS Ponta das Canas",         "CS Ratones",                 "CS Rio Vermelho",
#                      "CS Santo Antonio de Lisboa", "CS Vargem Grande",           "CS Prainha",
#                      "CS Vargem Pequena",          "CS Alto Ribeirao",           "CS Armacao",  
#                      "CS Caieira da Barra do Sul", "CS Campeche",                "CS Carianos", 
#                      "CS Costeira do Pirajubae",   "CS Fazenda do Rio Tavares",  "CS Morro das Pedras",
#                      "CS Pantano do Sul",          "CS Agronomica",              "CS Ribeirao da Ilha", 
#                      "CS Saco dos Limoes",         "CS Tapera",                  "CS Rio Tavares", 
#                      "CS Corrego Grande",          "CS Ingleses",                "CS Novo Continente",
#                      "CS Itacorubi ",              "CS Coqueiros",               "CS Jardim Atlantico",
#                     "CS Sape",                    "CS Coloninha",               "CS Balneario",  
#                      "CS Estreito")

#Tabela de coletas
janeiro <- read_csv("vigiagua/janeiro_2018.csv")
fevereiro <- read_csv("vigiagua/fevereiro_2018.csv")
marco <- read_csv("vigiagua/marco_2018.csv")
abril <- read_csv("vigiagua/abril_2018.csv")
maio <- read_csv("vigiagua/maio_2018.csv")

#Criando uma lista única de pontos de coleta
#pontos <- rbind(janeiro[,c(2,3)], fevereiro[,c(2,3)])
#pontos <- rbind(pontos, marco[,c(2,3)])
#pontos <- rbind(pontos, abril[,c(2,3)])
#pontos <- rbind(pontos, maio[,c(2,3)])
#pontos <- unique(pontos)
#pontos <- merge(pontos, pontos, by = c("PONTO_COLETA", "ENDERECO"), all.x = T)
#write.csv(pontos, "sala_situacao_ve/bases/vigiagua/pontos.csv", fileEncoding = "cp1252")

#Pontos de coleta já com longitude e latitude
pontos <- read_csv("vigiagua/pontos_long_lat.csv")

#Criando uma base única de coletas
janeiro_long_lat <- merge(pontos, janeiro[,-c(1,3)], by = "PONTO_COLETA", all.y = T)
fevereiro_long_lat <- merge(pontos, fevereiro[,-c(1,3)], by = "PONTO_COLETA", all.y = T)
marco_long_lat <- merge(pontos, marco[,-c(1,3)], by = "PONTO_COLETA", all.y = T)
abril_long_lat <- merge(pontos, abril[,-c(1,3)], by = "PONTO_COLETA", all.y = T)
maio_long_lat <- merge(pontos, maio[,-c(1,3)], by = "PONTO_COLETA", all.y = T) 


coleta <- rbind(janeiro_long_lat,fevereiro_long_lat, marco_long_lat, abril_long_lat, maio_long_lat)


for(i in 1:nrow(coleta)){
if(nchar(coleta$DATA_COLETA[i]) == 7 & !is.na(coleta$DATA_COLETA[i])){coleta$DATA_COLETA[i] <- paste0("0",coleta$DATA_COLETA[i])}
}


coleta <-coleta[-c(118,189,146),]#data está errada

coleta$DATA_COLETA <- as.Date(coleta$DATA_COLETA, format = "%d%m%Y")
coleta$MES_COLETA <- as.yearmon(coleta$DATA_COLETA)
coleta$DATA_COLETA <- NULL

coleta <- melt(coleta, c("PONTO_COLETA", "ENDERECO", "LONG", "LAT", "SISTEMA", "BAIRRO", "MES_COLETA"))
names(coleta)[c(8,9)] <- c("VARIAVEL", "VALOR")

#######################################################################
##Localização dos Pontos de Coleta
#######################################################################
#######################################################################
#User Interface
#######################################################################
# Define UI for application that draws a histogram
ui <- shinyUI(
        navbarPage(title = "Vigiágua - Florianópolis",
#######################################################################
##Qualidade da água
#######################################################################
    tabPanel("COLETAS",
                  fluidPage(
                   # Barra de navegação 
                   sidebarLayout(
                      sidebarPanel(
                         #Selecionando Indicadores
                         selectInput(inputId = "vigiagua_variavel", 
                                     label = "Selecione um parâmetro:",
                                     choices = sort(unique(coleta$VARIAVEL)),
                                     selected = "CLORO"),
                         #Selicionando Data
                         selectInput(inputId = "vigiagua_data", 
                                     label = "Selecione um período:",
                                     choices = sort(unique(coleta$MES_COLETA)),
                                     selected = "Jan 2018"),
                         #Selicionando Sistema
                         selectInput(inputId = "vigiagua_sistema", 
                                     label = "Selecione um ou mais sistemas:",
                                     choices = sort(unique(coleta$SISTEMA)),
                                     selected = "ACOLJOGOC",
                                     multiple = T),
                         #Selecionando se dados aparecerão ou não
                         checkboxInput(inputId =  "vigiagua_sistema_todos", 
                                    label = "Mostrar todos os sistemas", 
                                    value = FALSE),
                         #Selecionando se dados aparecerão ou não
                         checkboxInput(inputId =  "vigiagua_dados", 
                                    label = "Mostrar os dados", 
                                    value = TRUE),
                         #Tabela
                         DT::dataTableOutput(outputId = "vigiagua_table")
                      ),
                      
                      # Mapa
                      mainPanel(
                         #Mapa
                         leafletOutput(outputId="vigiagua_map", width = "100%", height = 660))
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
#subset por indicador
variavel_select <- reactive({
        req(input$vigiagua_variavel)
        a <- subset(coleta,coleta$VARIAVEL == input$vigiagua_variavel) 
        b <- subset(a, !is.na(a$VALOR))
        b
})

sistema_select <- reactive({
        req(input$vigiagua_sistema)
        if(input$vigiagua_sistema_todos){
        variavel_select()        
        }else{
        subset(variavel_select(), SISTEMA %in% input$vigiagua_sistema) 
        }
})


data_select <- reactive({
        req(input$vigiagua_data)
        subset(sistema_select(),sistema_select()$MES_COLETA == input$vigiagua_data) 
})



output$vigiagua_map <- renderLeaflet({
        
labels <- sprintf(
"<strong>%s</strong><br/>%s: %s",
data_select()$PONTO_COLETA,data_select()$VARIAVEL, data_select()$VALOR
) %>% lapply(htmltools::HTML)

leaflet(data = data_select()) %>% 
         addProviderTiles("Esri.WorldImagery")%>% 
         setView(lng =-48.47 , lat=-27.6,zoom=10.5) %>%
         clearShapes() %>%
         addAwesomeMarkers(data_select(), lng = data_select()$LONG, lat = data_select()$LAT, 
                           layerId = NULL,
                           group = data_select()$SISTEMA, 
                           icon = NULL, 
                           popup = NULL, 
                           popupOptions = NULL,
                           label = labels, 
                           labelOptions = labelOptions(
                                style = list("font-weight" = "normal", padding = "3px 8px"),
                                textsize = "15px",
                                direction = "auto"), 
                           options = markerOptions(),
                           clusterOptions = data_select()$SISTEMA, 
                           clusterId = data_select()$SISTEMA)
})




output$vigiagua_table <- DT::renderDataTable({
     if(input$vigiagua_dados){
     DT::datatable(data = data_select()[,-c(3,4,5,6,7,8)],
             rownames = FALSE,
             editable = FALSE,
             options = list(lengthMenu = c(5,10,20, 40, 60, 80, 100), pageLength = 5))}
})

        
        
}

# Run the application 
shinyApp(ui = ui, server = server)
