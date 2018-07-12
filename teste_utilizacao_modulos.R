# Sala de Situação da Vigilância Epidemiológica de Florianópolis
options("scipen" = 100, "digits"=2) #Não colocar notação científica
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
#Chamando os módulos
#######################################################################
source("dados_sifilis.R", encoding = "UTF-8")
source("modulo_mapa_tab_dens_serie.R", encoding = "UTF-8")
#######################################################################
#User Interface
#######################################################################
# Define UI for application that draws a histogram
ui <- shinyUI(
                navbarPage(title = "Agravos de Notificação",
                tabPanel("Sífilis",
                mapa_tab_dens_serie_UI(id = "sifilis", banco_geral = dados_sifilis, banco_cs = banco_sifilis_cs)
)))
#############################################################################
#Server
#############################################################################
server <- function(input, output) {
#############################################################################
        callModule(module = mapa_tab_dens_serie, id = "sifilis", banco_geral = dados_sifilis, banco_sifilis_cs, banco_sifilis_florianopolis)
}

# Run the application 
shinyApp(ui = ui, server = server)