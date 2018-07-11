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
source("data.R")
#source("modulo_mapa_tab_dens_serie_UI.R")
source("modulo_mapa_tab_dens_serie.R")
#######################################################################
#User Interface
#######################################################################
# Define UI for application that draws a histogram
ui <- shinyUI(
        mapa_tab_dens_serie_UI(id = "sifilis", data)
)
#############################################################################
#Server
#############################################################################
server <- function(input, output) {
#############################################################################
        callModule(mapa_tab_dens_serie, "sifilis", data)
}

# Run the application 
shinyApp(ui = ui, server = server)