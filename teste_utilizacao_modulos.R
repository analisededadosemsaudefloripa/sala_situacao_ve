# Sala de Situação da Vigilância Epidemiológica de Florianópolis
options("scipen" = 100, "digits"=2) #Não colocar notação científica
#######################################################################
#Bibliotecas
#######################################################################
library(shiny)
library(readr)
library(tidyverse)
library(corrplot)
library(DT)

#######################################################################
#Chamando os módulos
#######################################################################
source("dados_hiv.R", encoding = "UTF-8")
source("modulo_regressao.R", encoding = "UTF-8")


#######################################################################
#User Interface
#######################################################################
# Define UI for application that draws a histogram
ui <- shinyUI(
                navbarPage(title = "Agravos de Notificação",
                tabPanel("Sífilis",
                regressao_UI(id = "sifilis", banco = hiv)
)))
#############################################################################
#Server
#############################################################################
server <- function(input, output) {
#############################################################################
        callModule(module = regressao, id = "sifilis", banco = hiv)
}

# Run the application 
shinyApp(ui = ui, server = server)