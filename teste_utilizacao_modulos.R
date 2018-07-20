# Sala de Situação da Vigilância Epidemiológica de Florianópolis
options("scipen" = 100, "digits"=2) #Não colocar notação científica


#######################################################################
#Chamando os módulos
#######################################################################
source("sim_06_16.R", encoding = "UTF-8")
source("modulo_cid_local.R", encoding = "UTF-8")
source("modulo_serie_temporal.R", encoding = "UTF-8")


#######################################################################
#User Interface
#######################################################################
# Define UI for application that draws a histogram
ui <- shinyUI(
                navbarPage(title = "Sim",
                tabPanel("Sim",
                serie_temporal_UI(id = "serie",
                                  input_dados = cid_local_Input(id = "cid", banco = sim))
)))
#############################################################################
#Server
#############################################################################
server <- function(input, output) {
#############################################################################
        banco_preparado <- callModule(module = cid_local, 
                             id = "cid", 
                             banco = sim)
        callModule(module = serie_temporal, 
                   id = "serie", 
                   banco_preparado = banco_preparado)
}

# Run the application 
shinyApp(ui = ui, server = server)