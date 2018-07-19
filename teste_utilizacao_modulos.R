# Sala de Situação da Vigilância Epidemiológica de Florianópolis
options("scipen" = 100, "digits"=2) #Não colocar notação científica


#######################################################################
#Chamando os módulos
#######################################################################
source("sim_06_16.R", encoding = "UTF-8")
source("modulo_regressao.R", encoding = "UTF-8")


#######################################################################
#User Interface
#######################################################################
# Define UI for application that draws a histogram
ui <- shinyUI(
                navbarPage(title = "Agravos de Notificação",
                tabPanel("Sífilis",
                regressao_UI(id = "sifilis", banco = sim)
)))
#############################################################################
#Server
#############################################################################
server <- function(input, output) {
#############################################################################
        callModule(module = regressao, id = "sifilis", banco = sim)
}

# Run the application 
shinyApp(ui = ui, server = server)