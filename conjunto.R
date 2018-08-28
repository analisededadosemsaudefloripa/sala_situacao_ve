library(shiny)
library(dplyr)
source("data.R")

source("testeUI.R")
source("teste.R")

ui <- fluidPage(
        
        testeUI("one")
        
)

server <- function(input, output){
        
        callModule(teste, "one", all_data)
        
}

# Run the application 
shinyApp(ui = ui, server = server)