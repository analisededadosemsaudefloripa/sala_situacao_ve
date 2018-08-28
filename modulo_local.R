options(encoding = 'UTF-8')
#######################################################################
##DescriÃ§Ã£o
#######################################################################
#'Esse módulo permite ao usuário selecionar dados de municípios do sinasc,
#'filtrando por local de residência ou ocorrência; e exportar a série temporal destes dados
#'O banco de dados precisa conter 
#'CODMUNRES (Código do IBGE do município de residência)
#'CODMUNNASC (Código do IBGE do município de ocorrência)
#'CID
#'Com dados desagregados a partir de 2006
#'
#'O módulo deve ser utilizado como INPUT para outros módulos.
#'
#######################################################################
##Pacotes
#######################################################################
library(shiny)
library(readr)
library(tidyverse)
library(zoo)
library(fpp2)

#######################################################################
##UI
#######################################################################
#A função UI deve entra como argumento de um tabPanel

local_Input <- function(id, banco){
        ns <- NS(id)
        tagList(
                #Selecionando município
                         selectInput(inputId = ns("municipio_banco"), 
                                     label = "Selecione o código do município:",
                                     choices = c(unique(banco$CODMUNRES)), 
                                     selected = "420540"),
                         helpText("Entre com o código do IBGE.",
                                  "420540 é o código de Florianópolis"),
                         #Selecionando local de residencia ou de ocorrência
                         selectInput(inputId = ns("local_banco"), 
                                     label = "Município de residência ou ocorrência?",
                                     choices = c("Residência", "Ocorrência"), 
                                     selected = "Residência")
       )
}

#################################################################################
#Server
#################################################################################

local <- function(input, output, session, banco){
cod_municipio <- reactive({
       req(input$municipio_banco)
        input$municipio_banco
})        
        

banco_preparado<- reactive({
        
        req(input$local_banco)
        ifelse((input$local_banco == "Residência"), banco_floripa <- subset(banco, banco$CODMUNRES == cod_municipio()), banco_floripa <- subset(banco, banco$CODMUNOCOR == cod_municipio()) )      
        banco_floripa$VALOR <- 1 
        banco_floripa
        })

return(banco_preparado)

}


