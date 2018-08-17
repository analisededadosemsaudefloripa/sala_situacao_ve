#######################################################################
##Descrição
#######################################################################
#'Esse módulo permite ao usuário selecionar dados de municípios do sim, sinan e sinasc,
#'filtrando por local de residência ou ocorrência, e cid; e exportar a série temporal destes dados
#'O banco de dados precisa conter 
#'CODMUNRES (Código do IBGE do município de residência)
#'CODMUNRES (Código do IBGE do município de ocorrência)
#'CID
#'Com dados desagregados a partir de 2006
#'
#'O módulo é deve ser utilizado como INPUT para outros módulos.
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

cid_local_Input <- function(id, banco){
        ns <- NS(id)
        tagList(
                 #Selecionando local de residencia ou de ocorrência do óbito
                 selectInput(inputId = ns("local_banco"), 
                             label = "Município de residência ou ocorrência?",
                             choices = c("Residência", "Ocorrência"), 
                             selected = "Residência"),
                 #Selecionando cid
                 textInput(inputId = ns("cid_banco"), 
                           label = "Insira um CID:"),
                 helpText("Utilizar letra maiuscula",
                          "sem pontuações ou separadores",
                          "(Ex: I ou I2 ou I24)")
       )
}

#################################################################################
#Server
#################################################################################

cid_local <- function(input, output, session, banco){

banco_preparado<- reactive({
        
        req(input$local_banco)
        ifelse((input$local_banco == "Residência"), banco_floripa <- subset(banco, banco$CODMUNRES == 420540), banco_floripa <- subset(banco, banco$CODMUNOCOR == 420540) )      
        req(input$cid_banco)
        banco_floripa$CID <- substring(banco_floripa$CID, 0,as.numeric(nchar(input$cid_banco)))
        banco_floripa <- subset(banco_floripa, banco_floripa$CID == input$cid_banco)
        banco_floripa$VALOR <- 1 
        banco_floripa
        })

return(banco_preparado)

}
