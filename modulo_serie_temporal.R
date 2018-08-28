#######################################################################
##Descrição
#######################################################################
#'Esse módulo permite ao usuário receber uma série temporal, realizar transformação box-cox e escolher um tempo para predição
#'mostrando os dados usados, a predição, o sumário do modelo escolhido pelo sistema (o sistema escolhe entre modelos arima
#'e ets, o com o menor Mean Squared Error - MSE), o diagnóstico e os dados.
#'
#'O módulo tem como entrada um módulo, que deverá ser usado como INPUT_DADOS na sidebar (esse mode deve fazer a entrada de dados, a sua seleção
#'e transformação em uma série temporal; e a série temporal gerada pelo módulo. Ex: módulo = modulo_cid_local; e série teporal = banco_preparado())
#'
#'Este módulo deve ser usado em um TABPANEL
#'
#######################################################################
##Pacotes
#######################################################################
library(shiny)
library(readr)
library(tidyverse)
library(zoo)
library(fpp2)
library(DT)

#######################################################################
##UI
#######################################################################
#A função UI deve entra como argumento de um tabPanel
serie_temporal_UI <- function(id,input_dados){
        ns <- NS(id)
                tagList(
                fluidPage(
                   sidebarLayout(
                      sidebarPanel(
                         #INPUT para entrada de dados
                         input_dados,
                         #Selecionando lambda
                         textInput(inputId = ns("lambda_banco"), 
                                     label = "Transformação de Box-Cox(lambda):",
                                     value = "NULO"),
                         helpText("Se nenhuma transformação for necessária",
                                  "não intruduzir nenhum valor.",
                                  "Lambda = NULO = sem transforação.",
                                  "Lambda = 0 = transformação Logarítimica."),
                         #Selecionando período de previsão
                         numericInput(inputId = ns("periodo_prev_banco"), 
                                     label = "Período desejado para previsão:",
                                     value = 4, 
                                     min = 0)
                      ),
                      
                      # Série temporal 
                      mainPanel(
                              tabsetPanel(type = "tabs",
                         #Gráfico da série temporal
                         tabPanel("Previsão", plotOutput(outputId = ns("serie_banco"), width = "100%", height = 660),
                                 verbatimTextOutput(ns("summary_previsao_banco"))),
                         tabPanel("Decomposição", plotOutput(outputId = ns("decomposicao_banco"), width = "100%", height = 660),
                                 plotOutput(outputId = ns("sazonal_banco"), width = "100%", height = 330)),
                         tabPanel("Diagnóstico", plotOutput(outputId = ns("residuos_banco"), width = "100%", height = 660)),
                         tabPanel("Dados",DT::dataTableOutput(ns("dados"), width = "100%", height = 660))
                                        )
                                )
                        )
                )
       )
}

#################################################################################
#Server
#################################################################################

serie_temporal <- function(input, output, session, banco_preparado){


banco <- reactive({         
        banco <- banco_preparado()$DT_TRI  %>% as.data.frame()
        banco <- table(banco) %>% as.data.frame()
        banco <- ts(banco[,-1],start = 2006, frequency = 4)
        banco 
})        
        
        
        
lambda <-reactive({
        req(input$lambda_banco)
})     
        
        
periodo_prev_banco <-reactive({
        req(input$periodo_prev_banco)
})



banco_prev <-reactive({
        
        #Função para ETS
        ifelse(input$lambda_banco == "NULO",
        funcets <- ets(banco(), lambda = NULL),
        funcets <- ets(banco(), lambda = input$lambda_banco))
        fets <- function(x, h) {
          forecast(funcets, h = 1)
        }
        
        ##Função para ARIMA
        ifelse(input$lambda_banco == "NULO",
        funcarima<- auto.arima(banco(), lambda = NULL),
        funcarima<- auto.arima(banco(), lambda = input$lambda_banco))
        farima <- function(x, h) {
          forecast(funcarima, h = 1)
        }
        
        ## Compute CV errors for ETS as e1
        e1 <- tsCV(banco(), fets, h=1)
        
        ## Compute CV errors for ARIMA as e2
        e2 <- tsCV(banco(), farima, h=1)
        
        ## Find MSE of each model class
        g<-mean(e1^2, na.rm=TRUE)
        h<-mean(e2^2, na.rm=TRUE)
        
        ifelse(g > h, fit_banco <- funcets , fit_banco <- funcarima)
        "Intervalo de Confiança"
        forecast(fit_banco, periodo_prev_banco(), level = c(50,80,95))
})

#Gráfico com previsão
output$serie_banco <- renderPlot({
autoplot(banco_prev())+
        xlab("Ano") +
        ylab("")+
        ggtitle("Série Temporal - Trimestral")
})

#Gráfico dos resíduos
output$residuos_banco <- renderPlot({
checkresiduals(banco_prev())
})


#Sumário
output$summary_previsao_banco <- renderPrint({
        summary(banco_prev())
        checkresiduals(banco_prev())
          })


#Gráfico dos resíduos
output$residuos_banco <- renderPlot({
checkresiduals(banco_prev())
})

#Gráfico de decomposição 
output$decomposicao_banco <- renderPlot({
##Decomposição por STL
stl(banco(), s.window="periodic", robust=TRUE) %>% autoplot()
})


#Gráfico de sazolnalidade 
output$sazonal_banco <- renderPlot({
ggsubseriesplot(banco())+
  ylab("Óbitos")
})

#Tabela de Dados

dados <- reactive({
        VALOR <- as.data.frame(banco_prev())
})
                        


output$dados <- DT::renderDataTable({
                        DT::datatable(dados(),
                        rownames = FALSE,
                        editable = FALSE,
                        options = list(lengthMenu = c(10,20, 40, 60, 80, 100), pageLength = 20))
})



}


