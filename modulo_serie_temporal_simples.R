#######################################################################
##Descrição
#######################################################################
#'Esse módulo permite ao usuário receber uma série temporal, realizar transformação box-cox e escolher um tempo para predição
#'mostrando os dados usados, a predição, o sumário do modelo escolhido pelo sistema (o sistema escolhe entre modelos arima
#'e ets, o com o menor Mean Squared Error - MSE), o diagnóstico e os dados.
#'
#'Esse módulo não recebe informação de nenhum módulo
#'
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
serie_temporal_simples_UI <- function(id,banco){
        ns <- NS(id)
                tagList(
                fluidPage(
                   sidebarLayout(
                      sidebarPanel(
                        #Selecionando indicador
                        selectInput(inputId = ns("indicador_banco"), 
                             label = "Selecione o Indicador",
                             choices = names(banco[,-1]), 
                             selected = ""),     
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

serie_temporal_simples <- function(input, output, session, banco){
        
banco_preparado <- reactive({
        require(input$indicador_banco)
        banco[,c("DT_TRI", input$indicador_banco)]
        
})

inicio <- reactive({
        a <- banco_preparado()$DT_TRI
        a <- substr(a,0,4)
        a <- as.numeric(a)
        a <- min(a)
        a
})

banco_pre <- reactive({         
        banco_pre <- banco_preparado()  %>% as.data.frame()
        banco_pre <- ts(banco_pre[,-1],start = inicio(), frequency = 4)
        banco_pre 
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
        funcets <- ets(banco_pre(), lambda = NULL),
        funcets <- ets(banco_pre(), lambda = input$lambda_banco))
        fets <- function(x, h) {
          forecast(funcets, h = 1)
        }
        
        ##Função para ARIMA
        ifelse(input$lambda_banco == "NULO",
        funcarima<- auto.arima(banco_pre(), lambda = NULL),
        funcarima<- auto.arima(banco_pre(), lambda = input$lambda_banco))
        farima <- function(x, h) {
          forecast(funcarima, h = 1)
        }
        
        ## Compute CV errors for ETS as e1
        e1 <- tsCV(banco_pre(), fets, h=1)
        
        ## Compute CV errors for ARIMA as e2
        e2 <- tsCV(banco_pre(), farima, h=1)
        
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
stl(banco_pre(), s.window="periodic", robust=TRUE) %>% autoplot()
})


#Gráfico de sazolnalidade 
output$sazonal_banco <- renderPlot({
ggsubseriesplot(banco_pre())+
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


