# Sala de Situação da Vigilância Epidemiológica de Florianópolis
options("scipen" = 100, "digits"=2) #Não colocar notação científica
#################################################################################
#Leitura de dados
#################################################################################
#O banco deve conter colunas com as variáveis a serem analisadas


#################################################################################
#Bibliotecas necessárias
#################################################################################
library(shiny)
library(readr)
library(tidyverse)
library(corrplot)
library(DT)



#################################################################################
#UI
#################################################################################
#A função UI deve entra como argumento de um tabPanel

regressao_UI <- function(id, banco){
        ns <- NS(id)
        tagList(
                fluidPage(
                           # Barra de navegação 
                           sidebarLayout(
                                sidebarPanel(
                                 #Selecionando variável dependente
                                 selectInput(inputId = ns("vd"), 
                                             label = "Selecione uma variável dependente:",
                                             choices = sort(names(banco)),
                                             selected = NULL),
                                 #Selecionando variável(is) independente(s)
                                 selectInput(inputId = ns("vi"), 
                                             label = "Selecione uma ou mais variáveis independentes:",
                                             choices = sort(names(banco)),
                                             selected = NULL,
                                             multiple = T),
                                 #Escrevendo a fórmula
                                 textInput(inputId = ns("texto_formula"), 
                                             label = "Escreva a fórmula:"),
                                 #Selecionando modelo de regressão
                                 selectInput(inputId = ns("modelos"), 
                                             label = "Selecione modelo de regressão:",
                                             choices = c(Linear = "gaussian", Logística = "binomial", 
                                                         Gamma = "Gamma", Gaussiana_inversa = "inverse.gaussian",
                                                         Poisson = "poisson", Quasi_normal = "quasi",
                                                         Quasi_binomial = "quasibinomial", Quasi_poisson = "quasipoisson"),
                                             selected = NULL)
                                ),
                              
                                mainPanel(
                                        tabsetPanel(type = "tabs",
                                                tabPanel("Dados",DT::dataTableOutput(ns("dados"))),
                                                tabPanel("Pré-análise", plotOutput(ns("correlograma"),width = 900, height = 600)), 
                                                tabPanel("Resultado",verbatimTextOutput(ns("summary"))),
                                                tabPanel("Diagnóstico",plotOutput(ns("residuos")))
                                                
                                        ) 
                                                
                                )
                        )
                )
        )


}



#################################################################################
#Server
#################################################################################

regressao <- function(input, output, session, banco){
        #Preparando variáveis e banco
        variavel_dependente <- reactive({
                req(input$vd)
                banco[,input$vd]
        })
        
        variavel_independente <- reactive({
                req(input$vi)
                banco[,c(names(banco) %in% input$vi)]
        })
        
        banco_preparado <- reactive({
                cbind(variavel_dependente(), variavel_independente())
        })
        
       
        
        # Pré-análise
        output$correlograma <- renderPlot({
                corrplot.mixed(cor(na.omit(banco_preparado())),
                               lower = "number" , upper = "ellipse")
        })
        
        
        
        
        # Resultado
        
         
        fit <- reactive({
                req(input$modelos)
                
                glm(input$texto_formula , family = input$modelos, data = banco_preparado())
        
        })
        
                       
        
        output$summary <- renderPrint({
 
                summary(fit())
        })
        
        output$residuos <- renderPlot({
                par(mfrow =c(2,2))
                plot(fit())
        })

        # Data output
        output$dados = DT::renderDataTable({
                                DT::datatable(banco_preparado(),
                                rownames = FALSE,
                                editable = FALSE,
                                options = list(lengthMenu = c(10,20, 40, 60, 80, 100), pageLength = 20))
        })

}
