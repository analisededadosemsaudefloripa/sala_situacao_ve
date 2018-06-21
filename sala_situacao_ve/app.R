#
# Sala de Situação da Vigilância Epidemiológica de Florianópolis
#

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

#######################################################################
#Bases de dados
#######################################################################
#######################################################################
##Localização dos CSs
#######################################################################
acesso_dat <- read_csv("bases/dados_cs.csv", 
                       col_types = cols(BIENIO = col_factor(levels = c("2013/2014", 
                                                                       "2015/2016")), DISTRITO = col_factor(levels = c("Centro", 
                                                                                                                        "Continente", "Norte", "Sul"))))

#######################################################################
##Abrangência dos CSs
#######################################################################
#Abrangência CS
#ogrListLayers("bases/Abrangencia_Centros_de_Saude.kml") #Lê as layer do kml
abrangencia <- readOGR(dsn = "bases/Abrangencia_Centros_de_Saude.kml",layer =  "Abrangencia_Centros_de_Saude", encoding="UTF-8")
abrangencia <- spTransform(abrangencia, CRS("+proj=longlat +ellps=GRS80"))

abrangencia$Name <- c("CS Centro",                  "CS Trindade",                "CS Monte Cristo",                      "CS Capoeiras",               "CS Vila Aparecida",          "CS Abraao", 
                      "CS Barra da Lagoa",          "CS Canto da Lagoa",          "CS Costa da Lagoa",  
                      "CS Joao Paulo",              "CS Lagoa da Conceicao",      "CS Pantanal",
                      "CS Monte Serrat",            "CS Saco Grande",             "CS Cachoeira do Bom Jesus",
                      "CS Canasvieiras",            "CS Santinho",                "CS Jurere", 
                      "CS Ponta das Canas",         "CS Ratones",                 "CS Rio Vermelho",
                      "CS Santo Antonio de Lisboa", "CS Vargem Grande",           "CS Prainha",
                      "CS Vargem Pequena",          "CS Alto Ribeirao",           "CS Armacao",  
                      "CS Caieira da Barra do Sul", "CS Campeche",                "CS Carianos", 
                      "CS Costeira do Pirajubae",   "CS Fazenda do Rio Tavares",  "CS Morro das Pedras",
                      "CS Pantano do Sul",          "CS Agronomica",              "CS Ribeirao da Ilha", 
                      "CS Saco dos Limoes",         "CS Tapera",                  "CS Rio Tavares", 
                      "CS Corrego Grande",          "CS Ingleses",                "CS Novo Continente",
                      "CS Itacorubi ",              "CS Coqueiros",               "CS Jardim Atlantico",
                      "CS Sape",                    "CS Coloninha",               "CS Balneario",  
                      "CS Estreito")



#######################################################################
##Estatísticas Vitais e Demografia
#######################################################################
a<-gs_ls() #Lê o que tenho no google drive
#######################################################################
###Nascimentos
#######################################################################
obit_prioritarios <- gs_title('mortalidade_infantil')
gs_ws_ls(obit_prioritarios)
##Nascidos vivos
nascidos_vivos <- gs_read(ss = obit_prioritarios, ws = "NASCIDOS_VIVOS")


#######################################################################
###Óbitos
#######################################################################
##Óbitos materno infantis por semana
#######################################################################
gs_ws_ls(obit_prioritarios)
fetais <- gs_read(ss = obit_prioritarios, ws = "FETAL")
fetais$SEMANA <- as.character(fetais$SEMANA)
fetais <- melt(fetais)
fetais$VARIAVEL <- "fetais"
infantis <- gs_read(ss = obit_prioritarios, ws = "INFANTIL")
infantis$SEMANA <- as.character(infantis$SEMANA)
infantis <- melt(infantis)
infantis$VARIAVEL <- "infantis"
mif <- gs_read(ss = obit_prioritarios, ws = "MIF")
mif$SEMANA <- as.character(mif$SEMANA)
mif <- melt(mif)
mif$VARIAVEL <- "mif"
obt_mat_inf_semana <- rbind(na.omit(fetais), na.omit(infantis), na.omit(mif))
names(obt_mat_inf_semana) <- c("SEMANA", "ANO", "VALOR", "VARIAVEL")
#Transformando em semana-ano (https://stackoverflow.com/questions/45549449/transform-year-week-to-date-object/45587644)
obt_mat_inf_semana$SEMANA <- ifelse(str_length(obt_mat_inf_semana$SEMANA)<2, paste0("0", obt_mat_inf_semana$SEMANA),obt_mat_inf_semana$SEMANA)
obt_mat_inf_semana$SEM_ANO <- paste0(obt_mat_inf_semana$ANO,"-","W",obt_mat_inf_semana$SEMANA,"-7") #7 para começar no domingo
obt_mat_inf_semana$DATA <- ISOweek::ISOweek2date(obt_mat_inf_semana$SEM_ANO)
##Óbitos infantis série histórica
infantis_ser_hist <- gs_read(ss = obit_prioritarios, ws = "SERIE_HISTORICA")
write.csv(obt_mat_inf_semana, "obt_mat_inf_semana.csv")


#######################################################################
##Óbitos materno infantis por semana
#######################################################################
#Banco de teste
sim <- read_csv("bases/ministerio/sim_2006_16.csv")
sim$DTOBITO <- as.Date(sim$DTOBITO, format = "%d%m%Y")
sim$DTNASC <- as.Date(sim$DTNASC, format = "%d%m%Y")
sim$DTOBITO <- as.yearqtr(sim$DTOBITO)
sim$DTOBITO <- as.factor(sim$DTOBITO)


#######################################################################
##Agravos de Notificação
#######################################################################
#######################################################################
###HIV
#######################################################################


#Agravos de Notificação
##HIV
hiv <- read_csv("bases/hiv.csv")
hiv <- melt(hiv)
hiv <- unique(hiv)
names(hiv) <- c("Name", "INDICADOR", "VALOR")
hiv$INDICADOR <- as.character(hiv$INDICADOR)
hiv_cs <- abrangencia

#######################################################################
###Sífilis
#######################################################################








#######################################################################
###Doenças de Notificação Hídrico-alimentar
#######################################################################



#######################################################################
##Agravos Estratégicos
#######################################################################
#######################################################################
###COAP
#######################################################################
indicadores <- gs_title("Indicadores_2018.xlsx")
gs_ws_ls(indicadores)
serie_historica_coap<-gs_read(ss = indicadores, ws ="Série Histórica")
serie_historica_coap <- serie_historica_coap[-c(1,2, 50:52),-c(1:6,41)]
serie_historica_coap <- serie_historica_coap[,c(1, seq(4,34,3))] %>% as.data.frame()
names(serie_historica_coap) <- c("INDICADOR", seq(2007, 2017,1))
serie_historica_coap <- melt(serie_historica_coap,"INDICADOR")
names(serie_historica_coap) <- c("INDICADOR", "ANO", "VALOR")
serie_historica_coap <- lapply(serie_historica_coap, function(x) gsub("%", "", x)) %>% as.data.frame()
serie_historica_coap <- lapply(serie_historica_coap, function(x) gsub(".", "", x, fixed = TRUE)) %>% as.data.frame()
serie_historica_coap <- lapply(serie_historica_coap, function(x) gsub(",", ".", x)) %>% as.data.frame()
serie_historica_coap$VALOR[serie_historica_coap$VALOR == "NR"] <- NA
serie_historica_coap <- na.omit(serie_historica_coap)
serie_historica_coap$VALOR <- as.numeric(serie_historica_coap$VALOR)



#######################################################################
#User Interface
#######################################################################

# Define UI for application that draws a histogram
ui <- shinyUI(navbarPage(shinythemes::themeSelector(),
  title = "Vigilância Epidemiológica de Florianópolis",
  
#######################################################################
##Estatísticas Vitais e Demografia
#######################################################################
  navbarMenu("Estatísticas Vitais e Demografia",
#######################################################################
###Nascimentos
#######################################################################
    tabPanel("Nascimentos"),
#######################################################################
###Óbitos
#######################################################################
    tabPanel("Óbitos",
                fluidPage(
                   # Barra de navegação 
                   sidebarLayout(
                      sidebarPanel(
                         #Selecionando local de residencia ou de ocorrência do óbito
                         selectInput(inputId = "local_sim", 
                                     label = "Selecione o local de referência:",
                                     choices = c("Residência", "Ocorrência"), 
                                     selected = "Residência"),
                         #Selecionando cid
                         textInput(inputId = "cid_sim", 
                                   label = "Insira um CID:"),
                         helpText("Utilizar letra maiuscula",
                                  "sem pontuações ou separadores",
                                  "(Ex: I ou I2 ou I24)"),
                         #Selecionando lambda
                         textInput(inputId = "lambda_sim", 
                                     label = "Transformação de Box-Cox(lambda):",
                                     value = "NULO"),
                         helpText("Se nenhuma transformação for necessária",
                                  "não intruduzir nenhum valor.",
                                  "Lambda = NULO = sem transforação.",
                                  "Lambda = 0 = transformação Logarítimica."),
                         #Selecionando período de previsão
                         numericInput(inputId = "periodo_prev_sim", 
                                     label = "Período desejado para previsão:",
                                     value = 4, 
                                     min = 0),
                         #Resultado da previsão
                         verbatimTextOutput("summary_previsao_sim")
                      ),
                      
                      # Série temporal 
                      mainPanel(
                         #Gráfico da série temporal
                         plotOutput(outputId = "serie_cid_sim", width = "100%", height = 660),
                         plotOutput(outputId = "residuos_cid_sim", width = "100%", height = 660),
                         plotOutput(outputId = "decomposicao_cid_sim", width = "100%", height = 660),
                         plotOutput(outputId = "sazonal_cid1_sim", width = "100%", height = 330),
                         plotOutput(outputId = "sazonal_cid2_sim", width = "100%", height = 330)
              )
            )
          )
        ),
#######################################################################
###Demografia
#######################################################################
    tabPanel("Demografia")),
#######################################################################
##Agravos de Notificação
#######################################################################
  navbarMenu("Agravos de Notificação",
#######################################################################
###HIV
#######################################################################
    tabPanel("HIV",
                  fluidPage(
                   # Barra de navegação 
                   sidebarLayout(
                      sidebarPanel(
                         #Selecionando Indicadores
                         selectInput(inputId = "hiv_indicador", 
                                     label = "Selecione um indicadores:",
                                     choices = sort(unique(hiv$INDICADOR)),
                                     selected = "Aguardando_Investigacao"),
                         #Selecionando se dados aparecerão ou não
                         checkboxInput(inputId =  "hiv_dados", 
                                    label = "Mostrar os Dados:", 
                                    value = TRUE),
                         #Tabela
                         DT::dataTableOutput(outputId = "hiv_table")
                      ),
                      
                      # Mapa
                      mainPanel(
                         #Mapa
                         leafletOutput(outputId="hiv_map", width = "100%", height = 660))
             )
           )
         ),
#######################################################################
###Tuberculose
#######################################################################
    tabPanel("Tuberculose"),
#######################################################################
###Sífilis
#######################################################################
    tabPanel("Sífilis"),
#######################################################################
###Hepatites virais
#######################################################################
    tabPanel("Hepatites Virais"),
#######################################################################
###Tuberculose
#######################################################################
    tabPanel("Tuberculose"),
#######################################################################
###Doencas transmitidas pelo Aedes
#######################################################################
    tabPanel("Doencas transmitidas pelo Aedes"),
#######################################################################
###Leishmaniose Viceral
#######################################################################
    tabPanel("Leishmaniose Viceral"),
#######################################################################
###Doenças de Veiculação Hídrico-alimentar
#######################################################################
    tabPanel("Doenças de Veiculação Hídrico-alimentar")),
#######################################################################
##Agravos Estratégicos
#######################################################################
  navbarMenu("Agravos Estratégicos",
#######################################################################
###Trânsito
#######################################################################
    tabPanel("Trânsito"),
#######################################################################
###Inicadores COAP
#######################################################################
    tabPanel("Inicadores COAP",
                fluidPage(
                   # Barra de navegação 
                   sidebarLayout(
                      sidebarPanel(
                         #Selecionando Indicadores
                         selectInput(inputId = "indicador", 
                                     label = "Selecione um indicador:",
                                     choices = sort(unique(serie_historica_coap$INDICADOR)),
                                     selected = FALSE),
                         #Selecionando se dados aparecerão ou não
                         checkboxInput(inputId =  "mostrar_dados_coap", 
                                    label = "Mostrar os Dados:", 
                                    value = TRUE),
                         #Tabela
                         DT::dataTableOutput(outputId = "indica_coap_table")
                      ),
                      
                      # Série temporal dos inidicadores coap
                      mainPanel(
                         #Gráfico da série temporal
                         plotlyOutput(outputId = "indica_coap_plot", width = "100%", height = 660)))))
  )
 )
)
#############################################################################
#Server
#############################################################################
server <- function(input, output) {
#######################################################################
##Estatísticas Vitais e Demografia
#######################################################################
  
#######################################################################
###Nascimentos
#######################################################################
  
#######################################################################
###Óbitos
#######################################################################
# SIM
sim_preparado<- reactive({
        req(input$local_sim)
        ifelse((input$local_sim == "Residência"), sim_floripa <- subset(sim, sim$CODMUNRES == "420540"), sim_floripa <- subset(sim, sim$CODMUNOCOR == "420540") )      
        req(input$cid_sim)
        sim_floripa$CAUSABAS <- substring(sim_floripa$CAUSABAS, 0,as.numeric(nchar(input$cid_sim)))
        sim_floripa <- subset(sim_floripa, sim_floripa$CAUSABAS == input$cid_sim)
        sim_floripa  <- sim_floripa $DTOBITO %>% as.data.frame()
        sim_floripa  <- table(sim_floripa ) %>% as.data.frame()
        sim_floripa  <- ts(sim_floripa [,-1],start = 2006,frequency = 4)
        sim_floripa 
        })       
        
        
periodo_prev_sim <-reactive({
        req(input$periodo_prev_sim)
})

lambda <-reactive({
        req(input$lambda_sim)
})

sim_prev <-reactive({
        #Função para ETS
        ifelse(input$lambda_sim == "NULO",
        funcets <- ets(sim_preparado(), lambda = NULL),
        funcets <- ets(sim_preparado(), lambda = input$lambda_sim))
        fets <- function(x, h) {
          forecast(funcets, h = 1)
        }
        
        ##Função para ARIMA
        ifelse(input$lambda_sim == "NULO",
        funcarima<- auto.arima(sim_preparado(), lambda = NULL),
        funcarima<- auto.arima(sim_preparado(), lambda = input$lambda_sim))
        farima <- function(x, h) {
          forecast(funcarima, h = 1)
        }
        
        ## Compute CV errors for ETS as e1
        e1 <- tsCV(sim_preparado(), fets, h=1)
        
        ## Compute CV errors for ARIMA as e2
        e2 <- tsCV(sim_preparado(), farima, h=1)
        
        ## Find MSE of each model class
        g<-mean(e1^2, na.rm=TRUE)
        h<-mean(e2^2, na.rm=TRUE)
        
        ifelse(g > h, fit_banco <- funcets , fit_banco <- funcarima)
        "Intervalo de Confiança"
        forecast(fit_banco, periodo_prev_sim(), level = c(50,80,95))
})

#Gráfico com previsão
output$serie_cid_sim <- renderPlot({
autoplot(sim_prev())+
        xlab("Ano") +
        ylab("Óbitos")+
        ggtitle("Série Temporal de Óbitos - Trimestral")
})

#Gráfico dos resíduos
output$residuos_cid_sim <- renderPlot({
checkresiduals(sim_prev())
})


#Sumário
output$summary_previsao_sim <- renderPrint({
        summary(sim_prev())
        checkresiduals(sim_prev())
          })


#Gráfico dos resíduos
output$residuos_cid_sim <- renderPlot({
checkresiduals(sim_prev())
})

#Gráfico de decomposição 
output$decomposicao_cid_sim <- renderPlot({
##Decomposição por STL
stl(sim_preparado(), s.window="periodic", robust=TRUE) %>% autoplot()
})


#Gráfico de sazolnalidade 
output$sazonal_cid1_sim <- renderPlot({
ggsubseriesplot(sim_preparado())+
  ylab("Óbitos")
})

#Gráfico de sazolnalidade 
output$sazonal_cid2_sim <- renderPlot({
ggseasonplot(sim_preparado(), polar=TRUE)+
  ylab("Óbitos")
})


#######################################################################
###Demografia
#######################################################################
   

#######################################################################
##Agravos de Notificação
#######################################################################
#######################################################################
###HIV
#######################################################################
#Mapa hiv
hiv_select <- reactive({
sp::merge(x = hiv_cs, 
      y = (subset(hiv, INDICADOR == input$hiv_indicador)),  
      by.x = "Name", by.y = "Name")
})

colorpal <- reactive({
colorNumeric("YlOrRd", domain = hiv_select()@data$VALOR)
})


output$hiv_map <- renderLeaflet({

pal <- colorpal()

labels <- sprintf(
"<strong>%s</strong><br/>Valor: %g",
hiv_select()@data$Name, hiv_select()@data$VALOR
) %>% lapply(htmltools::HTML)

leaflet(data = hiv_select()) %>% 
         addProviderTiles("Esri.WorldImagery")%>% 
         setView(lng =-48.47 , lat=-27.6,zoom=10.5) %>%
         clearShapes() %>%
         addPolygons(fillColor = ~pal(hiv_select()@data$VALOR),
             weight = 2,
             opacity = 1,
             color = "white",
             dashArray = "3",
             fillOpacity = 0.7,
             popup = hiv_cs@data$Name,
             highlight = highlightOptions(
                            weight = 5,
                            color = "#666",
                            dashArray = "",
                            fillOpacity = 0.7,
                            bringToFront = TRUE),
             label = labels,
                     labelOptions = labelOptions(
                     style = list("font-weight" = "normal", padding = "3px 8px"),
                     textsize = "15px",
                     direction = "auto"))%>% 
        addLegend(pal = pal, values = ~hiv_select()@data$VALOR, opacity = 0.7, title = NULL,
position = "bottomright")
})




output$hiv_table <- DT::renderDataTable({
     if(input$hiv_dados){
     DT::datatable(data = hiv_select()@data[,-c(2,3)],
             rownames = FALSE,
             editable = FALSE,
             options = list(lengthMenu = c(10,20, 40, 60, 80, 100), pageLength = 10))}
}) 


#######################################################################
###Tuberculose
#######################################################################

 
#######################################################################
###Sífilis
#######################################################################


#######################################################################
###Hepatites virais
#######################################################################

 
#######################################################################
###Tuberculose
#######################################################################

 
#######################################################################
###Doencas transmitidas pelo Aedes
#######################################################################


#######################################################################
###Leishmaniose Viceral
#######################################################################


#######################################################################
###Doenças de Veiculação Hídrico-alimentar
#######################################################################


#######################################################################
##Agravos Estratégicos
#######################################################################


             
#######################################################################
###Trânsito
#######################################################################


#######################################################################
###Inicadores COAP
#######################################################################

# COAP
serie_historica_coap_select <- reactive({
req(input$indicador)
subset(serie_historica_coap, INDICADOR == input$indicador)
})



output$indica_coap_plot <- renderPlotly({

a <- ggplot(serie_historica_coap_select(), aes(ANO, VALOR, group = INDICADOR))+
    geom_line(color = "red")+
    geom_smooth(alpha = 0.3, fill = "blue", level = 0.90, method = "auto")+
    theme_classic()

ggplotly(a)

})


output$indica_coap_table <- DT::renderDataTable({
     if(input$mostrar_dados_coap){
     DT::datatable(data = serie_historica_coap_select(),
             rownames = FALSE,
             editable = FALSE,
             options = list(lengthMenu = c(10,20, 40, 60, 80, 100), pageLength = 10))}
})
       
    
}



# Run the application 
shinyApp(ui = ui, server = server)

