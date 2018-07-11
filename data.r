# Sala de Situação da Vigilância Epidemiológica de Florianópolis
options("scipen" = 100, "digits"=2) #Não colocar notação científica
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
library(shinyWidgets)

#######################################################################
#Bases de dados
#######################################################################
#Abrangência CS
#ogrListLayers("dados_cs/Abrangencia_Centros_de_Saude.kml") #Lê as layer do kml
abrangencia_cs <- readOGR(dsn = "dados_cs/Abrangencia_Centros_de_Saude.kml",layer =  "Abrangencia_Centros_de_Saude", encoding="UTF-8")
abrangencia_cs <- spTransform(abrangencia_cs, CRS("+proj=longlat +ellps=GRS80"))

abrangencia_cs$Name <- c("CS Centro",               "CS Trindade",                "CS Monte Cristo",                      
                      "CS Capoeiras",               "CS Vila Aparecida",          "CS Abraao", 
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



abrangencia_cs$COD <- c("cs.14","cs.46","cs.28","cs.12","cs.49","cs.01","cs.06","cs.11",
                        "cs.18","cs.25","cs.27","cs.32","cs.29","cs.41","cs.07","cs.10",
                        "cs.42","cs.26","cs.34","cs.36","cs.39","cs.43","cs.47","cs.35",
                        "cs.48","cs.03","cs.04","cs.08","cs.09","cs.13","cs.19","cs.21",
                        "cs.30","cs.33","cs.02","cs.37","cs.40","cs.45","cs.38","cs.17",
                        "cs.22","cs.31","cs.23","cs.16","cs.24","cs.44","cs.15","cs.05",
                        "cs.20") #O código foi colocado em ordem alfabética

#Tabela de notificações, tratamento e teste rápido
sifilis_cs <- read_csv("sifilis/bases/transformadas/sifilis_cs.csv", 
    col_types = cols(VALOR = col_double()))
sifilis_cs <- sifilis_cs[order(sifilis_cs$UNIDADE),]
unidades <- sifilis_cs[,2] %>% unique()
unidades$COD <- c("cs.01","cs.02","cs.03","cs.04","cs.05","cs.06","cs.07","cs.08",
                  "cs.09","cs.10","cs.11","cs.12","cs.13","cs.14","cs.15","cs.16",
                  "cs.17","cs.18","cs.19","cs.20","cs.21","cs.22","cs.23","cs.24",
                  "cs.25","cs.26","cs.27","cs.28","cs.29","cs.30","cs.31","cs.32",
                  "cs.33","cs.34","cs.35","cs.36","cs.37","cs.38","cs.39","cs.40",
                  "cs.41","cs.42","cs.43","cs.44","cs.45","cs.46","cs.47","cs.48",
                  "cs.49") #O código foi colocado em ordem alfabética


trimestre <- sifilis_cs[,3] %>% unique()%>% as.data.frame()
tipo <- sifilis_cs[,4] %>% unique()%>% as.data.frame()
unidades <- merge(unidades, trimestre, all = T)
unidades <- merge(unidades, tipo, all = T)
sifilis_cs <- merge(unidades,sifilis_cs, by = c("UNIDADE", "TRIMESTRE", "TIPO"), all = T)
sifilis_cs$UNIDADE <- NULL

testes_rapidos_cs <- read_csv("sifilis/bases/transformadas/testes_rapidos_sifilis_cs.csv", 
    col_types = cols(PROCEDIMENTO = col_character()))
testes_rapidos_cs <-testes_rapidos_cs [,-1]
colnames(testes_rapidos_cs)[4] <- "TIPO"
trimestrea <- testes_rapidos_cs[,3] %>% unique()%>% as.data.frame()
tipoa <- testes_rapidos_cs[,4] %>% unique()%>% as.data.frame()
unidadesa <- merge(unidades[,-4], trimestrea, all = T)
unidadesa <- merge(unidadesa, tipoa, all = T)
testes_rapidos_cs <- merge(unidadesa,testes_rapidos_cs, by = c("UNIDADE", "TRIMESTRE", "TIPO"), all = T)
testes_rapidos_cs$UNIDADE <- NULL



banco_sifilis_cs <-rbind(sifilis_cs,testes_rapidos_cs)
banco_sifilis_cs$TRIMESTRE <- as.character(banco_sifilis_cs$TRIMESTRE) 
banco_sifilis_cs$VALOR <- round(as.numeric(banco_sifilis_cs$VALOR),2)




sifilis_florianopolis <- read_csv("sifilis/bases/transformadas/sifilis_florianopolis.csv", 
    col_types = cols(VALOR = col_double()))
testes_rapidos_florianopolis <- read_csv("sifilis/bases/transformadas/testes_rapidos_sifilis_florianopolis.csv", 
    col_types = cols(PROCEDIMENTO = col_character())) 
testes_rapidos_florianopolis <- testes_rapidos_florianopolis[,-1]
colnames(testes_rapidos_florianopolis)[2] <- "TIPO"
banco_sifilis_florianopolis <- rbind(sifilis_florianopolis,testes_rapidos_florianopolis)
banco_sifilis_florianopolis$VALOR <- round(as.numeric(banco_sifilis_florianopolis$VALOR),2)

