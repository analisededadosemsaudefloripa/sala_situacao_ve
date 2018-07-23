# Sala de Situação da Vigilância Epidemiológica de Florianópolis
options("scipen" = 100, "digits"=2) #Não colocar notação científica
#######################################################################
#Bibliotecas
#######################################################################
library(readr)
library(tidyverse)

#######################################################################
#Bases de dados
#######################################################################
#######################################################################
#Bases de dados de abrangência e localização dos CS
#######################################################################
source("dados_localizacao_abrangencia_cs.R")
#######################################################################
#Bases de dados de notificação de sífilis retirados do INFO
#######################################################################
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

