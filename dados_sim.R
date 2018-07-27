# Sala de Situação da Vigilância Epidemiológica de Florianópolis
options("scipen" = 100, "digits"=2) #Não colocar notação científica
#######################################################################
#Bibliotecas
#######################################################################
library(readr)
library(lubridate)
library(zoo)
library(janitor)#converte data numérica do excel para data do R
#######################################################################
#Bases de dados
#######################################################################

#######################################################################
#Bases de Florianópolis - possuí bairro e dados mais atualizados
#######################################################################

sim_2006_2018 <- read_csv("obitos/DO23072018.csv")
sim <- sim_2006_2018[,c(8,15:21,23,25,26,33,34,35,37,38,44:57,59:61,66:71,132:135,138:140)]
sim$DTOBITO <- ifelse(nchar(sim$DTOBITO) == 7, paste0(0,sim$DTOBITO), sim$DTOBITO)
sim$DTOBITO <- as.Date(sim$DTOBITO, format = "%d%m%Y")
sim$DTNASC <- ifelse(nchar(sim$DTNASC) == 7, paste0(0,sim$DTNASC), sim$DTNASC)
sim$DTNASC <- as.Date(sim$DTNASC, format = "%d%m%Y")
sim$IDADE <- round((sim$DTOBITO - sim$DTNASC)/365,3)
sim$DTOBITO_TRI <- as.yearqtr(sim$DTOBITO)
sim$DTOBITO_TRI <- as.factor(sim$DTOBITO_TRI)
colnames(sim)[which(names(sim) == "CAUSABAS")] <- "CID" #O nome foi trocado, para que possa ser utilizado no modulo_cid_residencia
sim$IDADE <- as.numeric(sim$IDADE)
sim$SEXO <- as.character(sim$SEXO)
sim$SEXO[which(sim$SEXO == "I")] <- NA
sim$SEXO <- as.factor(sim$SEXO)
sim$RACACOR <- as.factor(sim$RACACOR)
sim$ESTCIV <- as.factor(sim$ESTCIV)
sim$ESC <- as.factor(sim$ESC)
sim$ESC2010 <- as.factor(sim$ESC2010)
sim$OCUP <- as.factor(sim$OCUP)
sim$CODMUNRES <- as.character(sim$CODMUNRES)
sim$CODMUNOCOR <- as.character(sim$CODMUNOCOR)
sim$CID <- as.character(sim$CID)

