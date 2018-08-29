# Sala de Situação da Vigilância Epidemiológica de Florianópolis
options("scipen" = 100, "digits"=2) #Não colocar notação científica
#######################################################################
#Bibliotecas
#######################################################################
library(readr)
library(lubridate)
library(zoo)
library(foreign)#Ler dbf
library(tidyverse)
#######################################################################
#Bases de dados
#######################################################################

#######################################################################
#Bases de SINAN AIDS ADULTO- Florianópolis - possuí área dos cs 
#######################################################################

sinan_aidsA <- read.dbf("sinan/Banco_sinan/AIDSANET_GEO.DBF")
sinan_aidsA <- sinan_aidsA[,-c(1:4,6:8,10,11,13,15,16,21,22,24:53,67:68,107:109,115,116,118:124)]
sinan_aidsA$DT_NOTIFIC <- as.character(sinan_aidsA$DT_NOTIFIC)
sinan_aidsA$DT_NOTIFIC <- as.Date(sinan_aidsA$DT_NOTIFIC, format = "%m/%d/%Y")
sinan_aidsA$DT_DIAG <- as.character(sinan_aidsA$DT_DIAG)
sinan_aidsA$DT_DIAG <- as.Date(sinan_aidsA$DT_DIAG, format = "%m/%d/%Y")
sinan_aidsA$DT_CONFIRM <- as.character(sinan_aidsA$DT_CONFIRM)
sinan_aidsA$DT_CONFIRM <- as.Date(sinan_aidsA$DT_CONFIRM, format = "%m/%d/%Y")
sinan_aidsA$DT_NASC <- as.character(sinan_aidsA$DT_NASC)
sinan_aidsA$DT_NASC <- as.Date(sinan_aidsA$DT_NASC, format = "%m/%d/%Y")
sinan_aidsA$DT_RAPIDO <- as.character(sinan_aidsA$DT_RAPIDO)
sinan_aidsA$DT_RAPIDO <- as.Date(sinan_aidsA$DT_RAPIDO, format = "%m/%d/%Y")
sinan_aidsA$DT_OBITO <- as.character(sinan_aidsA$DT_OBITO)
sinan_aidsA$DT_OBITO <- as.Date(sinan_aidsA$DT_OBITO, format = "%m/%d/%Y")
sinan_aidsA$IDADE_NOTIFIC <- round((sinan_aidsA$DT_NOTIFIC - sinan_aidsA$DT_NASC)/365,3)
sinan_aidsA$IDADE_NOTIFIC <- as.numeric(sinan_aidsA$IDADE_NOTIFIC)
sinan_aidsA$IDADE_DIAG <- round((sinan_aidsA$DT_DIAG - sinan_aidsA$DT_NASC)/365,3)
sinan_aidsA$IDADE_DIAG <- as.numeric(sinan_aidsA$IDADE_DIAG)
sinan_aidsA$IDADE_OBITO <- round((sinan_aidsA$DT_OBITO - sinan_aidsA$DT_NASC)/365,3)
sinan_aidsA$IDADE_OBITO <- as.numeric(sinan_aidsA$IDADE_OBITO)
sinan_aidsA$DT_TRI <- as.yearqtr(sinan_aidsA$DT_DIAG)
sinan_aidsA$DT_TRI <- as.factor(sinan_aidsA$DT_TRI)
sinan_aidsA$CS_SEXO <- as.character(sinan_aidsA$CS_SEXO)
sinan_aidsA$CS_SEXO[which(sinan_aidsA$CS_SEXO == "I")] <- NA
sinan_aidsA$CS_SEXO <- as.factor(sinan_aidsA$CS_SEXO)
sinan_aidsA$CS_RACA <- as.factor(sinan_aidsA$CS_RACA)
sinan_aidsA$CS_ESCOL_N <- as.factor(sinan_aidsA$CS_ESCOL_N)
sinan_aidsA$ID_OCUPA_N <- as.factor(sinan_aidsA$ID_OCUPA_N)
sinan_aidsA$CODMUNRES <- as.character(sinan_aidsA$CODMUNRES)
sinan_aidsA$CODMUNOCOR <- as.character(sinan_aidsA$ID_MUNICIP) #Município de notificação
sinan_aidsA$UNIDADE <- as.character(sinan_aidsA$UNIDADE)
sinan_aidsA$ANO_DIAG <- substr(sinan_aidsA$DT_DIAG, 0, 4) %>% as.numeric()
sinan_aidsA <- subset(sinan_aidsA, sinan_aidsA$ANO_DIAG >= 2006 )
sinan_aidsA$ANO_DIAG <- NULL
sinan_aidsA$UNIDADE <- as.integer(sinan_aidsA$UNIDADE) 
sinan_aidsA <- tbl_df(sinan_aidsA)

#######################################################################
#Bases de SINAN AIDS GESTANTE - Florianópolis - possuí área dos cs 
#######################################################################

sinan_aidsG <- read.dbf("sinan/Banco_sinan/HIVGENET_GEO.DBF")
sinan_aidsG <- sinan_aidsG[,-c(1:3,5,6,8,9,11,13,14,19,20,22:38,43,44:50,56:68,70:76)]
sinan_aidsG$DT_NOTIFIC <- as.character(sinan_aidsG$DT_NOTIFIC)
sinan_aidsG$DT_NOTIFIC <- as.Date(sinan_aidsG$DT_NOTIFIC, format = "%m/%d/%Y")
sinan_aidsG$DT_DIAG <- as.character(sinan_aidsG$DT_DIAG)
sinan_aidsG$DT_DIAG <- as.Date(sinan_aidsG$DT_DIAG, format = "%m/%d/%Y")
sinan_aidsG$DT_NASC <- as.character(sinan_aidsG$DT_NASC)
sinan_aidsG$DT_NASC <- as.Date(sinan_aidsG$DT_NASC, format = "%m/%d/%Y")
sinan_aidsG$IDADE_NOTIFIC <- round((sinan_aidsG$DT_NOTIFIC - sinan_aidsG$DT_NASC)/365,3)
sinan_aidsG$IDADE_NOTIFIC <- as.numeric(sinan_aidsG$IDADE_NOTIFIC)
sinan_aidsG$IDADE_DIAG <- round((sinan_aidsG$DT_DIAG - sinan_aidsG$DT_NASC)/365,3)
sinan_aidsG$IDADE_DIAG <- as.numeric(sinan_aidsG$IDADE_DIAG)
sinan_aidsG$DT_TRI <- as.yearqtr(sinan_aidsG$DT_DIAG)
sinan_aidsG$DT_TRI <- as.factor(sinan_aidsG$DT_TRI)
sinan_aidsG$CS_SEXO <- as.character(sinan_aidsG$CS_SEXO)
sinan_aidsG$CS_SEXO[which(sinan_aidsG$CS_SEXO == "I")] <- NA
sinan_aidsG$CS_SEXO <- as.factor(sinan_aidsG$CS_SEXO)
sinan_aidsG$CS_RACA <- as.factor(sinan_aidsG$CS_RACA)
sinan_aidsG$CS_ESCOL_N <- as.factor(sinan_aidsG$CS_ESCOL_N)
sinan_aidsG$ID_OCUPA_N <- as.factor(sinan_aidsG$ID_OCUPA_N)
sinan_aidsG$CODMUNRES <- as.character(sinan_aidsG$CODMUNRES)
sinan_aidsG$CODMUNOCOR <- as.character(sinan_aidsG$ID_MUNICIP) #Município de notificação
sinan_aidsG$UNIDADE <- as.character(sinan_aidsG$UNIDADE)
sinan_aidsG$ANO_DIAG <- substr(sinan_aidsG$DT_DIAG, 0, 4) %>% as.numeric()
sinan_aidsG <- subset(sinan_aidsG, sinan_aidsG$ANO_DIAG >= 2006 )
sinan_aidsG$ANO_DIAG <- NULL
sinan_aidsG$UNIDADE <- as.integer(sinan_aidsG$UNIDADE) 
sinan_aidsG <- tbl_df(sinan_aidsG)

#######################################################################
#Bases de SINAN SIFILIS - Florianópolis - possuí área dos cs 
#######################################################################

sinan_sifilis <- read.dbf("sinan/Banco_sinan/SIFILIS_GEO.DBF")
sinan_sifilis <- sinan_sifilis[,-c(1:6,8:10,12,13,15,17,18,23,24,26:44,49:51, 52:54, 59:76,78,84)]
sinan_sifilis$DT_NOTIFIC <- as.character(sinan_sifilis$DT_NOTIFIC)
sinan_sifilis$DT_NOTIFIC <- as.Date(sinan_sifilis$DT_NOTIFIC, format = "%m/%d/%Y")
sinan_sifilis$DT_SIN_PRI <- as.character(sinan_sifilis$DT_SIN_PRI) #Data da semana dos primeiros sintomas
sinan_sifilis$DT_SIN_PRI <- as.Date(sinan_sifilis$DT_SIN_PRI, format = "%m/%d/%Y")
sinan_sifilis$DT_NASC <- as.character(sinan_sifilis$DT_NASC)
sinan_sifilis$DT_NASC <- as.Date(sinan_sifilis$DT_NASC, format = "%m/%d/%Y")
sinan_sifilis$DT_OBITO <- as.character(sinan_sifilis$DT_OBITO)
sinan_sifilis$DT_OBITO <- as.Date(sinan_sifilis$DT_OBITO, format = "%m/%d/%Y")
sinan_sifilis$DT_ENCERRA <- as.character(sinan_sifilis$DT_ENCERRA)
sinan_sifilis$DT_ENCERRA <- as.Date(sinan_sifilis$DT_ENCERRA, format = "%m/%d/%Y")
sinan_sifilis$IDADE_NOTIFIC <- round((sinan_sifilis$DT_NOTIFIC - sinan_sifilis$DT_NASC)/365,3)
sinan_sifilis$IDADE_NOTIFIC <- as.numeric(sinan_sifilis$IDADE_NOTIFIC)
sinan_sifilis$IDADE_SIN_PRI <- round((sinan_sifilis$DT_SIN_PRI - sinan_sifilis$DT_NASC)/365,3)
sinan_sifilis$IDADE_SIN_PRI <- as.numeric(sinan_sifilis$IDADE_SIN_PRI)
sinan_sifilis$DT_TRI <- as.yearqtr(sinan_sifilis$DT_SIN_PRI)
sinan_sifilis$DT_TRI <- as.factor(sinan_sifilis$DT_TRI)
sinan_sifilis$CS_SEXO <- as.character(sinan_sifilis$CS_SEXO)
sinan_sifilis$CS_SEXO[which(sinan_sifilis$CS_SEXO == "I")] <- NA
sinan_sifilis$CS_SEXO <- as.factor(sinan_sifilis$CS_SEXO)
sinan_sifilis$CS_RACA <- as.factor(sinan_sifilis$CS_RACA)
sinan_sifilis$CS_ESCOL_N <- as.factor(sinan_sifilis$CS_ESCOL_N)
sinan_sifilis$ID_OCUPA_N <- as.factor(sinan_sifilis$ID_OCUPA_N)
sinan_sifilis$CODMUNRES <- as.character(sinan_sifilis$CODMUNRES)
sinan_sifilis$CODMUNOCOR <- as.character(sinan_sifilis$ID_MUNICIP) #Município de notificação
sinan_sifilis$UNIDADE <- as.character(sinan_sifilis$UNIDADE)
sinan_sifilis$ANO_SIN_PRI <- substr(sinan_sifilis$DT_SIN_PRI, 0, 4) %>% as.numeric()
sinan_sifilis <- subset(sinan_sifilis, sinan_sifilis$ANO_SIN_PRI >= 2006 )
sinan_sifilis$ANO_DIAG <- NULL
sinan_sifilis$UNIDADE <- as.integer(sinan_sifilis$UNIDADE) 
sinan_sifilis <- tbl_df(sinan_sifilis)

#######################################################################
#Bases de SINAN SIFILIS GESTANTES - Florianópolis - possuí área dos cs 
#######################################################################

sinan_sifilisG <- read.dbf("sinan/Banco_sinan/SIFGENET_GEO.DBF")
sinan_sifilisG <- sinan_sifilisG[,-c(1:3,5,6,8,9,11,13,14,15,19,20,22:38,40:43,54:68,70:76)]
sinan_sifilisG$DT_NOTIFIC <- as.character(sinan_sifilisG$DT_NOTIFIC)
sinan_sifilisG$DT_NOTIFIC <- as.Date(sinan_sifilisG$DT_NOTIFIC, format = "%m/%d/%Y")
sinan_sifilisG$DT_DIAG <- as.character(sinan_sifilisG$DT_DIAG)
sinan_sifilisG$DT_DIAG <- as.Date(sinan_sifilisG$DT_DIAG, format = "%m/%d/%Y")
sinan_sifilisG$DT_NASC <- as.character(sinan_sifilisG$DT_NASC)
sinan_sifilisG$DT_NASC <- as.Date(sinan_sifilisG$DT_NASC, format = "%m/%d/%Y")
sinan_sifilisG$IDADE_NOTIFIC <- round((sinan_sifilisG$DT_NOTIFIC - sinan_sifilisG$DT_NASC)/365,3)
sinan_sifilisG$IDADE_NOTIFIC <- as.numeric(sinan_sifilisG$IDADE_NOTIFIC)
sinan_sifilisG$IDADE_DIAG <- round((sinan_sifilisG$DT_DIAG - sinan_sifilisG$DT_NASC)/365,3)
sinan_sifilisG$IDADE_DIAG <- as.numeric(sinan_sifilisG$IDADE_DIAG)
sinan_sifilisG$DT_TRI <- as.yearqtr(sinan_sifilisG$DT_DIAG)
sinan_sifilisG$DT_TRI <- as.factor(sinan_sifilisG$DT_TRI)
sinan_sifilisG$CS_RACA <- as.factor(sinan_sifilisG$CS_RACA)
sinan_sifilisG$CS_ESCOL_N <- as.factor(sinan_sifilisG$CS_ESCOL_N)
sinan_sifilisG$ID_OCUPA_N_OCUP <- as.factor(sinan_sifilisG$ID_OCUPA_N)
sinan_sifilisG$CODMUNRES <- as.character(sinan_sifilisG$CODMUNRES)
sinan_sifilisG$CODMUNOCOR <- as.character(sinan_sifilisG$ID_MUNICIP) #Município de notificação
sinan_sifilisG$UNIDADE <- as.character(sinan_sifilisG$UNIDADE)
sinan_sifilisG$ANO_DIAG <- substr(sinan_sifilisG$DT_DIAG, 0, 4) %>% as.numeric()
sinan_sifilisG <- subset(sinan_sifilisG, sinan_sifilisG$ANO_DIAG >= 2006 )
sinan_sifilisG$ANO_DIAG <- NULL
sinan_sifilisG$UNIDADE <- as.integer(sinan_sifilisG$UNIDADE) 
sinan_sifilisG <- tbl_df(sinan_sifilisG)


#######################################################################
#Bases de SINAN SIFILIS CRIANÇA - Florianópolis - possuí área dos cs 
#######################################################################

sinan_sifilisC <- read.dbf("sinan/Banco_sinan/SIFICNET_GEO.DBF")
sinan_sifilisC <- sinan_sifilisC[,-c(1:5,7,8,10,11,13,15,16,21,22,24:40,46,47,51,54,56,58,61,63,66,72,76,87,88:101,103:109)]
sinan_sifilisC$DT_NOTIFIC <- as.character(sinan_sifilisC$DT_NOTIFIC)
sinan_sifilisC$DT_NOTIFIC <- as.Date(sinan_sifilisC$DT_NOTIFIC, format = "%m/%d/%Y")
sinan_sifilisC$DT_DIAG <- as.character(sinan_sifilisC$DT_DIAG)
sinan_sifilisC$DT_DIAG <- as.Date(sinan_sifilisC$DT_DIAG, format = "%m/%d/%Y")
sinan_sifilisC$DT_NASC <- as.character(sinan_sifilisC$DT_NASC)
sinan_sifilisC$DT_NASC <- as.Date(sinan_sifilisC$DT_NASC, format = "%m/%d/%Y")
sinan_sifilisC$IDADE_NOTIFIC <- round((sinan_sifilisC$DT_NOTIFIC - sinan_sifilisC$DT_NASC)/365,3)
sinan_sifilisC$IDADE_NOTIFIC <- as.numeric(sinan_sifilisC$IDADE_NOTIFIC)
sinan_sifilisC$IDADE_DIAG <- round((sinan_sifilisC$DT_DIAG - sinan_sifilisC$DT_NASC)/365,3)
sinan_sifilisC$IDADE_DIAG <- as.numeric(sinan_sifilisC$IDADE_DIAG)
sinan_sifilisC$DT_TRI <- as.yearqtr(sinan_sifilisC$DT_DIAG)
sinan_sifilisC$DT_TRI <- as.factor(sinan_sifilisC$DT_TRI)
sinan_sifilisC$CS_RACA <- as.factor(sinan_sifilisC$CS_RACA)
sinan_sifilisC$CS_ESCOL_N <- as.factor(sinan_sifilisC$CS_ESCOL_N)
sinan_sifilisC$ID_OCUPA_N_OCUP <- as.factor(sinan_sifilisC$ID_OCUPA_N)
sinan_sifilisC$CODMUNRES <- as.character(sinan_sifilisC$CODMUNRES)
sinan_sifilisC$CODMUNOCOR <- as.character(sinan_sifilisC$ID_MUNICIP) #Município de notificação
sinan_sifilisC$UNIDADE <- as.character(sinan_sifilisC$UNIDADE)
sinan_sifilisC$ANO_DIAG <- substr(sinan_sifilisC$DT_DIAG, 0, 4) %>% as.numeric()
sinan_sifilisC <- subset(sinan_sifilisC, sinan_sifilisC$ANO_DIAG >= 2006 )
sinan_sifilisC$ANO_DIAG <- NULL
sinan_sifilisC$UNIDADE <- as.integer(sinan_sifilisC$UNIDADE) 
sinan_sifilisC <- tbl_df(sinan_sifilisC)

#######################################################################
#Bases de SINAN TUBERCULOSE - Florianópolis - possuí área dos cs 
#######################################################################

sinan_tuberculose <- read.dbf("sinan/Banco_sinan/TUBENET_GEO.DBF")
sinan_tuberculose <- sinan_tuberculose[,-c(1:3,5,6,8,9,12,13,18,19,21:52,82,87,88,91:97)]
sinan_tuberculose$DT_NOTIFIC <- as.character(sinan_tuberculose$DT_NOTIFIC)
sinan_tuberculose$DT_NOTIFIC <- as.Date(sinan_tuberculose$DT_NOTIFIC, format = "%m/%d/%Y")
sinan_tuberculose$DT_DIAG <- as.character(sinan_tuberculose$DT_DIAG)
sinan_tuberculose$DT_DIAG <- as.Date(sinan_tuberculose$DT_DIAG, format = "%m/%d/%Y")
sinan_tuberculose$DT_NASC <- as.character(sinan_tuberculose$DT_NASC)
sinan_tuberculose$DT_NASC <- as.Date(sinan_tuberculose$DT_NASC, format = "%m/%d/%Y")
sinan_tuberculose$IDADE_NOTIFIC <- round((sinan_tuberculose$DT_NOTIFIC - sinan_tuberculose$DT_NASC)/365,3)
sinan_tuberculose$IDADE_NOTIFIC <- as.numeric(sinan_tuberculose$IDADE_NOTIFIC)
sinan_tuberculose$IDADE_DIAG <- round((sinan_tuberculose$DT_DIAG - sinan_tuberculose$DT_NASC)/365,3)
sinan_tuberculose$IDADE_DIAG <- as.numeric(sinan_tuberculose$IDADE_DIAG)
sinan_tuberculose$DT_TRI <- as.yearqtr(sinan_tuberculose$DT_DIAG)
sinan_tuberculose$DT_TRI <- as.factor(sinan_tuberculose$DT_TRI)
sinan_tuberculose$CS_RACA <- as.factor(sinan_tuberculose$CS_RACA)
sinan_tuberculose$CS_ESCOL_N <- as.factor(sinan_tuberculose$CS_ESCOL_N)
sinan_tuberculose$ID_OCUPA_N_OCUP <- as.factor(sinan_tuberculose$ID_OCUPA_N)
sinan_tuberculose$CODMUNRES <- as.character(sinan_tuberculose$CODMUNRES)
sinan_tuberculose$CODMUNOCOR <- as.character(sinan_tuberculose$ID_MUNICIP) #Município de notificação
sinan_tuberculose$UNIDADE <- as.character(sinan_tuberculose$UNIDADE)
sinan_tuberculose$ANO_DIAG <- substr(sinan_tuberculose$DT_DIAG, 0, 4) %>% as.numeric()
sinan_tuberculose <- subset(sinan_tuberculose, sinan_tuberculose$ANO_DIAG >= 2006 )
sinan_tuberculose$ANO_DIAG <- NULL
sinan_tuberculose$UNIDADE <- as.integer(sinan_tuberculose$UNIDADE) 
sinan_tuberculose <- tbl_df(sinan_tuberculose)

