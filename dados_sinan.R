# Sala de Situação da Vigilância Epidemiológica de Florianópolis
options("scipen" = 100, "digits"=2) #Não colocar notação científica
#######################################################################
#Bibliotecas
#######################################################################
library(readr)
library(lubridate)
library(zoo)
library(foreign)#Ler dbf
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
sinan_aidsA$DT_TRI <- as.character(sinan_aidsA$DT_TRI)
sinan_aidsA$CS_SEXO <- as.character(sinan_aidsA$CS_SEXO)
sinan_aidsA$CS_SEXO[which(sinan_aidsA$CS_SEXO == "I")] <- NA
sinan_aidsA$CS_SEXO <- as.factor(sinan_aidsA$CS_SEXO)
sinan_aidsA$CS_RACA <- as.factor(sinan_aidsA$CS_RACA)
sinan_aidsA$CS_ESCOL_N <- as.factor(sinan_aidsA$CS_ESCOL_N)
sinan_aidsA$ID_OCUPA_N_OCUP <- as.factor(sinan_aidsA$ID_OCUPA_N)
sinan_aidsA$CODMUNRES <- as.character(sinan_aidsA$CODMUNRES)
sinan_aidsA$CODMUNOCOR <- as.character(sinan_aidsA$ID_MUNICIP) #Município de notificação
sinan_aidsA$UNIDADE <- as.character(sinan_aidsA$UNIDADE)
sinan_aidsA$ANO_DIAG <- substr(sinan_aidsA$DT_DIAG, 0, 4) %>% as.numeric()
sinan_aidsA <- subset(sinan_aidsA, sinan_aidsA$ANO_DIAG >= 2006 )
sinan_aidsA$ANO_DIAG <- NULL
sinan_aidsA$UNIDADE <- as.integer(sinan_aidsA$UNIDADE) 
sinan_aidsA <- tbl_df(sinan_aidsA)

