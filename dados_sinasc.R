options(encoding = 'UTF-8')
# Sala de Situação da Vigilância Epidemiológica de Florianópolis
options("scipen" = 100, "digits"=2) #Não colocar notação científica
#######################################################################
#Bibliotecas
#######################################################################
library(readr)
library(lubridate)
library(zoo)

#######################################################################
#Bases de dados
#######################################################################

#######################################################################
#Bases de Florianópolis - possuí bairro e dados mais atualizados
#######################################################################
sinasc_2006 <- read_csv("sinasc/georreferenciado/DN2006.csv")
sinasc_2007 <- read_csv("sinasc/georreferenciado/DN2007.csv")
sinasc_2008 <- read_csv("sinasc/georreferenciado/DN2008.csv")
sinasc_2009 <- read_csv("sinasc/georreferenciado/DN2009.csv")
sinasc_2010 <- read_csv("sinasc/georreferenciado/DN2010.csv")
sinasc_2011 <- read_csv("sinasc/georreferenciado/DN2011.csv")
sinasc_2012 <- read_csv("sinasc/georreferenciado/DN2012.csv")
sinasc_2013 <- read_csv("sinasc/georreferenciado/DN2013.csv")
sinasc_2014 <- read_csv("sinasc/georreferenciado/DN2014.csv")
sinasc_2015 <- read_csv("sinasc/georreferenciado/DN2015.csv")
sinasc_2016 <- read_csv("sinasc/georreferenciado/DN2016.csv")
sinasc_2017 <- read_csv("sinasc/georreferenciado/DN2017.csv")
sinasc_2006_2017 <- rbind(sinasc_2006,sinasc_2007,sinasc_2008,sinasc_2009,sinasc_2010,sinasc_2011,sinasc_2012,sinasc_2013,sinasc_2014,sinasc_2015,sinasc_2016,sinasc_2017)

sinasc <- sinasc_2006_2017[,c(13,25,27:31,33,44:55,58,68,70:76,78:86,102,103,107)]
sinasc$DTNASC <- ifelse(nchar(sinasc$DTNASC) == 7, paste0(0,sinasc$DTNASC), sinasc$DTNASC)
sinasc$DTNASC <- as.Date(sinasc$DTNASC, format = "%d%m%Y")
sinasc$DT_TRI <- as.yearqtr(sinasc$DTNASC)
sinasc$DT_TRI <- as.factor(sinasc$DT_TRI)
sinasc$SEXO <- as.character(sinasc$SEXO)
sinasc$SEXO[which(sinasc$SEXO == "I")] <- NA
sinasc$SEXO <- as.factor(sinasc$SEXO)
sinasc$RACACOR <- as.factor(sinasc$RACACOR)
sinasc$ESTCIVMAE <- as.factor(sinasc$ESTCIVMAE)
sinasc$ESCMAE <- as.factor(sinasc$ESCMAE)
sinasc$ESCMAE2010 <- as.factor(sinasc$ESCMAE2010)
sinasc$ESCMAEAGR1 <- as.factor(sinasc$ESCMAEAGR1)
sinasc$ESCMAEAGR2 <- as.factor(sinasc$ESCMAEAGR2)
sinasc$CODOCUPMAE <- as.factor(sinasc$CODOCUPMAE)
sinasc$CODMUNRES <- as.character(sinasc$CODMUNRES)
sinasc$CODMUNOCOR <- as.character(sinasc$CODMUNNASC)#mudando o nome para casar com o do sim e do sinan
sinasc$CODMUNNATU <- as.character(sinasc$CODMUNNATU)
sinasc$APGAR1 <- as.factor(sinasc$APGAR1)
sinasc$APGAR5 <- as.factor(sinasc$APGAR5)
sinasc$SERIESCMAE <- as.factor(sinasc$SERIESCMAE)
sinasc$RACACORMAE <- as.factor(sinasc$RACACORMAE)
sinasc$UNIDADE <- as.character(sinasc$UNIDADE)

