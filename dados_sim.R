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
#Bases do MS - será utilizada até final de 2006, pois possuí todos os municípios
#######################################################################
sim_ms <- read_csv("obitos/ministerio/sim_2006_16.csv")
sim_ms <- sim_ms[,c(4,7:9,14,17,40)]
sim_ms$DTOBITO <- as.Date(sim_ms$DTOBITO, format = "%d%m%Y")
sim_ms$DTNASC <- as.Date(sim_ms$DTNASC, format = "%d%m%Y")
sim_ms$IDADE <- round((sim_ms$DTOBITO - sim_ms$DTNASC)/365)
sim_ms$DTOBITO <- as.yearqtr(sim_ms$DTOBITO)
sim_ms$DTOBITO <- as.factor(sim_ms$DTOBITO)
colnames(sim_ms)[which(names(sim_ms) == "CAUSABAS")] <- "CID" #O nome foi trocado, para que possa ser utilizado no modulo_cid_residencia
sim_ms$IDADE <- as.numeric(sim_ms$IDADE)
sim_ms$SEXO <- as.character(sim_ms$SEXO)
for(i in 1:length(sim_ms)){
        if(sim_ms$SEXO[i] == 0){
                sim_ms$SEXO[i] <- NA
        }else if(sim_ms$SEXO[i] == 9){
                sim_ms$SEXO[i] <- NA
        }else{  
                sim_ms$SEXO[i] <-sim_ms$SEXO[i]
        }
}
sim_ms$SEXO <- as.factor(sim_ms$SEXO)
sim_ms$CODMUNRES <- as.character(sim_ms$CODMUNRES)
sim_ms$CODMUNOCOR <- as.character(sim_ms$CODMUNOCOR)
sim_ms$CID <- as.character(sim_ms$CID)


#######################################################################
#Bases de Florianópolis - será utilizada a partir de 2007, pois é mais atualizada e possuí bairro
#######################################################################

sim_2006_2017 <- read_csv("obitos/DO2006-2017_23-07-2018.csv")
sim_2006_2017 <- sim_2006_2017[,c(9,14:20,22,24:26,38,39,75)]
sim_2018 <- read_csv("obitos/DO2018_23-07-2018.csv")
sim_2018 <- sim_2018[,c(8,12:18,20,22:24,34,35,68)]
sim <- rbind(sim_2006_2017, sim_2018)
sim$DTOBITO <- as.Date(sim$DTOBITO, format = "%d/%m/%y")
sim$DTNASC <- as.Date(sim$DTNASC, format = "%d/%m/%y")
sim$IDADE <- round((sim$DTOBITO - sim$DTNASC)/365)
sim$DTOBITO <- as.yearqtr(sim$DTOBITO)
sim$DTOBITO <- as.factor(sim$DTOBITO)
colnames(sim)[which(names(sim) == "CAUSABAS")] <- "CID" #O nome foi trocado, para que possa ser utilizado no modulo_cid_residencia
sim$IDADE <- as.numeric(sim$IDADE)
sim$SEXO <- as.character(sim$SEXO)
for(i in 1:length(sim)){
        if(sim$SEXO[i] == 0){
                sim$SEXO[i] <- NA
        }else if(sim$SEXO[i] == 9){
                sim$SEXO[i] <- NA
        }else{  
                sim$SEXO[i] <-sim$SEXO[i]
        }
}
sim$SEXO <- as.factor(sim$SEXO)
sim$CODMUNRES <- as.character(sim$CODMUNRES)
sim$CODMUNOCOR <- as.character(sim$CODMUNOCOR)
sim$CID <- as.character(sim$CID)
