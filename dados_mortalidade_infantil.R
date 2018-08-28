source("dados_sinasc.R")
source("dados_sim.R")

library(reshape2)



#Óbitos
sim_infantil <- sim[,c(3,10,44,47)]

#Extraindo dados de 2013 à 2017
sim_infantil$DT_TRI <- substr(sim_infantil$DT_TRI, 0, 4) %>% as.character()
sim_infantil$DT_TRI <- as.numeric(sim_infantil$DT_TRI)
sim_infantil <- subset(sim_infantil, sim_infantil$DT_TRI >=2013)
sim_infantil$DT_TRI <- NULL

#Extraindo residentes em Florianópolis
sim_infantil <- subset(sim_infantil, sim_infantil$CODMUNRES == 420540)

#Extraindo menores de 18 anos
sim_infantil <- subset(sim_infantil, sim_infantil$IDADE < 18)

#criando variável de menor de 5 anos
sim_infantil$MENOR_5 <- ifelse((sim_infantil$IDADE < 5),  1, 0)

#criando variável de infatil
sim_infantil$INFANTIL <- ifelse((sim_infantil$IDADE < 1),  1, 0)

#criando variável de neonatal
sim_infantil$IDADE_DIAS <- sim_infantil$IDADE*365
sim_infantil$NEONATAL <- ifelse((sim_infantil$IDADE_DIAS < 28),  1, 0)

#criando variável de neonatal tardio
sim_infantil$NEONATAL_TARDIO <- ifelse((sim_infantil$IDADE_DIAS < 28 & sim_infantil$IDADE_DIAS >= 7),  1, 0)

#criando variável de neonatal precoce
sim_infantil$NEONATAL_PRECOCE <- ifelse((sim_infantil$IDADE_DIAS < 7),  1, 0)

#Excluindo o que não será utilizado
sim_infantil$IDADE <- NULL
sim_infantil$IDADE_DIAS <- NULL
sim_infantil$CODMUNRES <- NULL
#Agregando a base
sim_infantil$UNIDADE <- as.character(sim_infantil$UNIDADE) 
sim_infantil <- aggregate(.~ UNIDADE, sim_infantil, FUN = sum)

#Nascidos vivos
#Selecionando rsidentes em Florianópolis
sinasc_calculo <- sinasc[,c(8,41,42)]
sinasc_calculo <- subset(sinasc_calculo,sinasc_calculo$CODMUNRES == 420540)

#Extraindo dados de 2013 à 2017
sinasc_calculo$DT_TRI <- substr(sinasc_calculo$DT_TRI, 0, 4) %>% as.character()
sinasc_calculo$DT_TRI <- as.numeric(sinasc_calculo$DT_TRI)
sinasc_calculo <- subset(sinasc_calculo, sinasc_calculo$DT_TRI >=2013)
sinasc_calculo$DT_TRI <- NULL

#Excluindo o que não será utilizado
sinasc_calculo$CODMUNRES <- NULL

#Agregando a base sinasc
sinasc_calculo$NASCIMENTOS <- 1
sinasc_calculo <- aggregate(.~ UNIDADE, sinasc_calculo, FUN = sum)


#Fazendo merge das bases e calculando as taxas
mort_infantil <- merge(sim_infantil, sinasc_calculo, by = c("UNIDADE"), all = T)
mort_infantil[is.na(mort_infantil)] <- 0
mort_infantil$TX_MENOR_5 <- mort_infantil$MENOR_5/mort_infantil$NASCIMENTOS * 1000
mort_infantil$TX_INFANTIL <- mort_infantil$INFANTIL/mort_infantil$NASCIMENTOS * 1000
mort_infantil$TX_NEONATAL <- mort_infantil$NEONATAL/mort_infantil$NASCIMENTOS * 1000
mort_infantil$TX_NEONATAL_TARDIO <- mort_infantil$NEONATAL_TARDIO/mort_infantil$NASCIMENTOS * 1000
mort_infantil$TX_NEONATAL_PRECOCE <- mort_infantil$NEONATAL_PRECOCE/mort_infantil$NASCIMENTOS * 1000

Cod_Unidades_VE <- read_csv("dados_cs/Cod_Unidades_VE.csv")

names(Cod_Unidades_VE)[1] <- "UNIDADE"

Cod_Unidades_VE$UNIDADE <- as.character(Cod_Unidades_VE$UNIDADE)

mort_infantil <- merge(Cod_Unidades_VE, mort_infantil, by = "UNIDADE", all = T)

mort_infantil <- mort_infantil[,-c(1,2,3)]

mort_infantil <- melt(mort_infantil) 

names(mort_infantil) <- c("COD", "TIPO", "VALOR")

mort_infantil <- na.omit(mort_infantil)

mort_infantil
