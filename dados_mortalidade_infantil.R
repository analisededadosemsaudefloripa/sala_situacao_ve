source("dados_sinasc.R")
source("dados_sim.R")

library(readr)
library(reshape2)
library(tidyverse)
library(stringdist)


#Óbitos
sim_infantil_mapa <- sim[,c(3,10,44,47)]

#Extraindo dados de 2013 à 2017
sim_infantil_mapa$DT_TRI <- substr(sim_infantil_mapa$DT_TRI, 0, 4) %>% as.character()
sim_infantil_mapa$DT_TRI <- as.numeric(sim_infantil_mapa$DT_TRI)
sim_infantil_mapa <- subset(sim_infantil_mapa, sim_infantil_mapa$DT_TRI >=2013)
sim_infantil_mapa$DT_TRI <- NULL

#Extraindo residentes em Florianópolis
sim_infantil_mapa <- subset(sim_infantil_mapa, sim_infantil_mapa$CODMUNRES == 420540)

#Extraindo menores de 18 anos
sim_infantil_mapa <- subset(sim_infantil_mapa, sim_infantil_mapa$IDADE < 18)

#criando variável de menor de 5 anos
sim_infantil_mapa$MENOR_5 <- ifelse((sim_infantil_mapa$IDADE < 5),  1, 0)

#criando variável de infatil
sim_infantil_mapa$INFANTIL <- ifelse((sim_infantil_mapa$IDADE < 1),  1, 0)

#criando variável de pós-neonatal
sim_infantil_mapa$IDADE_DIAS <- sim_infantil_mapa$IDADE*365
sim_infantil_mapa$POS_NEONATAL <- ifelse((sim_infantil_mapa$IDADE_DIAS >= 28) & (sim_infantil_mapa$IDADE < 1),  1, 0)

#criando variável de neonatal
sim_infantil_mapa$NEONATAL <- ifelse((sim_infantil_mapa$IDADE_DIAS < 28),  1, 0)

#criando variável de neonatal tardio
sim_infantil_mapa$NEONATAL_TARDIO <- ifelse((sim_infantil_mapa$IDADE_DIAS < 28 & sim_infantil_mapa$IDADE_DIAS >= 7),  1, 0)

#criando variável de neonatal precoce
sim_infantil_mapa$NEONATAL_PRECOCE <- ifelse((sim_infantil_mapa$IDADE_DIAS < 7),  1, 0)

#Excluindo o que não será utilizado
sim_infantil_mapa$IDADE <- NULL
sim_infantil_mapa$IDADE_DIAS <- NULL
sim_infantil_mapa$CODMUNRES <- NULL
#Agregando a base
sim_infantil_mapa$UNIDADE <- as.character(sim_infantil_mapa$UNIDADE) 
sim_infantil_mapa <- aggregate(.~ UNIDADE, sim_infantil_mapa, FUN = sum)

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
mort_infantil_mapa <- merge(sim_infantil_mapa, sinasc_calculo, by = c("UNIDADE"), all = T)
mort_infantil_mapa[is.na(mort_infantil_mapa)] <- 0
mort_infantil_mapa$TX_MENOR_5 <- mort_infantil_mapa$MENOR_5/mort_infantil_mapa$NASCIMENTOS * 1000
mort_infantil_mapa$TX_INFANTIL <- mort_infantil_mapa$INFANTIL/mort_infantil_mapa$NASCIMENTOS * 1000
mort_infantil_mapa$TX_NEONATAL <- mort_infantil_mapa$NEONATAL/mort_infantil_mapa$NASCIMENTOS * 1000
mort_infantil_mapa$TX_NEONATAL_TARDIO <- mort_infantil_mapa$NEONATAL_TARDIO/mort_infantil_mapa$NASCIMENTOS * 1000
mort_infantil_mapa$TX_NEONATAL_PRECOCE <- mort_infantil_mapa$NEONATAL_PRECOCE/mort_infantil_mapa$NASCIMENTOS * 1000

Cod_Unidades_VE <- read_csv("dados_cs/Cod_Unidades_VE.csv")

names(Cod_Unidades_VE)[1] <- "UNIDADE"

Cod_Unidades_VE$UNIDADE <- as.character(Cod_Unidades_VE$UNIDADE)

mort_infantil_mapa <- merge(Cod_Unidades_VE, mort_infantil_mapa, by = "UNIDADE", all = T)

mort_infantil_mapa <- mort_infantil_mapa[,-c(1,2,3)]

mort_infantil_mapa <- melt(mort_infantil_mapa) 

names(mort_infantil_mapa) <- c("COD", "TIPO", "VALOR")

mort_infantil_mapa <- na.omit(mort_infantil_mapa)

mort_infantil_mapa






#################################################################################################
#Linkagem sim_asso-sinasc_asso
#################################################################################################
#'Como não possuímos todo o banco do sinasc_asso nacional, é possível que crianças que tenham nascido em outro município
#'tenham se mudado para Florianópolis e tenham falecido aqui, sem que tenhamos seus dados de nascimento.
#'Por isso só serão trabalhados dados de crianças que ao nascer residiam em Florianópolis E que ao morrer também
#'residiam em Florianópolis.
#'
#################################################################################################
#Fazendo merge do sim_asso e do sinasc_asso
#################################################################################################

sim_asso_2014 <- read_csv("obitos/florianopolis/DO2014.csv")
sim_asso_2015 <- read_csv("obitos/florianopolis/DO2015.csv")
sim_asso_2016 <- read_csv("obitos/florianopolis/DO2016.csv")
sim_asso_2017 <- read_csv("obitos/florianopolis/DO2017.csv")
sim_asso_2014_2017 <- rbind(sim_asso_2014,sim_asso_2015,sim_asso_2016,sim_asso_2017)
#Extraindo residentes em Florianópolis
sim_asso_2014_2017 <- subset(sim_asso_2014_2017, sim_asso_2014_2017$`CODMUNRES,C,7` == "420540")
#ajustando a data de nascimento
sim_asso_2014_2017$`DTNASC,C,8` <- as.Date(sim_asso_2014_2017$`DTNASC,C,8`, format = "%d%m%Y") 
#ajustando a data do óbito
sim_asso_2014_2017$`DTOBITO,C,8`<- as.Date(sim_asso_2014_2017$`DTOBITO,C,8`, format = "%d%m%Y")
#calculando a idade do óbito e fazendo banco com menores de 5 anos
sim_asso_2014_2017$IDADE_OBITO <- (sim_asso_2014_2017$`DTOBITO,C,8` - sim_asso_2014_2017$`DTNASC,C,8`)
sim_asso_2014_2017 <- subset(sim_asso_2014_2017, sim_asso_2014_2017$IDADE_OBITO < 1825)#1825 dias == 5 anos
sim_asso_2014_2017$IDADE_OBITO <- NULL



sinasc_asso_2014 <- read_csv("sinasc/georreferenciado/DN2014.csv")
sinasc_asso_2015 <- read_csv("sinasc/georreferenciado/DN2015.csv")
sinasc_asso_2016 <- read_csv("sinasc/georreferenciado/DN2016.csv")
sinasc_asso_2017 <- read_csv("sinasc/georreferenciado/DN2017.csv")
sinasc_asso_2014_2017 <- rbind(sinasc_asso_2014,sinasc_asso_2015,sinasc_asso_2016,sinasc_asso_2017)
#Extraindo residentes em Florianópolis
sinasc_asso_2014_2017 <- subset(sinasc_asso_2014_2017, sinasc_asso_2014_2017$CODMUNRES == "420540")
#ajustando a data de nascimento
sinasc_asso_2014_2017$DTNASC <- ifelse(nchar(sinasc_asso_2014_2017$DTNASC) <8 , paste0("0",sinasc_asso_2014_2017$DTNASC),sinasc_asso_2014_2017$DTNASC)
sinasc_asso_2014_2017$DTNASC <- as.Date(sinasc_asso_2014_2017$DTNASC, format = "%d%m%Y") 



sim_asso_merge <- sim_asso_2014_2017[,c(1,14,15,17, 58)]
names(sim_asso_merge) <- c("NUMERODO", "NOMEMAE_SIM", "DTNASC","SEXO", "NUMERODN")
sinasc_asso_merge <- sinasc_asso_2014_2017[,c(1,23,48,50)]
names(sinasc_asso_merge) <- c("NUMERODN", "NOMEMAE_SINASC", "DTNASC", "SEXO")


#extraindo soundex
sim_asso_merge$soundex <- phonetic(sim_asso_merge$NOMEMAE_SIM, method = c("soundex"), 
                        useBytes = F)
sinasc_asso_merge$soundex <- phonetic(sinasc_asso_merge$NOMEMAE_SINASC, method = c("soundex"), 
                        useBytes = F)

#merge COM soundex 
merge_soudex <- merge(sinasc_asso_merge[,-c(4)], sim_asso_merge[,-c(4)], by=c("soundex", "DTNASC"), all.x = T) #Selecionado as que, ao nascer, residiam em Floripa E que ao morrer também.
result <- merge_soudex[!is.na(merge_soudex$NOMEMAE_SIM),]

#O soudex é muito permissivo, possibilitando que pessoas com nomes sim_assoilares sejam captadas.
#Para reduzir este erro, calcularam-se as distâncias entre os caracteres, retirando-se NOMEMAE_SIM e NOMEMAE_SINASC com distância superior a 10
result$distancia <- stringdist(result$NOMEMAE_SIM, result$NOMEMAE_SINASC, method = c("osa"), useBytes = FALSE, nthread = getOption("sd_num_thread")) 
result1 <- subset(result, result$distancia < 4) 

#Com o merge para a geração do merge_soudex, muitas casas duplicadas surgiram, por isso, extrair apenas os matchs únicos, assim_asso, mantiveram-se os NUMERODN.y,
#pois estes estão vinculados aos número das DOs, e excluiram-se os NUMERODN.x, que vieram do SINASC. Uma mãe pode ter gemelares ( o que geraria "soundex", "DTNASC"),
#Se um dos gemelares morrer, não se saberá qual foi.
#result1$NUMERODN.y <- ifelse(is.na(result1$NUMERODN.y), result1$NUMERODN.x, result1$NUMERODN.y)
#result1$NUMERODN.x <- NULL


#Utilizando os pares de base para vincular as planilhas do sim_asso e do sinasc_asso
result1<- result1[,c(3,5,7)] #mantendo apenas NUMERODN.y e NUMERODO
result1 <- unique(result1)
names(result1) <- c("NUMERODN_SINASC", "NUMERODO_SIM", "NUMERODN_SIM")

#mantendo apenas as colunas que interessam no SIM
sim_asso <- sim_asso_2014_2017[,c(1,8,14,15:21,23,25,26,33,34,35,37,38,44:57,59:61,66:71,132:135,138:140)]
#Extraindo carates que vem da conversão do DBF
colnames(sim_asso) <- gsub("\\,..*", "", colnames(sim_asso))
#Marcando todas as variáveis com o sufixo "_SIM"
colnames(sim_asso) <- paste0(colnames(sim_asso), "_SIM")

#mantendo apenas as colunas que interessam no SINASC
sinasc_asso <- sinasc_asso_2014_2017[,c(1,13,23,25,27:31,33,44:55,58,68,70:76,78:86,102,103,107)]
#Marcando todas as variáveis com o sufixo "_SINASC"
colnames(sinasc_asso) <- paste0(colnames(sinasc_asso), "_SINASC")

#Fazendo o merge
sim_asso_pre <- merge(sim_asso, result1, by = "NUMERODO_SIM", all.x = T)
#sim_asso_pre$NUMERODN_SIM <- ifelse(is.na(sim_asso_pre$NUMERODN_SIM), sim_asso_pre$NUMERODN_SINASC, sim_asso_pre$NUMERODN_SIM) #Preenchendo NA dos NUMERODN_SIM
#O merge duplicou NUMERODO_SIM. Aqueles que possuem NUMERODN_SINASC == NUMERODN_SIM, estão corretos, os diferentes são falsos matchs
#Assim_asso, serão eliminadas as linhas que possuam NUMERODN_SINASC diferente do NUMERODN_SIM.
sim_asso_pre <- filter(sim_asso_pre, sim_asso_pre$NUMERODN_SINASC == sim_asso_pre$NUMERODN_SIM | is.na(sim_asso_pre$NUMERODN_SIM)) #foi mantido os NA, para que se tenha ideia do núermo de óbitos em residentes. Este número deve diminuir 
#quando linkar com os nascidos residentes.
mort_infantil <- merge(sinasc_asso, sim_asso_pre, by = "NUMERODN_SINASC", all.x = T)
mort_infantil$OBITO <- ifelse(is.na(mort_infantil$NUMERODO_SIM), 0, 1)
#sum(mort_infantil$OBITO) #O número de óbitos passou de 203 para 171, provavelmente porque está se avaliando os que nasceram e morreram como residentes de Florianópolis
mort_infantil$IDADE_OBITO <- (mort_infantil$DTOBITO_SIM - mort_infantil$DTNASC_SINASC)
mort_infantil$MENOR_5 <- ifelse(mort_infantil$OBITO == 1 & mort_infantil$IDADE_OBITO < 1825 ,1,0 ) #1825 dias == 5 anos
mort_infantil$INFANTIL <- ifelse(mort_infantil$OBITO == 1 & mort_infantil$IDADE_OBITO < 365 ,1,0 ) 
mort_infantil$POS_NEONATAL <- ifelse(mort_infantil$OBITO == 1 & mort_infantil$IDADE_OBITO < 365 & mort_infantil$IDADE_OBITO >= 28,1,0 )
mort_infantil$NEONATAL <- ifelse(mort_infantil$OBITO == 1 & mort_infantil$IDADE_OBITO < 28,1,0 ) 
mort_infantil$NEONATAL_TARDIO <- ifelse(mort_infantil$OBITO == 1 & mort_infantil$IDADE_OBITO < 28 & mort_infantil$IDADE_OBITO >= 7,1,0 )
mort_infantil$NEONATAL_PRECOCE <- ifelse(mort_infantil$OBITO == 1 & mort_infantil$IDADE_OBITO < 7 ,1,0 ) 
mort_infantil <- mort_infantil[,-c(3,46)]
mort_infantil$DT_TRI <- as.yearqtr(mort_infantil$DTOBITO_SIM)
names(mort_infantil$CODMUNNASC_SINASC) <- "CODMUNRES"
names(mort_infantil$CODMUNOCOR_SIM) <- "CODMUNOCOR"
mort_infantil <- tbl_df(mort_infantil)
mort_infantil
