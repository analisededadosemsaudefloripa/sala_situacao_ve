library(readr)
compara_2007_2000 <- read_csv("populacao/setores_censitarios/material_passado_ibge/Compara_2007_2000.csv")
compara_2010_2007 <- read_csv("populacao/setores_censitarios/material_passado_ibge/Compara_2010_2007.csv")


#Separando dados de SC
compara_2007_2000 <- subset(compara_2007_2000, compara_2007_2000$uf2000 == "42")
compara_2010_2007 <- subset(compara_2010_2007, compara_2010_2007$uf2007 == "42")

#Separando dados de Florianópolis
compara_2007_2000 <- subset(compara_2007_2000, compara_2007_2000$munic2000 == "05407")
compara_2010_2007 <- subset(compara_2010_2007, compara_2010_2007$munic2007 == "05407")
compara_2007_2000 <- compara_2007_2000[,c(5,10)]
compara_2010_2007 <- compara_2010_2007[,c(5,10)]

#Compara dados de Floriaópolis 2000-2010
compara_2000_2010 <- merge(compara_2007_2000, compara_2010_2007, by = "setor2007", all = T)
#Retirando dados de 2007 e duplicações entre 2000-2010
compara_2000_2010$setor2007 <- NULL
compara_2000_2010 <- unique(compara_2000_2010)


#Compatibilidade 2000-2010
#compara_2000_2010$COMPARACAO <- NA
#for(i in 1:nrow(compara_2000_2010)){
#ifelse(compara_2000_2010$setor2000[i] == compara_2000_2010$setor[i], compara_2000_2010$COMPARACAO[i] <- 0, compara_2000_2010$COMPARACAO[i] <- 1)
#}
#compara_2000_2010 <- na.omit(compara_2000_2010)
#sum(compara_2000_2010$COMPARACAO)/nrow(compara_2000_2010)

#Analisando quantos setores estão duplicados. 
unicos2010 <- compara_2000_2010[-duplicated(compara_2000_2010$setor), ]
unicos2000 <- compara_2000_2010[-duplicated(compara_2000_2010$setor2000), ]
