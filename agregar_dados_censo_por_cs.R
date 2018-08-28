library(readr)
uniao_cs_censo_2000_basico <- read_csv("populacao/setores_censitarios/uniao_cs_censo_2000_basico.csv")
uniao_cs_censo_2000_basico <- uniao_cs_censo_2000_basico[,c(1,80)]
uniao_cs_censo_2000_basico$Name <- as.factor(uniao_cs_censo_2000_basico$Name)
uniao_cs_censo_2000_basico <- na.omit(uniao_cs_censo_2000_basico)
agreg <- aggregate(uniao_cs_censo_2000_basico$V002_AJUST, by = list(uniao_cs_censo_2000_basico$Name), FUN = sum)
names(agreg) <- c("Name", "Populacao")
write.csv(agreg, "populacao/setores_censitarios/uniao_cs_censo_2000_basico_agregado.csv", row.names = F)
