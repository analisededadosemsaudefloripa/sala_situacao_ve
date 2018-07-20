library(readr)
library(lubridate)
sim <- read_csv("obitos/ministerio/sim_2006_16.csv")
sim$DTOBITO <- as.Date(sim$DTOBITO, format = "%d%m%Y")
sim$DTNASC <- as.Date(sim$DTNASC, format = "%d%m%Y")
sim$DTOBITO <- as.yearqtr(sim$DTOBITO)
sim$DTOBITO <- as.factor(sim$DTOBITO)
colnames(sim)[which(names(sim) == "CAUSABAS")] <- "CID" #O nome foi trocado, para que possa ser utilizado no modulo_cid_residencia
