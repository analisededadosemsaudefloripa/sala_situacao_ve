library(googlesheets)
library(tidyverse)

indicadores <- gs_title("Indicadores_coap")
gs_ws_ls(indicadores)
serie_historica_coap<-gs_read(ss = indicadores, ws ="coap", col_names = T)
serie_historica_coap <- serie_historica_coap[,-c(1)] %>% as.data.frame()
names(serie_historica_coap)[1] <- c("INDICADOR")
serie_historica_coap <- melt(serie_historica_coap,"INDICADOR")
names(serie_historica_coap) <- c("INDICADOR", "ANO", "VALOR")
serie_historica_coap <- tbl_df(serie_historica_coap)
serie_historica_coap
