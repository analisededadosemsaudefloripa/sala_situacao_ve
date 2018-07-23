#######################################################################
##Bibliotecas
#######################################################################
library(readr)
library(rgdal)
#######################################################################
##Localização dos CSs
#######################################################################
acesso_dat <- read_csv("dados_cs/dados_cs.csv", 
                       col_types = cols(BIENIO = col_factor(levels = c("2013/2014", 
                                                                       "2015/2016")), DISTRITO = col_factor(levels = c("Centro", 
                                                                                                                        "Continente", "Norte", "Sul"))))
#######################################################################
##Abrangência dos CSs
#######################################################################
#Abrangência CS
#ogrListLayers("dados_cs/Abrangencia_Centros_de_Saude.kml") #Lê as layer do kml
abrangencia_cs <- readOGR(dsn = "dados_cs/Abrangencia_Centros_de_Saude.kml",layer =  "Abrangencia_Centros_de_Saude", encoding="UTF-8")
abrangencia_cs <- spTransform(abrangencia_cs, CRS("+proj=longlat +ellps=GRS80"))

abrangencia_cs$Name <- c("CS Centro",               "CS Trindade",                "CS Monte Cristo",                      
                      "CS Capoeiras",               "CS Vila Aparecida",          "CS Abraao", 
                      "CS Barra da Lagoa",          "CS Canto da Lagoa",          "CS Costa da Lagoa",  
                      "CS Joao Paulo",              "CS Lagoa da Conceicao",      "CS Pantanal",
                      "CS Monte Serrat",            "CS Saco Grande",             "CS Cachoeira do Bom Jesus",
                      "CS Canasvieiras",            "CS Santinho",                "CS Jurere", 
                      "CS Ponta das Canas",         "CS Ratones",                 "CS Rio Vermelho",
                      "CS Santo Antonio de Lisboa", "CS Vargem Grande",           "CS Prainha",
                      "CS Vargem Pequena",          "CS Alto Ribeirao",           "CS Armacao",  
                      "CS Caieira da Barra do Sul", "CS Campeche",                "CS Carianos", 
                      "CS Costeira do Pirajubae",   "CS Fazenda do Rio Tavares",  "CS Morro das Pedras",
                      "CS Pantano do Sul",          "CS Agronomica",              "CS Ribeirao da Ilha", 
                      "CS Saco dos Limoes",         "CS Tapera",                  "CS Rio Tavares", 
                      "CS Corrego Grande",          "CS Ingleses",                "CS Novo Continente",
                      "CS Itacorubi ",              "CS Coqueiros",               "CS Jardim Atlantico",
                      "CS Sape",                    "CS Coloninha",               "CS Balneario",  
                      "CS Estreito")



abrangencia_cs$COD <- c("cs.14","cs.46","cs.28","cs.12","cs.49","cs.01","cs.06","cs.11",
                        "cs.18","cs.25","cs.27","cs.32","cs.29","cs.41","cs.07","cs.10",
                        "cs.42","cs.26","cs.34","cs.36","cs.39","cs.43","cs.47","cs.35",
                        "cs.48","cs.03","cs.04","cs.08","cs.09","cs.13","cs.19","cs.21",
                        "cs.30","cs.33","cs.02","cs.37","cs.40","cs.45","cs.38","cs.17",
                        "cs.22","cs.31","cs.23","cs.16","cs.24","cs.44","cs.15","cs.05",
                        "cs.20") #O código foi colocado em ordem alfabética
