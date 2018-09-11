options(encoding = 'UTF-8')
# Sala de Situação da Vigilância Epidemiológica de Florianópolis
#
#######################################################################
#Bibliotecas
#######################################################################
library(shiny)


#######################################################################
#Bases de dados
#######################################################################
#######################################################################
##Localização e abrangëncia dos CSs
#######################################################################
source("dados_localizacao_abrangencia_cs.R")
#######################################################################
##Estatísticas Vitais e Demografia
#######################################################################
#######################################################################
###
#######################################################################
#######################################################################
##Nascimentos - Séries Temporais
#######################################################################
source("dados_sinasc.R")
source("modulo_local.R")
source("modulo_serie_temporal.R", encoding = "UTF-8")
#######################################################################
##Nascimentos - Associações
#######################################################################
#source("dados_sinasc.R")
#source("modulo_local.R")
source("modulo_regressao.R", encoding = "UTF-8")
#######################################################################
##Nascimentos - Mapa
#######################################################################
#source("dados_sinasc.R")
#source("modulo_local.R")
source("modulo_mapa.R", encoding = "UTF-8")
#######################################################################
###Óbitos
#######################################################################
#######################################################################
##Mortalidade Infantil - Série Temporal
#######################################################################

        
#######################################################################
##Mortalidade Infantil - Associações
#######################################################################

        
#######################################################################
##Mortalidade Infantil - Mapa
#######################################################################
source("dados_mortalidade_infantil.R", encoding = "UTF-8")
source("modulo_mapa_simples.R", encoding = "UTF-8")
#######################################################################
##Geral - Séries Temporais
#######################################################################
source("dados_sim.R", encoding = "UTF-8")
source("modulo_cid_local.R", encoding = "UTF-8")
#source("modulo_serie_temporal.R", encoding = "UTF-8")
#######################################################################
##Óbitos gerais - Associações
#######################################################################
#source("dados_sim.R", encoding = "UTF-8")
#source("modulo_cid_local.R", encoding = "UTF-8")
#source("modulo_regressao.R", encoding = "UTF-8")
#######################################################################
##Óbitos gerais - Mapa
#######################################################################
#source("dados_sim.R", encoding = "UTF-8")
#source("modulo_cid_local.R", encoding = "UTF-8")
#source("modulo_mapa.R", encoding = "UTF-8")


#######################################################################
##Agravos de Notificação
#######################################################################
#######################################################################
###HIV Adulto - Séries Temporais
#######################################################################
source("dados_sinan.R")
#source("modulo_local.R")
#source("modulo_serie_temporal.R", encoding = "UTF-8")
#######################################################################
###HIV Adulto - Associações
#######################################################################
#source("dados_sinan.R")
#source("modulo_local.R")
#source("modulo_regressao.R", encoding = "UTF-8")
#######################################################################
###HIV Adulto - Mapas
#######################################################################
#source("dados_sinan.R")
#source("modulo_local.R")
#source("modulo_mapa.R", encoding = "UTF-8")
#######################################################################
###HIV Gestante - Séries Temporais
#######################################################################
#source("dados_sinan.R")
#source("modulo_local.R")
#source("modulo_serie_temporal.R", encoding = "UTF-8")
#######################################################################
###HIV Gestante - Associações
#######################################################################
#source("dados_sinan.R")
#source("modulo_local.R")
#source("modulo_regressao.R", encoding = "UTF-8")
#######################################################################
###HIV Gestante - Mapas
#######################################################################
#source("dados_sinan.R")
#source("modulo_local.R")
#source("modulo_mapa.R", encoding = "UTF-8")
#######################################################################
###Sífilis  - Séries Temporais
#######################################################################
#source("dados_sinan.R")
#source("modulo_local.R")
#source("modulo_serie_temporal.R", encoding = "UTF-8")
#######################################################################
###Sífilis  - Associações
#######################################################################
#source("dados_sinan.R")
#source("modulo_local.R")
#source("modulo_regressao.R", encoding = "UTF-8")
#######################################################################
###Sífilis  - Mapas
#######################################################################
#source("dados_sinan.R")
#source("modulo_local.R")
#source("modulo_mapa.R", encoding = "UTF-8")
#######################################################################
#######################################################################
###Sífilis Gestante - Séries Temporais
#######################################################################
#source("dados_sinan.R")
#source("modulo_local.R")
#source("modulo_serie_temporal.R", encoding = "UTF-8")
#######################################################################
###Sífilis Gestante - Associações
#######################################################################
#source("dados_sinan.R")
#source("modulo_local.R")
#source("modulo_regressao.R", encoding = "UTF-8")
#######################################################################
###Sífilis Gestante - Mapas
#######################################################################
#source("dados_sinan.R")
#source("modulo_local.R")
#source("modulo_mapa.R", encoding = "UTF-8")
#######################################################################
###Sífilis Criança - Séries Temporais
#######################################################################
#source("dados_sinan.R")
#source("modulo_local.R")
#source("modulo_serie_temporal.R", encoding = "UTF-8")
#######################################################################
###Sífilis Criança - Associações
#######################################################################
#source("dados_sinan.R")
#source("modulo_local.R")
#source("modulo_regressao.R", encoding = "UTF-8")
#######################################################################
###Sífilis Criança - Mapas
#######################################################################
#source("dados_sinan.R")
#source("modulo_local.R")
#source("modulo_mapa.R", encoding = "UTF-8")
#######################################################################
#######################################################################
###Tuberculose - Séries Temporais
#######################################################################
#source("dados_sinan.R")
#source("modulo_local.R")
#source("modulo_serie_temporal.R", encoding = "UTF-8")
#######################################################################
###Tuberculose - Associações
#######################################################################
#source("dados_sinan.R")
#source("modulo_local.R")
#source("modulo_regressao.R", encoding = "UTF-8")
#######################################################################
###Tuberculose - Mapas
#######################################################################
#source("dados_sinan.R")
#source("modulo_local.R")
#source("modulo_mapa.R", encoding = "UTF-8")
#######################################################################
#######################################################################
###Sífilis InfoSAUDE
#######################################################################
source("dados_sifilis.R", encoding = "UTF-8")
source("modulo_mapa_tab_dens_serie.R", encoding = "UTF-8")
#######################################################################
###Doenças de Notificação Hídrico-alimentar
#######################################################################



#######################################################################
##Agravos Estratégicos
#######################################################################
#######################################################################
###COAP
#######################################################################


#######################################################################
#User Interface
#######################################################################
# Define UI for application that draws a histogram
ui <- shinyUI(navbarPage(shinythemes::themeSelector(),
  title = "Vigilância Epidemiológica de Florianópolis",
#######################################################################
##Estatísticas Vitais e Demografia
#######################################################################
  navbarMenu("Estatísticas Vitais e Demografia",
#######################################################################
###Nascimentos
#######################################################################
#######################################################################
##Nascimentos - Séries Temporais
#######################################################################
    tabPanel("Nascimentos- Séries Temporais",
                        serie_temporal_UI(id = "serie_nascimento",
                                          input_dados = local_Input(id = "local_serie_nascimento", banco = sinasc))
        ),
#######################################################################
##Nascimentos - Associações
#######################################################################
        tabPanel("Nascimentos - Associações",
                        regressao_UI(id = "associacao_nascimento",
                                     input_dados = local_Input(id = "local_associacao_nascimento", banco = sinasc),
                                     banco = sinasc)
        ),
#######################################################################
##Nascimentos - Mapa
#######################################################################
        tabPanel("Nascimentos - Mapa",
                        mapa_UI(id = "mapa_nascimento",
                                     input_dados = local_Input(id = "local_mapa_nascimento", banco = sinasc),
                                     banco = sinasc)
        ),
#######################################################################
###Óbitos
#######################################################################
#######################################################################
##Mortalidade Infantil - Série Temporal
#######################################################################

        
#######################################################################
##Mortalidade Infantil - Associações
#######################################################################

        
#######################################################################
##Mortalidade Infantil - Mapa
#######################################################################
        tabPanel("Mortalidade Infantil - Mapa",
                        mapa_simples_UI(id = "mapa_mortalidade_infantil",
                                      banco = mort_infantil_mapa)
        ),
#######################################################################
##Óbitos gerais - Séries Temporais
#######################################################################
        tabPanel("Óbitos gerais - Séries Temporais",
                        serie_temporal_UI(id = "serie_obito",
                                          input_dados = cid_local_Input(id = "cid_serie_obito", banco = sim))
        ),
#######################################################################
##Óbitos gerais - Associações
#######################################################################
        tabPanel("Óbitos gerais - Associações",
                        regressao_UI(id = "associacao_obito",
                                     input_dados = cid_local_Input(id = "cid_associacao_obito", banco = sim),
                                     banco = sim)
        ),
#######################################################################
##Óbitos gerais - Mapa
#######################################################################
        tabPanel("Óbitos gerais - Mapa",
                        mapa_UI(id = "mapa_obito",
                                     input_dados = cid_local_Input(id = "cid_mapa_obito", banco = sim),
                                     banco = sim)
        )
),
#######################################################################
###Demografia
#######################################################################
  
#######################################################################
##Agravos de Notificação
#######################################################################
  navbarMenu("Agravos de Notificação",
#######################################################################
##HIV Adulto - Séries Temporais
#######################################################################
    tabPanel("HIV Adulto - Séries Temporais",
                        serie_temporal_UI(id = "serie_hiv_adulto",
                                          input_dados = local_Input(id = "local_serie_hiv_adulto", banco = sinan_aidsA))
        ),
#######################################################################
##HIV Adulto - Associações
#######################################################################
        tabPanel("HIV Adulto - Associações",
                        regressao_UI(id = "associacao_hiv_adulto",
                                     input_dados = local_Input(id = "local_associacao_hiv_adulto", banco = sinan_aidsA),
                                     banco = sinan_aidsA)
        ),
#######################################################################
##HIV Adulto - Mapa
#######################################################################
        tabPanel("HIV Adulto - Mapa",
                        mapa_UI(id = "mapa_hiv_adulto",
                                     input_dados = local_Input(id = "local_mapa_hiv_adulto", banco = sinan_aidsA),
                                     banco = sinan_aidsA)
        ),
#######################################################################
##HIV Gestante - Séries Temporais
#######################################################################
    tabPanel("HIV Gestante - Séries Temporais",
                        serie_temporal_UI(id = "serie_hiv_gestante",
                                          input_dados = local_Input(id = "local_serie_hiv_gestante", banco = sinan_aidsG))
        ),
#######################################################################
##HIV Gestante - Associações
#######################################################################
        tabPanel("HIV Gestante - Associações",
                        regressao_UI(id = "associacao_hiv_gestante",
                                     input_dados = local_Input(id = "local_associacao_hiv_gestante", banco = sinan_aidsG),
                                     banco = sinan_aidsG)
        ),
#######################################################################
##HIV Gestante - Mapa
#######################################################################
        tabPanel("HIV Gestante - Mapa",
                        mapa_UI(id = "mapa_hiv_gestante",
                                     input_dados = local_Input(id = "local_mapa_hiv_gestante", banco = sinan_aidsG),
                                     banco = sinan_aidsG)
        ),
#######################################################################
##Sífilis - Séries Temporais
#######################################################################
    tabPanel("Sífilis - Séries Temporais",
                        serie_temporal_UI(id = "serie_sifilis",
                                          input_dados = local_Input(id = "local_serie_sifilis", banco = sinan_sifilis))
        ),
#######################################################################
##Sífilis - Associações
#######################################################################
        tabPanel("Sífilis - Associações",
                        regressao_UI(id = "associacao_sifilis",
                                     input_dados = local_Input(id = "local_associacao_sifilis", banco = sinan_sifilis),
                                     banco = sinan_sifilis)
        ),
#######################################################################
##Sífilis - Mapa
#######################################################################
        tabPanel("Sífilis - Mapa",
                        mapa_UI(id = "mapa_sifilis",
                                     input_dados = local_Input(id = "local_mapa_sifilis", banco = sinan_sifilis),
                                     banco = sinan_sifilis)
        ),
#######################################################################
##Sífilis Gestante - Séries Temporais
#######################################################################
    tabPanel("Sífilis Gestante - Séries Temporais",
                        serie_temporal_UI(id = "serie_sifilisG",
                                          input_dados = local_Input(id = "local_serie_sifilisG", banco = sinan_sifilisG))
        ),
#######################################################################
##Sífilis Gestante - Associações
#######################################################################
        tabPanel("Sífilis Gestante - Associações",
                        regressao_UI(id = "associacao_sifilisG",
                                     input_dados = local_Input(id = "local_associacao_sifilisG", banco = sinan_sifilisG),
                                     banco = sinan_sifilisG)
        ),
#######################################################################
##Sífilis Gestante - Mapa
#######################################################################
        tabPanel("Sífilis Gestante - Mapa",
                        mapa_UI(id = "mapa_sifilisG",
                                     input_dados = local_Input(id = "local_mapa_sifilisG", banco = sinan_sifilisG),
                                     banco = sinan_sifilisG)
        ),
#######################################################################
##Sífilis Criança - Séries Temporais
#######################################################################
    tabPanel("Sífilis Criança - Séries Temporais",
                        serie_temporal_UI(id = "serie_sifilisC",
                                          input_dados = local_Input(id = "local_serie_sifilisC", banco = sinan_sifilisC))
        ),
#######################################################################
##Sífilis Criança - Associações
#######################################################################
        tabPanel("Sífilis Criança - Associações",
                        regressao_UI(id = "associacao_sifilisC",
                                     input_dados = local_Input(id = "local_associacao_sifilisC", banco = sinan_sifilisC),
                                     banco = sinan_sifilisC)
        ),
#######################################################################
##Sífilis Criança - Mapa
#######################################################################
        tabPanel("Sífilis Criança - Mapa",
                        mapa_UI(id = "mapa_sifilisC",
                                     input_dados = local_Input(id = "local_mapa_sifilisC", banco = sinan_sifilisC),
                                     banco = sinan_sifilisC)
        ),
#######################################################################
##Tuberculose - Séries Temporais
#######################################################################
    tabPanel("Tuberculose - Séries Temporais",
                        serie_temporal_UI(id = "serie_tuberculose",
                                          input_dados = local_Input(id = "local_serie_tuberculose", banco = sinan_tuberculose))
        ),
#######################################################################
##Tuberculose - Associações
#######################################################################
        tabPanel("Tuberculose - Associações",
                        regressao_UI(id = "associacao_tuberculose",
                                     input_dados = local_Input(id = "local_associacao_tuberculose", banco = sinan_tuberculose),
                                     banco = sinan_tuberculose)
        ),
#######################################################################
##Tuberculose - Mapa
#######################################################################
        tabPanel("Tuberculose - Mapa",
                        mapa_UI(id = "mapa_tuberculose",
                                     input_dados = local_Input(id = "local_mapa_tuberculose", banco = sinan_tuberculose),
                                     banco = sinan_tuberculose)
        ),
#######################################################################
###Sífilis
#######################################################################
    tabPanel(title = "Sífilis - Série Histórica",
             mapa_tab_dens_serie_UI(id = "sifilis", 
                                    banco_geral = dados_sifilis, 
                                    banco_cs = banco_sifilis_cs))
),

#######################################################################
###Hepatites virais
#######################################################################
    
#######################################################################
###Tuberculose
#######################################################################
    
#######################################################################
###Doencas transmitidas pelo Aedes
#######################################################################
    
#######################################################################
###Leishmaniose Viceral
#######################################################################
    
#######################################################################
###Doenças de Veiculação Hídrico-alimentar
#######################################################################
  
#######################################################################
##Agravos Estratégicos
#######################################################################
  navbarMenu("Agravos Estratégicos",
#######################################################################
###Trânsito
#######################################################################
   
#######################################################################
###Inicadores COAP
#######################################################################
    tabPanel("Inicadores COAP")
  )
 )
)
#############################################################################
#Server
#############################################################################
server <- function(input, output) {
#######################################################################
##Estatísticas Vitais e Demografia
#######################################################################
  
#######################################################################
###Nascimentos
#######################################################################
#######################################################################
##Nascimentos - Séries Temporais
#######################################################################
        banco_preparado_serie_nascimento <- callModule(module = local, 
                     id = "local_serie_nascimento", 
                     banco = sinasc)
        callModule(module = serie_temporal, 
                   id = "serie_nascimento", 
                   banco_preparado = banco_preparado_serie_nascimento)
#######################################################################
##Nascimentos - Associação
#######################################################################
        banco_preparado_associacao_nascimento <- callModule(module = local, 
                             id = "local_associacao_nascimento", 
                             banco = sinasc)
        callModule(module = regressao, 
                   id = "associacao_nascimento",
                   banco_preparado = banco_preparado_associacao_nascimento)

#######################################################################
##Nascimentos - Mapa
#######################################################################
        banco_preparado_mapa_nascimento <- callModule(module = local, 
                             id = "local_mapa_nascimento", 
                             banco = sinasc)
        callModule(module = mapa, 
                   id = "mapa_nascimento",
                   banco_preparado = banco_preparado_mapa_nascimento)
        
          
#######################################################################
###Óbitos
#######################################################################
#######################################################################
##Mortalidade Infantil - Série Temporal
#######################################################################

        
#######################################################################
##Mortalidade Infantil - Associações
#######################################################################

        
#######################################################################
##Mortalidade Infantil - Mapa
#######################################################################
        callModule(module = mapa_simples, 
                   id = "mapa_mortalidade_infantil",
                   banco = mort_infantil_mapa) 
        
#######################################################################
##Geral - Série Temporal
#######################################################################
        banco_preparado_serie_obito <- callModule(module = cid_local, 
                             id = "cid_serie_obito", 
                             banco = sim)
        callModule(module = serie_temporal, 
                   id = "serie_obito", 
                   banco_preparado = banco_preparado_serie_obito)
#######################################################################
##Óbitos gerais - Associações
#######################################################################
        banco_preparado_associacao <- callModule(module = cid_local, 
                             id = "cid_associacao_obito", 
                             banco = sim)
        callModule(module = regressao, 
                   id = "associacao_obito",
                   banco_preparado = banco_preparado_associacao)
#######################################################################
##Óbitos gerais - Mapa
#######################################################################
        banco_preparado_mapa <- callModule(module = cid_local, 
                             id = "cid_mapa_obito", 
                             banco = sim)
        callModule(module = mapa, 
                   id = "mapa_obito",
                   banco_preparado = banco_preparado_mapa)
#######################################################################
###Demografia
#######################################################################
   

#######################################################################
##Agravos de Notificação
#######################################################################
#######################################################################
##HIV Adulto - Séries Temporais
#######################################################################
        banco_preparado_serie_hiv_adulto <- callModule(module = local, 
                     id = "local_serie_hiv_adulto", 
                     banco = sinan_aidsA)
        callModule(module = serie_temporal, 
                   id = "serie_hiv_adulto", 
                   banco_preparado = banco_preparado_serie_hiv_adulto)
#######################################################################
##HIV Adulto - Associação
#######################################################################
        banco_preparado_associacao_hiv_adulto <- callModule(module = local, 
                             id = "local_associacao_hiv_adulto", 
                             banco = sinan_aidsA)
        callModule(module = regressao, 
                   id = "associacao_hiv_adulto",
                   banco_preparado = banco_preparado_associacao_hiv_adulto)

#######################################################################
##HIV Adulto - Mapa
#######################################################################
        banco_preparado_mapa_hiv_adulto <- callModule(module = local, 
                             id = "local_mapa_hiv_adulto", 
                             banco = sinan_aidsA)
        callModule(module = mapa, 
                   id = "mapa_hiv_adulto",
                   banco_preparado = banco_preparado_mapa_hiv_adulto)
#######################################################################
##HIV Gestante - Séries Temporais
#######################################################################
        banco_preparado_serie_hiv_gestante <- callModule(module = local, 
                     id = "local_serie_hiv_gestante", 
                     banco = sinan_aidsG)
        callModule(module = serie_temporal, 
                   id = "serie_hiv_gestante", 
                   banco_preparado = banco_preparado_serie_hiv_gestante)
#######################################################################
##HIV Gestante - Associação
#######################################################################
        banco_preparado_associacao_hiv_gestante <- callModule(module = local, 
                             id = "local_associacao_hiv_gestante", 
                             banco = sinan_aidsG)
        callModule(module = regressao, 
                   id = "associacao_hiv_gestante",
                   banco_preparado = banco_preparado_associacao_hiv_gestante)

#######################################################################
##HIV Gestante - Mapa
#######################################################################
        banco_preparado_mapa_hiv_gestante <- callModule(module = local, 
                             id = "local_mapa_hiv_gestante", 
                             banco = sinan_aidsG)
        callModule(module = mapa, 
                   id = "mapa_hiv_gestante",
                   banco_preparado = banco_preparado_mapa_hiv_gestante)
#######################################################################
##Sífilis - Séries Temporais
#######################################################################
        banco_preparado_serie_sifilis <- callModule(module = local, 
                     id = "local_serie_sifilis", 
                     banco = sinan_sifilis)
        callModule(module = serie_temporal, 
                   id = "serie_sifilis", 
                   banco_preparado = banco_preparado_serie_sifilis)
#######################################################################
##Sífilis - Associação
#######################################################################
        banco_preparado_associacao_sifilis <- callModule(module = local, 
                             id = "local_associacao_sifilis", 
                             banco = sinan_sifilis)
        callModule(module = regressao, 
                   id = "associacao_sifilis",
                   banco_preparado = banco_preparado_associacao_sifilis)

#######################################################################
##Sífilis - Mapa
#######################################################################
        banco_preparado_mapa_sifilis <- callModule(module = local, 
                             id = "local_mapa_sifilis", 
                             banco = sinan_sifilis)
        callModule(module = mapa, 
                   id = "mapa_sifilis",
                   banco_preparado = banco_preparado_mapa_sifilis)
#######################################################################
##Sífilis Gestante - Séries Temporais
#######################################################################
        banco_preparado_serie_sifilis_gestante <- callModule(module = local, 
                     id = "local_serie_sifilisG", 
                     banco = sinan_sifilisG)
        callModule(module = serie_temporal, 
                   id = "serie_sifilisG", 
                   banco_preparado = banco_preparado_serie_sifilis_gestante)
#######################################################################
##Sífilis Gestante - Associação
#######################################################################
        banco_preparado_associacao_sifilis_gestante <- callModule(module = local, 
                             id = "local_associacao_sifilisG", 
                             banco = sinan_sifilisG)
        callModule(module = regressao, 
                   id = "associacao_sifilisG",
                   banco_preparado = banco_preparado_associacao_sifilis_gestante)

#######################################################################
##Sífilis Gestante - Mapa
#######################################################################
        banco_preparado_mapa_sifilis_gestante <- callModule(module = local, 
                             id = "local_mapa_sifilisG", 
                             banco = sinan_sifilisG)
        callModule(module = mapa, 
                   id = "mapa_sifilisG",
                   banco_preparado = banco_preparado_mapa_sifilis_gestante)
#######################################################################
##Sífilis Criança - Séries Temporais
#######################################################################
        banco_preparado_serie_sifilis_crianca <- callModule(module = local, 
                     id = "local_serie_sifilisC", 
                     banco = sinan_sifilisC)
        callModule(module = serie_temporal, 
                   id = "serie_sifilisC", 
                   banco_preparado = banco_preparado_serie_sifilis_crianca)
#######################################################################
##Sífilis Criança - Associação
#######################################################################
        banco_preparado_associacao_sifilis_crianca <- callModule(module = local, 
                             id = "local_associacao_sifilisC", 
                             banco = sinan_sifilisC)
        callModule(module = regressao, 
                   id = "associacao_sifilisC",
                   banco_preparado = banco_preparado_associacao_sifilis_crianca)

#######################################################################
##Sífilis Criança - Mapa
#######################################################################
        banco_preparado_mapa_sifilis_crianca <- callModule(module = local, 
                             id = "local_mapa_sifilisC", 
                             banco = sinan_sifilisC)
        callModule(module = mapa, 
                   id = "mapa_sifilisC",
                   banco_preparado = banco_preparado_mapa_sifilis_crianca)
#######################################################################
##Tuberculose - Séries Temporais
#######################################################################
        banco_preparado_serie_tuberculose <- callModule(module = local, 
                     id = "local_serie_tuberculose", 
                     banco = sinan_tuberculose)
        callModule(module = serie_temporal, 
                   id = "serie_tuberculose", 
                   banco_preparado = banco_preparado_serie_tuberculose)
#######################################################################
##Tuberculose - Associação
#######################################################################
        banco_preparado_associacao_tuberculose <- callModule(module = local, 
                             id = "local_associacao_tuberculose", 
                             banco = sinan_tuberculose)
        callModule(module = regressao, 
                   id = "associacao_tuberculose",
                   banco_preparado = banco_preparado_associacao_tuberculose)

#######################################################################
##Tuberculose - Mapa
#######################################################################
        banco_preparado_mapa_tuberculose <- callModule(module = local, 
                             id = "local_mapa_tuberculose", 
                             banco = sinan_tuberculose)
        callModule(module = mapa, 
                   id = "mapa_tuberculose",
                   banco_preparado = banco_preparado_mapa_tuberculose)

 
#######################################################################
###Sífilis
#######################################################################
        callModule(module = mapa_tab_dens_serie, 
                   id = "sifilis", 
                   banco_geral = dados_sifilis, 
                   banco_sifilis_cs, 
                   banco_sifilis_florianopolis)

#######################################################################
###Hepatites virais
#######################################################################

 
#######################################################################
###Tuberculose
#######################################################################

 
#######################################################################
###Doencas transmitidas pelo Aedes
#######################################################################


#######################################################################
###Leishmaniose Viceral
#######################################################################


#######################################################################
###Doenças de Veiculação Hídrico-alimentar
#######################################################################


#######################################################################
##Agravos Estratégicos
#######################################################################


             
#######################################################################
###Trânsito
#######################################################################


#######################################################################
###Inicadores COAP
#######################################################################

       
    
}



# Run the application 
shinyApp(ui = ui, server = server)

