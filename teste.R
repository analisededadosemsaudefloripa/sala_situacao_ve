library(shiny)
library(tidyverse)
library(sp)
library(leaflet)
library(DT)
library(plotly)
library(shinyWidgets)
source("dados_sim.R", encoding = "UTF-8")
source("dados_localizacao_abrangencia_cs.R", encoding = "UTF-8")

banco_preparado <- sim
banco <- sim

# Define UI for application that draws a histogram
ui <- fluidPage(
                   # Barra de navegação 
                   sidebarLayout(
                      sidebarPanel(
                        #INPUT para entrada de dados
                                        #Selecionando município
                         selectInput(inputId = "municipio_banco", 
                                     label = "Selecione o código do município:",
                                     choices = c(unique(banco$CODMUNRES)), 
                                     selected = "420540"),
                         helpText("Entre com o código do IBGE.",
                                  "420540 é o código de Florianópolis"),
                         #Selecionando local de residencia ou de ocorrência do óbito
                         selectInput(inputId = "local_banco", 
                                     label = "Município de residência ou ocorrência?",
                                     choices = c("Residência", "Ocorrência"), 
                                     selected = "Residência"),
                         #Selecionando cid
                         textInput(inputId = "cid_banco", 
                                   label = "Insira um CID:"),
                         helpText("Utilizar letra maiuscula",
                                  "sem pontuações ou separadores",
                                  "(Ex: I ou I2 ou I24)"),

                         #Selicionando Trimestre
                         sliderTextInput(inputId ="data", 
                                     label = "Selecione um trimestre:", 
                                     choices = sort(unique(banco_preparado$DT_TRI)), 
                                     selected = "2006 Q1",
                                     animate = animationOptions(interval = 2000, 
                                                                loop = FALSE, 
                                                                playButton = NULL,
                                                                pauseButton = NULL)),
                         #Selecionando se dados aparecerão ou não
                         checkboxInput(inputId =  "dados", 
                                    label = "Mostrar os Dados", 
                                    value = TRUE),
                         #Tabela
                         DT::dataTableOutput(outputId = "table")
                      ),
                      
                      # Mapa
                      mainPanel(
                         #Mapa
                         leafletOutput(outputId="map", width = "100%", height = 660),
                         splitLayout(
                                 plotlyOutput(outputId = "serie"),
                                 plotlyOutput(outputId = "densidade")
                                )
                        )
                )
        )

# Define server logic required to draw a histogram
server <- function(input, output) {
        
cod_municipio <- reactive({
       req(input$municipio_banco)
        input$municipio_banco
})        
        

banco_preparado<- reactive({
        
        req(input$local_banco)
        ifelse((input$local_banco == "Residência"), banco_floripa <- subset(banco, banco$CODMUNRES == cod_municipio()), banco_floripa <- subset(banco, banco$CODMUNOCOR == cod_municipio()) )      
        req(input$cid_banco)
        banco_floripa$CID <- substring(banco_floripa$CID, 0,as.numeric(nchar(input$cid_banco)))
        banco_floripa <- subset(banco_floripa, banco_floripa$CID == input$cid_banco)
        banco_floripa <- banco_floripa[,c("DT_TRI", "UNIDADE")]
        banco_floripa$VALOR <- 1 
        banco_floripa <- aggregate(banco_floripa$VALOR, by = list(banco_floripa$DT_TRI, banco_floripa$UNIDADE), FUN = sum)
        names(banco_floripa) <- c("DT_TRI", "UNIDADE", "VALOR")
        banco_floripa
})        
        
   

banco_prev <- reactive({
        Cod_Unidades_VE <- read_csv("dados_cs/Cod_Unidades_VE.csv", 
        col_types = cols(COD_VE = col_integer()))
        a <- merge(banco_preparado(), Cod_Unidades_VE, by.x = "UNIDADE", by.y = "COD_VE", all.x = TRUE) 
        a <- a[, c("DT_TRI", "COD", "VALOR")]
        a
})



cs_select <- reactive({
        sp::merge(x = abrangencia_cs, 
         y = banco_prev(),  
         by = c("COD"),duplicateGeoms = T, na.rm = F)
})

  
        
    
data_cs_select <- reactive({
        req(input$data)
        subset(cs_select(),cs_select()@data$DT_TRI == input$data) 
})

colorpal <- reactive({
colorNumeric("YlOrRd", domain = cs_select()$VALOR) #feito com banco_preparado completo, para pegar todos os valores da série temporal, permitindo a comparação entre os períodos
})



output$map <- renderLeaflet({
        pal <- colorpal() 
        
        labels <- sprintf(
        "<strong>%s</strong><br/>Valor: %g",
        data_cs_select()@data$Name, data_cs_select()@data$VALOR
        ) %>% lapply(htmltools::HTML)

leaflet(data = data_cs_select()) %>% 
        addProviderTiles("Esri.WorldImagery")%>% 
        setView(lng =-48.47 , lat=-27.6,zoom=10.5)%>%
        addPolygons(fillColor = ~pal(data_cs_select()@data$VALOR),
             weight = 2,
             opacity = 1,
             color = "white",
             dashArray = "3",
             fillOpacity = 0.7,
             popup = data_cs_select()@data$Name,
             highlight = highlightOptions(
                            weight = 5,
                            color = "#666",
                            dashArray = "",
                            fillOpacity = 0.7,
                            bringToFront = TRUE),
             label = labels,
                     labelOptions = labelOptions(
                     style = list("font-weight" = "normal", padding = "3px 8px"),
                     textsize = "15px",
                     direction = "auto"))%>%
              addLegend(pal = pal, values = ~cs_select()$VALOR, opacity = 0.7, title = NULL,
position = "bottomright") 
})


observe({
        
        pal <- colorpal() 
       
        labels <- sprintf(
        "<strong>%s</strong><br/>Valor: %g",
        data_cs_select()@data$Name, data_cs_select()@data$VALOR
        ) %>% lapply(htmltools::HTML)

        leafletProxy("map", data = data_cs_select()) %>%
        addPolygons(fillColor = ~pal(data_cs_select()@data$VALOR),
             weight = 2,
             opacity = 1,
             color = "white",
             dashArray = "3",
             fillOpacity = 0.7,
             popup = data_cs_select()@data$Name,
             highlight = highlightOptions(
                            weight = 5,
                            color = "#666",
                            dashArray = "",
                            fillOpacity = 0.7,
                            bringToFront = TRUE),
             label = labels,
                     labelOptions = labelOptions(
                     style = list("font-weight" = "normal", padding = "3px 8px"),
                     textsize = "15px",
                     direction = "auto"))
})



#Gráfico com densidade

max_valor <- reactive({
        a <- data_cs_select()@data$VALOR
        b <-as.numeric(max(a, na.rm = T))
        b
})


output$densidade <- renderPlotly({
        VALOR1 <- na.omit(data_cs_select()@data$VALOR)
        VALOR2 <- na.omit(cs_select()$VALOR)

c <- ggplot(data_cs_select()@data)+
        geom_density(aes(VALOR1),fill = "red", color = "red", alpha = 0.5,position = "identity",inherit.aes = F)+
        scale_x_continuous(limits = c(0, max(VALOR2)), na.value = F)+
        scale_y_continuous(limits = c(0, (max(density(VALOR2)$y)+1)), na.value = F)+
        xlab(" ") +
        ylab("Densidade") +
        ggtitle("Densidade - Trimestral")
ggplotly(c)
})




#Gráfico com série temporal
floripa_select <- reactive({
        a <- banco_preparado()[,c("DT_TRI", "VALOR")]
        a <- aggregate(a$VALOR, by = list(a$DT_TRI), FUN = sum)
        names(a) <- c("DT_TRI", "VALOR")
        a

})
        
data_floripa_select <- reactive({
        req(input$data)
        subset(floripa_select(),floripa_select()$DT_TRI == input$data)

})

output$serie <- renderPlotly({
        
d <-ggplot(floripa_select())+
        geom_line(aes(DT_TRI, as.numeric(VALOR), group = 1))+
        geom_point(aes(data_floripa_select()$DT_TRI, data_floripa_select()$VALOR), size = 5, fill = "red", color = "red")+        
        ylab("Valor")+
        xlab(" ")+
        ggtitle("Série Temporal - Trimestral")+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplotly(d, xaxis = list(automargin=TRUE))

})



#Tabela de dados
output$table <- DT::renderDataTable({
        
     if(input$dados){
         
     dados_organizados <- data_cs_select()@data[,c(2,4,5)]
    
     DT::datatable(data = dados_organizados,
             rownames = FALSE,
             editable = FALSE,
             options = list(lengthMenu = c(10,20, 40, 60, 80, 100), pageLength = 20))}
})


          
}

# Run the application 
shinyApp(ui = ui, server = server)

