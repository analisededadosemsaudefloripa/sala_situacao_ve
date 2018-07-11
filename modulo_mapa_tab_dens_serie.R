#################################################################################
#UI
#################################################################################


mapa_tab_dens_serie_UI <- function(id, data){
        ns <- NS(id)
        
        tagList(
        navbarPage(title = "Sífilis",
        tabPanel("Sífilis",
                  fluidPage(
                   # Barra de navegação 
                   sidebarLayout(
                      sidebarPanel(
                         #Selecionando Indicadores
                         selectInput(inputId = ns("sifilis_indicador"), 
                                     label = "Selecione um indicadores:",
                                     choices = sort(unique(banco_sifilis_cs$TIPO)),
                                     selected = "Aguardando_Investigacao"),
                         #Selicionando Trimestre
                         sliderTextInput(inputId =ns("sifilis_data"), 
                                     label = "Selecione um trimestre:", 
                                     choices = sort(unique(banco_sifilis_cs$TRIMESTRE)), 
                                     selected = "2013 Q1",
                                     animate = animationOptions(interval = 2000, 
                                                                loop = FALSE, 
                                                                playButton = NULL,
                                                                pauseButton = NULL)),
                         #Selecionando se dados aparecerão ou não
                         checkboxInput(inputId =  ns("sifilis_dados"), 
                                    label = "Mostrar os Dados:", 
                                    value = TRUE),
                         #Tabela
                         DT::dataTableOutput(outputId = ns("sifilis_table"))
                      ),
                      
                      # Mapa
                      mainPanel(
                         #Mapa
                         leafletOutput(outputId=ns("sifilis_map"), width = "100%", height = 660),
                         splitLayout(
                                 plotlyOutput(outputId = ns("serie_sifilis")),
                                 plotlyOutput(outputId = ns("densidade_sifilis"))
                         ))
             )
          )
     )
  )
)
}



#################################################################################
#Server
#################################################################################

mapa_tab_dens_serie <- function(input, output,session, data){

        #Mapa sifilis
sifilis_cs_select <- reactive({
        req(input$sifilis_indicador)
        sp::merge(x = abrangencia_cs, 
         y = (subset(banco_sifilis_cs, TIPO == input$sifilis_indicador)),  
         by = c("COD"),duplicateGeoms = T, na.rm = F)
})
        
data_cs_select <- reactive({
        req(input$sifilis_data)
        subset(sifilis_cs_select(),sifilis_cs_select()@data$TRIMESTRE == input$sifilis_data) 
})
        
        
colorpal <- reactive({
colorNumeric("YlOrRd", domain = sifilis_cs_select()@data$VALOR) #feito com banco_sífilis, para pegar todos os valores da série temporal, permitindo a comparação entre os períodos
})



output$sifilis_map <- renderLeaflet({


leaflet(data_cs_select()) %>% 
         addProviderTiles("Esri.WorldImagery")%>% 
         setView(lng =-48.47 , lat=-27.6,zoom=10.5) 
})


observe({
        
        pal <- colorpal() 
        
        labels <- sprintf(
        "<strong>%s</strong><br/>Valor: %g",
        data_cs_select()@data$Name, data_cs_select()@data$VALOR
        ) %>% lapply(htmltools::HTML)

        leafletProxy("sifilis_map", data = data_cs_select()) %>%
        clearShapes() %>%
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
              addLegend(pal = pal, values = ~data_cs_select()@data$VALOR, opacity = 0.7, title = NULL,
position = "bottomright")
})



#Gráfico com densidade

max_valor <- reactive({
        a <- subset(banco_sifilis_cs, TIPO == input$sifilis_indicador)
        b <-as.numeric(max(a$VALOR, na.rm = T))
        b
})


output$densidade_sifilis <- renderPlotly({

c <- ggplot(data_cs_select()@data)+
        geom_density(aes(data_cs_select()@data$VALOR),fill = "red", color = "red", alpha = 0.5,position = "identity",inherit.aes = F)+
        scale_x_continuous(limits = c(0, max_valor()), na.value = F)+
        xlab(" ") +
        ggtitle("Densidade - Trimestral")
ggplotly(c)
})




#Gráfico com série temporal


#Mapa sifilis
sifilis_floripa_select <- reactive({
        req(input$sifilis_indicador)
        subset(banco_sifilis_florianopolis, TIPO == input$sifilis_indicador)
})
        
data_floripa_select <- reactive({
        req(input$sifilis_data)
        subset(sifilis_floripa_select(),sifilis_floripa_select()$TRIMESTRE == input$sifilis_data)  
})

output$serie_sifilis <- renderPlotly({
        
d <-ggplot(sifilis_floripa_select())+
        geom_line(aes(TRIMESTRE, VALOR, group = TIPO))+
        geom_point(aes(data_floripa_select()$TRIMESTRE, data_floripa_select()$VALOR), size = 5, fill = "red", color = "red")+        
        ylab("VALOR")+
        xlab(" ")+
        ggtitle("Série Temporal - Trimestral")+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplotly(d, xaxis = list(automargin=TRUE))

})



#Tabela de dados
output$sifilis_table <- DT::renderDataTable({
        
     if(input$sifilis_dados){
                 
     
     DT::datatable(data = data_cs_select()@data[,c(2,5,6,7)],
             rownames = FALSE,
             editable = FALSE,
             options = list(lengthMenu = c(10,20, 40, 60, 80, 100), pageLength = 10))}
})


          
}
