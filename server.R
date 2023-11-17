library(shiny)
library(lubridate)
source("./global.R")

shinyServer(function(input,output) {
  
  conteoDiaVarA = reactive({
    conteoDiaVar2 = conteoDiaVar[which(conteoDiaVar$variable == input$variable),]
    conteoDiaVar2$anio_mes = sprintf("%s-%04d",month(conteoDiaVar2$diaCom, label = TRUE),year(conteoDiaVar2$diaCom))
    conteoDiaVar2 = conteoDiaVar2[which(conteoDiaVar2$anio_mes == input$anio_mes),]
    conteoDiaVar2$variable = NULL
    conteoDiaVar2$anio_mes = NULL
    return(conteoDiaVar2)
  })
  
  conteoDia_Temp = reactive({
    conteoDia2 = conteoDia
    conteoDia2$anio_mes = sprintf("%s-%04d",month(conteoDia2$diaCom, label = TRUE),year(conteoDia2$diaCom))
    conteoDia2 = conteoDia2[which(conteoDia2$anio_mes == input$anio_mes),]
    conteoDia2$anio_mes = NULL
    return(conteoDia2)
  })
  
  conteoDiaVariableEst2_Temp = reactive({
    conteoDiaVariableEst3 = conteoDiaVariableEst2
    conteoDiaVariableEst3$anio_mes = sprintf("%s-%04d",month(conteoDiaVariableEst3$diaCom, label = TRUE),year(conteoDiaVariableEst3$diaCom))
    conteoDiaVariableEst3 = conteoDiaVariableEst3[which(conteoDiaVariableEst3$anio_mes == input$anio_mes),]
    conteoDiaVariableEst3$anio_mes = NULL
    return(conteoDiaVariableEst3)
  })
  
  estBUFR = reactive({
    uniqEstBUFR = unique(conteoDiaVariableEst2_Temp()$codigo)
    CNE_IDEAM_BUFR = CNE_IDEAM[which(CNE_IDEAM$CODIGO %in% uniqEstBUFR),]
    return(CNE_IDEAM_BUFR)
  })
  
  estBUFR_prec = reactive({
    uniqEstBUFR = unique(conteoDiaVariableEst2_Temp()$codigo[!is.na(conteoDiaVariableEst2_Temp()$prec)])
    CNE_IDEAM_BUFR = CNE_IDEAM[which(CNE_IDEAM$CODIGO %in% uniqEstBUFR),]
    return(CNE_IDEAM_BUFR)
  })
  
  estBUFR_presion = reactive({
    uniqEstBUFR = unique(conteoDiaVariableEst2_Temp()$codigo[!is.na(conteoDiaVariableEst2_Temp()$presion)])
    CNE_IDEAM_BUFR = CNE_IDEAM[which(CNE_IDEAM$CODIGO %in% uniqEstBUFR),]
    return(CNE_IDEAM_BUFR)
  })
  
  estBUFR_hum = reactive({
    uniqEstBUFR = unique(conteoDiaVariableEst2_Temp()$codigo[!is.na(conteoDiaVariableEst2_Temp()$hum)])
    CNE_IDEAM_BUFR = CNE_IDEAM[which(CNE_IDEAM$CODIGO %in% uniqEstBUFR),]
    return(CNE_IDEAM_BUFR)
  })
  
  estBUFR_pRocio = reactive({
    uniqEstBUFR = unique(conteoDiaVariableEst2_Temp()$codigo[!is.na(conteoDiaVariableEst2_Temp()$pRocio)])
    CNE_IDEAM_BUFR = CNE_IDEAM[which(CNE_IDEAM$CODIGO %in% uniqEstBUFR),]
    return(CNE_IDEAM_BUFR)
  })
  
  estBUFR_dirV = reactive({
    uniqEstBUFR = unique(conteoDiaVariableEst2_Temp()$codigo[!is.na(conteoDiaVariableEst2_Temp()$dirV)])
    CNE_IDEAM_BUFR = CNE_IDEAM[which(CNE_IDEAM$CODIGO %in% uniqEstBUFR),]
    return(CNE_IDEAM_BUFR)
  })
  
  estBUFR_velV = reactive({
    uniqEstBUFR = unique(conteoDiaVariableEst2_Temp()$codigo[!is.na(conteoDiaVariableEst2_Temp()$velV)])
    CNE_IDEAM_BUFR = CNE_IDEAM[which(CNE_IDEAM$CODIGO %in% uniqEstBUFR),]
    return(CNE_IDEAM_BUFR)
  })
  
  estBUFR_temp = reactive({
    uniqEstBUFR = unique(conteoDiaVariableEst2_Temp()$codigo[!is.na(conteoDiaVariableEst2_Temp()$temp)])
    CNE_IDEAM_BUFR = CNE_IDEAM[which(CNE_IDEAM$CODIGO %in% uniqEstBUFR),]
    return(CNE_IDEAM_BUFR)
  })
  
  estBUFR_tempMax = reactive({
    uniqEstBUFR = unique(conteoDiaVariableEst2_Temp()$codigo[!is.na(conteoDiaVariableEst2_Temp()$tempMax)])
    CNE_IDEAM_BUFR = CNE_IDEAM[which(CNE_IDEAM$CODIGO %in% uniqEstBUFR),]
    return(CNE_IDEAM_BUFR)
  })
  
  estBUFR_tempMin = reactive({
    uniqEstBUFR = unique(conteoDiaVariableEst2_Temp()$codigo[!is.na(conteoDiaVariableEst2_Temp()$tempMin)])
    CNE_IDEAM_BUFR = CNE_IDEAM[which(CNE_IDEAM$CODIGO %in% uniqEstBUFR),]
    return(CNE_IDEAM_BUFR)
  })
  
  
  
  

  
  ## Elaboracion del mapa
  output$mapBUFR = renderLeaflet({
    estBUFR2 = estBUFR()
    
    estBUFR_prec2 = estBUFR_prec()
    estBUFR_presion2 = estBUFR_presion()
    estBUFR_hum2 = estBUFR_hum()
    estBUFR_pRocio2 = estBUFR_pRocio()
    estBUFR_dirV2 = estBUFR_dirV()
    estBUFR_velV2 = estBUFR_velV()
    estBUFR_temp2 = estBUFR_temp()
    estBUFR_tempMax2 = estBUFR_tempMax()
    estBUFR_tempMin2 = estBUFR_tempMin()
    
    
    pal = colorFactor(palette = c("blue", "red"),levels = c("SI","NO"))
    
    leaflet() %>% 
      addTiles(group = "Google Maps") %>%
      addProviderTiles("Esri.WorldImagery",group = "WorldImagery") %>%
      addProviderTiles(providers$Stamen.TonerLines,group = "WorldImagery") %>%
      addProviderTiles(providers$Stamen.TonerLabels,group = "WorldImagery") %>%
      addCircleMarkers(data = estBUFR2,group = "BUFR - AUTOMATICAS",
                                         lng = estBUFR2$longitud,lat = estBUFR2$latitud,
                                         color = ~pal(OSCAR),layerId = estBUFR2$CODIGO, 
                                         label = ~ CODIGO,radius = 2) %>%
      addCircleMarkers(data = estBUFR_prec2,group = "BUFR Prec",
                                         lng = estBUFR_prec2$longitud,lat = estBUFR_prec2$latitud,
                                         color = ~pal(OSCAR),layerId = estBUFR_prec2$CODIGO, 
                                         label = ~ CODIGO,radius = 2) %>%
      addCircleMarkers(data = estBUFR_presion2,group = "BUFR Presion",
                                         lng = estBUFR_presion2$longitud,lat = estBUFR_presion2$latitud,
                                         color = ~pal(OSCAR),layerId = estBUFR_presion2$CODIGO, 
                                         label = ~ CODIGO,radius = 2) %>%
      addCircleMarkers(data = estBUFR_hum2,group = "BUFR Humedad",
                                         lng = estBUFR_hum2$longitud,lat = estBUFR_hum2$latitud,
                                         color = ~pal(OSCAR),layerId = estBUFR_hum2$CODIGO, 
                                         label = ~ CODIGO,radius = 2) %>%
      addCircleMarkers(data = estBUFR_pRocio2,group = "BUFR Punto Rocio",
                                         lng = estBUFR_pRocio2$longitud,lat = estBUFR_pRocio2$latitud,
                                         color = ~pal(OSCAR),layerId = estBUFR_pRocio2$CODIGO, 
                                         label = ~ CODIGO,radius = 2) %>%
      addCircleMarkers(data = estBUFR_dirV2,group = "BUFR Direccion Viento",
                                         lng = estBUFR_dirV2$longitud,lat = estBUFR_dirV2$latitud,
                                         color = ~pal(OSCAR),layerId = estBUFR_dirV2$CODIGO, 
                                         label = ~ CODIGO,radius = 2) %>%
      addCircleMarkers(data = estBUFR_velV2,group = "BUFR Velocidad Viento",
                                         lng = estBUFR_velV2$longitud,lat = estBUFR_velV2$latitud,
                                         color = ~pal(OSCAR),layerId = estBUFR_velV2$CODIGO, 
                                         label = ~ CODIGO,radius = 2) %>%
      addCircleMarkers(data = estBUFR_temp2,group = "BUFR Temperatura",
                                         lng = estBUFR_temp2$longitud,lat = estBUFR_temp2$latitud,
                                         color = ~pal(OSCAR),layerId = estBUFR_temp2$CODIGO, 
                                         label = ~ CODIGO,radius = 2) %>%
      addCircleMarkers(data = estBUFR_tempMax2,group = "BUFR Temp Max",
                                         lng = estBUFR_tempMax2$longitud,lat = estBUFR_tempMax2$latitud,
                                         color = ~pal(OSCAR),layerId = estBUFR_tempMax2$CODIGO, 
                                         label = ~ CODIGO,radius = 2) %>%
      addCircleMarkers(data = estBUFR_tempMin2,group = "BUFR Temp Min",
                                         lng = estBUFR_tempMin2$longitud,lat = estBUFR_tempMin2$latitud,
                                         color = ~pal(OSCAR),layerId = estBUFR_tempMin2$CODIGO, 
                                         label = ~ CODIGO,radius = 2) %>%
      AddSearchButton( group = "BUFR - AUTOMATICAS", zoom = 20,  # For add search box in the map.
                       textPlaceholder = "Search accesion name...") %>%
      addLayersControl(
        baseGroups = c("Google Maps","WorldImagery"),
        overlayGroups = c("BUFR - AUTOMATICAS","BUFR Prec","BUFR Presion","BUFR Humedad",
                          "BUFR Punto Rocio","BUFR Direccion Viento","BUFR Velocidad Viento",
                          "BUFR Temperatura","BUFR Temp Max","BUFR Temp Min"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup(c("BUFR Prec","BUFR Presion","BUFR Humedad",
                  "BUFR Punto Rocio","BUFR Direccion Viento","BUFR Velocidad Viento",
                  "BUFR Temperatura","BUFR Temp Max","BUFR Temp Min")) %>% 
      leaflet::addLegend("bottomleft", 
                         pal = pal,
                         title = "Metadato en OSCAR/Surface",
                         values = estBUFR2$OSCAR,
                         opacity = 1)
  })
  
  ## Elaboracion del grafico de conteo de estaciones en el dia
  output$graficoCantidades = renderPlotly({
    y1 <- list(
      tickfont = list(color = "blue"),
      titlefont = list(color = "blue"),
      # overlaying = "y",
      side = "left",
      anchor="free",
      #position=1,
      title = "Conteo datos")
    
    y2 <- list(
      tickfont = list(color = "red"),
      titlefont = list(color = "red"),
      overlaying = "y",
      side = "right",
      # anchor="free",
      position=1,
      title = "Conteo estaciones")
    
    plotgraph = plot_ly(conteoDia_Temp(),x=~diaCom,y=~conteo,type = "bar",name = "Valor",marker = list(color = "cyan3")) %>% 
      add_trace(y=~Est,type = "scatter",yaxis = "y2",mode = 'lines+markers',type = "scatter",name = "Estación",marker = list(color = "red")) %>% 
      layout(yaxis =y1,
             yaxis2 = y2,
             xaxis = list(title = 'dia')
      )
  })
  
  ## Elaboracion del grafico de conteo de estaciones en el dia por cada variable
  output$graficoCantidadesVar = renderPlotly({
    y1 <- list(
      tickfont = list(color = "blue"),
      titlefont = list(color = "blue"),
      # overlaying = "y",
      side = "left",
      anchor="free",
      #position=1,
      title = "Conteo datos")
    
    y2 <- list(
      tickfont = list(color = "red"),
      titlefont = list(color = "red"),
      overlaying = "y",
      side = "right",
      # anchor="free",
      position=1,
      title = "Conteo estaciones")
    
    plot_ly(conteoDiaVarA(),x=~diaCom,y=~conteo,type = "bar",name = "Valor",marker = list(color = "cyan3")) %>% 
      add_trace(y=~Est,type = "scatter",yaxis = "y2",mode = 'lines+markers',type = "scatter",name = "Estación",marker = list(color = "red")) %>% 
      layout(yaxis =y1,
             yaxis2 = y2,
             xaxis = list(title = 'dia')
      )
    # print(plotgrapha)
  })
  
  ## Se genera grafica y visualizacion del metadato de la estacion seleccionada en el mapa
  observeEvent(input$mapBUFR_marker_click,{
    codigoZ = input$mapBUFR_marker_click$id # El codigo viene de la dinamizacion con el mapa
    
    CodTemp = reactive({
      temp = conteoDiaVariableEst2_Temp()[which(conteoDiaVariableEst2_Temp()$codigo == codigoZ),]
      return(temp)
    })
    
    ## Elaboracion del grafico de seguimiento de variables por estacion, una vez dandole click a la estacion en el lienzo del mapa    
    output$graficoEstBUFR = renderPlotly({
      graph = plot_ly(CodTemp(),x = ~diaCom,y = ~prec,name = "precipitacion",type = 'scatter',mode = 'lines+markers') %>% 
        add_trace(y = ~dirV,name = "direccion del viento") %>% 
        add_trace(y = ~velV,name = "velocidad del viento") %>% 
        add_trace(y = ~hum,name = "humedad") %>% 
        add_trace(y = ~presion,name = "presion") %>% 
        add_trace(y = ~temp,name = "temperatura") %>%
        add_trace(y = ~tempMax,name = "temperatura maxima") %>% 
        add_trace(y = ~tempMin,name = "temperatura minima") %>%
        add_trace(y = ~pRocio,name = "punto de rocio") %>%
        layout(yaxis = list(title = 'Cantidad datos'),
               xaxis = list(title = 'dia'),
               legend = list(orientation = 'h',y = 1.13))
    })
    
    ## Despliegue del metadato de la estacion seleccionada en el mapa, desde OSCAR - SURFACE
    output$frame = renderUI({
      my_test = tags$iframe(src = paste0("https://oscar.wmo.int/surface/#/search/station/stationReportDetails/0-170-0-",codigoZ),height=500, width=1050)
      print(my_test)
    })
    
  })
})