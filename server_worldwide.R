##### File: server_worldwide.R
##### License: GPLv3 or later
##### Modification date: 05 Apr 2020
##### Written by: Adam Taiti Harth Utsunomiya
##### Contact: adamtaiti@gmail.com
##### Description: backend for the 'Worldwide' page

# Plot of worldwide cases
output$worldwidecases<-renderPlotly({
  df <- worldwidecases(realdata)
  plot_worldwide(df, translate, lang)
})

# Plot of worlwide deaths
output$worldwidedeaths<-renderPlotly({
  df <- worldwidedeaths(realdata)
  plot_worldwide(df, translate, lang)
})

# Text for box of total cases
output$tcases<-renderText({
  as.numeric(ncases(realdata))
})

# Text for box of total deaths
output$tdeaths<-renderText({
  as.numeric(ndeaths(realdata))
})

# Text for box of number of countries or territories
output$tcountries<-renderText({
  as.numeric(ncountries(realdata))
})

# Plot leaflet 
output$leafletcases <- renderLeaflet({
  pal <- colorNumeric(palette = pallete, domain=c(min(weeks$ccases.log),max(weeks$ccases.log)))
  leaflet(data = weeks) %>% addTiles() %>%
    setView(lng = map_centre[, "X"], map_centre[, "Y"], zoom = 1) %>% clearShapes() %>% 
    addLegend(position = "bottomright", pal = pal, values = ~ ccases.log, opacity = 0.7, title = "log10 - Prevalence")
})

# Plot of worldwide cases dissemination
observeEvent(input$selmapview,{
  updateSliderInput(session = session, inputId = "leafletdates", value = min(weeks$week))
  if(!is.null(input$selmapview)){
    # pallete<-colorRamp(c('green','yellow', '#ff1e1e'))
    opacity<-0.8
    proxy = leafletProxy("leafletcases", data = weeks)
    if(input$selmapview=="Prevalence"){#translate$text[which(translate$item == "selmapview1" & translate$language == lang)]){#
      pal <- colorNumeric(palette = pallete, domain=c(min(weeks$ccases.log),max(weeks$ccases.log)))
      proxy %>% clearControls() %>% clearShapes() %>% 
        addLegend(position = "bottomright", pal = pal, values = ~ ccases.log, opacity = opacity, title = "log10 - Prevalence")
    }
    if(input$selmapview=="Prevalence per million inhabitants"){#translate$text[which(translate$item == "selmapview2" & translate$language == lang)]){#
      pal <- colorNumeric(palette = pallete, domain=c(min(weeks$ccases.million.log),max(weeks$ccases.million.log)))
      proxy %>% clearControls() %>% clearShapes() %>% 
        addLegend(position = "bottomright", pal = pal, values = ~ ccases.million.log, opacity = opacity, title = "log10 - Prevalence/million")
    }
    if(input$selmapview=="Weekly cases"){#translate$text[which(translate$item == "selmapview3" & translate$language == lang)]){#
      pal <- colorNumeric(palette = pallete, domain=c(min(weeks$Cases.log),max(weeks$Cases.log)))
      proxy %>% clearControls() %>% clearShapes() %>% 
        addLegend(position = "bottomright", pal = pal, values = ~ Cases.log, opacity = opacity, title = "log10 - Weekly cases")
    }
  } 
})

# Reset leaflet to the first epidemiological week
observeEvent(input$leafletdates,{
  if(input$leafletdates==1){
    proxy = leafletProxy("leafletcases", data = weeks)
    proxy %>% clearShapes()
  }
})

# React to the epidemiological week
filteredData = reactive({
  tmp<-weeks[which(weeks$week==input$leafletdates),]
  world_weeks = left_join(worldmap, tmp, by = "Countries and territories")
  world_weeks
})

# Update leaflet based on selected display
observe({

  tmp<-filteredData()
  tmp$fill<-NULL
  tmp$val<-NULL
  if(!is.null(input$selmapview)){
    # pallete<-colorRamp(c('green','yellow', '#ff1e1e'))
    if(input$selmapview=="Prevalence"){#translate$text[which(translate$item == "selmapview1" & translate$language == lang)]){#
      pal <- colorNumeric(palette = pallete, domain=c(min(weeks$ccases.log),max(weeks$ccases.log)),na.color = "none")
      tmp$color_ccases<-pal(tmp$ccases.log)
      if(length(tmp$color_ccases[which(is.na(tmp$color_ccases) | tmp$color_ccases=="#FFFFFF")])>0){
        tmp$color_ccases[which(is.na(tmp$color_ccases) | tmp$color_ccases=="#FFFFFF")]<-"none"
      }
      tmp$val<-tmp$ccases
      tmp$fill<-tmp$color_ccases
    }
    if(input$selmapview=="Prevalence per million inhabitants"){#translate$text[which(translate$item == "selmapview2" & translate$language == lang)]){#
      pal <- colorNumeric(palette = pallete, domain=c(min(weeks$ccases.million.log),max(weeks$ccases.million.log)),na.color = "none")
      tmp$color_ccases.million<-pal(tmp$ccases.million.log)
      if(length(tmp$color_ccases.million[which(is.na(tmp$color_ccases.million) | tmp$color_ccases.million=="#FFFFFF")])>0){
        tmp$color_ccases.million[which(is.na(tmp$color_ccases.million) | tmp$color_ccases.million=="#FFFFFF")]<-"none"
      }
      tmp$val<-round(tmp$ccases.million)
      tmp$fill<-tmp$color_ccases.million
    }
    if(input$selmapview=="Weekly cases"){#translate$text[which(translate$item == "selmapview3" & translate$language == lang)]){#
      pal <- colorNumeric(palette = pallete, domain=c(min(weeks$Cases.log),max(weeks$Cases.log)),na.color = "none")
      tmp$color_Cases<-pal(tmp$Cases.log)
      if(length(tmp$color_Cases[which(is.na(tmp$color_Cases) | tmp$color_Cases=="#FFFFFF")])>0){
        tmp$color_Cases[which(is.na(tmp$color_Cases) | tmp$color_Cases=="#FFFFFF")]<-"none"
      }
      tmp$fill<-tmp$color_Cases
      tmp$val<-tmp$Cases
    }
    
    tmp$fill[which(tmp$fill=="#FFFFFF")]<-"none"
    proxy = leafletProxy("leafletcases", data = tmp)
    
    # Color Polygons
    if (!is.null(input$leafletdates)) {
      proxy %>% clearShapes %>% addPolygons(fillColor = ~fill, stroke = T, color = "gray", 
                                            weight = 0.2, fillOpacity = 0.8, smoothFactor = 0.3, label = ~`Countries and territories`)
    } else {
      proxy
    }
  } 
})