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
  pal <- colorNumeric(palette = pallete<-colorRamp(c('white', '#ff1e1e')), domain=c(min(weeks$ccases),max(weeks$ccases)))
  leaflet(data = weeks) %>% addTiles() %>%
    setView(lng = map_centre[, "X"], map_centre[, "Y"], zoom = 1) %>% clearShapes() %>% 
    addLegend(position = "bottomright", pal = pal, values = ~ ccases, opacity = 1, title = "Prevalence")
})

# Plot of worldwide cases dissemination
observeEvent(input$selmapview,{
  updateSliderInput(session = session,inputId = "leafletdates",
                    label = "Select date:",
                    min = min(weeks$week), 
                    max = max(weeks$week),value = min(weeks$week),step = 1)
  if(!is.null(input$selmapview)){
    pallete<-colorRamp(c('white', '#ff1e1e'))
    if(input$selmapview=="Prevalence"){
      pal <- colorNumeric(palette = pallete, domain=c(min(weeks$ccases),max(weeks$ccases)))
      proxy = leafletProxy("leafletcases", data = weeks)
      proxy %>% clearControls() %>%
        addLegend(position = "bottomright", pal = pal, values = ~ ccases, opacity = 1, title = "Prevalence") %>% 
        clearShapes()
    }
    if(input$selmapview=="Weekly cases"){
      pal <- colorNumeric(palette = pallete, domain=c(min(weeks$Cases),max(weeks$Cases)))
      proxy = leafletProxy("leafletcases", data = weeks)
      proxy %>% clearControls() %>% 
        addLegend(position = "bottomright", pal = pal, values = ~ Cases, opacity = 1, title = "Weekly cases") %>% 
        clearShapes()
    }
    if(input$selmapview=="Prevalence per million habitants"){
      pal <- colorNumeric(palette = pallete, domain=c(min(weeks$ccases.million),max(weeks$ccases.million)))
      proxy = leafletProxy("leafletcases", data = weeks)
      proxy %>% clearControls() %>% 
        addLegend(position = "bottomright", pal = pal, values = ~ ccases.million, opacity = 1, title = "Prevalence/million") %>% 
        clearShapes()
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
})

# Update leaflet based on selected display
observe({
  tmp<-filteredData()
  tmp$fill<-NULL
  if(!is.null(input$selmapview)){
    pallete<-colorRamp(c('white', '#ff1e1e'))
    
    if(input$selmapview=="Prevalence"){
      pal <- colorNumeric(palette = pallete, domain=c(min(weeks$ccases.log),max(weeks$ccases.log)))
      tmp$color_ccases[which(is.na(tmp$color_ccases) | tmp$color_ccases=="#FFFFFF")]<-"none"
      tmp$fill<-tmp$color_ccases
      tmp$val<-tmp$ccases
    }
    if(input$selmapview=="Weekly cases"){
      pal <- colorNumeric(palette = pallete, domain=c(min(weeks$Cases.log),max(weeks$Cases.log)))
      tmp$color_Cases[which(is.na(tmp$color_Cases) | tmp$color_Cases=="#FFFFFF")]<-"none"
      tmp$fill<-tmp$color_Cases
      tmp$val<-tmp$Cases
    }
    if(input$selmapview=="Prevalence per million habitants"){
      pal <- colorNumeric(palette = pallete, domain=c(min(weeks$ccases.million.log),max(weeks$ccases.million.log)))
      tmp$color_ccases.million[which(is.na(tmp$color_ccases.million) | tmp$color_ccases.million=="#FFFFFF")]<-"none"
      tmp$fill<-tmp$color_ccases.million
      tmp$val<-tmp$ccases.million
    }
    
    proxy = leafletProxy("leafletcases", data = tmp)
    
    # Color Polygons
    if (!is.null(input$leafletdates)) {
      proxy %>% addPolygons(fillColor = ~fill, stroke = F, fillOpacity = 1, smoothFactor = 0.3)
    } else {
      proxy
    }
  } 
})