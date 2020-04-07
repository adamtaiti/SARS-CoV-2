##### File: server_country.R
##### License: GPLv3 or later
##### Modification date: 05 Apr 2020
##### Written by: Adam Taiti Harth Utsunomiya
##### Contact: adamtaiti@gmail.com
##### Description: backend for the 'by Country' page

# Plot of growth curve for simulated data
output$exdata_growth_curve<-renderPlotly({
  ggplotly(plot_simulated_growth_curve(exdata, translate, lang), tooltip="text")
})

# Plot of growth rate for simulated data
output$exdata_growth_rate<-renderPlotly({
  ggplotly(plot_simulated_growth_rate(exdata, translate, lang), tooltip="text")
})

# Plot of growth accelerations for simulated data
output$exdata_growth_acceleration<-renderPlotly({
  ggplotly(plot_simulated_growth_acceleration(exdata, translate, lang), tooltip="text")
})

# Plot of growth curve for real data
output$realdata_growth_curve<-renderPlotly({
  plot_realdata_growth_curve(df = real(), translate, lang)
})

# Plot of growth rate for real data
output$realdata_growth_rate<-renderPlotly({
  plot_realdata_growth_rate(df = real(), translate, lang)
})

# Plot of growth acceleration for real data
output$realdata_growth_acceleration<-renderPlotly({
  plot_realdata_growth_acceleration(df = real(), translate, lang)
})

# Prevalence next day
output$prev<-renderText({
  as.numeric(real()[nrow(real()),"fitted"])
})

# New cases next day
output$newc<-renderText({
  as.numeric(real()[nrow(real()),"c"])
})
