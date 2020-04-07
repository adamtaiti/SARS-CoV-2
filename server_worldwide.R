##### File: server_worldwide.R
##### License: GPLv3 or later
##### Modification date: 05 Apr 2020
##### Written by: Adam Taiti Harth Utsunomiya
##### Contact: adamtaiti@gmail.com
##### Description: backend for the 'Worldwide' page

# Plot of worldwide cases
output$worldwidecases<-renderPlotly({
  df <- worldwidecases(realdata)
  names(df) <- c("n")
  df$n <- df$n/1000
  df$date <- as.Date(row.names(df))#,format = "%YYYY-%mm-%dd")
  df <- df[order(df$date),]
  Pandemic <- as.Date("2020-03-11")
  plot_worldwide(df, Pandemic, translate, lang)
})

# Plot of worlwide deaths
output$worldwidedeaths<-renderPlotly({
  df <- worldwidedeaths(realdata)
  names(df) <- c("n")
  df$n <- df$n/1000
  df$date <- as.Date(row.names(df))
  Pandemic <- as.Date("2020-03-11")
  plot_worldwide(df, Pandemic, translate, lang)
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