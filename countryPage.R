output$countryPage<-renderUI({
  fluidPage(
    column(
      width = 4, offset = 2,
      infoBox(title = "Prevalence next day", value = textOutput(outputId = "prev"),
              width = "100%",fill = T, color = "yellow")),
    column(
      width = 4, offset = 0,
      infoBox(title = "New cases next day", value = textOutput(outputId = "newc"),
              width = "100%",fill = T, color = "red")),
    fluidRow(
      column(
        width = 6,
        box(title = "Real Data", width = "100%",
            fluidPage(
              fluidRow(plotlyOutput(outputId = "realdata_growth_curve")),
              fluidRow(plotlyOutput(outputId = "realdata_growth_rate")),
              fluidRow(plotlyOutput(outputId = "realdata_growth_acceleration"))
            )
        )
      ),
      column(
        width = 6,
        box(title = "Theoretical model",width = "100%",
            fluidPage(
              fluidRow(plotlyOutput(outputId = "exdata_growth_curve")),
              fluidRow(plotlyOutput(outputId = "exdata_growth_rate")),
              fluidRow(plotlyOutput(outputId = "exdata_growth_acceleration"))
            )
        )
      )
    )
  )
})