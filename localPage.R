output$localPage<-renderUI({
  fluidPage(
    fluidRow(
      column(
        width = 6,
        box(title = "Real Data", width = "100%",
            fluidPage(
              fluidRow(plotlyOutput(outputId = "localdata_growth_curve")),
              fluidRow(plotlyOutput(outputId = "localdata_growth_rate")),
              fluidRow(plotlyOutput(outputId = "localdata_growth_acceleration"))
            )
        )
      ),
      column(
        width = 6,
        box(title = "Theoretical model",width = "100%",
            fluidPage(
              fluidRow(plotlyOutput(outputId = "lexdata_growth_curve")),
              fluidRow(plotlyOutput(outputId = "lexdata_growth_rate")),
              fluidRow(plotlyOutput(outputId = "lexdata_growth_acceleration"))
            )
        )
      )
    )
  )
})