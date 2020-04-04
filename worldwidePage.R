output$worldwidePage<-renderUI({
  fluidPage(
    fluidRow(
      column(
        width = 12,
        valueBox(subtitle = "Cases", value = textOutput("tcases"), color = "yellow", icon = icon("chart-bar")),
        valueBox(subtitle = "Deaths", value = textOutput("tdeaths"), color = "red", icon = icon("chart-line")),
        valueBox(subtitle = "Countries", value = textOutput("tcountries"), color = "teal", icon = icon("globe"))
      )
    ),
    fluidRow(
      flipBox(
        id = 1,width = 12,
        main_img = "img/cough.svg",
        header_img = "img/quarantine.svg",
        front_title = "Worldwide cases",
        back_title = "Worldwide deaths",
        plotlyOutput(outputId = "worldwidecases"),
        back_content = tagList(
          column(
            width = 12,
            align = "center",
            plotlyOutput(outputId = "worldwidedeaths")#, height = "600px")
          )
          
        )
      )
    )
  )
})