##### File: ui_worldwide.R
##### License: GPLv3 or later
##### Modification date: 05 Apr 2020
##### Written by: Adam Taiti Harth Utsunomiya
##### Contact: adamtaiti@gmail.com
##### Description: user interface for Worldwide page

tabItem(
  tabName = "worldwide",
  fluidPage(
    fluidRow(
      column(
        width = 4,
        fluidRow(
          valueBox(subtitle = translate$text[which(translate$item == "worldcases" & translate$language == lang)],
                   value = textOutput("tcases"),
                   color = "yellow", width = "100%",
                   icon = icon("chart-bar"))),
        fluidRow(
          valueBox(subtitle = translate$text[which(translate$item == "worlddeaths" & translate$language == lang)],
                   value = textOutput("tdeaths"),
                   color = "red", width = "100%",
                   icon = icon("chart-line"))),
        fluidRow(
          valueBox(subtitle = translate$text[which(translate$item == "worldcountries" & translate$language == lang)],
                   value = textOutput("tcountries"),
                   color = "teal", width = "100%",
                   icon = icon("globe")))
      ),
      column(
        width = 8, 
        fluidRow(
          column(width = 12, withSpinner(leafletOutput(outputId = "leafletcases", width = "100%")))
          ),
        fluidRow(
          column(width = 4,
                 selectInput(
                   inputId = "selmapview", label = "Display",width = "100%",
                   choices = c("Prevalence","Prevalence per million habitants","Weekly cases")
                 )
          ),
          column(width = 8,
                 sliderInput(inputId = "leafletdates",width = "100%",
                             label = "Epidemiological week:",
                             min = min(weeks$week), 
                             max = max(weeks$week),value = min(weeks$week),step = 1,
                             animate = T, ticks = T)
          )
        )
      )
    ),
    fluidRow(
      flipBox(
        id = 1, width = 12,
        main_img = "img/cough.svg",
        header_img = "img/quarantine.svg",
        front_title = translate$text[which(translate$item == "worldwidecases" & translate$language == lang)],
        back_title = translate$text[which(translate$item == "worldwidedeaths" & translate$language == lang)],
        withSpinner(plotlyOutput(outputId = "worldwidecases", width = "100%")),
        back_content = tagList(
          withSpinner(plotlyOutput(outputId = "worldwidedeaths", width = "100%"))
        )
      )
    )
  )
)