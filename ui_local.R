##### File: ui_local.R
##### License: GPLv3 or later
##### Modification date: 05 Apr 2020
##### Written by: Adam Taiti Harth Utsunomiya
##### Contact: adamtaiti@gmail.com
##### Description: user interface for the 'Local' page

tabItem(tabName = "local",
        fluidPage(
          fluidRow(
            column(width = 3, uiOutput("partitioncol")),
            column(width = 3, uiOutput("partitionopt"))
          ),
          fluidRow(
            column(
              width = 6,
              box(title = translate$text[which(translate$item == "realdata" & translate$language == lang)], width = "100%",
                  fluidPage(
                    fluidRow(plotlyOutput(outputId = "localdata_growth_curve")),
                    fluidRow(plotlyOutput(outputId = "localdata_growth_rate")),
                    fluidRow(plotlyOutput(outputId = "localdata_growth_acceleration"))
                  )
              )
            ),
            column(
              width = 6,
              box(title = translate$text[which(translate$item == "tmodel" & translate$language == lang)],width = "100%",
                  fluidPage(
                    fluidRow(withSpinner(plotlyOutput(outputId = "lexdata_growth_curve"))),
                    fluidRow(withSpinner(plotlyOutput(outputId = "lexdata_growth_rate"))),
                    fluidRow(withSpinner(plotlyOutput(outputId = "lexdata_growth_acceleration")))
                  )
              )
            )
          )
        )
)