##### File: ui_country.R
##### License: GPLv3 or later
##### Modification date: 05 Apr 2020
##### Written by: Adam Taiti Harth Utsunomiya
##### Contact: adamtaiti@gmail.com
##### Description: user interface for the 'by Country' page

tabItem(tabName = "country",
        fluidPage(
          column(width = 4, offset = 2,
                 infoBox(title = translate$text[which(translate$item == "pnextday" & translate$language == lang)],
                         value = textOutput(outputId = "prev"), width = "100%", fill = TRUE, color = "yellow")),
          column(width = 4, offset = 0,
                 infoBox(title = translate$text[which(translate$item == "cnextday" & translate$language == lang)],
                         value = textOutput(outputId = "newc"), width = "100%",fill = TRUE, color = "red")),
          fluidRow(
            column(width = 6,
                   box(title = translate$text[which(translate$item == "realdata" & translate$language == lang)],
                       width = "100%",
                       fluidPage(
                         fluidRow(withSpinner(plotlyOutput(outputId = "realdata_growth_curve"))),
                         fluidRow(withSpinner(plotlyOutput(outputId = "realdata_growth_rate"))),
                         fluidRow(withSpinner(plotlyOutput(outputId = "realdata_growth_acceleration")))
                       )
                   )
            ),
            column(
              width = 6,
              box(title = translate$text[which(translate$item == "tmodel" & translate$language == lang)],width = "100%",
                  fluidPage(
                    fluidRow(withSpinner(plotlyOutput(outputId = "exdata_growth_curve"))),
                    fluidRow(withSpinner(plotlyOutput(outputId = "exdata_growth_rate"))),
                    fluidRow(withSpinner(plotlyOutput(outputId = "exdata_growth_acceleration")))))
            )
          )
        )
)