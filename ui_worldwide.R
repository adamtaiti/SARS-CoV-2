##### File: ui_worldwide.R
##### License: GPLv3 or later
##### Modification date: 05 Apr 2020
##### Written by: Adam Taiti Harth Utsunomiya
##### Contact: adamtaiti@gmail.com
##### Description: user interface for Worldwide page

tabItem(tabName = "worldwide",
        fluidPage(
          fluidRow(
            column(width = 12,
                   valueBox(subtitle = translate$text[which(translate$item == "worldcases" & translate$language == lang)],
                            value = textOutput("tcases"),
                            color = "yellow",
                            icon = icon("chart-bar")),
                   valueBox(subtitle = translate$text[which(translate$item == "worlddeaths" & translate$language == lang)],
                            value = textOutput("tdeaths"),
                            color = "red",
                            icon = icon("chart-line")),
                   valueBox(subtitle = translate$text[which(translate$item == "worldcountries" & translate$language == lang)],
                            value = textOutput("tcountries"),
                            color = "teal",
                            icon = icon("globe")))),
          fluidRow(
            flipBox(id = 1, width = 12,
                    main_img = "img/cough.svg",
                    header_img = "img/quarantine.svg",
                    front_title = translate$text[which(translate$item == "worldwidecases" & translate$language == lang)],
                    back_title = translate$text[which(translate$item == "worldwidedeaths" & translate$language == lang)],
                    plotlyOutput(outputId = "worldwidecases"),
                    back_content = tagList(column(width = 12,
                                                  align = "center",
                                                  plotlyOutput(outputId = "worldwidedeaths")))))
        )
)