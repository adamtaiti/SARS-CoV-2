##### File: ui_language.R
##### License: GPLv3 or later
##### Modification date: 05 Apr 2020
##### Written by: Adam Taiti Harth Utsunomiya
##### Contact: adamtaiti@gmail.com
##### Description: user interface for language selection

langselect <- fluidPage(
  title = "The guarani",
  align="center",
  br(),br(),
  fluidRow(column(width=2,offset=5,
                  selectInput(inputId = "language",
                              label = "Select language",
                              choices = langchoices),#c("English","Português","Italiano")),
                  actionButton(inputId = "enter", label = "Enter")))
)