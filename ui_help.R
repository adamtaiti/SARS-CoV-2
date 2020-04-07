##### File: ui_help.R
##### License: GPLv3 or later
##### Modification date: 05 Apr 2020
##### Written by: Adam Taiti Harth Utsunomiya
##### Contact: adamtaiti@gmail.com
##### Description: user interface for the 'Help' page

tabItem(tabName = "help",
        fluidPage(
          HTML(translate$text[which(translate$item == "help" & translate$language == lang)])
        )
)