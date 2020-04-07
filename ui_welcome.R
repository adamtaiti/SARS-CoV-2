##### File: ui_welcome.R
##### License: GPLv3 or later
##### Modification date: 05 Apr 2020
##### Written by: Adam Taiti Harth Utsunomiya
##### Contact: adamtaiti@gmail.com
##### Description: user interface for the 'Worldwide' page

tabItem(tabName = "welcome",
        fluidPage(
          HTML(translate$text[which(translate$item == "welcome" & translate$language == lang)])
        )
)