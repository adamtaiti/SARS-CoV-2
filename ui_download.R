##### File: ui_welcome.R
##### License: GPLv3 or later
##### Modification date: 06 Apr 2020
##### Written by: Adam Taiti Harth Utsunomiya
##### Contact: adamtaiti@gmail.com
##### Description: user interface for the 'Download' page

tabItem(tabName = "download",
        fluidPage(
          HTML(translate$text[which(translate$item == "download" & translate$language == lang)])
        )
)