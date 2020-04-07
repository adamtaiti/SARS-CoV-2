##### File: dashboardbody.R
##### License: GPLv3 or later
##### Modification date: 05 Apr 2020
##### Written by: Adam Taiti Harth Utsunomiya
##### Contact: adamtaiti@gmail.com
##### Description: dashboard body for the COVID-19 accelerometer app

tabItems(
    source(file.path("./", "ui_welcome.R"),  local = TRUE)$value,
    source(file.path("./", "ui_worldwide.R"),  local = TRUE)$value,
    source(file.path("./", "ui_country.R"),  local = TRUE)$value,
    source(file.path("./", "ui_local.R"),  local = TRUE)$value,
    source(file.path("./", "ui_download.R"),  local = TRUE)$value,
    source(file.path("./", "ui_help.R"),  local = TRUE)$value
)
