#--------------------------------------------------------------------------------------------------
# Initial setup
#--------------------------------------------------------------------------------------------------

# To get the application running on your computer you must have installed the following packages:
install.packages("shiny")
install.packages("shinydashboard")
install.packages("shinydashboardPlus")
install.packages("shinyalert")
install.packages("shinyjs")
install.packages("readxl")
install.packages("plotly")
install.packages("HMM")

# To install the package bellow you must have the following library installed in your machine
#--------------------------------------------------------------------------------------------------
# * deb: libssl-dev (Debian, Ubuntu, etc)
# * rpm: openssl-devel (Fedora, CentOS, RHEL)
# * csw: libssl_dev (Solaris)
# * brew: openssl@1.1 (Mac OSX)
#--------------------------------------------------------------------------------------------------
# * deb: libcurl4-openssl-dev (Debian, Ubuntu, etc)
# * rpm: libcurl-devel (Fedora, CentOS, RHEL)
# * csw: libcurl_dev (Solaris)
#--------------------------------------------------------------------------------------------------
# > sudo apt install libssl-dev libcurl4-openssl-dev  gdal-bin proj-bin libgdal-dev libproj-dev
#--------------------------------------------------------------------------------------------------
install.packages("sf")
install.packages("spData")
install.packages("spDataLarge")
install.packages("DT")
install.packages("tidyverse")
install.packages("httr")
install.packages("shinycssloaders")
install.packages("devtools")
install.packages("tmaptools")
install.packages("tmap")
install.packages("leaflet")
install.packages("jsonlite", dependencies=TRUE, INSTALL_opts = c('--no-lock'))
install.packages("rgdal", dependencies=TRUE, INSTALL_opts = c('--no-lock'))
