##### File: app.R
##### License: GPLv3 or later
##### Modification date: 05 Apr 2020
##### Written by: Adam Taiti Harth Utsunomiya
##### Contact: adamtaiti@gmail.com
##### Description: main script to run the COVID-19 accelerometer app
library(shinytest)
# Required libraries -----------------------------------------------------------------------------------------------------
source("libraries.R")
source("functions.R")

# Pre-sets for the environment -------------------------------------------------------------------------------------------
options(shiny.maxRequestSize = 5*(1024^2))
translate <- read.table(file = "translations.txt", header = T, sep = ",", stringsAsFactors = F)
langchoices<-unique(sort(translate$language))
lang <- "English"#NULL
theme_set(theme_bw())
realdata <- NULL
exdata <- simulation()
# Get countries shapes
worldmap<-world
pallete<-colorRamp(c('white','yellow', '#ff1e1e'))

#Get code for user interface ---------------------------------------------------------------------------------------------
header <- dashboardHeaderPlus(
  title = "COVID-19", 
  left_menu = tagList(
    dropdownBlock(
      id = "selectlanguage",
      title = "Select language",
      icon = "language",
      selectInput(inputId = "lt",label = "",choices = langchoices, selected = langchoices[1]))
  )
)

sidebar <- dashboardSidebar(withSpinner(sidebarMenuOutput("sidebarmenu")), collapsed = FALSE, disable = FALSE)
body <- dashboardBody(
  useShinyalert(),
  tags$head(tags$style(
    HTML(".tab-content a {color:#3d3ec1; font-weight:700;}
         code {background-color:#f4d5de; color:#ac3006; font-weigth:700;}
         span.label {color:none;}
         span.label.label-danger {opacity: 0; background-color:#d9534f00!important;}"))
    ),
  withSpinner(uiOutput("dashboardbody"))
)
ui <- dashboardPage(
  title = "Covid-19 Accelerometer",
  header = header,
  sidebar = sidebar,
  body = body,
  skin = "blue"
)

#Get code for server reaction --------------------------------------------------------------------------------------------
server <- function(input, output, session) {
  tablelang<-read.csv(file = "example_data/languages.csv", header = T, sep = ";")
  
  # Clicking button
  observeEvent(input$lt,{
    
    # Define language
    lang <- input$lt
  
    # Get real data
    realdata <- connection()
    
    # Get epidemiological week
    weeks <- weeks(realdata, pallete)
    
    names(worldmap)[2]<-"Countries and territories"
    map_centre = st_centroid(worldmap %>% filter(`Countries and territories` == "Tunisia")) %>% 
      st_coordinates()
    
    real <- reactive(getrealdata(realdata=realdata,
                                 country=input$selcountry,
                                 smooth = input$smoothrange,
                                 hmmclass = input$hmmrange))
    
    # Render sidebarmenu
    output$sidebarmenu <- renderMenu({
      source(file.path("./", "sidebarmenu.R"),  local = TRUE)$value
    })
    
    # Render dashboard body
    output$dashboardbody <- renderUI({
      source(file.path("./", "dashboardbody.R"),  local = TRUE)$value
    })
    
    # Run backend
    source(file.path("./", "server_worldwide.R"),  local = TRUE)$value
    source(file.path("./", "server_local.R"),  local = TRUE)$value
    source(file.path("./", "server_download.R"),  local = TRUE)$value
    if(!is.null(realdata)){
      source(file.path("./", "server_country.R"),  local = TRUE)$value
    }
    
  })
  
}

#Launch app
shinyApp(ui, server, options = "test.mode")
