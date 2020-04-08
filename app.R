##### File: app.R
##### License: GPLv3 or later
##### Modification date: 05 Apr 2020
##### Written by: Adam Taiti Harth Utsunomiya
##### Contact: adamtaiti@gmail.com
##### Description: main script to run the COVID-19 accelerometer app

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

#Get code for user interface ---------------------------------------------------------------------------------------------
header <- dashboardHeaderPlus(
  title = "COVID-19", 
  left_menu = tagList(
    dropdownBlock(
      id = "selectlanguage",
      title = "Select language",
      icon = "language",
      selectInput(inputId = "lt",label = "English",choices = langchoices, selected = langchoices[1]))
  )
)
  
sidebar <- dashboardSidebar(withSpinner(sidebarMenuOutput("sidebarmenu")), collapsed = FALSE, disable = FALSE)
body <- dashboardBody(withSpinner(uiOutput("dashboardbody")))
ui <- dashboardPage(
  useShinyalert(),
  header = header,
  sidebar = sidebar,
  body = body,
  skin = "blue"
)

#Get code for server reaction --------------------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Clicking button
  observeEvent(input$lt,{
    
    updateSelectInput(session = session, inputId = "lt",label = translate$text[which(translate$item == "lt" & translate$language == lang)], choices = langchoices, selected = input$lt)
    # Define language
    #lang <- input$language
    lang <- input$lt
    
    # # Clear interface
    shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    
    # Get real data
    realdata <- connection()
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
    if(!is.null(realdata)){
      source(file.path("./", "server_country.R"),  local = TRUE)$value
    }
    
  })
  
}

#Launch app
shinyApp(ui, server)
