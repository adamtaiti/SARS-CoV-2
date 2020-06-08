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
# Obs: configured for uploads of up to 10Mb
# For larger files, set shiny.maxRequestSize to the desired upper bound
# It may be furhter necessary to enter /etc/nginx/nginx.conf
# After Basic settings, add client_max_body_size 10M;
# Then restart nginx with $sudo service nginx restart
options(shiny.maxRequestSize = 10*(1024^2))
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
  title = "COVID-19"
  #, 
  #left_menu = tagList(
  #  dropdownBlock(
  #    id = "selectlanguage",
  #    title = "Select language",
  #    icon = "language",
  #    selectInput(inputId = "lt",label = "",choices = langchoices, selected = "English"))
  #)
)

sidebar <- dashboardSidebar(withSpinner(sidebarMenuOutput("sidebarmenu")), collapsed = T, disable = F)
body <- dashboardBody(
  #tags$head(includeHTML("./google-analytics.html")),
  tags$head(includeScript("google-analytics.js")),
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
  
  # Get real data
  realdata <- connection()
  
  # Get epidemiological week
  weeks <- weeks(realdata, pallete)
  names(worldmap)[2]<-"Countries and territories"
  
  # Get map centroid
  map_centre = st_centroid(worldmap %>% filter(`Countries and territories` == "Tunisia")) %>% 
    st_coordinates()
  
  # Update language button
  observeEvent(input$language,{
    updateActionButton(session = session, inputId = "enter",label = translate$text[which(translate$item == "enter" & translate$language == input$language)])
  })
  
  # Initialize page
  output$dashboardbody <- renderUI({
    source(file.path("./", "ui_language.R"),  local = TRUE)$value
  })
  
  # Clicking button
  observeEvent(input$enter,{
    # Clear interface
    shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    #shinyjs::removeClass(selector = "body", class = "sidebar-disable")
    
    # Define language
    #lang <- input$lt
    lang <- input$language
    
    real <- reactive(getrealdata(realdata=realdata,
                                 country=input$selcountry,
                                 smooth = input$smoothrange,
                                 hmmclass = input$hmmrange))
    
    # # Alert
    # shinyalert(title = translate$text[which(translate$item == "welcomewarn" & translate$language == lang)],
    #            text = translate$text[which(translate$item == "welcomewarnmsg" & translate$language == lang)],
    #            type = "warning")
    
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
  
  session$onSessionEnded(function(){
    if(file.exists(paste0("./temp/",session$token,"*"))){
      file.remove(paste0("./temp/",session$token,"*"))
    }
  })
}

#Launch app
shinyApp(ui, server, options = "test.mode")
