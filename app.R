if(!require("shiny")) install.packages("shiny");library(shiny, quietly = T, verbose = F, warn.conflicts = F)
if(!require("shinydashboard")) install.packages("shinydashboard");library(shinydashboard, quietly = T, verbose = F, warn.conflicts = F)
if(!require("shinydashboardPlus")) install.packages("shinydashboardPlus");library(shinydashboardPlus, quietly = T, verbose = F, warn.conflicts = F)
if(!require("shinyalert")) install.packages("shinyalert");library(shinyalert, quietly = T, verbose = F, warn.conflicts = F)
if(!require("readxl")) install.packages("readxl");library(readxl, quietly = T, verbose = F, warn.conflicts = F)
if(!require("httr")) install.packages("httr");library(httr, quietly = T, verbose = F, warn.conflicts = F)
if(!require("utils")) install.packages("utils");library(httr, quietly = T, verbose = F, warn.conflicts = F)
if(!require("plotly")) install.packages("plotly");library(plotly, quietly = T, verbose = F, warn.conflicts = F)

source("functions.R")
#source("excurve.R")

theme_set(theme_bw())
realdata<-NULL
exdata<-simulation()

ui <- dashboardPagePlus(
  useShinyalert(),
  header = dashboardHeader(title = "COVID-19 ACCELEROMETER"),
  sidebar = dashboardSidebar(sidebarMenuOutput(outputId = "menu")),
  body = dashboardBody(
    tabItems(
      tabItem(tabName = "welcome",
              fluidPage(
                h1(strong("Welcome to the COVID-19 Accelerometer")),br(),
                p("This application is designed to visualize the growth rate and acceleration of the COVID-19 pandemic."),
                p("It automatically loads the latest cases reports from European Center for Disease Prevention and Control (ECDC)."),
                a(href="https://opendata.ecdc.europa.eu/covid19/casedistribution/csv","https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"),br(),
                h3(strong("How to use:")),
                p("Here we provided an easy and acessible way to interpretate the dissemination of COVID-19 via the following tabs:"),
                tags$ul(
                  tags$li(p(strong("Worldwide: "),"Overview of the worldwide cases, deaths and countries affected.")),
                  tags$li(p(strong("Country: ")," Presents growth curve, growth rate and growth acceleration for any selectable country contained in the ECDC data.")),
                  tags$li(p(strong("Local: "),"Allows user to upload their own data to visualize the growth curve, growth rate and growth acceleration of specific states, provinces, cities or aggregate data from arbitrary territory definitions."),
                          p(em("It is necessary for the user to create an excel spreadsheet containing only two columns, namely ",strong("date ('YYYY-mm-dd')")," and ",strong("ncases (number of cases)"))))
                ),br(),
                h3(strong("Contact us")),
                p("If you have any suggestions for improving the application or would like to report a bug, please send an email to", code("adamtaiti@gmail.com."))
              )
      ),
      tabItem(tabName = "worldwide",
              fluidPage(
                fluidRow(
                  column(width = 12,
                         valueBox(subtitle = "Cases", value = textOutput("tcases"), color = "yellow", icon = icon("chart-bar")),
                         valueBox(subtitle = "Deaths", value = textOutput("tdeaths"), color = "red", icon = icon("chart-line")),
                         valueBox(subtitle = "Countries", value = textOutput("tcountries"), color = "teal", icon = icon("globe"))
                  )
                ),
                fluidRow(
                  flipBox(
                    id = 1,width = 12,
                    main_img = "img/cough.svg",
                    header_img = "img/quarantine.svg",
                    front_title = "Worldwide cases",
                    back_title = "Worldwide deaths",
                    plotlyOutput(outputId = "worldwidecases"),
                    back_content = tagList(
                      column(
                        width = 12,
                        align = "center",
                        plotlyOutput(outputId = "worldwidedeaths")#, height = "600px")
                      )
                      
                    )
                  )
                )
              )
      ),
      tabItem(
        tabName = "country",
        fluidPage(
          column(
            width = 4, offset = 2,
            infoBox(title = "Prevalence next day", value = textOutput(outputId = "prev"),
                    width = "100%",fill = T, color = "yellow")),
          column(
            width = 4, offset = 0,
            infoBox(title = "New cases next day", value = textOutput(outputId = "newc"),
                    width = "100%",fill = T, color = "red")),
          fluidRow(
            column(
              width = 6,
              box(title = "Real Data", width = "100%",
                  fluidPage(
                    fluidRow(plotlyOutput(outputId = "realdata_growth_curve")),
                    fluidRow(plotlyOutput(outputId = "realdata_growth_rate")),
                    fluidRow(plotlyOutput(outputId = "realdata_growth_acceleration"))
                  )
              )
            ),
            column(
              width = 6,
              box(title = "Theoretical model",width = "100%",
                  fluidPage(
                    fluidRow(plotOutput(outputId = "exdata_growth_curve")),
                    fluidRow(plotOutput(outputId = "exdata_growth_rate")),
                    fluidRow(plotOutput(outputId = "exdata_growth_acceleration"))
                  )
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "local",
        fluidPage(
          fluidRow(
            column(
              width = 6,
              box(title = "Real Data", width = "100%",
                  fluidPage(
                    fluidRow(plotlyOutput(outputId = "localdata_growth_curve")),
                    fluidRow(plotlyOutput(outputId = "localdata_growth_rate")),
                    fluidRow(plotlyOutput(outputId = "localdata_growth_acceleration"))
                  )
              )
            ),
            column(
              width = 6,
              box(title = "Theoretical model",width = "100%",
                  fluidPage(
                    fluidRow(plotOutput(outputId = "lexdata_growth_curve")),
                    fluidRow(plotOutput(outputId = "lexdata_growth_rate")),
                    fluidRow(plotOutput(outputId = "lexdata_growth_acceleration"))
                  )
              )
            )
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  realdata<-connection()
  source(file.path("./", "sidebarmenu.R"),  local = TRUE)$value
  output$exdata_growth_curve<-renderPlot({
    plot_simulated_growth_curve(exdata)
  })
  output$exdata_growth_rate<-renderPlot({
    plot_simulated_growth_rate(exdata)
  })
  output$exdata_growth_acceleration<-renderPlot({
    plot_simulated_growth_acceleration(exdata)
  })
  
  output$lexdata_growth_curve<-renderPlot({
    plot_simulated_growth_curve(exdata)
  })
  output$lexdata_growth_rate<-renderPlot({
    plot_simulated_growth_rate(exdata)
  })
  output$lexdata_growth_acceleration<-renderPlot({
    plot_simulated_growth_acceleration(exdata)
  })
  
  if(!is.null(realdata)){
    
    output$worldwidecases<-renderPlotly({
      df<-worldwidecases(realdata)
      names(df)<-c("n")
      df$n<-df$n/1000
      df$date <- as.Date(row.names(df))#,format = "%YYYY-%mm-%dd")
      df<-df[order(df$date),]
      Pandemic<-as.Date("2020-03-11")
      plot_worldwide(df, Pandemic)
      
    })
    
    output$worldwidedeaths<-renderPlotly({
      df<-worldwidedeaths(realdata)
      names(df)<-c("n")
      df$n<-df$n/1000
      df$date <- as.Date(row.names(df))
      Pandemic<-as.Date("2020-03-11")
      
      plot_worldwide(df, Pandemic)
    })
    
    # Real data plots
    # -------------------------------------------------------------------------------------
    
    real<-reactive(getrealdata(realdata=realdata, country=input$selcountry, smooth = input$smoothrange, hmmclass = input$hmmrange))
    
    output$tcases<-renderText({
      as.numeric(ncases(realdata))
    })
    output$tdeaths<-renderText({
      as.numeric(ndeaths(realdata))
    })
    output$tcountries<-renderText({
      as.numeric(ncountries(realdata))
    })
    
    output$prev<-renderText({
      as.numeric(real()[nrow(real()),"fitted"])
      #as.numeric(dataframe$real[nrow(dataframe$real),"fitted"])
    })
    output$newc<-renderText({
      as.numeric(real()[nrow(real()),"c"])
      #as.numeric(dataframe$real[nrow(realdataframe$real),"c"])
    })
    
    output$realdata_growth_curve<-renderPlotly({
      plot_realdata_growth_curve(df = real())
      #plot_realdata_growth_curve(df = dataframe$real)
    })
    
    output$realdata_growth_rate<-renderPlotly({
      plot_realdata_growth_rate(df = real())
      #plot_realdata_growth_rate(df = dataframe$real)
    })
    
    output$realdata_growth_acceleration<-renderPlotly({
      plot_realdata_growth_acceleration(df = real())
      #plot_realdata_growth_acceleration(df = dataframe$real)
    })
    
    # Local data
    # -----------------------------------------------------------------------------------------------
    observeEvent(input$loadfile,{
      dflocal<-getfile(file = input$loadfile)
      
      output$localdata_growth_curve<-renderPlotly({
        plot_localdata_growth_curve(df = dflocal, smooth = input$smoothrangel)
      })
      output$localdata_growth_rate<-renderPlotly({
        plot_localdata_growth_rate(df = dflocal, smooth = input$smoothrangel)
      })
      
      output$localdata_growth_acceleration<-renderPlotly({
        plot_localdata_growth_acceleration(df = dflocal, smooth = input$smoothrangel)
      })
    })
  }
}

shinyApp(ui = ui, server = server)