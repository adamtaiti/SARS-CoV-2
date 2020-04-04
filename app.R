if(!require("shiny")) library(shiny, quietly = T, verbose = F, warn.conflicts = F)
if(!require("shinydashboard")) library(shinydashboard, quietly = T, verbose = F, warn.conflicts = F)
if(!require("shinydashboardPlus")) library(shinydashboardPlus, quietly = T, verbose = F, warn.conflicts = F)
if(!require("shinyalert")) library(shinyalert, quietly = T, verbose = F, warn.conflicts = F)
if(!require("readxl")) library(readxl, quietly = T, verbose = F, warn.conflicts = F)
# To install the bellow package you must have the following library installed in you machine
# ----------------------------------------------------------------
# * deb: libssl-dev (Debian, Ubuntu, etc)
# * rpm: openssl-devel (Fedora, CentOS, RHEL)
# * csw: libssl_dev (Solaris)
# * brew: openssl@1.1 (Mac OSX)
# ----------------------------------------------------------------
# * deb: libcurl4-openssl-dev (Debian, Ubuntu, etc)
# * rpm: libcurl-devel (Fedora, CentOS, RHEL)
# * csw: libcurl_dev (Solaris)
if(!require("httr")) library(httr, quietly = T, verbose = F, warn.conflicts = F)
if(!require("plotly")) library(plotly, quietly = T, verbose = F, warn.conflicts = F)
if(!require("HMM")) library(HMM, quietly = T, verbose = F, warn.conflicts = F)

source("functions.R")

theme_set(theme_bw())
realdata<-NULL
exdata<-simulation()

ui <- dashboardPagePlus(
  useShinyalert(),enable_preloader = T,loading_duration = 0.5,
  header = dashboardHeader(title = "COVID-19 ACCELEROMETER"),
  sidebar = dashboardSidebar(sidebarMenuOutput(outputId = "menu")),
  body = dashboardBody(
    tabItems(
      tabItem(tabName = "welcome", uiOutput(outputId = "welcomePage")),
      tabItem(tabName = "worldwide", uiOutput(outputId = "worldwidePage")),
      tabItem(tabName = "country", uiOutput(outputId = "countryPage")),
      tabItem(tabName = "local", uiOutput(outputId = "localPage")),
      tabItem(tabName = "help", uiOutput(outputId = "helpPage"))
    )
  )
)

server <- function(input, output, session) {
  realdata<-connection()
  source(file.path("./", "sidebarmenu.R"),  local = TRUE)$value
  source(file.path("./", "welcomePage.R"),  local = TRUE)$value
  source(file.path("./", "worldwidePage.R"),  local = TRUE)$value
  source(file.path("./", "countryPage.R"),  local = TRUE)$value
  source(file.path("./", "localPage.R"),  local = TRUE)$value
  source(file.path("./", "helpPage.R"),  local = TRUE)$value
  
  output$exdata_growth_curve<-renderPlotly({
    ggplotly(plot_simulated_growth_curve(exdata), tooltip="text")
  })
  output$exdata_growth_rate<-renderPlotly({
    ggplotly(plot_simulated_growth_rate(exdata), tooltip="text")
  })
  output$exdata_growth_acceleration<-renderPlotly({
    ggplotly(plot_simulated_growth_acceleration(exdata), tooltip="text")
  })
  
  output$lexdata_growth_curve<-renderPlotly({
    ggplotly(plot_simulated_growth_curve(exdata), tooltip="text")
  })
  output$lexdata_growth_rate<-renderPlotly({
    ggplotly(plot_simulated_growth_rate(exdata), tooltip="text")
  })
  output$lexdata_growth_acceleration<-renderPlotly({
    ggplotly(plot_simulated_growth_acceleration(exdata), tooltip="text")
  })
  
  if(!is.null(realdata)){
    
    output$worldwidecases<-renderPlotly({
      df<-realdata[which(realdata$Cases>=1),]
      df<-as.data.frame(tapply(df$Cases, df$DateRep, sum))
      df<-cumsum(df)
      names(df)<-c("n")
      df$n<-df$n/1000
      df$date <- as.Date(row.names(df))
      df<-df[order(df$date),]
      Pandemic<-as.Date("2020-03-11")
      plot_worldwide(df, Pandemic)
    })
    
    output$worldwidedeaths<-renderPlotly({
      df<-realdata[which(realdata$Deaths>=1),]
      df<-as.data.frame(tapply(df$Deaths, df$DateRep, sum))
      df<-cumsum(df)
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
    })
    
    output$newc<-renderText({
      as.numeric(real()[nrow(real()),"c"])
    })
    
    output$realdata_growth_curve<-renderPlotly({
      plot_realdata_growth_curve(df = real())
    })
    
    output$realdata_growth_rate<-renderPlotly({
      plot_realdata_growth_rate(df = real())
    })
    
    output$realdata_growth_acceleration<-renderPlotly({
      plot_realdata_growth_acceleration(df = real())
    })
    
  }
  
  # Local data
  # -----------------------------------------------------------------------------------------------
  observeEvent(input$loadfile,{
    dflocal<-getfile(file = input$loadfile)
    if(is.null(dflocal)){
      NULL
    } else{
      if(ncol(dflocal)==2){
        output$localdata_growth_curve<-renderPlotly({
          plot_localdata_growth_curve(df = dflocal, smooth = input$smoothrangel, hmmfactor = input$hmmrangel)
        })
        output$localdata_growth_rate<-renderPlotly({
          plot_localdata_growth_rate(df = dflocal, smooth = input$smoothrangel, hmmfactor = input$hmmrangel)
        })
        output$localdata_growth_acceleration<-renderPlotly({
          plot_localdata_growth_acceleration(df = dflocal, smooth = input$smoothrangel, hmmfactor = input$hmmrangel)
        })
      } else{
        insertUI(
          selector = "#loadfile",where = "beforeEnd",session = session,
          ui = selectInput(
            inputId = "filterColumn",label = "Choose your grouping variable",choices = colnames(dflocal)[-(1:2)]
          )
        ) 
      }
    }
  })
  
}

shinyApp(ui = ui, server = server)