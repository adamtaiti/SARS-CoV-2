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
  useShinyalert(),
  header = dashboardHeader(title = "COVID-19 ACCELEROMETER"),
  sidebar = dashboardSidebar(sidebarMenuOutput(outputId = "menu")),
  body = dashboardBody(
    tabItems(
      tabItem(tabName = "welcome",
              fluidPage(
                h1(strong("Welcome to the COVID-19 Accelerometer")),br(),
                p("This application is designed to visualize the growth rate and acceleration of the COVID-19 pandemic."),
                p("It automatically loads the latest cases reports from the European Center for Disease Prevention and Control (ECDC)."),
                a(href="https://opendata.ecdc.europa.eu/covid19/casedistribution/csv","https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"),br(),
                h3(strong("How to use:")),
                p("Here we provided an easy and acessible way to interpretate the dissemination of COVID-19 via the following tabs:"),
                tags$ul(
                  tags$li(p(strong("Worldwide: "),"Overview of the worldwide cases, deaths and countries affected.")),
                  tags$li(p(strong("Country: ")," Presents growth curve, growth rate and growth acceleration for any selectable country contained in the ECDC data.")),
                  tags$li(p(strong("Local: "),"Allows user to upload their own data to visualize the growth curve, growth rate and growth acceleration of specific states, provinces, cities or aggregate data from arbitrary territory definitions."),
                          p(em("It is necessary for the user to create an excel spreadsheet containing only two columns, namely ",strong("date ('YYYY-mm-dd')")," and ",strong("ncases (number of cases)")))),
                  tags$li(p(strong("Help: "),"Provides details on how to interpret the growth graphs, as well as on the methodology used."))
                ),br(),
                h3(strong("Reference")),
                p("The article describing the methodology used here is currently under peer-review. A preprint of an earlier draft can be accessed at medRxiv"),
                p("Utsunomiya YT, Utsunomiya ATH, Torrecilha RBP, Paulan SC, Milanesi M, Garcia JF. ",br(),"Growth rate and acceleration analysis of the COVID-19 pandemic reveals the effect of public health measures in real time."),
                p("doi: ",a(href="https://doi.org/10.1101/2020.03.30.20047688","https://doi.org/10.1101/2020.03.30.20047688")),
                br(),
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
                    fluidRow(plotlyOutput(outputId = "exdata_growth_curve")),
                    fluidRow(plotlyOutput(outputId = "exdata_growth_rate")),
                    fluidRow(plotlyOutput(outputId = "exdata_growth_acceleration"))
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
                    fluidRow(plotlyOutput(outputId = "lexdata_growth_curve")),
                    fluidRow(plotlyOutput(outputId = "lexdata_growth_rate")),
                    fluidRow(plotlyOutput(outputId = "lexdata_growth_acceleration"))
                  )
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "help",
        fluidPage(
          tags$div(
            h1("Understanding the plots"),
            br(),
            h3("The behavior of the prevalence of COVID-19 over time is decomposed into three graphs (estimated via moving regression):"),
            tags$ul(
              tags$li(strong("Growth curve - total number of cases")),
              p("In this graph, each point represents the cummulative number of cases in a given day. The fitted curve is approximated by moving regression and corresponds to a smoothed trend obtained from the points. The whole graph can be interpreted as the evolution of prevalence over time. Theoretical standard growth curves are typically sigmoid ('S'-shaped), but the dynamics of COVID-19 prevalence in practice may deviate substantially from that shape. In an analogy to a moving car, the growth curve could be interpreted as the travaled distance over time. For the car, we would measure growth in meters (m) or kilometers (km), whereas for prevalence growth is measured in total number of cases."),
              tags$li(strong("Growth rate - new cases per day")),
              p("This curve reveals the instanteneous rate of change in number of new cases over time. It is usually bell-shaped, with its peak corresponding to maximum growth. The growth rate is technically defined as the first order derivative of the growth curve, and in the moving car analogy is equivalent to speed. For the car, we would measure growth rate in meters per second (m/s) or kilometers per hour (km/h), while for prevalence we measure growth rate in cases per day (cases/day)."),
              tags$li(strong("Growth acceleration - new cases per day per day")),
              p("This graph shows how the growth rate changes over time, and it is technically the second order derivative of the growth curve. It consists of combinations of two bell-shaped curves: the first one with a peak and the second with a valley. The peak indicates the maximum acceleration and is the point where acceleration starts descending towards zero. The moment when acceleration is exactly zero coincides with the inflection of growth curve, which marks the beginning of growth deceleration (i.e., negative acceleration). The latter corresponds to the entire concave section of the curve, but the very bottom of the valley indicates that the prevalence is moving towards stagnation. Back to the car, we would measure acceleration in meters per second squared (m/s²) or kilometers per hour squared (km/h²), while for prevalence we express it in cases per day squared (cases/day²). When acceleration is rising, it is analagous to pressing the accelerator pedal. When it is declining, it is equivalent to releasing the accelerator pedal. When acceleration is negative, then it is similar to hitting the brakes.")
            ),
            br(),
            h3("The dynamics of the COVID-19 pandemic is complex and may vary greatly across regions. However, the pandemic evolves by combination of five distinguishable stages (predicted by Hidden Markov Model):"),
            tags$ul(
              tags$li(strong("The lagging stage (Green)")),
              p("This corresponds to the beginning of the outbreak or disease importation, where the number of cases are low and increase only marginally every day."),
              tags$li(strong("The exponential stage (Red)")),
              p("Growth is accelerated and the number of new cases increase rapidly every day."),
              tags$li(strong("The deceleration stage (Yellow)")),
              p("This is a hopeful phase, where the number of new cases reduces day-by-day."),
              tags$li(strong("The stationary stage (Blue)")),
              p("Characterized by stagnation of the prevalence with sporadic new cases occurring each day. This is the stage everyone wants to reach."),
              tags$li(strong("The linear stage (Purple)")),
              p("This is an atypical phase, characterized by constant growth. This stage may happen when the deceleration phase does not deepen enough to produce a stationary phase.")
            ),
            h3("Users are encouraged to pay attention to the dates near stage transitions, as they may be related to events that had an impact on disease dynamics. Was any public health measure implemented or discontinued? Do these dates coincide with big government announcements?"),
            br(),
            h3("Below we exemplify possible scenarios for the evolution of a COVID-19 epidemic within a country or territory."),
            tags$ul(
              tags$li(strong("lagging -> exponential -> deceleration -> stationary")),
              p("This is the expected scenario for most countries. The disease is introduced, grows exponentially, decelerates thanks to effective control measures and then become stationary."),
              tags$li(strong("lagging -> exponential -> deceleration -> stationary -> exponential -> deceleration -> stationary")),
              p("Reaching a stationary growth does not guarantee indefinite control. The disease can grow again, for example by re-introduction of the disease via importation."),
              tags$li(strong("lagging -> N*(exponential -> deceleration) -> stationary")),
              p("A country that is decelerating could accelerate again, generating multiple cycles of exponential and deceleration stages. This could result from relaxing control measures when deceleration is perceived.")
            ),
            tags$ul(
              tags$li(p("Apart from these three likely scenarios, we anticipate that more complex permutations of these stages may take place in the evolution of some countries."))
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
    
    # Local data
    # -----------------------------------------------------------------------------------------------
    observeEvent(input$loadfile,{
      dflocal<-getfile(file = input$loadfile)
      
      output$localdata_growth_curve<-renderPlotly({
        plot_localdata_growth_curve(df = dflocal, smooth = input$smoothrangel, hmmfactor = input$hmmrangel)
      })
      output$localdata_growth_rate<-renderPlotly({
        plot_localdata_growth_rate(df = dflocal, smooth = input$smoothrangel, hmmfactor = input$hmmrangel)
      })
      output$localdata_growth_acceleration<-renderPlotly({
        plot_localdata_growth_acceleration(df = dflocal, smooth = input$smoothrangel, hmmfactor = input$hmmrangel)
      })
    })
  }
}

shinyApp(ui = ui, server = server)