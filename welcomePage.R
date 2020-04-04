output$welcomePage<-renderUI({
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
})