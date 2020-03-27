output$menu<-renderMenu({
  sidebarMenu(
    id = "sidebarmenu",
    menuItem(text = "Welcome", icon = icon("info"), tabName = "welcome", selected = T),
    menuItem(text = "Worldwide", icon = icon("globe"), tabName = "worldwide", selected = F),
    menuItem(text = "by Country", icon = icon("flag"), tabName = "country", selected = F),
    conditionalPanel(
      "input.sidebarmenu === 'country'",
      selectInput(
        inputId = "selcountry", label = "Select country", choices = getcountries(realdata),
        selected = "China"
      ),
      sliderInput(
        inputId = "smoothrange", label = "Smooth factor", min = 3, max = 10, step = 1, value = 5)
    ),
    menuItem(text = "Local", icon = icon("flag"), tabName = "local", selected = F),
    conditionalPanel(
      "input.sidebarmenu === 'local'",
      fileInput(inputId = "loadfile",label = NULL,buttonLabel = "Upload",accept = c("xlsx", "xls"), multiple = F, width = "100%"),
      sliderInput(
        inputId = "smoothrangel", label = "Smooth factor", min = 3, max = 10, step = 1, value = 5)
    )
  )
})