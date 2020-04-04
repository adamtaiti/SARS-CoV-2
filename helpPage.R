output$helpPage<-renderUI({
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
})