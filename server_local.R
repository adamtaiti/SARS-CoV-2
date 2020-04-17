##### File: server_local.R
##### License: GPLv3 or later
##### Modification date: 05 Apr 2020
##### Written by: Adam Taiti Harth Utsunomiya
##### Contact: adamtaiti@gmail.com
##### Description: backend for the 'Local' page

output$lexdata_growth_curve<-renderPlotly({
  ggplotly(plot_simulated_growth_curve(exdata, translate, lang), tooltip="text")
})

output$lexdata_growth_rate<-renderPlotly({
  ggplotly(plot_simulated_growth_rate(exdata, translate, lang), tooltip="text")
})

output$lexdata_growth_acceleration<-renderPlotly({
  ggplotly(plot_simulated_growth_acceleration(exdata, translate, lang), tooltip="text")
})

observeEvent(input$loadfile,{
  dflocal<-getfile(file = input$loadfile, session = session$token)
  dflocal2 <- dflocal
  if(ncol(dflocal) > 2){
    output$partitioncol <- renderUI({
      selectInput(inputId = "partitioncolsel",
                  label = translate$text[which(translate$item == "selectcol" & translate$language == lang)],
                  choices = colnames(dflocal)[-c(1:2)])
    })
    observeEvent(input$partitioncolsel,{
      output$partitionopt <- renderUI({
        selectInput(inputId = "partitionoptsel",
                    label = translate$text[which(translate$item == "selectterritory" & translate$language == lang)],
                    choices = c(translate$text[which(translate$item == "selectall" & translate$language == lang)],sort(unique(dflocal[,input$partitioncolsel]))))
      })
    })
    observeEvent(input$partitionoptsel,{
      if(input$partitionoptsel == translate$text[which(translate$item == "selectall" & translate$language == lang)]){
        dflocal2 <- dflocal
      }else{
        dflocal2 <- dflocal[which(dflocal[,input$partitioncolsel] == input$partitionoptsel),]
      }
      dflocal2 <- aggregate(Cases ~ DateRep, data = dflocal2, FUN = sum)
      output$localdata_growth_curve<-renderPlotly({
        plot_localdata_growth_curve(df = dflocal2, smooth = input$smoothrangel, hmmfactor = input$hmmrangel, translate, lang)
      })
      output$localdata_growth_rate<-renderPlotly({
        plot_localdata_growth_rate(df = dflocal2, smooth = input$smoothrangel, hmmfactor = input$hmmrangel, translate, lang)
      })
      output$localdata_growth_acceleration<-renderPlotly({
        plot_localdata_growth_acceleration(df = dflocal2, smooth = input$smoothrangel, hmmfactor = input$hmmrangel, translate, lang)
      })
    })
  }else{
    output$partitioncol <- renderUI({

    })
    output$partitionopt <- renderUI({

    })
    dflocal2 <- aggregate(Cases ~ DateRep, data = dflocal2, FUN = sum)
    output$localdata_growth_curve<-renderPlotly({
      plot_localdata_growth_curve(df = dflocal2, smooth = input$smoothrangel, hmmfactor = input$hmmrangel, translate, lang)
    })
    output$localdata_growth_rate<-renderPlotly({
      plot_localdata_growth_rate(df = dflocal2, smooth = input$smoothrangel, hmmfactor = input$hmmrangel, translate, lang)
    })
    output$localdata_growth_acceleration<-renderPlotly({
      plot_localdata_growth_acceleration(df = dflocal2, smooth = input$smoothrangel, hmmfactor = input$hmmrangel, translate, lang)
    })
  }

})
