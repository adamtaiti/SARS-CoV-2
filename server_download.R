output$downloadtable<-renderDT(
  {
    sourcedata<-read.csv(file = "example_data/exampledata.csv", header = T, sep = ";")
    sourcedata <- sourcedata[,c(lang,"file","maintainer","updated","source","link_file","link_source")] %>% 
      mutate(file = paste0("<a href='", link_file,"' target='_blank'>", file,"</a>")) %>% 
      mutate(source = paste0("<a href='", link_source,"' target='_blank'>", source,"</a>"))
    
    sourcedata<-sourcedata[,c(lang,"file","maintainer","updated","source")]
    sourcedata$updated<-as.Date(sourcedata$updated, origin="1900-01-01")
    colnames(sourcedata)<-c("Dataset","File","Maintainer","Updated","Source")
    colnames(sourcedata)<-c(translate$text[which(translate$item == "datasetcol" & translate$language == lang)],
                            translate$text[which(translate$item == "filecol" & translate$language == lang)],
                            translate$text[which(translate$item == "maintainercol" & translate$language == lang)],
                            translate$text[which(translate$item == "updatedcol" & translate$language == lang)],
                            translate$text[which(translate$item == "sourcecol" & translate$language == lang)]
                            )
    sourcedata
  },rownames=F,
  options = list(
    language = list(url = tablelang$file[which(tablelang$language==lang)]),
    pageLength = 5,
    lengthMenu = c(5, 10, 15, 20),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#9EBFE0', 'color': '#fff'});",
      "}")
  ), escape = F
)