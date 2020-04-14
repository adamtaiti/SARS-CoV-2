##### File: functions.R
##### License: GPLv3 or later
##### Modification date: 05 Apr 2020
##### Written by: Adam Taiti Harth Utsunomiya
##### Contact: adamtaiti@gmail.com
##### Description: auxiliary functions for the COVID-19 app

# Get ECDC data
connection <- function(){
  tryCatch(
    {
      # Checking if the file was already exists
      # If TRUE then load the file else download and save it to the data directory
      # This process is repeated daily
      file<-paste0(getwd(),"/data/ecdc-",Sys.Date(),".csv")
      
      if(file.exists(file)){
        
        #read the Dataset sheet into “R”. The dataset will be called "data".
        realdata <- read.csv(file)
        
        names(realdata)<-c("DateRep","Day","Month","Year","Cases","Deaths","Countries and territories","GeoId","Code","Pop_Data.2018")
        
        realdata$DateRep<-as.Date(realdata$DateRep, format = "%d/%m/%Y")
        realdata<-realdata[order(realdata$DateRep),]
        realdata$`Countries and territories`<-gsub(pattern = "_", replacement = " ", x = realdata$`Countries and territories`)
        
        return(realdata)
        
      } else{
        
        oldfile<-paste0(getwd(),"/data/ecdc-",Sys.Date()-1,".csv")
        if(file.exists(oldfile)){
          command<-paste0("rm ", oldfile)
          system(command = command)
          
        }
        #download the dataset from the ECDC website to a local temporary file
        GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".csv")))
        
        command<-paste0("mv ",tf, " ",getwd(),"/data/ecdc-",Sys.Date(),".csv")
        system(command = command)
        
        #read the Dataset sheet into “R”. The dataset will be called "data".
        realdata <- read.csv(file)
        
        names(realdata)<-c("DateRep","Day","Month","Year","Cases","Deaths","Countries and territories","GeoId","Code","Pop_Data.2018")
        
        realdata$DateRep<-as.Date(realdata$DateRep, format = "%d/%m/%Y")
        realdata<-realdata[order(realdata$DateRep),]
        realdata$`Countries and territories`<-gsub(pattern = "_", replacement = " ", x = realdata$`Countries and territories`)
        
        return(realdata)
      }
    },
    error=function(e) {
      shinyalert("Error!", paste0(e,"\nUnable to connect to the dataset!\nPlease try it again later."), type = "error")
      realdata<-NULL
      return(realdata)
    },
    warning=function(w) {
      shinyalert("Warning!", paste0(w), type = "warning")
    }
  )
}

# Data simulator
simulation <- function(){
  # SIMULATED DATA
  #----------------------------------------------------------------------------------------
  # Gompertz example data
  exdata <- data.frame(day = 1:100, cases = ceiling(f0(x=1:100,a=10000,k=0.18,i=40))/1000,
                       growth = f1(x=1:100,a=10000,k=0.18,i=40)/1000,
                       acceleration = f2(x=1:100,a=10000,k=0.18,i=40)/1000)
  #----------------------------------------------------------------------------------------
  return(exdata)
}

# Map list of unique countries
getcountries <- function(data){
  if(is.null(data)){
    return("none")
  } else{
    df <- unique(sort(data$`Countries and territories`))
    return(df)
  }
}

# Get number of cases
ncases <- function(data){
  if(is.null(data)){
    return("none")
  } else{
    return(sum(data$Cases))
  }
}

# Get number of deaths
ndeaths <- function(data){
  if(is.null(data)){
    return("none")
  } else{
    return(sum(data$Deaths))
  }
}

# Get number of countries
ncountries <- function(data){
  if(is.null(data)){
    return("none")
  } else{
    return(length(unique(sort(data$`Countries and territories`))))
  }
}

# Growth curve for simulated data
plot_simulated_growth_curve <- function(exdata, translate, lang){
  exdata$phase <- hmm(exdata,5)
  
  p<-ggplot(data = exdata, mapping = aes(x = day, y = cases))+
    geom_line()+
    xlab(label = translate$text[which(translate$item == "daysrecorded" & translate$language == lang)])+
    ylab(label = translate$text[which(translate$item == "thousandcases" & translate$language == lang)])+
    ggtitle(label = translate$text[which(translate$item == "growthcurve" & translate$language == lang)])+
    theme(legend.position = "none")
  
  # Map phases
  runs <- rle(as.vector(exdata$phase))
  phase <- cumsum(runs$lengths)
  names(phase) <- runs$values
  
  params<-ggplot_build(p)
  yrange<-params$layout$panel_params[[1]]$y.range
  
  xmin<-NULL; xmax<-NULL; ymin<-NULL; ymax<-NULL; ph<-NULL;
  
  for(i in 1:length(phase)){
    if(i == 1){
      xmin<-c(xmin,(min(exdata$day)))
      xmax<-c(xmax,(exdata$day[as.numeric(phase[i])]))
      ymin<-c(ymin,yrange[1])
      ymax<-c(ymax,yrange[2])
      ph<-c(ph,names(phase[i]))
    }else if(i < length(phase)){
      xmin<-c(xmin,(exdata$day[as.numeric(phase[i-1])]))
      xmax<-c(xmax,(exdata$day[as.numeric(phase[i])]))
      ymin<-c(ymin,yrange[1])
      ymax<-c(ymax,yrange[2])
      ph<-c(ph,names(phase[i]))
    }else{
      xmin<-c(xmin,(exdata$day[as.numeric(phase[i-1])]))
      xmax<-c(xmax,(max(exdata$day)))
      ymin<-c(ymin,yrange[1])
      ymax<-c(ymax,yrange[2])
      ph<-c(ph,names(phase[i]))
    }
  }
  
  params<-data.frame(xmin,xmax,ymin,ymax,ph)
  
  p<-p+geom_rect(
    data = params,
    mapping=aes(x = NULL, y = NULL, xmin = xmin, xmax = xmax, fill = ph),
    ymin = ymin, ymax = ymax, inherit.aes = F, colour = "black", alpha = 0.3)+
    scale_fill_manual(
      values = c("lagging"="#7ACC9B","exponential"="#FFB3B3","deceleration"="#F0FFB3","stationary"="#B3D1FF")
    )
  p
}

# Growth rate for simulated data
plot_simulated_growth_rate <- function(exdata, translate, lang){
  exdata$phase <- hmm(exdata,5)
  
  p<-ggplot(data = exdata, mapping = aes(x = day, y = growth))+
    geom_line()+
    xlab(label = translate$text[which(translate$item == "daysrecorded" & translate$language == lang)])+
    ylab(label = translate$text[which(translate$item == "thousandcasesday" & translate$language == lang)])+
    ggtitle(label = translate$text[which(translate$item == "growthrate" & translate$language == lang)])+
    theme(legend.position = "none")
  
  # Map phases
  runs <- rle(as.vector(exdata$phase))
  phase <- cumsum(runs$lengths)
  names(phase) <- runs$values
  
  params<-ggplot_build(p)
  yrange<-params$layout$panel_params[[1]]$y.range
  
  xmin<-NULL; xmax<-NULL; ymin<-NULL; ymax<-NULL; ph<-NULL;
  
  for(i in 1:length(phase)){
    if(i == 1){
      xmin<-c(xmin,(min(exdata$day)))
      xmax<-c(xmax,(exdata$day[as.numeric(phase[i])]))
      ymin<-c(ymin,yrange[1])
      ymax<-c(ymax,yrange[2])
      ph<-c(ph,names(phase[i]))
    }else if(i < length(phase)){
      xmin<-c(xmin,(exdata$day[as.numeric(phase[i-1])]))
      xmax<-c(xmax,(exdata$day[as.numeric(phase[i])]))
      ymin<-c(ymin,yrange[1])
      ymax<-c(ymax,yrange[2])
      ph<-c(ph,names(phase[i]))
    }else{
      xmin<-c(xmin,(exdata$day[as.numeric(phase[i-1])]))
      xmax<-c(xmax,(max(exdata$day)))
      ymin<-c(ymin,yrange[1])
      ymax<-c(ymax,yrange[2])
      ph<-c(ph,names(phase[i]))
    }
  }
  
  params<-data.frame(xmin,xmax,ymin,ymax,ph)
  
  p<-p+geom_rect(
    data = params,
    mapping=aes(x = NULL, y = NULL, xmin = xmin, xmax = xmax, fill = ph),
    ymin = ymin, ymax = ymax, inherit.aes = F, colour = "black", alpha = 0.3)+
    scale_fill_manual(
      values = c("lagging"="#7ACC9B","exponential"="#FFB3B3","deceleration"="#F0FFB3","stationary"="#B3D1FF")
    )
  p
}

# Growth acceleration for simulated data
plot_simulated_growth_acceleration <- function(exdata, translate, lang){
  exdata$phase <- hmm(exdata,5)
  
  p<-ggplot(data = exdata, mapping = aes(x = day, y = acceleration))+
    geom_line()+
    xlab(label = translate$text[which(translate$item == "daysrecorded" & translate$language == lang)])+
    ylab(label = translate$text[which(translate$item == "thousandcasesday2" & translate$language == lang)])+
    ggtitle(label = translate$text[which(translate$item == "growthacceleration" & translate$language == lang)])+
    theme(legend.position = "none")
  
  # Map phases
  runs <- rle(as.vector(exdata$phase))
  phase <- cumsum(runs$lengths)
  names(phase) <- runs$values
  
  params<-ggplot_build(p)
  yrange<-params$layout$panel_params[[1]]$y.range
  
  xmin<-NULL; xmax<-NULL; ymin<-NULL; ymax<-NULL; ph<-NULL;
  
  for(i in 1:length(phase)){
    if(i == 1){
      xmin<-c(xmin,(min(exdata$day)))
      xmax<-c(xmax,(exdata$day[as.numeric(phase[i])]))
      ymin<-c(ymin,yrange[1])
      ymax<-c(ymax,yrange[2])
      ph<-c(ph,names(phase[i]))
    }else if(i < length(phase)){
      xmin<-c(xmin,(exdata$day[as.numeric(phase[i-1])]))
      xmax<-c(xmax,(exdata$day[as.numeric(phase[i])]))
      ymin<-c(ymin,yrange[1])
      ymax<-c(ymax,yrange[2])
      ph<-c(ph,names(phase[i]))
    }else{
      xmin<-c(xmin,(exdata$day[as.numeric(phase[i-1])]))
      xmax<-c(xmax,(max(exdata$day)))
      ymin<-c(ymin,yrange[1])
      ymax<-c(ymax,yrange[2])
      ph<-c(ph,names(phase[i]))
    }
  }
  
  params<-data.frame(xmin,xmax,ymin,ymax,ph)
  
  p<-p+geom_rect(
    data = params,
    mapping=aes(x = NULL, y = NULL, xmin = xmin, xmax = xmax, fill = ph),
    ymin = ymin, ymax = ymax, inherit.aes = F, colour = "black", alpha = 0.3)+
    scale_fill_manual(
      values = c("lagging"="#7ACC9B","exponential"="#FFB3B3","deceleration"="#F0FFB3","stationary"="#B3D1FF")
    )
  p
}

# Get worldwide cases
worldwidecases <- function(realdata){
  df <- realdata[which(realdata$Cases>=1),]
  df <- as.data.frame(tapply(df$Cases, df$DateRep, sum))
  df <- cumsum(df)
  names(df) <- c("n")
  df$n <- df$n/1000
  df$date <- as.Date(row.names(df))
  df <- df[order(df$date),]
  return(df)
}

# Get worldwide deaths
worldwidedeaths <- function(realdata){
  df <- realdata[which(realdata$Deaths>=1),]
  df <- as.data.frame(tapply(df$Deaths, df$DateRep, sum))
  df <- cumsum(df)
  names(df) <- c("n")
  df$n <- df$n/1000
  df$date <- as.Date(row.names(df))
  return(df)
}

# Plot worldwide numbers
plot_worldwide <- function(df, translate, lang){
  Pandemic <- as.Date("2020-03-11")
  gp<-ggplot(data = df, mapping = aes(x = date, y = n, group=1, text = paste(
    paste(translate$text[which(translate$item == "date" & translate$language == lang)],": ",sep=""), date,
    "\nn: ", n
  )))+
    geom_bar(stat = "identity")+
    geom_vline(aes(xintercept = as.numeric(Pandemic) ,color="Pandemic"), show.legend = F)+
    scale_color_manual("Alert", values = c(Pandemic = "red"))+
    labs(x = translate$text[which(translate$item == "date" & translate$language == lang)], y = translate$text[which(translate$item == "thousandcases" & translate$language == lang)])+
    theme(axis.text.x = element_text(size = 12, angle = 0, hjust = 1, vjust = 1),
          axis.text.y = element_text(size = 12, angle = 0, hjust = 0, vjust = 0, face = "plain"),  
          axis.title.x = element_text(size = 16, angle = 0, hjust = 0, vjust = 0, face = "plain"),
          axis.title.y = element_text(size = 16, angle = 90, hjust = 0, vjust = 0, face = "plain"),
          legend.title=element_blank(),
          legend.text = element_text(size = 10, face = "bold.italic"),
          legend.position = "top",
          rect = element_rect(fill = "transparent")
    ) 
  ggplotly(gp, tooltip = "text") %>% layout(legend = list(orientation = "h"))
}

# Local regression function
locr <- function(x,y,offset){
  idx1 <- 1:length(y) - offset
  idx2 <- 1:length(y) + offset
  idx1[which(idx1 < 1)] <- 1
  idx2[which(idx2 > length(y))] <- length(y)
  fitted <- rep(NA,times=length(y))
  slope <- rep(NA,times=length(y))
  for(i in 1:length(y)){
    X <- cbind(1,x[idx1[i]:idx2[i]])
    b <- solve(t(X)%*%X)%*%t(X)%*%y[idx1[i]:idx2[i]]
    fitted[i] <- cbind(1,x[i])%*%b
    slope[i] <- b[2]
  }
  results <- NULL
  results$fitted <- fitted
  results$slope <- slope
  results$residuals <- y - results$fitted
  results$sse <- sum(results$residuals^2)
  return(results)
}

# Filter data
getrealdata <- function(realdata, country, smooth, hmmclass){
  if(is.null(realdata)){
    return(NULL)
  } else {
    df <- realdata[which(realdata$`Countries and territories` == country),]
    dayone<-which(df$Cases!=0)[1]
    df <- df[dayone:nrow(df),]
    if(nrow(df)>smooth+1){
      df <- df[order(df$DateRep),]
      dif <- cumsum(as.vector(diff(df$DateRep)))
      df$d <- c(1,1+dif)
      df$c <- cumsum(df$Cases)
      w <- smooth
      model <- locr(x = df$d, y = df$c, offset = w)
      model2 <- locr(x = df$d, y = model$slope, offset = w)
      df$c <- df$c/1000
      df$fitted <- model$fitted/1000
      df$growth <- model$slope/1000
      df$acceleration <- model2$slope/1000
      
      # Clipping
      clipping <- (nrow(df)-w):nrow(df)
      df$acceleration[clipping] <- NA
      df$growth[clipping] <- NA
      df$fitted[clipping] <- NA
      predacc <- df$acceleration[(nrow(df)-w-1)]
      for(k in clipping){
        df$growth[k] <- df$growth[k-1] + predacc
        df$fitted[k] <- df$fitted[k-1] + df$growth[k]
      }
      
      newrow <- df[nrow(df),]
      newrow$d <- newrow$d + 1
      newrow$growth <- newrow$growth + predacc
      newrow$fitted <- newrow$fitted + newrow$growth
      newrow$c <- round((newrow$fitted - newrow$c)*1000, digits = 0)
      if(newrow$c<0){
        newrow$c<-0
        newrow$fitted <- as.numeric(df[nrow(df),"c"])
      } 
      newrow$fitted <- round(newrow$fitted * 1000, digits = 0)
      
      df <- rbind(df, newrow)
      df$phase <- hmm(df,hmmclass)
      return(df)
    } else{
      shinyalert(title = translate$text[which(translate$item == "smoothfactor" & translate$language == lang)], text = translate$text[which(translate$item == "notenoughdata" & translate$language == lang)], type = "error")
      return(NULL)
    }
  }
}

# Get predicted prevalence
getpredprev <- function(){
  return(info$prev)
}

# Get predicted cases
getpredcases <- function(){
  return(info$newc)
}

# Gompertz functions
f0 <- function(x,a,k,i){
  return(a*exp(-exp(-k*(x - i))))
}
f1 <- function(x,a,k,i){
  return(a*(exp(-exp(-k*(x - i)))*(exp(-k*(x - i))*k)))
}
f2 <- function(x,a,k,i){
  return(a*(exp(-exp(-k*(x - i)))*(exp(-k*(x - i))*k)*(exp(-k*(x - i))*k) - exp(-exp(-k*(x - i)))*(exp(-k * (x - i))*k*k)))
}

# Get input file from user
getfile <- function(file){
  inFile <- file
  if(is.null(inFile)){
    return(NULL)
  } else {
    ext <- tools::file_ext(inFile$name)
    if(ext %in% c("xls", "xlsx")){
      file.rename(from = inFile$datapath, to = paste(inFile$datapath, ext, sep="."))
      df <- read_excel(paste(inFile$datapath, ext, sep="."), 1)
      colnames(df)[1:2] <- c("DateRep", "Cases")
      df <- as.data.frame(df)
      df$DateRep <- as.Date(df$DateRep)
      return(df)
    } else {
      shinyalert(title = translate$text[which(translate$item == "error" & translate$language == lang)], text = translate$text[which(translate$item == "extensionxls" & translate$language == lang)], type = "error")
      reset("loadfile")
    }
  }
}

# Growth curve for real data
plot_realdata_growth_curve <- function(df, translate, lang){
  
  p <- ggplot(
    data = df[-nrow(df),],
    mapping = aes(
      x = DateRep, y = c,
      group=1,
      text = paste(
        paste(translate$text[which(translate$item == "date" & translate$language == lang)],": ",sep=""), DateRep,
        translate$text[which(translate$item == "predprevalence" & translate$language == lang)], round(fitted*1000, digits = 0),
        translate$text[which(translate$item == "obsprevalence" & translate$language == lang)], c*1000,
        translate$text[which(translate$item == "newcases" & translate$language == lang)], Cases,
        translate$text[which(translate$item == "newdeaths" & translate$language == lang)], Deaths
      )
    )
  )+
    theme(legend.position = "none")+
    xlab(label = "")+
    ylab(label = translate$text[which(translate$item == "thousandcases" & translate$language == lang)])+
    ggtitle(label = translate$text[which(translate$item == "growthcurve" & translate$language == lang)])
  
  # Map phases
  runs <- rle(as.vector(df$phase))
  phase <- cumsum(runs$lengths)
  names(phase) <- runs$values
  
  params<-ggplot_build(p)
  yrange<-params$layout$panel_params[[1]]$y.range
  
  xmin<-NULL; xmax<-NULL; ymin<-NULL; ymax<-NULL; ph<-NULL;
  
  for(i in 1:length(phase)){
    if(i == 1){
      xmin<-c(xmin,as.character(min(df$DateRep)))
      xmax<-c(xmax,as.character(df$DateRep[as.numeric(phase[i])]))
      ymin<-c(ymin,yrange[1])
      ymax<-c(ymax,yrange[2])
      ph<-c(ph,names(phase[i]))
    }else if(i < length(phase)){
      xmin<-c(xmin,as.character(df$DateRep[as.numeric(phase[i-1])]))
      xmax<-c(xmax,as.character(df$DateRep[as.numeric(phase[i])]))
      ymin<-c(ymin,yrange[1])
      ymax<-c(ymax,yrange[2])
      ph<-c(ph,names(phase[i]))
    }else{
      xmin<-c(xmin,as.character(df$DateRep[as.numeric(phase[i-1])]))
      xmax<-c(xmax,as.character(max(df$DateRep)))
      ymin<-c(ymin,yrange[1])
      ymax<-c(ymax,yrange[2])
      ph<-c(ph,names(phase[i]))
    }
  }
  
  params<-data.frame(xmin,xmax,ymin,ymax,ph)
  xmin<-as.Date(xmin)
  xmax<-as.Date(xmax)
  params$xmin<-as.Date(params$xmin)
  params$xmax<-as.Date(params$xmax)
  
  p <- p+
    geom_rect(
      data = params,
      mapping=aes(x = NULL, y = NULL, xmin = xmin, xmax = xmax, fill = ph),
      ymin = ymin, ymax = ymax, inherit.aes = F, colour = "black", alpha = 0.3)+
    scale_fill_manual(
      values = c("lagging"="#7ACC9B","exponential"="#FFB3B3","deceleration"="#F0FFB3","stationary"="#B3D1FF","linear"="#C2B3FF")
    )+
    geom_line(mapping = aes(x = DateRep, y = fitted), inherit.aes = T)+
    geom_point(mapping = aes(x = DateRep, y = c),show.legend = F,inherit.aes = T)
  ggplotly(p, tooltip="text")
}

# Growth rate for real data
plot_realdata_growth_rate <- function(df,translate,lang){
  p<-ggplot(
    data = df[-nrow(df),],
    mapping = aes(
      x = DateRep, y = growth, group = 1,
      text = paste(
        paste(translate$text[which(translate$item == "date" & translate$language == lang)],": ",sep=""), DateRep,
        translate$text[which(translate$item == "predprevalence" & translate$language == lang)], round(fitted*1000, digits = 0),
        translate$text[which(translate$item == "obsprevalence" & translate$language == lang)], c*1000,
        translate$text[which(translate$item == "newcases" & translate$language == lang)], Cases,
        translate$text[which(translate$item == "newdeaths" & translate$language == lang)], Deaths
      )
    )
  )+
    theme(legend.position = "none")+
    xlab(label = "")+
    ylab(label = translate$text[which(translate$item == "thousandcasesday" & translate$language == lang)])+
    xlim(min(df$DateRep),max(df$DateRep))+
    ggtitle(label = translate$text[which(translate$item == "growthrate" & translate$language == lang)])
  
  # Map phases
  runs <- rle(as.vector(df$phase))
  phase <- cumsum(runs$lengths)
  names(phase) <- runs$values
  
  params<-ggplot_build(p)
  yrange<-params$layout$panel_params[[1]]$y.range
  
  xmin<-NULL; xmax<-NULL; ymin<-NULL; ymax<-NULL; ph<-NULL;
  
  for(i in 1:length(phase)){
    if(i == 1){
      xmin<-c(xmin,as.character(min(df$DateRep)))
      xmax<-c(xmax,as.character(df$DateRep[as.numeric(phase[i])]))
      ymin<-c(ymin,yrange[1])
      ymax<-c(ymax,yrange[2])
      ph<-c(ph,names(phase[i]))
    }else if(i < length(phase)){
      xmin<-c(xmin,as.character(df$DateRep[as.numeric(phase[i-1])]))
      xmax<-c(xmax,as.character(df$DateRep[as.numeric(phase[i])]))
      ymin<-c(ymin,yrange[1])
      ymax<-c(ymax,yrange[2])
      ph<-c(ph,names(phase[i]))
    }else{
      xmin<-c(xmin,as.character(df$DateRep[as.numeric(phase[i-1])]))
      xmax<-c(xmax,as.character(max(df$DateRep)))
      ymin<-c(ymin,yrange[1])
      ymax<-c(ymax,yrange[2])
      ph<-c(ph,names(phase[i]))
    }
  }
  
  params<-data.frame(xmin,xmax,ymin,ymax,ph)
  xmin<-as.Date(xmin)
  xmax<-as.Date(xmax)
  params$xmin<-as.Date(params$xmin)
  params$xmax<-as.Date(params$xmax)
  
  p <- p+
    geom_rect(
      data = params,
      mapping=aes(x = NULL, y = NULL, xmin = xmin, xmax = xmax, fill = ph),
      ymin = ymin, ymax = ymax, inherit.aes = F, colour = "black", alpha = 0.3)+
    scale_fill_manual(
      values = c("lagging"="#7ACC9B","exponential"="#FFB3B3","deceleration"="#F0FFB3","stationary"="#B3D1FF","linear"="#C2B3FF")
    )+
    geom_line()
  
  ggplotly(p, tooltip="text")
}

# Growth acceleration for real data
plot_realdata_growth_acceleration <- function(df,translate,lang){
  y <-  c(abs(min(df$acceleration)), abs(max(df$acceleration)))
  y <- max(y)
  p<-ggplot(
    data = df, 
    mapping = aes(
      x = DateRep, y = acceleration,group=1,
      text = paste(
        paste(translate$text[which(translate$item == "date" & translate$language == lang)],": ",sep=""), DateRep,
        translate$text[which(translate$item == "predprevalence" & translate$language == lang)], round(fitted*1000, digits = 0),
        translate$text[which(translate$item == "obsprevalence" & translate$language == lang)], c*1000,
        translate$text[which(translate$item == "newcases" & translate$language == lang)], Cases,
        translate$text[which(translate$item == "newdeaths" & translate$language == lang)], Deaths
      )
    )
  )+
    theme(legend.position = "none")+
    ylim(-y,y)+
    xlim(min(df$DateRep),max(df$DateRep))+
    xlab(label = "")+
    ylab(label = translate$text[which(translate$item == "thousandcasesday2" & translate$language == lang)])+
    ggtitle(label = translate$text[which(translate$item == "growthacceleration" & translate$language == lang)])
  
  ############### Map phases
  runs <- rle(as.vector(df$phase))
  phase <- cumsum(runs$lengths)
  names(phase) <- runs$values
  
  params<-ggplot_build(p)
  yrange<-params$layout$panel_params[[1]]$y.range
  
  xmin<-NULL; xmax<-NULL; ymin<-NULL; ymax<-NULL; ph<-NULL;
  
  for(i in 1:length(phase)){
    if(i == 1){
      xmin<-c(xmin,as.character(min(df$DateRep)))
      xmax<-c(xmax,as.character(df$DateRep[as.numeric(phase[i])]))
      ymin<-c(ymin,yrange[1])
      ymax<-c(ymax,yrange[2])
      ph<-c(ph,names(phase[i]))
    }else if(i < length(phase)){
      xmin<-c(xmin,as.character(df$DateRep[as.numeric(phase[i-1])]))
      xmax<-c(xmax,as.character(df$DateRep[as.numeric(phase[i])]))
      ymin<-c(ymin,yrange[1])
      ymax<-c(ymax,yrange[2])
      ph<-c(ph,names(phase[i]))
    }else{
      xmin<-c(xmin,as.character(df$DateRep[as.numeric(phase[i-1])]))
      xmax<-c(xmax,as.character(max(df$DateRep)))
      ymin<-c(ymin,yrange[1])
      ymax<-c(ymax,yrange[2])
      ph<-c(ph,names(phase[i]))
    }
  }
  
  params<-data.frame(xmin,xmax,ymin,ymax,ph)
  xmin<-as.Date(xmin)
  xmax<-as.Date(xmax)
  params$xmin<-as.Date(params$xmin)
  params$xmax<-as.Date(params$xmax)
  
  p <- p+
    geom_rect(
      data = params,
      mapping=aes(x = NULL, y = NULL, xmin = xmin, xmax = xmax, fill = ph),
      ymin = ymin, ymax = ymax, inherit.aes = F, colour = "black", alpha = 0.3)+
    scale_fill_manual(
      values = c("lagging"="#7ACC9B","exponential"="#FFB3B3","deceleration"="#F0FFB3","stationary"="#B3D1FF","linear"="#C2B3FF")
    )+
    geom_line()
  
  ggplotly(p, tooltip="text")
}

# Growth curve for local data
plot_localdata_growth_curve <- function(df,smooth,hmmfactor,translate,lang){
  
  if(nrow(df)>smooth+1){
    df <- df[order(df$DateRep),]
    dif <- cumsum(as.vector(diff(df$DateRep)))
    df$d <- c(1,1+dif)
    df$c <- cumsum(df$Cases)
    w <- smooth
    model <- locr(x = df$d, y = df$c, offset = w)
    model2 <- locr(x = df$d, y = model$slope, offset = w)
    df$c <- df$c/1000
    df$fitted <- model$fitted/1000
    df$growth <- model$slope/1000
    df$acceleration <- model2$slope/1000
    
    # Clipping
    clipping <- (nrow(df)-w):nrow(df)
    df$acceleration[clipping] <- NA
    df$growth[clipping] <- NA
    df$fitted[clipping] <- NA
    predacc <- df$acceleration[(nrow(df)-w-1)]
    for(k in clipping){
      df$growth[k] <- df$growth[k-1] + predacc
      df$fitted[k] <- df$fitted[k-1] + df$growth[k]
    }
    
    newrow <- df[nrow(df),]
    newrow$d <- newrow$d + 1
    newrow$growth <- newrow$growth + predacc
    newrow$fitted <- newrow$fitted + newrow$growth
    newrow$c <- round((newrow$fitted - newrow$c)*1000, digits = 0)
    if(newrow$c<0){
      newrow$c<-0
      newrow$fitted <- as.numeric(df[nrow(df),"c"])
    } 
    newrow$fitted <- round(newrow$fitted * 1000, digits = 0)
    
    df <- rbind(df, newrow)
    
    df$phase<-hmm(df,hmmfactor)
    
    p <- ggplot(
      data = df[-nrow(df),],
      mapping = aes(
        x = DateRep, y = c,
        group=1,
        text = paste(
          paste(translate$text[which(translate$item == "date" & translate$language == lang)],": ",sep=""), DateRep,
          translate$text[which(translate$item == "predprevalence" & translate$language == lang)], round(fitted*1000, digits = 0),
          translate$text[which(translate$item == "obsprevalence" & translate$language == lang)], c*1000,
          translate$text[which(translate$item == "newcases" & translate$language == lang)], Cases
        )
      )
    )+
      xlab(label = "")+
      ylab(label = translate$text[which(translate$item == "thousandcases" & translate$language == lang)])+
      ggtitle(label = translate$text[which(translate$item == "growthcurve" & translate$language == lang)])
    
    # Map phases
    runs <- rle(as.vector(df$phase))
    phase <- cumsum(runs$lengths)
    names(phase) <- runs$values
    
    params<-ggplot_build(p)
    yrange<-params$layout$panel_params[[1]]$y.range
    
    xmin<-NULL; xmax<-NULL; ymin<-NULL; ymax<-NULL; ph<-NULL;
    
    for(i in 1:length(phase)){
      if(i == 1){
        xmin<-c(xmin,as.character(min(df$DateRep)))
        xmax<-c(xmax,as.character(df$DateRep[as.numeric(phase[i])]))
        ymin<-c(ymin,yrange[1])
        ymax<-c(ymax,yrange[2])
        ph<-c(ph,names(phase[i]))
      }else if(i < length(phase)){
        xmin<-c(xmin,as.character(df$DateRep[as.numeric(phase[i-1])]))
        xmax<-c(xmax,as.character(df$DateRep[as.numeric(phase[i])]))
        ymin<-c(ymin,yrange[1])
        ymax<-c(ymax,yrange[2])
        ph<-c(ph,names(phase[i]))
      }else{
        xmin<-c(xmin,as.character(df$DateRep[as.numeric(phase[i-1])]))
        xmax<-c(xmax,as.character(max(df$DateRep)))
        ymin<-c(ymin,yrange[1])
        ymax<-c(ymax,yrange[2])
        ph<-c(ph,names(phase[i]))
      }
    }
    
    params<-data.frame(xmin,xmax,ymin,ymax,ph)
    
    xmin<-as.Date(xmin)
    xmax<-as.Date(xmax)
    params$xmin<-as.Date(params$xmin)
    params$xmax<-as.Date(params$xmax)
    
    p <- p+theme(legend.position = "none")+
      geom_rect(
        data = params,
        mapping=aes(x = NULL, y = NULL, xmin = xmin, xmax = xmax, fill = ph),
        ymin = ymin, ymax = ymax, inherit.aes = F, colour = "black", alpha = 0.3)+
      scale_fill_manual(
        values = c("lagging"="#7ACC9B","exponential"="#FFB3B3","deceleration"="#F0FFB3","stationary"="#B3D1FF","linear"="#C2B3FF")
      )+
      geom_line(mapping = aes(x = DateRep, y = fitted), inherit.aes = T)+
      geom_point(mapping = aes(x = DateRep, y = c),show.legend = F,inherit.aes = T)
    
    ggplotly(p, tooltip="text")
    
  } else{
    shinyalert(title = "Error!", text = 'There is not enough data to display!', type = "error")
  }
}

# Growth rate for local data
plot_localdata_growth_rate <- function(df,smooth,hmmfactor,translate,lang){
  if(nrow(df)>smooth+1){
    df <- df[order(df$DateRep),]
    dif <- cumsum(as.vector(diff(df$DateRep)))
    df$d <- c(1,1+dif)
    df$c <- cumsum(df$Cases)
    w <- smooth
    model <- locr(x = df$d, y = df$c, offset = w)
    model2 <- locr(x = df$d, y = model$slope, offset = w)
    df$c <- df$c/1000
    df$fitted <- model$fitted/1000
    df$growth <- model$slope/1000
    df$acceleration <- model2$slope/1000
    
    # Clipping
    clipping <- (nrow(df)-w):nrow(df)
    df$acceleration[clipping] <- NA
    df$growth[clipping] <- NA
    df$fitted[clipping] <- NA
    predacc <- df$acceleration[(nrow(df)-w-1)]
    for(k in clipping){
      df$growth[k] <- df$growth[k-1] + predacc
      df$fitted[k] <- df$fitted[k-1] + df$growth[k]
    }
    
    newrow <- df[nrow(df),]
    newrow$d <- newrow$d + 1
    newrow$growth <- newrow$growth + predacc
    newrow$fitted <- newrow$fitted + newrow$growth
    newrow$c <- round((newrow$fitted - newrow$c)*1000, digits = 0)
    if(newrow$c<0){
      newrow$c<-0
      newrow$fitted <- as.numeric(df[nrow(df),"c"])
    } 
    newrow$fitted <- round(newrow$fitted * 1000, digits = 0)
    
    df <- rbind(df, newrow)
    
    df$phase<-hmm(df,hmmfactor)
    
    p<-ggplot(
      data = df[-nrow(df),],
      mapping = aes(
        x = DateRep, y = growth, group = 1,
        text = paste(
          paste(translate$text[which(translate$item == "date" & translate$language == lang)],": ",sep=""), DateRep,
          translate$text[which(translate$item == "predprevalence" & translate$language == lang)], round(fitted*1000, digits = 0),
          translate$text[which(translate$item == "obsprevalence" & translate$language == lang)], c*1000,
          translate$text[which(translate$item == "newcases" & translate$language == lang)], Cases
        )
      )
    )+
      xlab(label = "")+
      ylab(label = translate$text[which(translate$item == "thousandcasesday" & translate$language == lang)])+
      xlim(min(df$DateRep),max(df$DateRep))+
      ggtitle(label = translate$text[which(translate$item == "growthrate" & translate$language == lang)])
    
    # Map phases
    runs <- rle(as.vector(df$phase))
    phase <- cumsum(runs$lengths)
    names(phase) <- runs$values
    
    params<-ggplot_build(p)
    yrange<-params$layout$panel_params[[1]]$y.range
    
    xmin<-NULL; xmax<-NULL; ymin<-NULL; ymax<-NULL; ph<-NULL;
    
    for(i in 1:length(phase)){
      if(i == 1){
        xmin<-c(xmin,as.character(min(df$DateRep)))
        xmax<-c(xmax,as.character(df$DateRep[as.numeric(phase[i])]))
        ymin<-c(ymin,yrange[1])
        ymax<-c(ymax,yrange[2])
        ph<-c(ph,names(phase[i]))
      }else if(i < length(phase)){
        xmin<-c(xmin,as.character(df$DateRep[as.numeric(phase[i-1])]))
        xmax<-c(xmax,as.character(df$DateRep[as.numeric(phase[i])]))
        ymin<-c(ymin,yrange[1])
        ymax<-c(ymax,yrange[2])
        ph<-c(ph,names(phase[i]))
      }else{
        xmin<-c(xmin,as.character(df$DateRep[as.numeric(phase[i-1])]))
        xmax<-c(xmax,as.character(max(df$DateRep)))
        ymin<-c(ymin,yrange[1])
        ymax<-c(ymax,yrange[2])
        ph<-c(ph,names(phase[i]))
      }
    }
    
    params<-data.frame(xmin,xmax,ymin,ymax,ph)
    
    xmin<-as.Date(xmin)
    xmax<-as.Date(xmax)
    params$xmin<-as.Date(params$xmin)
    params$xmax<-as.Date(params$xmax)
    
    p <- p+theme(legend.position = "none")+
      geom_rect(
        data = params,
        mapping=aes(x = NULL, y = NULL, xmin = xmin, xmax = xmax, fill = ph),
        ymin = ymin, ymax = ymax, inherit.aes = F, colour = "black", alpha = 0.3)+
      scale_fill_manual(
        values = c("lagging"="#7ACC9B","exponential"="#FFB3B3","deceleration"="#F0FFB3","stationary"="#B3D1FF","linear"="#C2B3FF")
      )+
      geom_line()
    
    ggplotly(p, tooltip="text")
    
  } else{
    shinyalert(title = "Error!", text = 'There is not enough data to display!', type = "error")
  }
}

# Growth acceleration for local data
plot_localdata_growth_acceleration <- function(df, smooth, hmmfactor, translate, lang){
  if(nrow(df)>smooth+1){
    df <- df[order(df$DateRep),]
    dif <- cumsum(as.vector(diff(df$DateRep)))
    df$d <- c(1,1+dif)
    df$c <- cumsum(df$Cases)
    w <- smooth
    model <- locr(x = df$d, y = df$c, offset = w)
    model2 <- locr(x = df$d, y = model$slope, offset = w)
    df$c <- df$c/1000
    df$fitted <- model$fitted/1000
    df$growth <- model$slope/1000
    df$acceleration <- model2$slope/1000
    
    # Clipping
    clipping <- (nrow(df)-w):nrow(df)
    df$acceleration[clipping] <- NA
    df$growth[clipping] <- NA
    df$fitted[clipping] <- NA
    predacc <- df$acceleration[(nrow(df)-w-1)]
    for(k in clipping){
      df$growth[k] <- df$growth[k-1] + predacc
      df$fitted[k] <- df$fitted[k-1] + df$growth[k]
    }
    
    newrow <- df[nrow(df),]
    newrow$d <- newrow$d + 1
    newrow$growth <- newrow$growth + predacc
    newrow$fitted <- newrow$fitted + newrow$growth
    newrow$c <- round((newrow$fitted - newrow$c)*1000, digits = 0)
    if(newrow$c<0){
      newrow$c<-0
      newrow$fitted <- as.numeric(df[nrow(df),"c"])
    } 
    newrow$fitted <- round(newrow$fitted * 1000, digits = 0)
    
    df <- rbind(df, newrow)
    y <-  c(abs(min(df$acceleration)), abs(max(df$acceleration)))
    y <- max(y)
    
    df$phase<-hmm(df,hmmfactor)
    
    p<-ggplot(
      data = df, 
      mapping = aes(
        x = DateRep, y = acceleration,group=1,
        text = paste(
          paste(translate$text[which(translate$item == "date" & translate$language == lang)],": ",sep=""), DateRep,
          translate$text[which(translate$item == "predprevalence" & translate$language == lang)], round(fitted*1000, digits = 0),
          translate$text[which(translate$item == "obsprevalence" & translate$language == lang)], c*1000,
          translate$text[which(translate$item == "newcases" & translate$language == lang)], Cases
        )
      )
    )+
      ylim(-y,y)+
      xlim(min(df$DateRep),max(df$DateRep))+
      xlab(label = "")+
      ylab(label = translate$text[which(translate$item == "thousandcasesday2" & translate$language == lang)])+
      ggtitle(label = translate$text[which(translate$item == "growthacceleration" & translate$language == lang)])
    
    # Map phases
    runs <- rle(as.vector(df$phase))
    phase <- cumsum(runs$lengths)
    names(phase) <- runs$values
    
    params<-ggplot_build(p)
    yrange<-params$layout$panel_params[[1]]$y.range
    
    xmin<-NULL; xmax<-NULL; ymin<-NULL; ymax<-NULL; ph<-NULL;
    
    for(i in 1:length(phase)){
      if(i == 1){
        xmin<-c(xmin,as.character(min(df$DateRep)))
        xmax<-c(xmax,as.character(df$DateRep[as.numeric(phase[i])]))
        ymin<-c(ymin,yrange[1])
        ymax<-c(ymax,yrange[2])
        ph<-c(ph,names(phase[i]))
      }else if(i < length(phase)){
        xmin<-c(xmin,as.character(df$DateRep[as.numeric(phase[i-1])]))
        xmax<-c(xmax,as.character(df$DateRep[as.numeric(phase[i])]))
        ymin<-c(ymin,yrange[1])
        ymax<-c(ymax,yrange[2])
        ph<-c(ph,names(phase[i]))
      }else{
        xmin<-c(xmin,as.character(df$DateRep[as.numeric(phase[i-1])]))
        xmax<-c(xmax,as.character(max(df$DateRep)))
        ymin<-c(ymin,yrange[1])
        ymax<-c(ymax,yrange[2])
        ph<-c(ph,names(phase[i]))
      }
    }
    
    params<-data.frame(xmin,xmax,ymin,ymax,ph)
    
    xmin<-as.Date(xmin)
    xmax<-as.Date(xmax)
    params$xmin<-as.Date(params$xmin)
    params$xmax<-as.Date(params$xmax)
    
    p <- p+theme(legend.position = "none")+
      geom_rect(
        data = params,
        mapping=aes(x = NULL, y = NULL, xmin = xmin, xmax = xmax, fill = ph),
        ymin = ymin, ymax = ymax, inherit.aes = F, colour = "black", alpha = 0.3)+
      scale_fill_manual(
        values = c("lagging"="#7ACC9B","exponential"="#FFB3B3","deceleration"="#F0FFB3","stationary"="#B3D1FF","linear"="#C2B3FF")
      )+
      geom_line()
    
    ggplotly(p, tooltip="text")
    
  } else{
    shinyalert(title = "Error!", text = 'There is not enough data to display!', type = "error")
  }
}

# Classification function
hmm <- function(df, acc.cutoff=5){
  df$acceleration <- df$acceleration*1000
  trans <- c(0.8,0.2,0.0,0.0,
             0.0,0.8,0.2,0.0,
             0.0,0.0,0.8,0.2,
             0.0,0.2,0.0,0.8)
  emiss <- c(0.40,0.25,0.25,
             0.10,0.00,0.50,
             0.10,0.50,0.00,
             0.40,0.25,0.25)
  start <- c(1.0,0.0,0.0,0.0)
  markov <- initHMM(States = c("lagging","exponential","deceleration","stationary"),
                    Symbols = c("0","-1","1"),
                    startProbs = start, transProbs = matrix(trans,4,byrow = T),
                    emissionProbs = matrix(emiss,4,byrow = T))
  obs <- sign(df$acceleration)
  obs[which(abs(df$acceleration) < acc.cutoff)] <- 0
  nas <- which(is.na(obs) == T)
  if(length(nas) > 0){
    states <- viterbi(hmm = markov, observation = as.character(obs[-nas]))
  }else{
    states <- viterbi(hmm = markov, observation = as.character(obs))
  }
  growing <- which(states == "exponential")
  flat <- which(states == "stationary")
  if(length(growing) > 0){
    states[flat[which(flat < min(growing))]] <- "lagging"
    flat <- which(states == "stationary")
  }
  for(i in flat){
    if(states[i-1] == "exponential"){
      states[i] <- "exponential"
    }
  }
  results <- rep(NA, times=length(obs))
  if(length(nas) > 0){
    results[-nas] <- states
    for(i in nas){
      results[i] <- results[i-1]
    }
  }else{
    results <- states
  }
  runs <- rle(results)
  runs$pos <- cumsum(runs$lengths)
  flat <- which(runs$values == "stationary")
  lag <- which(runs$values == "lagging")
  if(length(flat) > 0){
    growth.cutoff <- max(df$growth[1:runs$pos[lag]])
    for(j in 1:length(flat)){
      g <- df$growth[runs$pos[flat[j]-1]:runs$pos[flat[j]]]
      dev <- median(g)
      if(dev > growth.cutoff){
        results[runs$pos[flat[j]-1]:runs$pos[flat[j]]] <- "linear"
      }
    }
  }
  return(results)
}

# Epidemiological week
weeks<-function(df){
  # Map unique territories
  countries <- unique(df$`Countries and territories`)
  dates <- sort(unique(df$DateRep))
  
  # Map population size
  pops <- unique(df[,c("Countries and territories","Pop_Data.2018","Code")])
  popsize <- pops$Pop_Data.2018
  names(popsize) <- pops$`Countries and territories`
  
  # Assemble output table
  results <- matrix(data = NA, nrow = length(dates)*length(countries), ncol = 7)
  results <- as.data.frame(results)
  colnames(results) <- c("Countries and territories","DateRep","Pop_Data.2018",
                         "Cases","Deaths","ccases","cdeaths")
  results$`Countries and territories` <- rep(x = countries, each = length(dates))
  results$DateRep <- rep(x = dates, times = length(countries))
  results$Pop_Data.2018 <- popsize[results$`Countries and territories`]
  
  # Make final table
  for(country in countries){
    
    # Get indices
    idx1 <- which(df$`Countries and territories` == country)
    idx2 <- which(results$`Countries and territories` == country)
    
    # Get country data
    tmp <- df[idx1,]
    
    # Map cases and deaths
    res <- rep(x = NA, times = length(dates))
    names(res) <- dates
    res[as.character(tmp$DateRep)] <- tmp$Cases
    results$Cases[idx2] <- res
    res <- rep(x = NA, times = length(dates))
    names(res) <- dates
    res[as.character(tmp$DateRep)] <- tmp$Deaths
    results$Deaths[idx2] <- res
    
    # Compute cumulative cases
    res <- rep(x = NA, times = length(dates))
    names(res) <- dates
    Cases <- results$Cases[idx2]
    names(Cases) <- results$DateRep[idx2]
    Cases <- Cases[which(is.na(Cases) == F)]
    ccases <- cumsum(Cases)
    res[names(ccases)] <- ccases
    results$ccases[idx2] <- res
    
    # Compute cumulative deaths
    res <- rep(x = NA, times = length(dates))
    names(res) <- dates
    Deaths <- results$Deaths[idx2]
    names(Deaths) <- results$DateRep[idx2]
    Deaths <- Deaths[which(is.na(Deaths) == F)]
    cdeaths <- cumsum(Deaths)
    res[names(cdeaths)] <- cdeaths
    results$cdeaths[idx2] <- res
    
  }
  
  # Assemble week summary
  idx1 <- seq(from=1, to=length(dates), by=7)
  idx2 <- idx1 - 1
  idx1 <- idx1[-length(idx1)]
  idx2 <- idx2[-1]
  weeks <- matrix(data = NA, nrow = length(idx1)*length(countries), ncol = 9)
  weeks <- as.data.frame(weeks)
  colnames(weeks) <- c("Countries and territories","Pop_Data.2018","week","start","end",
                       "Cases","Deaths","ccases","cdeaths")
  weeks$`Countries and territories` <- rep(x = countries, each = length(idx1))
  weeks$week <- rep(x = 1:length(idx1), times = length(countries))
  weeks$Pop_Data.2018 <- popsize[weeks$`Countries and territories`]
  weeks$start <- rep(x = dates[idx1], times = length(countries))
  weeks$end <- rep(x = dates[idx2], times = length(countries))
  
  # Make week table
  for(country in countries){
    
    # Get country data
    tmp <- results[which(results$`Countries and territories` == country),]
    
    # Loop through weeks
    for(j in 1:length(idx1)){
      line <- which(weeks$`Countries and territories` == country & weeks$week == j)
      tmpweek <- tmp[idx1[j]:idx2[j],]
      weeks$Cases[line] <- sum(tmpweek$Cases, na.rm = T)
      weeks$Deaths[line] <- sum(tmpweek$Deaths, na.rm = T)
      if(length(which(!is.na(tmpweek$ccases)))==0){
        weeks$ccases[line] <- 0
      } else{
        weeks$ccases[line] <- max(tmpweek$ccases, na.rm = T)
      }
      if(length(which(!is.na(tmpweek$cdeaths)))==0){
        weeks$cdeaths[line] <- 0
      } else{
        weeks$cdeaths[line] <- max(tmpweek$cdeaths, na.rm = T)
      }
    }
  }
  
  weeks<-merge(pops[,-2], weeks, by = "Countries and territories")
  code<-c("N/A")
  weeks<-weeks[-which(weeks$Code=="N/A" | is.na(weeks$Pop_Data.2018)),]
  
  weeks$ccases.million<-1e6*weeks$ccases/weeks$Pop_Data.2018
  weeks$cdeaths.million<-1e6*weeks$cdeaths/weeks$Pop_Data.2018
  
  weeks$`Countries and territories`[which(weeks$`Countries and territories`=="United States of America")]<-"United States"
  weeks$`Countries and territories`[which(weeks$`Countries and territories`=="Russia")]<-"Russian Federation"
  weeks$`Countries and territories`[which(weeks$`Countries and territories`=="Cote dIvoire")]<-"Côte d'Ivoire"
  weeks$`Countries and territories`[which(weeks$`Countries and territories`=="Congo")]<-"Republic of the Congo"
  weeks$`Countries and territories`[which(weeks$`Countries and territories`=="United Republic of Tanzania")]<-"Tanzania"
  weeks$`Countries and territories`[which(weeks$`Countries and territories`=="North Macedonia")]<-"Macedonia"
  weeks$`Countries and territories`[which(weeks$`Countries and territories`=="South Korea")]<-"Republic of Korea"
  weeks$`Countries and territories`[which(weeks$`Countries and territories`=="Laos")]<-"Lao PDR"
  weeks$`Countries and territories`[which(weeks$`Countries and territories`=="Czechia")]<-"Czech Republic"
  
  weeks<-weeks[order(weeks$Cases),]
  weeks$Cases.log <- log10(weeks$Cases)
  weeks$Cases.log[which(weeks$Cases.log<0)]<-0
  
  weeks<-weeks[order(weeks$ccases),]
  weeks$ccases.log <- log10(weeks$ccases)
  weeks$ccases.log[which(weeks$ccases.log<0)]<-0
  
  weeks<-weeks[order(weeks$ccases.million),]
  weeks$ccases.million.log <- log10(weeks$ccases.million)
  weeks$ccases.million.log[which(weeks$ccases.million.log<0)]<-0
  
  pallete<-colorRamp(c('white', '#ff1e1e'))
  
  pal <- colorNumeric(palette = pallete, domain=c(min(weeks$Cases.log),max(weeks$Cases.log)))
  weeks$color_Cases<-pal(weeks$Cases.log)
  
  pal <- colorNumeric(palette = pallete, domain=c(min(weeks$ccases.log),max(weeks$ccases.log)))
  weeks$color_ccases<-pal(weeks$ccases.log)
  
  pal <- colorNumeric(palette = pallete, domain=c(min(weeks$ccases.million.log),max(weeks$ccases.million.log)))
  weeks$color_ccases.million<-pal(weeks$ccases.million.log)
  
  if(length(weeks$color_ccases[which(is.na(weeks$color_ccases) | weeks$color_ccases=="#FFFFFF")])>0){
    weeks$color_ccases[which(is.na(weeks$color_ccases) | weeks$color_ccases=="#FFFFFF")]<-"none"
  }
  if(length(weeks$color_Cases[which(is.na(weeks$color_Cases) | weeks$color_Cases=="#FFFFFF")])>0){
    weeks$color_Cases[which(is.na(weeks$color_Cases) | weeks$color_Cases=="#FFFFFF")]<-"none"
  }
  if(length(weeks$color_ccases.million[which(is.na(weeks$color_ccases.million) | weeks$color_ccases.million=="#FFFFFF")])>0){
    weeks$color_ccases.million[which(is.na(weeks$color_ccases.million) | weeks$color_ccases.million=="#FFFFFF")]<-"none"
  }
  
  print(head(weeks))
  return(weeks)
}

