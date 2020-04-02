connection <- function(){
  tryCatch(
    {
      #download the dataset from the ECDC website to a local temporary file
      GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".csv")))
      
      #read the Dataset sheet into “R”. The dataset will be called "data".
      realdata <- read.csv(tf)
      
      names(realdata)<-c("DateRep","Day","Month","Year","Cases","Deaths","Countries and territories","GeoId","Code","Pop_Data.2018")
      
      realdata$DateRep<-as.Date(realdata$DateRep, format = "%d/%m/%Y")
      realdata<-realdata[order(realdata$DateRep),]
      realdata$`Countries and territories`<-gsub(pattern = "_", replacement = " ", x = realdata$`Countries and territories`)
      
      return(realdata)
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

getcountries <- function(data){
  if(is.null(data)){
    return("none")
  } else{
    df<-unique(sort(data$`Countries and territories`))
    return(df)
  }
}

ncases <- function(data){
  if(is.null(data)){
    return("none")
  } else{
    return(sum(data$Cases))
  }
}

ndeaths <- function(data){
  if(is.null(data)){
    return("none")
  } else{
    return(sum(data$Deaths))
  }
}

ncountries <- function(data){
  if(is.null(data)){
    return("none")
  } else{
    return(length(unique(sort(data$`Countries and territories`))))
  }
}

plot_simulated_growth_curve <- function(exdata){
  exdata$phase <- hmm(exdata,5)
  
  p<-ggplot(data = exdata, mapping = aes(x = day, y = cases))+
    geom_line()+
    xlab(label = "Days recorded")+
    ylab(label = "Thousand cases")+
    ggtitle(label = "Growth curve")+
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

plot_simulated_growth_rate <- function(exdata){
  exdata$phase <- hmm(exdata,5)
  
  p<-ggplot(data = exdata, mapping = aes(x = day, y = growth))+
    geom_line()+
    xlab(label = "Days recorded")+
    ylab(label = "Thousand cases per day")+
    ggtitle(label = "Growth rate")+
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

plot_simulated_growth_acceleration <- function(exdata){
  exdata$phase <- hmm(exdata,5)
  
  p<-ggplot(data = exdata, mapping = aes(x = day, y = acceleration))+
    geom_line()+
    xlab(label = "Days recorded")+
    ylab(label = "Thousand cases per day²")+
    ggtitle(label = "Growth accelaration")+
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

worldwidecases <- function(realdata){
  df<-realdata[which(realdata$Cases>=1),]
  df<-as.data.frame(tapply(df$Cases, df$DateRep, sum))
  df<-cumsum(df)
  return(df)
}

worldwidedeaths <- function(realdata){
  df<-realdata[which(realdata$Deaths>=1),]
  df<-as.data.frame(tapply(df$Deaths, df$DateRep, sum))
  df<-cumsum(df)
  return(df)
}

plot_worldwide <- function(df, Pandemic){
  gp<-ggplot(data = df, mapping = aes(x = date, y = n))+
    geom_bar(stat = "identity")+
    geom_vline(aes(xintercept = as.numeric(Pandemic) ,color="Pandemic"), show.legend = T)+
    scale_color_manual("Alert", values = c(Pandemic = "red"))+
    labs(x = "Date", y = "Thousand cases")+
    theme(axis.text.x = element_text(size = 12, angle = 0, hjust = 1, vjust = 1),
          axis.text.y = element_text(size = 12, angle = 0, hjust = 0, vjust = 0, face = "plain"),  
          axis.title.x = element_text(size = 16, angle = 0, hjust = 0, vjust = 0, face = "plain"),
          axis.title.y = element_text(size = 16, angle = 90, hjust = 0, vjust = 0, face = "plain"),
          legend.title=element_blank(),
          legend.text = element_text(size = 12, face = "bold.italic"),
          rect = element_rect(fill = "transparent")
    )
  gp
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
  } else{
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
      shinyalert(title = "Error!", text = 'There is not enough data to display!', type = "error")
      return(NULL)
    }
  }
}

getpredprev <- function(){
  return(info$prev)
}

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

getfile <- function(file){
  inFile <- file
  if(is.null(inFile)){
    return(NULL)
  } else {
    ext <- tools::file_ext(inFile$name)
    if(ext %in% c("xls", "xlsx")){
      file.rename(from = inFile$datapath, to = paste(inFile$datapath, ext, sep="."))
      df <- read_excel(paste(inFile$datapath, ext, sep="."), 1)
      names(df) <- c("DateRep", "Cases")
      df <- as.data.frame(df)
      df$DateRep <- as.Date(df$DateRep)
      return(df)
    } else {
      shinyalert(title = "Error!", text = 'The file must have ".xlsx" or ".xls" extension!', type = "error")
      reset("loadfile")
    }
  }
}

plot_realdata_growth_curve <- function(df){
  
  p <- ggplot(
    data = df[-nrow(df),],
    mapping = aes(
      x = DateRep, y = c,
      group=1,
      text = paste(
        'Date: ', DateRep,
        '<br>Predicted prevalence: ', round(fitted*1000, digits = 0),
        '<br>Observed prevalence: ', c*1000,
        '<br>New cases: ', Cases,
        '<br>New deaths: ', Deaths
      )
    )
  )+
    theme(legend.position = "none")+
    xlab(label = "")+
    ylab(label = "Thousand cases")+
    ggtitle(label = "Growth curve")
  
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

plot_realdata_growth_rate <- function(df){
  p<-ggplot(
    data = df[-nrow(df),],
    mapping = aes(
      x = DateRep, y = growth, group = 1,
      text = paste(
        'Date: ', DateRep,
        '<br>Predicted prevalence: ', round(fitted*1000, digits = 0),
        '<br>Observed prevalence: ', c*1000,
        '<br>New cases: ', Cases,
        '<br>New deaths: ', Deaths
      )
    )
  )+
    theme(legend.position = "none")+
    xlab(label = "")+
    ylab(label = "Thousand cases per day")+
    xlim(min(df$DateRep),max(df$DateRep))+
    ggtitle(label = "Growth rate")
  
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

plot_realdata_growth_acceleration <- function(df){
  y <-  c(abs(min(df$acceleration)), abs(max(df$acceleration)))
  y <- max(y)
  p<-ggplot(
    data = df, 
    mapping = aes(
      x = DateRep, y = acceleration,group=1,
      text = paste(
        'Date: ', DateRep,
        '<br>Predicted prevalence: ', round(fitted*1000, digits = 0),
        '<br>Observed prevalence: ', c*1000,
        '<br>New cases: ', Cases,
        '<br>New deaths: ', Deaths
      )
    )
  )+
    theme(legend.position = "none")+
    ylim(-y,y)+
    xlim(min(df$DateRep),max(df$DateRep))+
    xlab(label = "")+
    ylab(label = "Thousand cases per day²")+
    ggtitle(label = "Growth accelaration")
  
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

plot_localdata_growth_curve <- function(df,smooth,hmmfactor){
  
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
          'Date: ', DateRep,
          '<br>Fitted: ', round(fitted*1000, digits = 0),
          '<br>Cumulative: ', c*1000,
          '<br>New cases: ', Cases
        )
      )
    )+
      xlab(label = "")+
      ylab(label = "Thousand cases")+
      ggtitle(label = "Growth curve")
    
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

plot_localdata_growth_rate <- function(df,smooth,hmmfactor){
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
          'Date: ', DateRep,
          '<br>Fitted: ', round(fitted*1000, digits = 0),
          '<br>Cumulative: ', c*1000,
          '<br>New cases: ', Cases
        )
      )
    )+
      xlab(label = "")+
      ylab(label = "Thousand cases per day")+
      xlim(min(df$DateRep),max(df$DateRep))+
      ggtitle(label = "Growth rate")
    
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

plot_localdata_growth_acceleration <- function(df,smooth,hmmfactor){
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
          'Date: ', DateRep,
          '<br>Fitted: ', round(fitted*1000, digits = 0),
          '<br>Cumulative: ', c*1000,
          '<br>New cases: ', Cases
        )
      )
    )+
      ylim(-y,y)+
      xlim(min(df$DateRep),max(df$DateRep))+
      xlab(label = "")+
      ylab(label = "Thousand cases per day²")+
      ggtitle(label = "Growth accelaration")
    
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