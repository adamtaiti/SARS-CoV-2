connection <- function(){
  tryCatch(
    {
      #download the dataset from the ECDC website to a local temporary file
      GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".csv")))
      
      #read the Dataset sheet into “R”. The dataset will be called "data".
      realdata <- read.csv(tf)
      
      names(realdata)<-c("DateRep","Day","Month","Year","Cases","Deaths","Countries and territories","GeoId","Code","Pop_Data.2018")
<<<<<<< HEAD
      
=======
>>>>>>> 9902d758b83357edf19871b9f74ffaf2ff15a612
      realdata$DateRep<-as.Date(realdata$DateRep, format = "%d/%m/%Y")
      realdata<-realdata[order(realdata$DateRep),]
      
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
  ggplot(data = exdata, mapping = aes(x = day, y = cases))+
    geom_line()+
    xlab(label = "Days recorded")+
    ylab(label = "Thousand cases")+
    ggtitle(label = "Growth curve")
}

plot_simulated_growth_rate <- function(exdata){
  ggplot(data = exdata, mapping = aes(x = day, y = growth))+
    geom_line()+
    xlab(label = "Days recorded")+
    ylab(label = "Thousand cases per day")+
    ggtitle(label = "Growth rate")
}

plot_simulated_growth_acceleration <- function(exdata){
  ggplot(data = exdata, mapping = aes(x = day, y = acceleration))+
    geom_line()+
    xlab(label = "Days recorded")+
    ylab(label = "Thousand cases per day²")+
    ggtitle(label = "Growth accelaration")
}

worldwidecases <- function(realdata){
  df<-realdata[which(realdata$Cases>=1),]
  df<-as.data.frame(tapply(df$Cases, df$DateRep, sum))
  print(head(df))
  df<-cumsum(df)
  print(head(df))
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
getrealdata <- function(realdata, country, smooth){
  if(is.null(realdata)){
    return(NULL)
  } else{
    df <- realdata[which(realdata$`Countries and territories` == country & realdata$Cases >= 1),]
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
      group=1,
      text = paste(
        'Date: ', DateRep,
        '<br>Fitted: ', round(fitted*1000, digits = 0),
        '<br>Cumulative: ', c*1000,
        '<br>New cases: ', Cases
      )
    )
  )+
    geom_line(mapping = aes(x = DateRep, y = fitted), inherit.aes = T)+
    geom_point(mapping = aes(x = DateRep, y = c),show.legend = F,inherit.aes = T)+
    xlab(label = "")+
    ylab(label = "Thousand cases")+
    ggtitle(label = "Growth curve")
  ggplotly(p, tooltip="text")
}

plot_realdata_growth_rate <- function(df){
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
    geom_line()+
    xlab(label = "")+
    ylab(label = "Thousand cases per day")+
    xlim(min(df$DateRep),max(df$DateRep))+
    ggtitle(label = "Growth rate")
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
        '<br>Fitted: ', round(fitted*1000, digits = 0),
        '<br>Cumulative: ', c*1000,
        '<br>New cases: ', Cases
      )
    )
  )+
    geom_line()+
    ylim(-y,y)+
    xlim(min(df$DateRep),max(df$DateRep))+
    xlab(label = "")+
    ylab(label = "Thousand cases per day²")+
    ggtitle(label = "Growth accelaration")
  ggplotly(p, tooltip="text")
}

plot_localdata_growth_curve <- function(df,smooth){
  
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
    
    p <- ggplot(
      data = df[-nrow(df),],
      mapping = aes(
        group=1,
        text = paste(
          'Date: ', DateRep,
          '<br>Fitted: ', round(fitted*1000, digits = 0),
          '<br>Cumulative: ', c*1000,
          '<br>New cases: ', Cases
        )
      )
    )+
      geom_line(mapping = aes(x = DateRep, y = fitted), inherit.aes = T)+
      geom_point(mapping = aes(x = DateRep, y = c),show.legend = F,inherit.aes = T)+
      xlab(label = "")+
      ylab(label = "Thousand cases")+
      ggtitle(label = "Growth curve")
    ggplotly(p, tooltip="text")
    
  } else{
    shinyalert(title = "Error!", text = 'There is not enough data to display!', type = "error")
  }
}

plot_localdata_growth_rate <- function(df,smooth){
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
      geom_line()+
      xlab(label = "")+
      ylab(label = "Thousand cases per day")+
      xlim(min(df$DateRep),max(df$DateRep))+
      ggtitle(label = "Growth rate")
    ggplotly(p, tooltip="text")
    
  } else{
    shinyalert(title = "Error!", text = 'There is not enough data to display!', type = "error")
  }
}

plot_localdata_growth_acceleration <- function(df,smooth){
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
      geom_line()+
      ylim(-y,y)+
      xlim(min(df$DateRep),max(df$DateRep))+
      xlab(label = "")+
      ylab(label = "Thousand cases per day²")+
      ggtitle(label = "Growth accelaration")
    ggplotly(p, tooltip="text")
    
  } else{
    shinyalert(title = "Error!", text = 'There is not enough data to display!', type = "error")
  }
}