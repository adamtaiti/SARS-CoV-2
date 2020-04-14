df<-realdata

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
  
  # log
  cat(country,"\n")
  
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
  
  # log
  cat(country,"\n")
  
  # Get country data
  tmp <- results[which(results$`Countries and territories` == country),]
  
  # Loop through weeks
  for(j in 1:length(idx1)){
    line <- which(weeks$`Countries and territories` == country & weeks$week == j)
    tmpweek <- tmp[idx1[j]:idx2[j],]
    weeks$Cases[line] <- sum(tmpweek$Cases, na.rm = T)
    weeks$Deaths[line] <- sum(tmpweek$Deaths, na.rm = T)
    weeks$ccases[line] <- max(tmpweek$ccases, na.rm = T)
    weeks$cdeaths[line] <- max(tmpweek$cdeaths, na.rm = T)
  }
  
}
weeks$ccases[which(weeks$ccases == -Inf)] <- 0
weeks$cdeaths[which(weeks$cdeaths == -Inf)] <- 0

weeks<-merge(pops[,-2], weeks, by = "Countries and territories")
code<-c("N/A")
weeks<-weeks[-which(weeks$Code=="N/A" | is.na(weeks$Pop_Data.2018)),]

return(weeks)
}