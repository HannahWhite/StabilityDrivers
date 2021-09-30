########################################################################
### Calculating resilience metrics using biggest drop window methods ### 
### from 2010-2019 at 10 km Scale with 2 sd                          ###
########################################################################

### Hannah White 27.08.2021
## This is based on Resilience_10km_method2sd_REFIT2.R but with a shorter time 
## period in response to reviewer at Functional Ecology 2021


rm(list=ls())

## read in anomaly data

load('D:\\Postdoc Grassland Resilience\\MODIS6\\Anomalies_18.02.2019\\refit2.anomaly10km.2010.RData')
load('D:\\Postdoc Grassland Resilience\\MODIS6\\Anomalies_18.02.2019\\refit2.anomaly10km.cent.2010.RData')


## read in EVI data so that have Julian dates

load('D:/Postdoc Grassland Resilience/MODIS6/modisv6_10km_all_REFIT2_FLIP.RData')

rm(evi.mean.z, evi.median.z, evi.ncell.z, evi.sd.z)

year10 <- which(year >= 2010 & year <= 2019) 

modis.dates <- modis.dates[year10]
month <- month[year10]
year <- year[year10]
julian.date <- julian.date[year10]

modis.dates <- as.Date(modis.dates)

################ Resilience metric calculations #################

##### Variability

evi.variability <- array(NA, dim =c(dim(eastings)))


## calculate variability from 2010-2019

for (i in 1:dim(eastings)[1]){
  for (j in 1:dim(eastings)[2]){
    if (any(is.finite(evi.anomaly[i,j,]))) evi.variability[i,j] <- var(na.omit(evi.anomaly.cent[i,j,]))
    
  }
}


##### Resistance

#anomaly.03 <- evi.anomaly[,,ind2] # evi anomaly array just for years 2003-2017

evi.magnitude <- array(NA, dim=c(dim(eastings)))

evi.events <- array(NA, dim=c(dim(eastings)))

evi.all.events <- array(NA, dim=c(dim(eastings), 158)) # made 3rd dimension 158 as maximum number of resistance events is 157

evi.dates <- array(NA, dim=c(dim(eastings), 158))
class(evi.dates) <- 'Date' # for array to fill with dates needs to be set as Date class


### testing
#i <- 23
#j <- 18

## Function returns NA is y is within 6 months of x
magnitude.compare <- function(vec, x, y){
  return(
    ifelse(vec[y]-vec[x]<=180 & vec[y]-vec[x]>=-180, NA, order.anom[y])
  )
}

#Calculating magnitude (resistance) 

try(
  for (i in 1:dim(eastings)[1]){
    for (j in 1:dim(eastings)[2]){
      ind.sd <- which(evi.anomaly[i, j, ]<=-2) # finds anomalies which deviate more than two standard deviations from the zero baseline
      anomaly.sub <- evi.anomaly[i,j,ind.sd] # subsets the anomaly dataframe to just those that are greater than two standard deviations
      
      julian.ind <- julian.date[ind.sd]  # finds the julian dates for these anomalies
      dates.ind <- modis.dates[ind.sd]
      
      order.anom <- anomaly.sub[order(anomaly.sub)]  # orders anomalies from largest to smallest
      order.julian <- julian.ind[order(anomaly.sub)] # orders julian dates in same order as anomalies
      order.dates <- dates.ind[order(anomaly.sub)] # orders modis dates in same order as anomalies
      
      if(length(ind.sd)>0){
        close.ind <- array(NA, dim = c(length(ind.sd), length(ind.sd)))
        
        
        for (k in 1:length(ind.sd)) {
          for (l in 1:length(ind.sd)){
            
            close.ind[k, l] <- magnitude.compare(order.julian, k, l) # compares number of days between anomalies, if within 6 months each way, returns NA
          } 
        }
        
        diag(close.ind) <- order.anom # sets diagonal to anomalies rather than NA
        
        resistances <- close.ind[1,] # sets up resistances vector initially with anomalies kept that fall outside of 6 months of the largest anomaly
        
        if(length(ind.sd)>1){
          
          for (m in 2:length(ind.sd)){   ## loops through close.ind matrix diagonally and adds new NAs
            sub <- close.ind[m,]    
            na.ind <- which(is.na(sub))
            na.ind2 <- na.ind[which(na.ind > m)]
            resistances[na.ind2] <- NA
            
          } 
          
          dates <- order.dates
          dates[which(is.na(resistances))] <- NA
          
        } else {
          resistances <- resistances
          dates <- order.dates
        } 
        
        
      } else { 
        resistances <- NA
        dates <- NA
      }
      
      
      evi.all.events[i,j, ] <- c(resistances, rep(NA, 158-length(resistances))) # fills each site with all resistances plus remainder NAs
      evi.dates[i,j, ] <- c(dates, rep(NA, 158-length(dates))) 
      
      evi.events[i,j] <- ifelse(length(resistances)==length(which(is.na(resistances))), 0, length(which(!is.na(resistances)))) # if resistances is just NA then indicates there are 0 resistance events
      evi.magnitude[i,j] <- mean(resistances, na.rm = TRUE) # calculates mean of resistances for each individual pixel
      
    }
  }
)

evi.magnitude.median <- apply(evi.all.events, c(1,2), median, na.rm = TRUE) # calculates median of all drops below -2 sd


##### Recovery time


### First need to calculate moving windows

# Create empty array to hold moving window averages where window size is 6 units

mov.avg <- array(NA, dim = c(dim(eastings),(dim(evi.anomaly)[3])-5))


for (kk in 1:455){ #begins moving window at each separate time point
  window.frame <- evi.anomaly[,,kk:(kk+5)] # moving window of 6 units
  mov.avg[,,kk] <- apply(window.frame, c(1,2), mean, na.rm = TRUE)
  
}


# Return times  and rates - calculated simultaneously


evi.all.rectime <- array(NA, dim=c(dim(eastings), 158))


evi.all.rate <- array(NA, dim=c(dim(eastings), 158))



### need to match dates

for (ii in 1:dim(eastings)[1]){
  for (jj in 1:dim(eastings)[2]){
    if (any(!is.na(evi.anomaly[ii,jj,]))){
      
      event.length <- length(which(is.finite(evi.all.events[ii,jj,]))) ## number of 'resistance' events
      date.event <- evi.dates[ii, jj, ] # need to extract date so that know when to start searching moving windows
      date.event.finite <- date.event[which(!is.na(date.event))] 
      
      if (event.length > 0){
        for (mm in 1:event.length){
          
          
          d.day <- date.event.finite[mm]
          start <- which(modis.dates==d.day)
          
          if (start<=454) {
            
            return.time <- which(mov.avg[ii, jj, (start+1):455]>=0)[1] # calculates recovery time in units of time windows
            # needs to be start + 1 so that event window isn't counted
            
            evi.all.rectime[ii, jj, mm] <- return.time
            
            evi.all.rate[ii, jj, mm] <- abs(evi.anomaly[ii, jj, start])/return.time # uses absolute so that rates are positive 
            
            
          } else {
            evi.all.rectime[ii, jj, mm] <- NA # gives recovery of NA if there are no events that dropped below -1 standard deviations
            evi.all.rate[ii, jj, mm] <- NA # gives recovery rate of NA if there are no events that dropped below -1 standard deviations
          }
          
          
        } 
        
        
      }
    }
    
  }
  
}


evi.rectime <- apply(evi.all.rectime, c(1,2), mean, na.rm = TRUE)  # calculates mean return time 
evi.rectime.median <- apply(evi.all.rectime, c(1,2), median, na.rm = TRUE) #calculates median return time
evi.rectime.big <- evi.all.rectime[,,1] # recovery time from biggest drop

evi.rate <- apply(evi.all.rate, c(1,2), mean, na.rm = TRUE) # calculates mean rate
evi.rate.median <- apply(evi.all.rate, c(1,2), median, na.rm = TRUE) # calculates median rate
evi.rate.big <- evi.all.rate[,,1] # calculates rate of return from biggest drop


#evi.slowest.rate <- apply(evi.all.rate, c(1,2), max, na.rm = TRUE) # slowest recovery rate from drops below -1 sd




### Cross Correlations

## Calculate and evi.drop which is the biggest drop in each site

evi.drop <- apply(evi.all.events, c(1,2), min, na.rm = TRUE) # has to be min as currently negative



## Firstly for variability and mean measures across time periods for others

# Get mean measures and variability into dataframe

## var
evi.var <- evi.variability[which(is.finite(evi.variability))]
eastings.var <- eastings[which(is.finite(evi.variability))]
northings.var <- northings[which(is.finite(evi.variability))]

evi.var <- data.frame(evi.var, eastings = eastings.var, northings = northings.var)


## resistance
evi.mag <- evi.magnitude[which(is.finite(evi.magnitude))]
eastings.mag <- eastings[which(is.finite(evi.magnitude))]
northings.mag <- northings[which(is.finite(evi.magnitude))]

evi.mag <- data.frame(evi.mag, eastings = eastings.mag, northings = northings.mag)

## median resistance
evi.magmed <- evi.magnitude.median[which(is.finite(evi.magnitude.median))]
eastings.magmed <- eastings[which(is.finite(evi.magnitude.median))]
northings.magmed <- northings[which(is.finite(evi.magnitude.median))]

evi.magmed <- data.frame(evi.magmed = evi.magmed, eastings = eastings.magmed, northings = northings.magmed)

## max drop
evi.maxdrop <- evi.drop[which(is.finite(evi.drop))]
eastings.maxdrop <- eastings[which(is.finite(evi.drop))]
northings.maxdrop <- northings[which(is.finite(evi.drop))]

evi.maxdrop <- data.frame(evi.drop = evi.maxdrop, eastings = eastings.maxdrop, northings = northings.maxdrop)

## recovery time
evi.rec <- evi.rectime[which(is.finite(evi.rectime))]
eastings.rec <- eastings[which(is.finite(evi.rectime))]
northings.rec <- northings[which(is.finite(evi.rectime))]

evi.rec <- data.frame(evi.rec, eastings = eastings.rec, northings = northings.rec)

## recovery time median
evi.recmed <- evi.rectime.median[which(is.finite(evi.rectime.median))]
eastings.recmed <- eastings[which(is.finite(evi.rectime.median))]
northings.recmed <- northings[which(is.finite(evi.rectime.median))]

evi.rec.med<- data.frame(evi.recmed, eastings = eastings.recmed, northings = northings.recmed)

## recovery time from biggest drop
evi.rec.big <- evi.rectime.big[which(is.finite(evi.rectime.big))]
eastings.recbig <- eastings[which(is.finite(evi.rectime.big))]
northings.recbig <- northings[which(is.finite(evi.rectime.big))]

evi.rec.big <- data.frame(evi.rec.big, eastings = eastings.recbig, northings = northings.recbig)

## rate
evi.recrate <- evi.rate[which(is.finite(evi.rate))]
eastings.recrate <- eastings[which(is.finite(evi.rate))]
northings.recrate <- northings[which(is.finite(evi.rate))]

evi.recrate <- data.frame(evi.recrate, eastings = eastings.recrate, northings = northings.recrate)


## rate median
evi.recrate.med <- evi.rate.median[which(is.finite(evi.rate.median))]
eastings.recrate.med <- eastings[which(is.finite(evi.rate.median))]
northings.recrate.med <- northings[which(is.finite(evi.rate.median))]

evi.recrate.med <- data.frame(evi.recrate.med, eastings = eastings.recrate.med, northings = northings.recrate.med)


## rate from biggest drop
evi.recrate.big <- evi.rate.big[which(is.finite(evi.rate.big))]
eastings.recratebig <- eastings[which(is.finite(evi.rate.big))]
northings.recratebig <- northings[which(is.finite(evi.rate.big))]

evi.recrate.big <- data.frame(evi.recrate.big, eastings = eastings.recratebig, northings = northings.recratebig)




### Merge

stability10km <- merge(evi.var, evi.mag, by = c('eastings', 'northings'), all = TRUE)
stability10km <- merge(stability10km, evi.magmed, by = c('eastings', 'northings'), all = TRUE)
stability10km <- merge(stability10km, evi.rec, by = c('eastings', 'northings'), all = TRUE)
stability10km <- merge(stability10km, evi.recrate, by = c('eastings', 'northings'), all = TRUE)
stability10km <- merge(stability10km, evi.maxdrop, by = c('eastings', 'northings'), all = TRUE)

stability10km <- merge(stability10km, evi.rec.med, by = c('eastings', 'northings'), all = TRUE)
stability10km <- merge(stability10km, evi.rec.big, by = c('eastings', 'northings'), all = TRUE)
stability10km <- merge(stability10km, evi.recrate.med, by = c('eastings', 'northings'), all = TRUE)
stability10km <- merge(stability10km, evi.recrate.big, by = c('eastings', 'northings'), all = TRUE)


# change magnitude to the absolute number

stability10km$evi.mag <- abs(stability10km$evi.mag)
stability10km$evi.magmed <- abs(stability10km$evi.magmed)
stability10km$evi.drop <- abs(stability10km$evi.drop)


write.csv(stability10km, 'D:\\Postdoc Grassland Resilience\\MODIS6\\Resilience\\REFIT2stability10km2sd.2010.csv', row.names = FALSE)



