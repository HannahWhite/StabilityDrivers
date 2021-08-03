####################################################################
### Create Eobs variables for whole dataset with 0.1 degree data ###
####################################################################

## Hannah White 28.08.2018
## Modified 15.11.2018 so only climate from 1950-1999
##  Modified 19.05.20 to use vascplant coords rather than MODIS

library(raster)
library(rgdal)
library(rgeos)
library(spdep)

load("J:\\Postdoc Grassland Resilience\\Climate Data\\eobs_0.1deg.RData")

eobs <- eobs_data

# cut down dates so only from 1950-1999

eobs <- eobs[eobs$date < '2000-01-01', ]



coords <- eobs[,c(1,2)]

coords.sp <- SpatialPointsDataFrame(coords, eobs) 

proj4string(coords.sp) <- CRS('+init=epsg:4326') # project to WGS 84 latitude and longitude


## transform to ITM

eobs.itm <- spTransform(coords.sp, CRS('+init=epsg:29903')) #  Irish grid





## read in dataframe with sites want to get climate data for
corine <- read.csv('E:\\Postdoc Grassland Resilience\\LandCoverData\\corine.agg.csv', header = TRUE)

vasc.nat <- read.csv('E:\\Postdoc Grassland Resilience\\Species richness\\vasc.nat.csv', header = TRUE)
vasc.nat <- vasc.nat[vasc.nat$Location %in% corine$GR, ] #just get terrestrial hectads'

coords.data <- vasc.nat[,2:3]

coords.data.sp <- SpatialPointsDataFrame(coords.data, vasc.nat) # turn into spatial data so that can use nearest neighbour approach 

proj4string(coords.data.sp) <- CRS('+init=epsg:29903') # Irish grid



#### Loop through dates

# create empty array within which to hold data

date.array <- array(data = NA, dim = c(830, 9, 18262)) #  24837 is use dataset that HW downloaded from 1950-2017
#  18262 is dataset from 1950-1999

dates <- unique(eobs.itm$date)


for (i in 1:length(dates)){
  
  temp.eobs <- eobs.itm[eobs.itm$date == dates[i],]
  temp.eobs.df <- data.frame(temp.eobs)[,-c(3,11)]
  
  for (j in 1:830){
    temp.coords <- coords.data.sp[j,]
    distances <- spDists(temp.coords@coords, temp.eobs@coords)
    
    min.index <- which.min(distances)
    date.array[j,1:9,i] <- as.matrix(temp.eobs.df[min.index,])
    
  }
}




save(date.array, file = 'J:\\Postdoc Grassland Resilience\\Climate Data\\nearneighEOBS99_0.1deg.RData')



### Calculate mean, variance and fat tail across each temperature and precipiation measure


mean.clim <- apply(date.array[,4:5,], c(1,2), FUN = mean, na.rm = TRUE)

var.clim <- apply(date.array[,4:5,], c(1,2), FUN = var, na.rm = TRUE)

sd.clim <- apply(date.array[,4:5,], c(1,2), FUN = sd, na.rm = TRUE)


names(mean.clim) <- c('mean.rr', 'mean.tg')
names(var.clim) <- c('var.rr', 'var.tg')
names(sd.clim) <- c('sd.rr', 'sd.tg')



### fat tail function taken from Schmid and Trede 2003

fattail_func <- function(x){
  quants <- quantile(x, probs = c(0.975, 0.025, 0.875, 0.125), na.rm = TRUE)
  ft <- (quants[1]-quants[2])/(quants[3]-quants[4])
  return(ft)
}

ft.clim <- apply(date.array[,4:5,], c(1,2), FUN = fattail_func)


climate <- data.frame(east = recovery$east, north = recovery$north, mean.clim, var.clim, sd.clim, ft.clim)
names(climate)[3:10] <- c('mean.rr', 'mean.tg',  
                          'var.rr', 'var.tg', 
                          'sd.rr', 'sd.tg', 
                          'ft.rr', 'ft.tg') 

save(climate, file = 'J:\\Postdoc Grassland Resilience\\Climate Data\\eobs99_processed0.1deg.RData')



