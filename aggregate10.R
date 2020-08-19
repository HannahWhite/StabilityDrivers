#####################################
### Aggregate MODIS data to 10 km ###
#####################################

# Script to read in processed MODIS data for Ireland
# and analyse the variance, sum, median and mean within hectads
#
# Jon Yearsley Aug 2016 / Edited Hannah White March 2017 / 
# Edited Hannah White May 2020 / Edited July 2020


# Code now also changes negative NDVI and EVI values to zero
# raster now clipped to extent of biological records

library(raster)
library(foreach)
library(doParallel)
#library(doMC)


# Register cluster with 2 nodes
cl<-makeCluster(2)
registerDoParallel(cl)


rm(list=ls())
#setwd("/media/jon/3TB/jon/PeopleStuff/Resilience_MarkJack")
setwd("E:/Postdoc Grassland Resilience/")

# Scaling for aggregation. Raw data is at 250mx250m
scaleFactor=40  # Scale to 10km
filename_out = 'MODIS6/modisv6_10km_all_REFIT2_FLIP.RData'

# Load MODIS data
evi.files = list.files(path='F:/MODISProcess',pattern='EVI_([0-9]{4})_([0-9]{2})_([0-9]{2}).grd',full.names=T)
#ndvi.files = list.files(path='F:/MODIS6/MODISProcess',pattern='NDVI_([0-9]{4})_([0-9]{2})_([0-9]{2}).grd',full.names=T)

dateInd = c(regexpr(evi.files[1],pattern='([0-9]{4})_([0-9]{2})_([0-9]{2})',fixed=F))
evi.dates = strptime(substr(evi.files,start=dateInd, stop=dateInd+9), format="%Y_%m_%d")

date.order=order(evi.dates)

modis.dates = evi.dates[date.order]
julian.date = julian(modis.dates)
day = format(modis.dates, '%d')
month = format(modis.dates, '%m')
year = format(modis.dates, '%Y')


# # Read in Ireland coastline
# ie = readOGR(dsn='Data', layer='country')
# ie.grid = spTransform(ie, CRS=CRS("+init=epsg:29903"))   # Transform to Irish Grid TM75

## read in covariates raster file to get extent of data
covar.rast <- raster('E:\\Postdoc Grassland Resilience\\DataAlignment\\covar.tif')
extent(covar.rast) # these are actually 5000 out becauseoriginal coord is bottom left not centre
rasterExtent <- extent(c(30000, 360000, 30000, 460000))

# Get the coords of the new aggregated data - flip to aggregate from bottom corner and flip back so that Ireland is not upside down
tmp = flip(aggregate(flip(crop(raster(evi.files[1]), rasterExtent), direction = 'y'), fact=scaleFactor, fun=mean, na.rm=T), direction= 'y')
coord = coordinates(tmp) - 5000 # minus 5000 so label is bottom left corner
eastings = t(array(coord[,1],dim=rev(dim(tmp)[1:2])))
northings = t(array(coord[,2],dim=rev(dim(tmp)[1:2])))
projection = proj4string(tmp)


# evi.mean.list <- foreach (f = icount(3), .packages='raster', .inorder=T) %dopar% {
#   # Read in processed MODIS data (subsetted to pasture and CRS set to Irish TM75, rounded to nearest hectad)
#   as.matrix(aggregate(raster(evi.files[f]), fact=scaleFactor, fun=mean, na.rm=T))
# }

# Aggregate EVI data
evi.mean.list <- foreach (f = icount(length(evi.files)), .packages='raster', .inorder=T) %dopar% {
  # Read in processed MODIS data (CRS set to Irish TM75, rounded to nearest hectad)
  tmp <- raster(evi.files[f])
  tmp[tmp < 0] <- 0 # change negative EVI pixels to 0
  as.matrix(flip(aggregate(flip(crop(tmp, rasterExtent), direction = 'y'), fact=scaleFactor, fun=mean, na.rm=T), direction = 'y'))
}
evi.median.list <- foreach (f = icount(length(evi.files)), .packages='raster', .inorder=T) %dopar% {
  # Read in processed MODIS data (CRS set to Irish TM75, rounded to nearest hectad)
  tmp <- raster(evi.files[f])
  tmp[tmp < 0] <- 0
  as.matrix(flip(aggregate(flip(crop(tmp, rasterExtent), direction = 'y'), fact=scaleFactor, fun=median, na.rm=T), direction = 'y'))
}
evi.sd.list <- foreach (f = icount(length(evi.files)), .packages='raster', .inorder=T) %dopar% {
  # Read in processed MODIS data (CRS set to Irish TM75, rounded to nearest hectad)
  tmp <- raster(evi.files[f])
  tmp[tmp < 0] <- 0
  as.matrix(flip(aggregate(flip(crop(tmp, rasterExtent), direction = 'y'), fact=scaleFactor, fun=sd, na.rm=T), direction = 'y'))
}

evi.sum.list <- foreach (f = icount(length(evi.files)), .packages='raster', .inorder=TRUE) %dopar% {
  # Read in processed MODIS data (CRS set to Irish TM75, rounded to nearest hectad)
  tmp <- raster(evi.files[f])
  tmp[tmp < 0] <- 0
  as.matrix(flip(aggregate(flip(crop(tmp, rasterExtent), direction = 'y'), fact = scaleFactor,  fun = sum, na.rm = TRUE), direction = 'y'))
}
evi.ncell.list <- foreach (f = icount(length(evi.files)), .packages='raster', .inorder=T) %dopar% {
  # Read in processed MODIS data (CRS set to Irish TM75, rounded to nearest hectad)
  tmp <- raster(evi.files[f])
  tmp[tmp < 0] <- 0
  as.matrix(flip(aggregate(flip(crop(tmp, rasterExtent), direction = 'y'), fact=scaleFactor, fun=function(x, na.rm=T){sum(!is.na(x), na.rm=na.rm)}), direction = 'y'))
}

# Aggregate NDVI data
#ndvi.mean.list <- foreach (f = icount(length(ndvi.files)), .packages='raster', .inorder=T) %dopar% {
#  # Read in processed MODIS data (CRS set to Irish TM75, rounded to nearest hectad)
#  tmp <- raster(ndvi.files[f])
#  tmp[tmp < 0] <- 0
#  as.matrix(aggregate(crop(tmp, rasterExtent), fact=scaleFactor, fun=mean, na.rm=T))
#}
#ndvi.median.list <- foreach (f = icount(length(ndvi.files)), .packages='raster', .inorder=T) %dopar% {
#  # Read in processed MODIS data (CRS set to Irish TM75, rounded to nearest hectad)
#  tmp <- raster(ndvi.files[f])
#  tmp[tmp < 0] <- 0
#  as.matrix(aggregate(crop(tmp, rasterExtent), fact=scaleFactor, fun=median, na.rm=T))
#}
#ndvi.sd.list <- foreach (f = icount(length(ndvi.files)), .packages='raster', .inorder=T) %dopar% {
#  # Read in processed MODIS data (CRS set to Irish TM75, rounded to nearest hectad)
#  tmp <- raster(ndvi.files[f])
#  tmp[tmp < 0] <- 0
#  as.matrix(aggregate(tmp, fact=scaleFactor, fun=sd, na.rm=T))
#}
#ndvi.sum.list <- foreach (f = icount(length(ndvi.files)), .packages='raster', .inorder = TRUE) %dopar% {
#  # Read in processed MODIS data (CRS set to Irish TM75, rounded to nearest hectad)
#  tmp <- raster(ndvi.files[f])
#  tmp[tmp < 0] <- 0
#  as.matrix(aggregate(tmp, fact=scaleFactor, fun = sum, na.rm = TRUE))
#}
#ndvi.ncell.list <- foreach (f = icount(length(ndvi.files)), .packages='raster', .inorder=T) %dopar% {
#  # Read in processed MODIS data (CRS set to Irish TM75, rounded to nearest hectad)
#  tmp <- raster(ndvi.files[f])
#  tmp[tmp < 0] <- 0
#  as.matrix(aggregate(tmp, fact=scaleFactor, fun=function(x, na.rm=T){sum(!is.na(x), na.rm=na.rm)}))
#}

# Put the results in a big matrix
dimMatrix=c(dim(evi.mean.list[[1]]), length(modis.dates))
evi.mean.z = array(NA, dim=dimMatrix)
evi.median.z =array(NA, dim=dimMatrix)
evi.sd.z = array(NA, dim=dimMatrix)
evi.sum.z = array(NA, dim=dimMatrix)
evi.ncell.z = array(NA, dim=dimMatrix)
for (f in 1:length(evi.files)) {
  evi.mean.z[,,f] = evi.mean.list[[date.order[f]]]
  evi.median.z[,,f] = evi.median.list[[date.order[f]]]
  evi.sd.z[,,f] = evi.sd.list[[date.order[f]]]
  evi.sum.z[,,f] = evi.sum.list[[date.order[f]]]
  evi.ncell.z[,,f] = evi.ncell.list[[date.order[f]]]
}

#ndvi.mean.z = array(NA, dim=dimMatrix)
#ndvi.median.z = array(NA, dim=dimMatrix)
#ndvi.sd.z = array(NA, dim=dimMatrix)
#ndvi.sum.z = array(NA, dim=dimMatrix)
#ndvi.ncell.z = array(NA, dim=dimMatrix)
#for (f in 1:length(evi.files)) {
#  ndvi.mean.z[,,f] = ndvi.mean.list[[date.order[f]]]
#  ndvi.median.z[,,f] = ndvi.median.list[[date.order[f]]]
#  ndvi.sd.z[,,f] = ndvi.sd.list[[date.order[f]]]
#  ndvi.sum.z[,,f] = ndvi.sum.list[[date.order[f]]]
#  ndvi.ncell.z[,,f] = ndvi.ncell.list[[date.order[f]]]
#}


save(evi.mean.z, evi.median.z, evi.sd.z, evi.sum.z, evi.ncell.z, eastings, northings, projection, modis.dates, julian.date, day,month,year, rasterExtent,file=filename_out )


stopCluster(cl)
