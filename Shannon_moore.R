###########################################################################################
### Calculate Shannon diversity of landcover within Moore neighbourhood of focal square ###
###########################################################################################

### Hannah White 27.07.2018

## read in dataframe that contains %cover of different CORINE land cover classes

corine.agg <- read.csv('G:\\Postdoc Grassland Resilience\\LandCoverData\\corine.agg.csv', header = TRUE)
corine.agg$east <- corine.agg$east + 5000
corine.agg$north <- corine.agg$north + 5000



### Calculate distances between squares

dd <- dist(corine.agg[,2:3], method = 'maximum')
matrixdd <- as.matrix(dd)
rm(dd)


landscape.het <- rep(NA, 830)


for (i in 1: 830){
  ind <- which(matrixdd[i,]==10000)
  temp.set <- corine.agg[ind,]
  
  habs <- colSums(temp.set[,4:18])
  shan.moore <- diversity(habs, index = 'shannon')
  landscape.het[i] <- shan.moore
}

# add to original corine.agg where easting and northing haven't been transformed
corine.agg$landscape.het <- landscape.het 
 
write.csv(corine.agg, 'G:\\Postdoc Grassland Resilience\\LandCoverData\\corine.agg.csv', row.names = FALSE)







