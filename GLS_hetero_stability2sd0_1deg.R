###############################################################################################################################
### Stability analyses with 2 sd threshold using generalised least squares in heterogeneous areas with 0.1 degree EOBs data ###
###############################################################################################################################

### Hannah White 22.08.2019

## Uses 2 sd threshold for 'large' event (i.e. a 2 sigma event) and the means (rather than medians as before)

library(nlme)

rm(list=ls())

## load data

### Load in data

## Read in data and extract what is required

vasc.env <- read.csv('E:/Postdoc Grassland Resilience/Species richness/vasc.env.csv', header = TRUE)

vasc.env <- vasc.env[,c(1:2, 43:46)] # extracts diversity estimates


# stability metrics

stability10km <- read.csv('E:\\Postdoc Grassland Resilience\\MODIS6\\Resilience\\stability10km2sd.csv', header = TRUE)

stability10km$evi.rec <- stability10km$evi.rec*8
stability10km$evi.recrate <- stability10km$evi.recrate*8
stability10km$evi.long <- stability10km$evi.long*8
stability10km$evi.slow <- stability10km$evi.slow*8

# extract metrics required for modelling

stability.red <- stability10km[, c(1:4, 6:7, 13:14)]

# merge and get stability just down to 830 squares

stability.env <- merge(vasc.env, stability.red, by.x = c('east', 'north'), by.y = c('eastings', 'northings'), all.x = TRUE, all.y = FALSE)

## climate
load('E:\\Postdoc Grassland Resilience\\Climate Data\\eobs99_processed0.1deg.RData')

stability.env <- merge(stability.env, climate, by = c('east', 'north'), all = TRUE)

## add in landscape heterogeneity data

corine.agg <- read.csv('E:\\Postdoc Grassland Resilience\\LandCoverData\\corine.agg.csv', header = TRUE)
corine.agg$east <- corine.agg$east + 5000
corine.agg$north <- corine.agg$north + 5000

stability.env <- merge(stability.env, corine.agg, by = c('east', 'north'), all = TRUE)


## Create dominant habitat column at 50% threshold

dominant <- function (x, threshold) {
  ifelse(max(x) >=threshold, names(which.max(x)), 'Heterogenous') 
}

stability.env$dom50 <- apply(stability.env[,22:36], 1, function(x) dominant(x, threshold = 0.5))

stability.env$hetero.dom50 <- ifelse(stability.env$dom50 == 'Heterogenous', 'H', 'N')

stability.env$pasture.dom50 <- ifelse(stability.env$dom50 == 'Pasture', 'P', 'N')


stability.env$hetpast <- ifelse(stability.env$dom50 == 'Heterogenous', 'H',
                                ifelse(stability.env$dom50 == 'Pasture', 'P', 'N'))


### 

# Extract heterogeneous squares 

hetero.div <- stability.env[stability.env$hetpast == 'H',]


### Scale and centre covariates


hetero.div[, c(3:6, 13:20, 38:40)] <- scale(hetero.div[, c(3:6, 13:20, 38:40)]) 


#### MODELS ####


## Variability


## Null 

m.var.null <- gls(log(evi.var) ~ 1, correlation=corExp(form=~east+north, metric='euclidean'),
                  method = 'ML', data = hetero.div)

## AIC = -33.15045

## Full

#m.var <- gls(log(evi.var) ~ nat.fres + landscape.het + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
#             method = 'ML', data = hetero.div) 

## AIC = -25.0937

## Diversity only

m.var.div <- gls(log(evi.var) ~ nat.fres, correlation=corExp(form=~east+north, metric='euclidean'),
                 method = 'ML', data = hetero.div)

## AIC =  -31.22892

## Landscape only

m.var.land <- gls(log(evi.var) ~ landscape.het, correlation=corExp(form=~east+north, metric='euclidean'),
                  method = 'ML', data = hetero.div)

## AIC = -31.59915


## Climate only

#m.var.clim <- gls(log(evi.var) ~ var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
#                  method = 'ML', data = hetero.div)

## AIC = -28.56445

## Diversity and land

#m.var.divland <- gls(log(evi.var) ~ nat.fres + landscape.het, correlation=corExp(form=~east+north, metric='euclidean'),
#                     method = 'ML', data = hetero.div) 

## AIC = -29.61089

## Diversity and climate

#m.var.divclim <- gls(log(evi.var) ~ nat.fres + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
#                     method = 'ML', data = hetero.div)

## AIC =  -26.67977

## Landcover and climate

#m.var.landclim <- gls(log(evi.var) ~ landscape.het + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
#                      method = 'ML', data = hetero.div)

## AIC =  -27.08168



### Resistance


#m.resist.null <- gls(evi.mag ~ 1, correlation=corExp(form=~east+north, metric='euclidean'),
#                     method = 'ML', data = hetero.div)

## AIC = -1048.361


m.resist <- gls(evi.mag ~ nat.fres + landscape.het + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
                method = 'ML', data = hetero.div)

## AIC =  -1057.401


## Diversity only

#m.resist.div <- gls(evi.mag ~ nat.fres , correlation=corExp(form=~east+north, metric='euclidean'),
#                    method = 'ML', data = hetero.div)


## AIC = -1047.326


## Landcover only


#m.resist.land <- gls(evi.mag ~ landscape.het, correlation=corExp(form=~east+north, metric='euclidean'),
#                     method = 'ML', data = hetero.div)

## AIC = -1046.386


## Climate only

m.resist.clim <- gls(evi.mag ~ var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
                     method = 'ML', data = hetero.div)

## AIC = -1057.817


## Diversity and landcover

#m.resist.divland <- gls(evi.mag ~ nat.fres + landscape.het, correlation=corExp(form=~east+north, metric='euclidean'),
#                        method = 'ML', data = hetero.div)

## AIC = -1045.551


## Diversity and climate

m.resist.divclim <- gls(evi.mag ~ nat.fres + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
                        method = 'ML', data = hetero.div)
intervals(m.resist.divclim)
## AIC =  -1058.217


## Landcover and climate

#m.resist.landclim <- gls(evi.mag ~ landscape.het + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
#                         method = 'ML', data = hetero.div)

## AIC = -1055.91


### Recovery time

## Null
#m.rec.null <- gls(log(evi.rec) ~ 1, correlation=corExp(form=~east+north, metric='euclidean'),
#                  method = 'ML', data = hetero.div)

## AIC =  233.0514

## Full
m.rec <- gls(log(evi.rec) ~ nat.fres + landscape.het + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
             method = 'ML', data = hetero.div)

## AIC = 210.1574

## Diversity only
#m.rec.div <- gls(log(evi.rec) ~ nat.fres, correlation=corExp(form=~east+north, metric='euclidean'),
#                 method = 'ML', data = hetero.div)

## AIC = 234.6547

## Landcover only

#m.rec.land <- gls(log(evi.rec) ~ landscape.het, correlation=corExp(form=~east+north, metric='euclidean'),
#                  method = 'ML', data = hetero.div)

## AIC = 234.5075

## Climate only

m.rec.clim <- gls(log(evi.rec) ~ var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
                  method = 'ML', data = hetero.div)

## AIC = 209.3305

## Diversity and landcover

#m.rec.divland <- gls(log(evi.rec) ~ nat.fres + landscape.het, correlation=corExp(form=~east+north, metric='euclidean'),
#                     method = 'ML', data = hetero.div)

## AIC = 236.3257

## Diversity and climate

m.rec.divclim <- gls(log(evi.rec) ~ nat.fres + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
                     method = 'ML', data = hetero.div)
intervals(m.rec.divclim)
## AIC = 208.7443

## Landcover and climate

#m.rec.landclim <- gls(log(evi.rec) ~ landscape.het + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
#                      method = 'ML', data = hetero.div)

## AIC = 211.3287


### Recovery rate

## Null model
#m.recrate.null <- gls(evi.recrate ~ 1, correlation=corExp(form=~east+north, metric='euclidean'),
#                      method = 'ML', data = hetero.div)

## AIC = 1181.881

## Full
m.recrate <- gls(evi.recrate ~ nat.fres + landscape.het + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
                 method = 'ML', data = hetero.div)
intervals(m.recrate)
## AIC =  1167.527

## Diversity only

#m.recrate.div <- gls(evi.recrate ~ nat.fres, correlation=corExp(form=~east+north, metric='euclidean'),
#                     method = 'ML', data = hetero.div)

## AIC =  1180.947

## Landcover only 
#m.recrate.land <- gls(evi.recrate ~ landscape.het, correlation=corExp(form=~east+north, metric='euclidean'),
#                      method = 'ML', data = hetero.div)

## AIC =  1183.877

## Climate only
#m.recrate.clim <- gls(evi.recrate ~ var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
#                      method = 'ML', data = hetero.div)
## AIC = 1170.36

## Diversity and landcover

#m.recrate.divland <- gls(evi.recrate ~ nat.fres + landscape.het, correlation=corExp(form=~east+north, metric='euclidean'),
#                         method = 'ML', data = hetero.div)

## AIC = 1182.729

## Diversity and climate
m.recrate.divclim <- gls(evi.recrate ~ nat.fres + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
                         method = 'ML', data = hetero.div)
## AIC = 1167.827

## Landcover and climate
#m.recrate.landclim <- gls(evi.recrate ~ landscape.het + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
#                          method = 'ML', data = hetero.div)

## AIC = 1172.154


### Long

## Null
#m.long.null <- gls(log(evi.long) ~ 1, correlation=corExp(form=~east+north, metric='euclidean'),
#                   method = 'ML', data = hetero.div)

## AIC = -0.4696414

## Full
#m.long <- gls(log(evi.long) ~ nat.fres + landscape.het + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
#              method = 'ML', data = hetero.div)

## AIC =  -13.20408

## Diversity only
#m.long.div <- gls(log(evi.long) ~ nat.fres, correlation=corExp(form=~east+north, metric='euclidean'),
#                  method = 'ML', data = hetero.div)

## AIC =  1.513142

## Landcover only
#m.long.land <- gls(log(evi.long) ~ landscape.het, correlation=corExp(form=~east+north, metric='euclidean'),
#                   method = 'ML', data = hetero.div)

## AIC =  1.355383

## Climate only
#m.long.clim <- gls(log(evi.long) ~ var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
#                   method = 'ML', data = hetero.div)
## AIC = -15.68884

## Diversity and landcover
#m.long.divland <- gls(log(evi.long) ~ nat.fres + landscape.het, correlation=corExp(form=~east+north, metric='euclidean'),
 #                     method = 'ML', data = hetero.div)
## AIC =  3.294015


## Diversity and climate
#m.long.divclim <- gls(log(evi.long) ~ nat.fres + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
#                      method = 'ML', data = hetero.div)
## AIC = -15.17737

## Landcover and climate

#m.long.landclim <- gls(log(evi.long) ~ landscape.het + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
#                       method = 'ML', data = hetero.div)

## AIC =  -13.7871










