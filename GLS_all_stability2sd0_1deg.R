######################################################################################################################
### Stability analyses with 2 sd threshold using generalised least squares in all hectads and 0.1 degree EOBs data ###
######################################################################################################################

### Hannah White 30.09.2019

## Uses 2 sd threshold for 'large' event (i.e. a 2 sigma event) and the means (rather than medians as before)
## Now uses more accurate EOBs data at 0.1 degree resolution

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

### Scale and centre covariates


stability.env[, c(3:6, 13:20, 38:40)] <- scale(stability.env[, c(3:6, 13:20, 38:40)]) 

###### MODELS ######

### varability

## Null 

#m.var.null <- gls(log(evi.var) ~ 1, correlation=corExp(form=~east+north, metric='euclidean'),
#                  method = 'ML', data = stability.env)

## AIC =  -578.1228

## Full

m.var <- gls(log(evi.var) ~ nat.fres + landscape.het + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
             method = 'ML', data = stability.env) 

## AIC = -604.5215

## Diversity only

#m.var.div <- gls(log(evi.var) ~ nat.fres, correlation=corExp(form=~east+north, metric='euclidean'),
#                 method = 'ML', data = stability.env)

## AIC =  -602.0599

## Landscape only

#m.var.land <- gls(log(evi.var) ~ landscape.het, correlation=corExp(form=~east+north, metric='euclidean'),
#                  method = 'ML', data = stability.env)

## AIC = -576.1297


## Climate only

#m.var.clim <- gls(log(evi.var) ~ var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
#                  method = 'ML', data = stability.env)

## AIC = -581.5557

## Diversity and land

#m.var.divland <- gls(log(evi.var) ~ nat.fres + landscape.het, correlation=corExp(form=~east+north, metric='euclidean'),
#                     method = 'ML', data = stability.env) 

## AIC = -600.0712

## Diversity and climate

m.var.divclim <- gls(log(evi.var) ~ nat.fres + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
                     method = 'ML', data = stability.env)

intervals(m.var.divclim)
## AIC = -606.2877

## Landcover and climate

#m.var.landclim <- gls(log(evi.var) ~ landscape.het + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
#                      method = 'ML', data = stability.env)

## AIC =   -579.6581



### Resistance


#m.resist.null <- gls(evi.mag ~ 1, correlation=corExp(form=~east+north, metric='euclidean'),
#                     method = 'ML', data = stability.env)

## AIC = -3928.806


m.resist <- gls(evi.mag ~ nat.fres + landscape.het + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
                method = 'ML', data = stability.env)

## AIC = -3953.832


## Diversity only

#m.resist.div <- gls(evi.mag ~ nat.fres , correlation=corExp(form=~east+north, metric='euclidean'),
#                    method = 'ML', data = stability.env)


## AIC = -3936.819


## Landcover only


#m.resist.land <- gls(evi.mag ~ landscape.het, correlation=corExp(form=~east+north, metric='euclidean'),
#                     method = 'ML', data = stability.env)

## AIC =  -3932.607


## Climate only

#m.resist.clim <- gls(evi.mag ~ var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
#                     method = 'ML', data = stability.env)

## AIC = -3952.662


## Diversity and landcover

#m.resist.divland <- gls(evi.mag ~ nat.fres + landscape.het, correlation=corExp(form=~east+north, metric='euclidean'),
#                        method = 'ML', data = stability.env)

## AIC = -3936.623


## Diversity and climate

m.resist.divclim <- gls(evi.mag ~ nat.fres + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
                        method = 'ML', data = stability.env)

intervals(m.resist.divclim)
## AIC =  -3954.756


## Landcover and climate

m.resist.landclim <- gls(evi.mag ~ landscape.het + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
                         method = 'ML', data = stability.env)

## AIC = -3954.068


### Recovery time

## Null
#m.rec.null <- gls(log(evi.rec) ~ 1, correlation=corExp(form=~east+north, metric='euclidean'),
#                  method = 'ML', data = stability.env)

## AIC =  768.7379

## Full
m.rec <- gls(log(evi.rec) ~ nat.fres + landscape.het + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
             method = 'ML', data = stability.env)

## AIC = 670.9439

## Diversity only
#m.rec.div <- gls(log(evi.rec) ~ nat.fres, correlation=corExp(form=~east+north, metric='euclidean'),
#                 method = 'ML', data = stability.env)

## AIC = 751.6888

## Landcover only

#m.rec.land <- gls(log(evi.rec) ~ landscape.het, correlation=corExp(form=~east+north, metric='euclidean'),
#                  method = 'ML', data = stability.env)

## AIC = 760.6367

## Climate only

#m.rec.clim <- gls(log(evi.rec) ~ var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
#                  method = 'ML', data = stability.env)

## AIC = 686.5793

## Diversity and landcover

#m.rec.divland <- gls(log(evi.rec) ~ nat.fres + landscape.het, correlation=corExp(form=~east+north, metric='euclidean'),
#                     method = 'ML', data = stability.env)

## AIC = 749.7418

## Diversity and climate

m.rec.divclim <- gls(log(evi.rec) ~ nat.fres + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
                     method = 'ML', data = stability.env)

intervals(m.rec.divclim)
## AIC =  669.2708

## Landcover and climate

#m.rec.landclim <- gls(log(evi.rec) ~ landscape.het + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
#                      method = 'ML', data = stability.env)

## AIC = 682.5583




### Recovery rate

## Null model
#m.recrate.null <- gls(evi.recrate ~ 1, correlation=corExp(form=~east+north, metric='euclidean'),
#                      method = 'ML', data = stability.env)

## AIC = 4308.9

## Full
m.recrate <- gls(evi.recrate ~ nat.fres + landscape.het + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
                 method = 'ML', data = stability.env)
intervals(m.recrate)
## AIC = 4282.185

## Diversity only

#m.recrate.div <- gls(evi.recrate ~ nat.fres, correlation=corExp(form=~east+north, metric='euclidean'),
#                     method = 'ML', data = stability.env)

## AIC =  4308.266

## Landcover only 
#m.recrate.land <- gls(evi.recrate ~ landscape.het, correlation=corExp(form=~east+north, metric='euclidean'),
#                      method = 'ML', data = stability.env)

## AIC =  4310.824

## Climate only
m.recrate.clim <- gls(evi.recrate ~ var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
                      method = 'ML', data = stability.env)
## AIC = 4283.791

## Diversity and landcover

#m.recrate.divland <- gls(evi.recrate ~ nat.fres + landscape.het, correlation=corExp(form=~east+north, metric='euclidean'),
#                         method = 'ML', data = stability.env)

## AIC = 4310.151

## Diversity and climate
m.recrate.divclim <- gls(evi.recrate ~ nat.fres + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
                         method = 'ML', data = stability.env)
## AIC = 4282.566

## Landcover and climate
#m.recrate.landclim <- gls(evi.recrate ~ landscape.het + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
#                          method = 'ML', data = stability.env)

## AIC = 4285.408






