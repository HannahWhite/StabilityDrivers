###############################################################################################################################
### Stability analyses with 2 sd threshold using generalised least squares in heterogeneous areas with 0.1 degree EOBs data ###
###############################################################################################################################

### Hannah White 09.07.2020

## Uses 2 sd threshold for 'large' event (i.e. a 2 sigma event) 
## and the means (rather than medians as before)
## Now uses correctly aligned MODIS data - 2nd refit

library(nlme)

rm(list=ls())

## load data

### Load in data

## Read in data and extract what is required

## Read in data and extract what is required

covars <- read.csv('E:\\Postdoc Grassland Resilience\\EnvironmentData\\covariates.csv', header = TRUE)

# stability metrics

stability10km <- read.csv('E:\\Postdoc Grassland Resilience\\MODIS6\\Resilience\\REFIT2stability10km2sd.csv', header = TRUE)

stability10km$evi.rec <- stability10km$evi.rec*8
stability10km$evi.recrate <- stability10km$evi.recrate*8


# extract metrics required for modelling

stability.red <- stability10km[, c(1:4, 6:7)]

# merge and get stability just down to 830 squares

stability.env <- merge(covars, stability.red, by.x = c('east', 'north'), by.y = c('eastings', 'northings'), all.x = TRUE, all.y = FALSE)

# change Spnum_out to nat.fres
names(stability.env)[4] <- 'nat.fres'

## Create dominant habitat column at 50% threshold

dominant <- function (x, threshold) {
  ifelse(max(x) >=threshold, names(which.max(x)), 'Heterogenous') 
}

stability.env$dom50 <- apply(stability.env[,6:20], 1, function(x) dominant(x, threshold = 0.5))

stability.env$hetero.dom50 <- ifelse(stability.env$dom50 == 'Heterogenous', 'H', 'N')

stability.env$pasture.dom50 <- ifelse(stability.env$dom50 == 'Pasture', 'P', 'N')


stability.env$hetpast <- ifelse(stability.env$dom50 == 'Heterogenous', 'H',
                                ifelse(stability.env$dom50 == 'Pasture', 'P', 'N'))


### 

# Extract heterogeneous squares 

hetero.div <- stability.env[stability.env$hetpast == 'H',]


### Scale and centre covariates


hetero.div[, c(4, 22:32)] <- scale(hetero.div[, c(4, 22:32)]) 


#### MODELS ####


## Variability


## Null 

m.var.null <- gls(log(evi.var) ~ 1, correlation=corExp(form=~east+north, metric='euclidean'),
                  method = 'ML', data = hetero.div)

## AIC = 6.844142

## Full

#m.var <- gls(log(evi.var) ~ nat.fres + landscape.het + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
#             method = 'ML', data = hetero.div) 

## AIC= 14.95615

## Diversity only

m.var.div <- gls(log(evi.var) ~ nat.fres, correlation=corExp(form=~east+north, metric='euclidean'),
                 method = 'ML', data = hetero.div)

## AIC = 8.801104

## Landscape only

m.var.land <- gls(log(evi.var) ~ landscape.het, correlation=corExp(form=~east+north, metric='euclidean'),
                  method = 'ML', data = hetero.div)
intervals(m.var.land)
## AIC = 8.083441


## Climate only

#m.var.clim <- gls(log(evi.var) ~ var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
#                  method = 'ML', data = hetero.div)

## AIC = 12.12986

## Diversity and land

#m.var.divland <- gls(log(evi.var) ~ nat.fres + landscape.het, correlation=corExp(form=~east+north, metric='euclidean'),
#                     method = 'ML', data = hetero.div) 
#intervals(m.var.divland)
## AIC = 9.875509

## Diversity and climate

#m.var.divclim <- gls(log(evi.var) ~ nat.fres + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
#                     method = 'ML', data = hetero.div)

## AIC =  14.02321

## Landcover and climate

#m.var.landclim <- gls(log(evi.var) ~ landscape.het + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
#                      method = 'ML', data = hetero.div)

## AIC = 13.38165



### Resistance


#m.resist.null <- gls(evi.mag ~ 1, correlation=corExp(form=~east+north, metric='euclidean'),
#                     method = 'ML', data = hetero.div)

## AIC = -1050.25


#m.resist <- gls(evi.mag ~ nat.fres + landscape.het + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
#                method = 'ML', data = hetero.div)
#intervals(m.resist)
## AIC =  -1058.176


## Diversity only

#m.resist.div <- gls(evi.mag ~ nat.fres , correlation=corExp(form=~east+north, metric='euclidean'),
#                    method = 'ML', data = hetero.div)


## AIC = -1049.382


## Landcover only


#m.resist.land <- gls(evi.mag ~ landscape.het, correlation=corExp(form=~east+north, metric='euclidean'),
#                     method = 'ML', data = hetero.div)

## AIC = -1048.28


## Climate only

m.resist.clim <- gls(evi.mag ~ var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
                     method = 'ML', data = hetero.div)

## AIC = -1060.285


## Diversity and landcover

#m.resist.divland <- gls(evi.mag ~ nat.fres + landscape.het, correlation=corExp(form=~east+north, metric='euclidean'),
#                        method = 'ML', data = hetero.div)

## AIC = -1047.677


## Diversity and climate

m.resist.divclim <- gls(evi.mag ~ nat.fres + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
                        method = 'ML', data = hetero.div)
intervals(m.resist.divclim)
## AIC = -1059.479


## Landcover and climate

m.resist.landclim <- gls(evi.mag ~ landscape.het + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
                         method = 'ML', data = hetero.div)

## AIC = -1058.346



### Recovery time

## Null
#m.rec.null <- gls(log(evi.rec) ~ 1, correlation=corExp(form=~east+north, metric='euclidean'),
#                  method = 'ML', data = hetero.div)

## AIC =  195.1802

## Full
#m.rec <- gls(log(evi.rec) ~ nat.fres + landscape.het + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
#             method = 'ML', data = hetero.div)
#intervals(m.rec)
## AIC = 177.8669

## Diversity only
#m.rec.div <- gls(log(evi.rec) ~ nat.fres, correlation=corExp(form=~east+north, metric='euclidean'),
#                 method = 'ML', data = hetero.div)

## AIC = 197.0787

## Landcover only

#m.rec.land <- gls(log(evi.rec) ~ landscape.het, correlation=corExp(form=~east+north, metric='euclidean'),
#                  method = 'ML', data = hetero.div)

## AIC = 197.1726

## Climate only

m.rec.clim <- gls(log(evi.rec) ~ var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
                  method = 'ML', data = hetero.div)

## AIC = 175.499

## Diversity and landcover

#m.rec.divland <- gls(log(evi.rec) ~ nat.fres + landscape.het, correlation=corExp(form=~east+north, metric='euclidean'),
#                     method = 'ML', data = hetero.div)

## AIC =  199.0786

## Diversity and climate

m.rec.divclim <- gls(log(evi.rec) ~ nat.fres + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
                     method = 'ML', data = hetero.div)
#intervals(m.rec.divclim)
## AIC =  177.1735

## Landcover and climate

m.rec.landclim <- gls(log(evi.rec) ~ landscape.het + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
                      method = 'ML', data = hetero.div)
intervals(m.rec.landclim)
## AIC = 176.9176


### Recovery rate

## Null model
#m.recrate.null <- gls(evi.recrate ~ 1, correlation=corExp(form=~east+north, metric='euclidean'),
#                      method = 'ML', data = hetero.div)

## AIC = 1169.054

## Full
m.recrate <- gls(evi.recrate ~ nat.fres + landscape.het + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
                 method = 'ML', data = hetero.div)
intervals(m.recrate)
## AIC = 1161.067  

## Diversity only

#m.recrate.div <- gls(evi.recrate ~ nat.fres, correlation=corExp(form=~east+north, metric='euclidean'),
#                     method = 'ML', data = hetero.div)

## AIC =  1169.189

## Landcover only 
#m.recrate.land <- gls(evi.recrate ~ landscape.het, correlation=corExp(form=~east+north, metric='euclidean'),
#                      method = 'ML', data = hetero.div)

## AIC = 1170.927 

## Climate only
m.recrate.clim <- gls(evi.recrate ~ var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
                      method = 'ML', data = hetero.div)
intervals(m.recrate.clim)
## AIC = 1159.807

## Diversity and landcover

#m.recrate.divland <- gls(evi.recrate ~ nat.fres + landscape.het, correlation=corExp(form=~east+north, metric='euclidean'),
#                         method = 'ML', data = hetero.div)

## AIC = 1171.187

## Diversity and climate
m.recrate.divclim <- gls(evi.recrate ~ nat.fres + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
                         method = 'ML', data = hetero.div)
intervals(m.recrate.divclim)
## AIC = 1159.749

## Landcover and climate
#m.recrate.landclim <- gls(evi.recrate ~ landscape.het + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
##                          method = 'ML', data = hetero.div)
#intervals(m.recrate.landclim)
## AIC = 1161.792


