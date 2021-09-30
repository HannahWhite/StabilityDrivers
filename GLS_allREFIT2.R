######################################################################################################################
### Stability analyses with 2 sd threshold using generalised least squares in all hectads and 0.1 degree EOBs data ###
######################################################################################################################

### Hannah White 09.07.2020
### Edited 11.11.2020 for new recovery rate models with new units

## Uses 2 sd threshold for 'large' event (i.e. a 2 sigma event) and the means (rather than medians as before)
## Now uses more accurate EOBs data at 0.1 degree resolution
## Run with correctly aligned MODIS data - 2nd refit

library(nlme)

rm(list=ls())

## load data

### Load in data

## Read in data and extract what is required

covars <- read.csv('D:\\Postdoc Grassland Resilience\\EnvironmentData\\covariates.csv', header = TRUE)

# stability metrics

stability10km <- read.csv('D:\\Postdoc Grassland Resilience\\MODIS6\\Resilience\\REFIT2stability10km2sd.csv', header = TRUE)

stability10km$evi.rec <- stability10km$evi.rec*8
stability10km$evi.recrate <- stability10km$evi.recrate/8


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
### Scale and centre covariates


stability.env[, c(4, 22:32)] <- scale(stability.env[, c(4, 22:32)]) 

###### MODELS ######

### varability

## Null 

#m.var.null <- gls(log(evi.var) ~ 1, correlation=corExp(form=~east+north, metric='euclidean'),
#                  method = 'ML', data = stability.env)

## AIC =  -503.3531

## Full

#m.var <- gls(log(evi.var) ~ nat.fres + landscape.het + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
#             method = 'ML', data = stability.env) 

## AIC = -510.2989

## Diversity only

m.var.div <- gls(log(evi.var) ~ nat.fres, correlation=corExp(form=~east+north, metric='euclidean'),
                 method = 'ML', data = stability.env)

## AIC = -513.4229

## Landscape only

#m.var.land <- gls(log(evi.var) ~ landscape.het, correlation=corExp(form=~east+north, metric='euclidean'),
#                  method = 'ML', data = stability.env)
#intervals(m.var.land)
## AIC =  -501.7332


## Climate only

#m.var.clim <- gls(log(evi.var) ~ var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
#                  method = 'ML', data = stability.env)

## AIC = -501.7575

## Diversity and land

m.var.divland <- gls(log(evi.var) ~ nat.fres + landscape.het, correlation=corExp(form=~east+north, metric='euclidean'),
                     method = 'ML', data = stability.env) 
intervals(m.var.divland)
## AIC = -511.6217

## Diversity and climate

m.var.divclim <- gls(log(evi.var) ~ nat.fres + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
                     method = 'ML', data = stability.env)

#intervals(m.var.divclim)
## AIC = -512.274

## Landcover and climate

#m.var.landclim <- gls(log(evi.var) ~ landscape.het + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
#                      method = 'ML', data = stability.env)

## AIC = -499.8649  



### Resistance


#m.resist.null <- gls(evi.mag ~ 1, correlation=corExp(form=~east+north, metric='euclidean'),
#                     method = 'ML', data = stability.env)

## AIC = -51.39468


m.resist <- gls(evi.mag ~ nat.fres + landscape.het + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
                method = 'ML', data = stability.env)
intervals(m.resist)
## AIC = -77.56113


## Diversity only

#m.resist.div <- gls(evi.mag ~ nat.fres , correlation=corExp(form=~east+north, metric='euclidean'),
#                    method = 'ML', data = stability.env)

## AIC = -56.39934


## Landcover only


#m.resist.land <- gls(evi.mag ~ landscape.het, correlation=corExp(form=~east+north, metric='euclidean'),
#                     method = 'ML', data = stability.env)

## AIC = -56.89603


## Climate only

#m.resist.clim <- gls(evi.mag ~ var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
#                     method = 'ML', data = stability.env)

## AIC = -76.73921


## Diversity and landcover

#m.resist.divland <- gls(evi.mag ~ nat.fres + landscape.het, correlation=corExp(form=~east+north, metric='euclidean'),
#                        method = 'ML', data = stability.env)

## AIC =   -57.88722


## Diversity and climate

m.resist.divclim <- gls(evi.mag ~ nat.fres + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
                        method = 'ML', data = stability.env)

intervals(m.resist.divclim)
## AIC = -77.50832


## Landcover and climate

m.resist.landclim <- gls(evi.mag ~ landscape.het + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
                         method = 'ML', data = stability.env)

## AIC = -78.90291


### Recovery time

## Null
#m.rec.null <- gls(log(evi.rec) ~ 1, correlation=corExp(form=~east+north, metric='euclidean'),
#                  method = 'ML', data = stability.env)

## AIC =  701.1355

## Full
m.rec <- gls(log(evi.rec) ~ nat.fres + landscape.het + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
             method = 'ML', data = stability.env)
intervals(m.rec)
## AIC = 617.2677

## Diversity only
#m.rec.div <- gls(log(evi.rec) ~ nat.fres, correlation=corExp(form=~east+north, metric='euclidean'),
#                 method = 'ML', data = stability.env)

## AIC = 684.6035

## Landcover only

#m.rec.land <- gls(log(evi.rec) ~ landscape.het, correlation=corExp(form=~east+north, metric='euclidean'),
#                  method = 'ML', data = stability.env)

## AIC = 697.1257

## Climate only

#m.rec.clim <- gls(log(evi.rec) ~ var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
#                  method = 'ML', data = stability.env)

## AIC = 634.6448

## Diversity and landcover

#m.rec.divland <- gls(log(evi.rec) ~ nat.fres + landscape.het, correlation=corExp(form=~east+north, metric='euclidean'),
#                     method = 'ML', data = stability.env)

## AIC = 684.8254 

## Diversity and climate

m.rec.divclim <- gls(log(evi.rec) ~ nat.fres + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
                     method = 'ML', data = stability.env)

#intervals(m.rec.divclim)
## AIC =  615.3422

## Landcover and climate

#m.rec.landclim <- gls(log(evi.rec) ~ landscape.het + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
#                      method = 'ML', data = stability.env)
#intervals(m.rec.landclim)
## AIC = 631.5408




### Recovery rate

## Null model
#m.recrate.null <- gls(evi.recrate ~ 1, correlation=corExp(form=~east+north, metric='euclidean'),
#                      method = 'ML', data = stability.env)

## AIC = -2613.694

## Full
m.recrate <- gls(evi.recrate ~ nat.fres + landscape.het + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
                 method = 'ML', data = stability.env)
intervals(m.recrate)
## AIC = -2645.89

## Diversity only

#m.recrate.div <- gls(evi.recrate ~ nat.fres, correlation=corExp(form=~east+north, metric='euclidean'),
#                     method = 'ML', data = stability.env)

## AIC =  -2613.975 

## Landcover only 
#m.recrate.land <- gls(evi.recrate ~ landscape.het, correlation=corExp(form=~east+north, metric='euclidean'),
#                      method = 'ML', data = stability.env)

## AIC =  -2611.768

## Climate only
m.recrate.clim <- gls(evi.recrate ~ var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
                      method = 'ML', data = stability.env)
intervals(m.recrate.clim)
## AIC = -2645.283

## Diversity and landcover

#m.recrate.divland <- gls(evi.recrate ~ nat.fres + landscape.het, correlation=corExp(form=~east+north, metric='euclidean'),
#                         method = 'ML', data = stability.env)

## AIC = -2612.055

## Diversity and climate
m.recrate.divclim <- gls(evi.recrate ~ nat.fres + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
                         method = 'ML', data = stability.env)
intervals(m.recrate.divclim)
## AIC = -2646.065

## Landcover and climate
#m.recrate.landclim <- gls(evi.recrate ~ landscape.het + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
#                          method = 'ML', data = stability.env)

## AIC = -2643.516


