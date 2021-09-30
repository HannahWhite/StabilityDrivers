##############################################################
### Models of pasture with shorted climatic history window ###
##############################################################

### Hannah White 03.08.2021
### Checked and rerun 31.08.2021
### In response to reviewer comments from Functional Ecology

library(nlme)

rm(list = ls())

####### Read in data #########
load('D:\\Postdoc Grassland Resilience\\Climate Data\\eobs80_99_processed0.1deg.RData')

## Read in data and extract what is required

covars <- read.csv('D:\\Postdoc Grassland Resilience\\EnvironmentData\\covariates.csv', header = TRUE)

## remove climate variables in covars and add ones from climate (calculated from 1980-99)

covars.short <- covars[,1:24]

covars.short <- merge(covars.short, climate, by = c('east', 'north'), all = TRUE)

rm(covars)

# stability metrics

stability10km <- read.csv('D:\\Postdoc Grassland Resilience\\MODIS6\\Resilience\\REFIT2stability10km2sd.csv', header = TRUE)

stability10km$evi.rec <- stability10km$evi.rec*8
stability10km$evi.recrate <- stability10km$evi.recrate/8


# extract metrics required for modelling

stability.red <- stability10km[, c(1:4, 6:7)]

# merge and get stability just down to 830 squares

stability.env <- merge(covars.short, stability.red, by.x = c('east', 'north'), by.y = c('eastings', 'northings'), all.x = TRUE, all.y = FALSE)

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

# Extract pasture 

pasture.div <- stability.env[stability.env$hetpast == 'P',]

### Scale and centre covariates (nat.fres, landscape diversity and climate)

pasture.div[, c(4, 22:32)] <- scale(pasture.div[, c(4, 22:32)]) 


############# Models #################

## Variability


## Null 

m.var.null <- gls(log(evi.var) ~ 1, correlation=corExp(form=~east+north, metric='euclidean'),
                  method = 'ML', data = pasture.div)

## AIC =  -642.1002

## Full

#m.var <- gls(log(evi.var) ~ nat.fres + landscape.het + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
#             method = 'ML', data = pasture.div) 
#intervals(m.var)
# AIC = -635.2912

## Diversity only

m.var.div <- gls(log(evi.var) ~ nat.fres, correlation=corExp(form=~east+north, metric='euclidean'),
                 method = 'ML', data = pasture.div)

## AIC = -640.1209 

## Landscape only

m.var.land <- gls(log(evi.var) ~ landscape.het, correlation=corExp(form=~east+north, metric='euclidean'),
                  method = 'ML', data = pasture.div)
intervals(m.var.land)
## AIC = -640.114


## Climate only

#m.var.clim <- gls(log(evi.var) ~ var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
#                  method = 'ML', data = pasture.div)

## AIC = -639.2743

## Diversity and land

#m.var.divland <- gls(log(evi.var) ~ nat.fres + landscape.het, correlation=corExp(form=~east+north, metric='euclidean'),
#                     method = 'ML', data = pasture.div) 
#intervals(m.var.divland)
## AIC = -638.1339

## Diversity and climate

#m.var.divclim <- gls(log(evi.var) ~ nat.fres + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
#                     method = 'ML', data = pasture.div)
#intervals(m.var.divclim)

## AIC =  -637.2808

## Landcover and climate

#m.var.landclim <- gls(log(evi.var) ~ landscape.het + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
#                      method = 'ML', data = pasture.div)

## AIC = -637.2839



### Resistance


#m.resist.null <- gls(evi.mag ~ 1, correlation=corExp(form=~east+north, metric='euclidean'),
#                    method = 'ML', data = pasture.div)

## AIC = -23.26604


#m.resist <- gls(evi.mag ~ nat.fres + landscape.het + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
#                method = 'ML', data = pasture.div)
#intervals(m.resist)
## AIC = -33.66384 


## Diversity only


#m.resist.div <- gls(evi.mag ~ nat.fres , correlation=corExp(form=~east+north, metric='euclidean'),
#                    method = 'ML', data = pasture.div)
## AIC = -23.47764


## Landcover only


#m.resist.land <- gls(evi.mag ~ landscape.het, correlation=corExp(form=~east+north, metric='euclidean'),
#                     method = 'ML', data = pasture.div)

## AIC = -24.35502


## Climate only

m.resist.clim <- gls(evi.mag ~ var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
                     method = 'ML', data = pasture.div)
#intervals(m.resist.clim)
## AIC = -35.85954


## Diversity and landcover

#m.resist.divland <- gls(evi.mag ~ nat.fres + landscape.het, correlation=corExp(form=~east+north, metric='euclidean'),
#                        method = 'ML', data = pasture.div)

## AIC = -23.63436


## Diversity and climate

m.resist.divclim <- gls(evi.mag ~ nat.fres + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
                        method = 'ML', data = pasture.div)
#intervals(m.resist.divclim)
## AIC =  -33.96026


## Landcover and climate

m.resist.landclim <- gls(evi.mag ~ landscape.het + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
                         method = 'ML', data = pasture.div)
#intervals(m.resist.landclim)
## AIC = -35.65687



### Recovery time

## Null
#m.rec.null <- gls(log(evi.rec) ~ 1, correlation=corExp(form=~east+north, metric='euclidean'),
#                  method = 'ML', data = pasture.div)

## AIC =  414.2189

## Full
m.rec <- gls(log(evi.rec) ~ nat.fres + landscape.het + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
             method = 'ML', data = pasture.div)
#intervals(m.rec)
## AIC = 386.6934

## Diversity only
#m.rec.div <- gls(log(evi.rec) ~ nat.fres, correlation=corExp(form=~east+north, metric='euclidean'),
#                 method = 'ML', data = pasture.div)

## AIC = 412.8001

## Landcover only

#m.rec.land <- gls(log(evi.rec) ~ landscape.het, correlation=corExp(form=~east+north, metric='euclidean'),
#                  method = 'ML', data = pasture.div)

## AIC = 413.786

## Climate only

#m.rec.clim <- gls(log(evi.rec) ~ var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
#                  method = 'ML', data = pasture.div)
#intervals(m.rec.clim)
## AIC = 387.2638

## Diversity and landcover

#m.rec.divland <- gls(log(evi.rec) ~ nat.fres + landscape.het, correlation=corExp(form=~east+north, metric='euclidean'),
#                     method = 'ML', data = pasture.div)

## AIC = 413.3079

## Diversity and climate

m.rec.divclim <- gls(log(evi.rec) ~ nat.fres + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
                     method = 'ML', data = pasture.div)
#intervals(m.rec.divclim)

## AIC = 385.1263

## Landcover and climate

#m.rec.landclim <- gls(log(evi.rec) ~ landscape.het + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
#                      method = 'ML', data = pasture.div)
#intervals(m.rec.landclim)
## AIC = 387.904


### Recovery rate

## Null model
#m.recrate.null <- gls(evi.recrate ~ 1, correlation=corExp(form=~east+north, metric='euclidean'),
#                      method = 'ML', data = pasture.div)

## AIC = -1639.975

## Full
#m.recrate <- gls(evi.recrate ~ nat.fres + landscape.het + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
#                 method = 'ML', data = pasture.div)
#intervals(m.recrate)
## AIC = -1651.093

## Diversity only

#m.recrate.div <- gls(evi.recrate ~ nat.fres, correlation=corExp(form=~east+north, metric='euclidean'),
#                     method = 'ML', data = pasture.div)

## AIC = -1638.096

## Landcover only 
#m.recrate.land <- gls(evi.recrate ~ landscape.het, correlation=corExp(form=~east+north, metric='euclidean'),
#                      method = 'ML', data = pasture.div)

## AIC =  -1637.987

## Climate only
m.recrate.clim <- gls(evi.recrate ~ var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
                      method = 'ML', data = pasture.div)
intervals(m.recrate.clim)
## AIC = -1653.172

## Diversity and landcover

#m.recrate.divland <- gls(evi.recrate ~ nat.fres + landscape.het, correlation=corExp(form=~east+north, metric='euclidean'),
#                         method = 'ML', data = pasture.div)

## AIC =  -1636.128

## Diversity and climate
m.recrate.divclim <- gls(evi.recrate ~ nat.fres + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
                         method = 'ML', data = pasture.div)
intervals(m.recrate.divclim)
## AIC = -1652.514

## Landcover and climate
m.recrate.landclim <- gls(evi.recrate ~ landscape.het + var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
                          method = 'ML', data = pasture.div)

## AIC = -1651.369

