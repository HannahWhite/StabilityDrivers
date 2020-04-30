#####################################################
### SEMs with historical and contemporary climate ###
#####################################################

### Hannah White 7.10.2019

library(nlme)
library(piecewiseSEM)


#####
# read in data


## Read in data and extract what is required

vasc.env <- read.csv('J:/Postdoc Grassland Resilience/Species richness/vasc.env.csv', header = TRUE)

vasc.env <- vasc.env[,c(1:2, 43:46)] # extracts diversity estimates


# stability metrics

stability10km <- read.csv('J:\\Postdoc Grassland Resilience\\MODIS6\\Resilience\\stability10km2sd.csv', header = TRUE)

stability10km$evi.rec <- stability10km$evi.rec*8
stability10km$evi.recrate <- stability10km$evi.recrate*8
stability10km$evi.long <- stability10km$evi.long*8
stability10km$evi.slow <- stability10km$evi.slow*8

# extract metrics required for modelling

stability.red <- stability10km[, c(1:4, 6:7, 13:14)]

# merge and get stability just down to 830 squares

stability.env <- merge(vasc.env, stability.red, by.x = c('east', 'north'), by.y = c('eastings', 'northings'), all.x = TRUE, all.y = FALSE)

## climate
load('J:\\Postdoc Grassland Resilience\\Climate Data\\eobs99_processed0.1deg.RData')

load("J:\\Postdoc Grassland Resilience\\Climate Data\\eobs_contemporary_processed0.1deg.RData")
names(climate.contemp)[3:10] <- c('mean.rr.con', 'mean.tg.con', 'var.rr.con', 'var.tg.con',
                                'sd.rr.con', 'sd.tg.con', 'ft.rr.con', 'ft.tg.con')



stability.env <- merge(stability.env, climate, by = c('east', 'north'), all = TRUE)
stability.env <- merge(stability.env, climate.contemp, by = c('east', 'north'), all = TRUE)

## add in landscape heterogeneity data

corine.agg <- read.csv('J:\\Postdoc Grassland Resilience\\LandCoverData\\corine.agg.csv', header = TRUE)
corine.agg$east <- corine.agg$east + 5000
corine.agg$north <- corine.agg$north + 5000

stability.env <- merge(stability.env, corine.agg, by = c('east', 'north'), all = TRUE)

### Scale and centre covariates


stability.env[, c(3:6, 13:28, 46:48)] <- scale(stability.env[, c(3:6, 13:28, 46:48)]) 


#### SEM models

### variability


sem.var.climate <- psem(gls(var.rr.con ~ var.rr, correlation=corExp(form=~east+north, metric='euclidean'), method = 'ML', data = stability.env),
                        gls(var.tg.con ~ var.tg, correlation = corExp(form=~east+north, metric='euclidean'), method = 'ML', data = stability.env),
                        gls(ft.rr.con ~ ft.rr, correlation = corExp(form=~east+north, metric='euclidean'), method = 'ML', data = stability.env),
                        gls(ft.tg.con ~ ft.tg, correlation = corExp(form=~east+north, metric='euclidean'), method = 'ML', data = stability.env),
                        gls(log(evi.var) ~ var.rr + var.tg + ft.rr + ft.tg + var.rr.con + var.tg.con + ft.rr.con + ft.tg.con, 
                            correlation = corExp(form=~east+north, metric='euclidean'), method = 'ML', data = stability.env))


coefs(sem.var.climate)


model.list <- list(gls(var.rr.con ~ var.rr, correlation=corExp(form=~east+north, metric='euclidean'), method = 'ML', data = stability.env),
                   gls(var.tg.con ~ var.tg, correlation = corExp(form=~east+north, metric='euclidean'), method = 'ML', data = stability.env),
                   gls(ft.rr.con ~ ft.rr, correlation = corExp(form=~east+north, metric='euclidean'), method = 'ML', data = stability.env),
                   gls(ft.tg.con ~ ft.tg, correlation = corExp(form=~east+north, metric='euclidean'), method = 'ML', data = stability.env),
                   gls(log(evi.var) ~ var.rr + var.tg + ft.rr + ft.tg + var.rr.con + var.tg.con + ft.rr.con + ft.tg.con, 
                       correlation = corExp(form=~east+north, metric='euclidean'), method = 'ML', data = stability.env))


#sem.plot(model.list) # function no longer exists in new version of piecewiseSEM

library(semPlot)

semPaths(sem.var.climate) # can't do this as not a sem object

semPlotModel(model.list)
semPlotModel(sem.var.climate)


### resistance 

sem.res.climate <- psem(gls(var.rr.con ~ var.rr, correlation=corExp(form=~east+north, metric='euclidean'), method = 'ML', data = stability.env),
                        gls(var.tg.con ~ var.tg, correlation = corExp(form=~east+north, metric='euclidean'), method = 'ML', data = stability.env),
                        gls(ft.rr.con ~ ft.rr, correlation = corExp(form=~east+north, metric='euclidean'), method = 'ML', data = stability.env),
                        gls(ft.tg.con ~ ft.tg, correlation = corExp(form=~east+north, metric='euclidean'), method = 'ML', data = stability.env),
                        gls(evi.mag ~ var.rr + var.tg + ft.rr + ft.tg + var.rr.con + var.tg.con + ft.rr.con + ft.tg.con, 
                            correlation = corExp(form=~east+north, metric='euclidean'), method = 'ML', data = stability.env))

coefs(sem.res.climate)


### recovery time
sem.rec.climate <-  psem(gls(var.rr.con ~ var.rr, correlation=corExp(form=~east+north, metric='euclidean'), method = 'ML', data = stability.env),
                         gls(var.tg.con ~ var.tg, correlation = corExp(form=~east+north, metric='euclidean'), method = 'ML', data = stability.env),
                         gls(ft.rr.con ~ ft.rr, correlation = corExp(form=~east+north, metric='euclidean'), method = 'ML', data = stability.env),
                         gls(ft.tg.con ~ ft.tg, correlation = corExp(form=~east+north, metric='euclidean'), method = 'ML', data = stability.env),
                         gls(log(evi.rec) ~ var.rr + var.tg + ft.rr + ft.tg + var.rr.con + var.tg.con + ft.rr.con + ft.tg.con, 
                             correlation = corExp(form=~east+north, metric='euclidean'), method = 'ML', data = stability.env))

coefs(sem.rec.climate)

### recovery rate

sem.recrate.climate <-  psem(gls(var.rr.con ~ var.rr, correlation=corExp(form=~east+north, metric='euclidean'), method = 'ML', data = stability.env),
                         gls(var.tg.con ~ var.tg, correlation = corExp(form=~east+north, metric='euclidean'), method = 'ML', data = stability.env),
                         gls(ft.rr.con ~ ft.rr, correlation = corExp(form=~east+north, metric='euclidean'), method = 'ML', data = stability.env),
                         gls(ft.tg.con ~ ft.tg, correlation = corExp(form=~east+north, metric='euclidean'), method = 'ML', data = stability.env),
                         gls(evi.recrate ~ var.rr + var.tg + ft.rr + ft.tg + var.rr.con + var.tg.con + ft.rr.con + ft.tg.con, 
                             correlation = corExp(form=~east+north, metric='euclidean'), method = 'ML', data = stability.env))

coefs(sem.recrate.climate)


