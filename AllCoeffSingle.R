############################################################################################
### Neat singular graphics for GLS models of stability all hectads across across Ireland ###
############################################################################################

## Hannah White 3.10.2019

## Graphics for all best performing models all hectads across Ireland

library(ggplot2)
library(cowplot)
library(RColorBrewer)
library(viridis)


source('E:\\Postdoc Grassland Resilience\\MODIS R code\\Resilience\\Analyses\\Reanalysis_Method2\\GLS_all_stability2sd0_1deg.R')


###################
### Variability ###
###################

## combine coefficient estimates into single dataframe

var.full.ci <- data.frame(intervals(m.var)$coef)
var.full.df <- data.frame(covariate = rownames(var.full.ci), var.full.ci, row.names = NULL)
names(var.full.df)[3] <- 'est'
var.full.df <- var.full.df[-1,]
var.full.df$Model <- rep('full', length(var.full.df$covariate))

var.full.df$covariate <- factor(var.full.df$covariate, as.character(var.full.df$covariate))


var.divclim.ci <- data.frame(intervals(m.var.divclim)$coef)
var.divclim.df <- data.frame(covariate = rownames(var.divclim.ci), var.divclim.ci, row.names = NULL)
names(var.divclim.df)[3] <- 'est'
var.divclim.df <- var.divclim.df[-1,]
var.divclim.df$Model <- rep('divclim', length(var.divclim.df$covariate))

var.divclim.df$covariate <- factor(var.divclim.df$covariate, as.character(var.divclim.df$covariate))


var.df <- rbind(var.full.df, var.divclim.df)

## need to make sure factor order of covariates matches that in other graphics

var.df$covariate <- factor(var.df$covariate, levels = c('nat.fres', 'landscape.het', 'var.tg', 'var.rr', 'ft.tg', 'ft.rr'))

var.df$Model <- factor(var.df$Model, levels = c('divclim', 'full'))



## Graphic

p.var <- ggplot() + geom_pointrange(data = var.df, position = position_dodge(width = 0.6), size = 0.7, 
                                    aes(x = covariate, y = est, ymin = lower, ymax = upper, group = Model, colour = Model))
p.var <- p.var + geom_hline(yintercept = 0) 
p.var <- p.var + scale_x_discrete(breaks = c("nat.fres", "landscape.het",
                                             "var.tg", "var.rr",
                                             "ft.tg", "ft.rr"),
                                  labels = c("SR", "Hetero",
                                             "Var \ntemp", "Var \nprecip",
                                             "Ext \ntemp", "Ext \nprecip"))
p.var <- p.var + scale_colour_manual(values = c('#2D708EFF', '#B8DE29FF'))
p.var <- p.var + theme_bw() + theme(axis.text = element_text(size = 18, face = 'bold'), axis.title = element_blank(),
                                    legend.text = element_text(size = 18), legend.title = element_text(size = 18))


##################
### Resistance ###
##################

resist.full.ci <- data.frame(intervals(m.resist)$coef)
resist.full.df <- data.frame(covariate = rownames(resist.full.ci), resist.full.ci, row.names = NULL)
names(resist.full.df)[3] <- 'est'
resist.full.df <- resist.full.df[-1,]
resist.full.df$Model <- rep('full', length(resist.full.df$covariate))

resist.full.df$covariate <- factor(resist.full.df$covariate, as.character(resist.full.df$covariate))


#resist.clim.ci <- data.frame(intervals(m.resist.clim)$coef)
#resist.clim.df <- data.frame(covariate = rownames(resist.clim.ci), resist.clim.ci, row.names = NULL)
#names(resist.clim.df)[3] <- 'est'  
#resist.clim.df <- resist.clim.df[-1,]
#resist.clim.df$Model <- rep('climate', length(resist.clim.df$covariate))

#resist.clim.df$covariate <- factor(resist.clim.df$covariate, as.character(resist.clim.df$covariate))


resist.divclim.ci <- data.frame(intervals(m.resist.divclim)$coef)
resist.divclim.df <- data.frame(covariate = rownames(resist.divclim.ci), resist.divclim.ci, row.names = NULL)
names(resist.divclim.df)[3] <- 'est'
resist.divclim.df <- resist.divclim.df[-1,]
resist.divclim.df$Model <- rep('divclim', length(resist.divclim.df$covariate))

resist.divclim.df$covariate <- factor(resist.divclim.df$covariate, as.character(resist.divclim.df$covariate))


resist.landclim.ci <- data.frame(intervals(m.resist.landclim)$coef)
resist.landclim.df <- data.frame(covariate = rownames(resist.landclim.ci), resist.landclim.ci, row.names = NULL)
names(resist.landclim.df)[3] <- 'est'
resist.landclim.df <- resist.landclim.df[-1,]
resist.landclim.df$Model <- rep('landclim', length(resist.landclim.df$covariate))

resist.landclim.df$covariate <- factor(resist.landclim.df$covariate, as.character(resist.landclim.df$covariate))


resist.df <- rbind(resist.full.df, #resist.clim.df, 
                   resist.divclim.df, resist.landclim.df)

resist.df$Model <- factor(resist.df$Model, levels = c('divclim', 'landclim', 'full'))


## Graphic 

p.resist <- ggplot() + geom_pointrange(data = resist.df, position = position_dodge(width = 0.6), size = 0.7, 
                                       aes(x = covariate, y = est, ymin = lower, ymax = upper, group = Model, colour = Model))
p.resist <- p.resist + geom_hline(yintercept = 0) 
p.resist <- p.resist + scale_x_discrete(breaks = c("nat.fres", "landscape.het",
                                                   "var.tg", "var.rr",
                                                   "ft.tg", "ft.rr"),
                                        labels = c("SR", "Hetero",
                                                   "Var \ntemp", "Var \nprecip",
                                                   "Ext \ntemp", "Ext \nprecip"))
p.resist <- p.resist + scale_colour_manual(values = c('#2D708EFF', '#3CBB75FF', '#B8DE29FF'))
p.resist <- p.resist + theme_bw() + theme(axis.text = element_text(size = 18, face = 'bold'), axis.title = element_blank(),
                                          legend.text = element_text(size = 18), legend.title = element_text(size = 18))



#####################
### Recovery time ###
#####################


rec.full.ci <- data.frame(intervals(m.rec)$coef)
rec.full.df <- data.frame(covariate = rownames(rec.full.ci), rec.full.ci, row.names = NULL)
names(rec.full.df)[3] <- 'est'
rec.full.df <- rec.full.df[-1,]
rec.full.df$Model <- rep('full', length(rec.full.df$covariate))

rec.full.df$covariate <- factor(rec.full.df$covariate, as.character(rec.full.df$covariate))


rec.divclim.ci <- data.frame(intervals(m.rec.divclim)$coef)
rec.divclim.df <- data.frame(covariate = rownames(rec.divclim.ci), rec.divclim.ci, row.names = NULL)
names(rec.divclim.df)[3] <- 'est'
rec.divclim.df <- rec.divclim.df[-1,]
rec.divclim.df$Model <- rep('divclim', length(rec.divclim.df$covariate))

rec.divclim.df$covariate <- factor(rec.divclim.df$covariate, as.character(rec.divclim.df$covariate))


rec.df <- rbind(rec.full.df, rec.divclim.df)

## need to make sure factor order of covariates matches that in other graphics

rec.df$covariate <- factor(rec.df$covariate, levels = c('nat.fres', 'landscape.het', 'var.tg', 'var.rr', 'ft.tg', 'ft.rr'))

rec.df$Model <- factor(rec.df$Model, levels = c('divclim', 'full'))



## Graphic

p.rec <- ggplot() + geom_pointrange(data = rec.df, position = position_dodge(width = 0.6), size = 0.7, 
                                    aes(x = covariate, y = est, ymin = lower, ymax = upper, group = Model, colour = Model))
p.rec <- p.rec + geom_hline(yintercept = 0) 
p.rec <- p.rec + scale_x_discrete(breaks = c("nat.fres", "landscape.het",
                                             "var.tg", "var.rr",
                                             "ft.tg", "ft.rr"),
                                  labels = c("SR", "Hetero",
                                             "Var \ntemp", "Var \nprecip",
                                             "Ext \ntemp", "Ext \nprecip"))
p.rec <- p.rec + scale_colour_manual(values = c('#2D708EFF', '#B8DE29FF'))
p.rec <- p.rec + theme_bw() + theme(axis.text = element_text(size = 18, face = 'bold'), axis.title = element_blank(),
                                    legend.text = element_text(size = 18), legend.title = element_text(size = 18))


#####################
### Recovery rate ###
#####################

recrate.full.ci <- data.frame(intervals(m.recrate)$coef)
recrate.full.df <- data.frame(covariate = rownames(recrate.full.ci), recrate.full.ci, row.names = NULL)
names(recrate.full.df)[3] <- 'est'
recrate.full.df <- recrate.full.df[-1,]
recrate.full.df$Model <- rep('full', length(recrate.full.df$covariate))

recrate.full.df$covariate <- factor(recrate.full.df$covariate, as.character(recrate.full.df$covariate))


recrate.clim.ci <- data.frame(intervals(m.recrate.clim)$coef)
recrate.clim.df <- data.frame(covariate = rownames(recrate.clim.ci), recrate.clim.ci, row.names = NULL)
names(recrate.clim.df)[3] <- 'est'
recrate.clim.df <- recrate.clim.df[-1,]
recrate.clim.df$Model <- rep('climate', length(recrate.clim.df$covariate))

recrate.clim.df$covariate <- factor(recrate.clim.df$covariate, as.character(recrate.clim.df$covariate))


recrate.divclim.ci <- data.frame(intervals(m.recrate.divclim)$coef)
recrate.divclim.df <- data.frame(covariate = rownames(recrate.divclim.ci), recrate.divclim.ci, row.names = NULL)
names(recrate.divclim.df)[3] <- 'est'
recrate.divclim.df <- recrate.divclim.df[-1,]
recrate.divclim.df$Model <- rep('divclim', length(recrate.divclim.df$covariate))

recrate.divclim.df$covariate <- factor(recrate.divclim.df$covariate, as.character(recrate.divclim.df$covariate))


recrate.df <- rbind(recrate.full.df, recrate.clim.df, recrate.divclim.df)

recrate.df$covariate <- factor(recrate.df$covariate, levels = c('nat.fres', 'landscape.het', 'var.tg', 'var.rr', 'ft.tg', 'ft.rr'))

recrate.df$Model <- factor(recrate.df$Model, levels = c('climate', 'divclim', 'full'))


## Graphic 

p.recrate <- ggplot() + geom_pointrange(data = recrate.df, position = position_dodge(width = 0.6), size = 0.7, 
                                        aes(x = covariate, y = est, ymin = lower, ymax = upper, group = Model, colour = Model))
p.recrate <- p.recrate + geom_hline(yintercept = 0) 
p.recrate <- p.recrate + scale_x_discrete(breaks = c("nat.fres", "landscape.het",
                                                     "var.tg", "var.rr",
                                                     "ft.tg", "ft.rr"),
                                          labels = c("SR", "Hetero",
                                                     "Var \ntemp", "Var \nprecip",
                                                     "Ext \ntemp", "Ext \nprecip"))
p.recrate <- p.recrate + scale_colour_manual(values = c('#481567FF', '#2D708EFF', '#B8DE29FF'))
p.recrate <- p.recrate + theme_bw() + theme(axis.text = element_text(size = 18, face = 'bold'), axis.title = element_blank(),
                                            legend.text = element_text(size = 18), legend.title = element_text(size = 18))

tiff('E:\\Postdoc Grassland Resilience\\Graphics\\Diversity-Resilience\\Paper\\AllCoefsCol2.tiff',
     height = 10, width = 14, units = 'in', res = 360, compression = 'lzw')
plot_grid(p.var, p.resist, p.rec, p.recrate, nrow = 2, align = 'hv', labels = c('(a)', '(b)', '(c)', '(d)'), label_x = c(0, -0.027, 0, -0.027))
dev.off()












