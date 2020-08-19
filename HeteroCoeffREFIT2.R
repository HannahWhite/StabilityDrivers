###########################################################################
### Neat singular graphics for GLS models of stability of              ###
### heterogeneous hectads across across Ireland - corrected refit data ###
##########################################################################

## Hannah White 21/07/2020

## Graphics for all best performing models all hectads across Ireland

library(ggplot2)
library(cowplot)
library(RColorBrewer)
library(viridis)

rm(list=ls())

source('E:\\Postdoc Grassland Resilience\\MODIS R code\\Resilience\\Analyses\\Reanalysis_Method2\\ReanalysisRefitData\\GLS_heteroREFIT2.R')


###################
### Variability ###
###################

## null one of best performing so omit?

var.div.ci <- data.frame(intervals(m.var.div)$coef)
var.div.df <- data.frame(covariate = rownames(var.div.ci), var.div.ci, row.names = NULL)
names(var.div.df)[3] <- 'est'
var.div.df <- var.div.df[-1,]
var.div.df$Model <- rep('div', length(var.div.df$covariate))

var.div.df$covariate <- factor(var.div.df$covariate, as.character(var.div.df$covariate))


var.land.ci <- data.frame(intervals(m.var.land)$coef)
var.land.df <- data.frame(covariate = rownames(var.land.ci), var.land.ci, row.names = NULL)
names(var.land.df)[3] <- 'est'
var.land.df <- var.land.df[-1,]
var.land.df$Model <- rep('land', length(var.land.df$covariate))

var.land.df$covariate <- factor(var.land.df$covariate, as.character(var.land.df$covariate))

var.df <- rbind(var.div.df, var.land.df)

var.df$Model <- factor(var.df$Model, levels = c('div', 'land'))



## Graphic 

p.var <- ggplot() + geom_pointrange(data = var.df, position = position_dodge(width = 0.6), size = 0.7, 
                                    aes(x = covariate, y = est, ymin = lower, ymax = upper, colour = Model))
p.var <- p.var + geom_hline(yintercept = 0) 
p.var <- p.var + scale_x_discrete(breaks = c("nat.fres", "landscape.het",
                                             "var.tg", "var.rr",
                                             "ft.tg", "ft.rr"),
                                  labels = c("SR", "Hetero",
                                             "Var \ntemp", "Var \nprecip",
                                             "Ext \ntemp", "Ext \nprecip"))
p.var <- p.var + scale_colour_manual(values = c('#E6AB02', '#66A61E'))
p.var <- p.var + theme_bw() + theme(axis.text = element_text(size = 18, face = 'bold'), axis.title = element_blank(),
                                    legend.text = element_text(size = 18), legend.title = element_text(size = 18))

##################
### Resistance ###
##################


resist.clim.ci <- data.frame(intervals(m.resist.clim)$coef)
resist.clim.df <- data.frame(covariate = rownames(resist.clim.ci), resist.clim.ci, row.names = NULL)
names(resist.clim.df)[3] <- 'est'
resist.clim.df <- resist.clim.df[-1,]
resist.clim.df$Model <- rep('clim', length(resist.clim.df$covariate))

resist.clim.df$covariate <- factor(resist.clim.df$covariate, as.character(resist.clim.df$covariate))


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


resist.df <- rbind(resist.clim.df, resist.divclim.df, resist.landclim.df)

resist.df$covariate <- factor(resist.df$covariate, levels = c('nat.fres', 'landscape.het', 'var.tg', 'var.rr', 'ft.tg', 'ft.rr'))
resist.df$Model <- factor(resist.df$Model, levels = c('clim', 'divclim', 'landclim'))


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
p.resist <- p.resist + scale_colour_manual(values = c('#1B9E77', '#7570B3', '#E7298A'))
p.resist <- p.resist + theme_bw() + theme(axis.text = element_text(size = 18, face = 'bold'), axis.title = element_blank(),
                                          legend.text = element_text(size = 18), legend.title = element_text(size = 18))


#####################
### Recovery time ###
#####################


rec.clim.ci <- data.frame(intervals(m.rec.clim)$coef)
rec.clim.df <- data.frame(covariate = rownames(rec.clim.ci), rec.clim.ci, row.names = NULL)
names(rec.clim.df)[3] <- 'est'
rec.clim.df <- rec.clim.df[-1,]
rec.clim.df$Model <- rep('clim', length(rec.clim.df$covariate))

rec.clim.df$covariate <- factor(rec.clim.df$covariate, as.character(rec.clim.df$covariate))

rec.divclim.ci <- data.frame(intervals(m.rec.divclim)$coef)
rec.divclim.df <- data.frame(covariate = rownames(rec.divclim.ci), rec.divclim.ci, row.names = NULL)
names(rec.divclim.df)[3] <- 'est'
rec.divclim.df <- rec.divclim.df[-1,]
rec.divclim.df$Model <- rep('divclim', length(rec.divclim.df$covariate))

rec.divclim.df$covariate <- factor(rec.divclim.df$covariate, as.character(rec.divclim.df$covariate))


rec.landclim.ci <- data.frame(intervals(m.rec.landclim)$coef)
rec.landclim.df <- data.frame(covariate = rownames(rec.landclim.ci), rec.landclim.ci, row.names = NULL)
names(rec.landclim.df)[3] <- 'est'
rec.landclim.df <- rec.landclim.df[-1,]
rec.landclim.df$Model <- rep('landclim', length(rec.landclim.df$covariate))

rec.landclim.df$covariate <- factor(rec.landclim.df$covariate, as.character(rec.landclim.df$covariate))

rec.df <- rbind(rec.clim.df, rec.divclim.df, rec.landclim.df)

## need to make sure factor order of covariates matches that in other graphics

rec.df$covariate <- factor(rec.df$covariate, levels = c('nat.fres', 'landscape.het', 'var.tg', 'var.rr', 'ft.tg', 'ft.rr'))

rec.df$Model <- factor(rec.df$Model, levels = c('clim', 'divclim', 'landclim'))



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
p.rec <- p.rec + scale_colour_manual(values = c('#1B9E77', '#7570B3', '#E7298A'))
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
p.recrate <- p.recrate + scale_colour_manual(values = c('#1B9E77', '#7570B3', '#D95F02'))
p.recrate <- p.recrate + theme_bw() + theme(axis.text = element_text(size = 18, face = 'bold'), axis.title = element_blank(),
                                            legend.text = element_text(size = 18), legend.title = element_text(size = 18))





tiff('E:\\Postdoc Grassland Resilience\\Graphics\\Diversity-Resilience\\Paper\\FigsRefit2\\HeteroCoefs.tiff',
     height = 10, width = 14, units = 'in', res = 360, compression = 'lzw')
plot_grid(p.var, p.resist, p.rec, p.recrate, nrow = 2, align = 'hv', labels = c('(a)', '(b)', '(c)', '(d)'), label_x = c(0, -0.028, 0, -0.028))
dev.off()









