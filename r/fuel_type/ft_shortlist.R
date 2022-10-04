library(raster)
library(randomForest)
require(caTools)
library(caret)
source('r/function_rf.R')
#####
ft.in.df <- readRDS('cache/ft.met.lai.rds')

fuel.name.lut <- read.csv('fuelName.csv')
fuel.name.lut <- fuel.name.lut[fuel.name.lut$new_nm != '',]
######remove non natual types##########
evc.df <- raster('data/EVC_fuelType/evc/VICSANSW161.tif')

atrribute.df <- evc.df@data@attributes[[1]]
atrribute.df <- atrribute.df[,c('VICSANSW.FUEL_TYPE' , 'VICSANSW.TYPE_NAME')]
nm.vec <- atrribute.df$VICSANSW.TYPE_NAME[atrribute.df$VICSANSW.FUEL_TYPE %in%
                                            unique(ft.in.df$ft)]
ft.nm.df <- data.frame(nm = atrribute.df$VICSANSW.TYPE_NAME[atrribute.df$VICSANSW.FUEL_TYPE %in%
                                                              unique(ft.in.df$ft)],
                       ID = unique(ft.in.df$ft))

ID.natue <- ft.nm.df$ID[!ft.nm.df$nm %in% c('Eaten Out Grass','Orchard / Vineyard',
                                            'Softwood Plantation','Hardwood Plantation',
                                            'Water, sand, no vegetation')]

# subsut
ft.in.df <- ft.in.df[ft.in.df$ft %in% ID.natue,]
ft.in.df <- ft.in.df[ft.in.df$ft %in% fuel.name.lut$ID,]
ft.in.df$ft.new <- NA
ft.in.df$ft.new.num <- NA
for (i in seq_along(fuel.name.lut$ID)) {
  ft.in.df$ft.new[ft.in.df$ft == fuel.name.lut$ID[i]] <- fuel.name.lut$new_nm[i]
  
  # ft.in.df$ft.new.num[ft.in.df$ft == fuel.name.lut$ID[i]] <- i
}
ft.in.df$ft.new <- as.factor(ft.in.df$ft.new)
ft.in.df$ft.new.num <- as.numeric(ft.in.df$ft.new)
table(ft.in.df$ft.new.num)
# do model fitting########
tmp.ft.df <- ft.in.df[!is.na(ft.in.df$ft.new.num),]

tmp.ft.df$fuelType_vicnsw <- factor(tmp.ft.df$ft.new.num)
table(tmp.ft.df$ft.new.num)
saveRDS(tmp.ft.df,'ft.train.evaluation.rds')
# unique(tmp.ft.df$VICSANSW.TYPE_NAME)
rf.fit.ft <- fit.rf.func(dat = tmp.ft.df,
                         y.nm = 'fuelType_vicnsw',
                         x.nm = c('soil.density', 'clay' ,'soil.depth', #soil attributes
                                  'rad.short.jan' ,'rad.short.jul', 'wi' ,'curvature_profile','curvature_plan',#topo
                                  # 'tmax' , 'rain' , 'rh.min', #climate
                                  'tmax.mean','map','pr.seaonality',#long term clim
                                  'lai.opt.mean'
                         ))

# varImpPlot(rf.fit.ft)
# previous.fit <-rf.fit.ft <-  readRDS('cache/rf.fit.fuelType.new.rds')
# unique(previous.fit$predicted)
# x <- previous.fit$confusion
saveRDS(rf.fit.ft,'cache/rf.fit.fuelType.new.short.rds')
