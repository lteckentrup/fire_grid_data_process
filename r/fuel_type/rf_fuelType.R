# library(raster)
# evc.df <- raster('data/EVC_fuelType/evc/VICSANSW161.tif')
# plot(evc.df,breaks=c(0,2000,2001:2070,3001))
# atrribute.df <- evc.df@data@attributes[[1]]
# atrribute.df <- atrribute.df[,c('VICSANSW.FUEL_TYPE' , 'VICSANSW.TYPE_NAME')]
# plot(evc.df)
# sub.ra <- evc.df[evc.df==atrribute.df$VICSANSW.FUEL_TYPE[1]]
# write.csv(atrribute.df,'fuelType.csv')
# # 
# set.seed(1935)
# sub.df <- sample(1:length(evc.df), 100000, replace = FALSE)
# train <- sample(1:length(evc.df), 0.7*length(evc.df), replace = FALSE)
# TrainSet <- test.df[train,]
# # ValidSet <- test.df[-train,]
# 
# names(evc.df@data@attributes[[1]])

library(randomForest)
require(caTools)
library(caret)
source('r/function_rf.R')
#####
ft.in.df <- readRDS('cache/ft.met.lai.rds')
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

# do model fitting########
tmp.ft.df <- ft.in.df[!is.na(ft.in.df$ft),]

tmp.ft.df$fuelType_vicnsw <- factor(tmp.ft.df$ft)
tmp.ft.df <- tmp.ft.df[complete.cases(tmp.ft.df),]

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
saveRDS(rf.fit.ft,'cache/rf.fit.fuelType.new.rds')
# varImpPlot(previous.fit)
# evaluate#####
# #####
pred.rf.all.func <- function(dat,y.nm,fit.in, 
                             x.nm = c('soil.density'  , 'clay' , #soil attributes
                                      'rad.short.jan' ,'rad.short.jul', 'wi' ,'curvature_profile','curvature_plan',#topo
                                      'tmax' , 'rain' , 'rh.min', #climate
                                      'tmax.mean','map','pr.seaonality',#long term clim 
                                      'lai.opt.mean'),#vegetation,,#vegetation,
                             ...){
  formula.use <- as.formula(paste0(y.nm,'~.'))
  test.df <- dat[,c(y.nm,#target
                    x.nm)]
  # clean data
  test.df <- test.df[test.df[,y.nm] != '0',]
  test.df[,y.nm] <- as.factor(as.character(test.df[,y.nm]))
  test.df <- test.df[complete.cases(test.df),]
  # ##separate training and validting
  set.seed(1935)
  train <- sample(nrow(test.df), 0.7*nrow(test.df), replace = FALSE)
  ValidSet <- test.df[-train,]
  # 
  prediction_for_roc_curve <- predict(fit.in,ValidSet)
  # model.hieght <- predict(rf.fit.canopy.h,)
  # varImpPlot(model.hieght)
  # summary(TrainSet)
  ValidSet$pred.value <- prediction_for_roc_curve
  
  
  return(ValidSet)
}

out.stat.df <- data.frame(Strata = c('Elevated','Near-sruface','Surface'),
                          Accuracy = NA,
                          No_information_rate = NA,
                          Kappa = NA)

#######
predic.test <- pred.rf.all.func(dat = tmp.ft.df,
                                fit.in = rf.fit.ft,
                                y.nm = 'fuelType_vicnsw',
                                x.nm = c('soil.density'  , 'clay' , #soil attributes
                                         'rad.short.jan' ,'rad.short.jul', 'wi' ,'curvature_profile','curvature_plan',#topo
                                         # 'tmax' , 'rain' , 'rh.min', #climate
                                         'tmax.mean','map','pr.seaonality',#long term clim 
                                         'lai.opt.mean'))
confu.m.test <- confusionMatrix(predic.test$pred.value, 
                                predic.test[,'fuelType_vicnsw'])

# x.evlau <- as.data.frame(confu.m.test$table)

