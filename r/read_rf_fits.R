library(randomForest)
require(caTools)
library(caret)
library(raster)
# 
source('r/process_input.R')
# # read fits
rf.fit.ft <- readRDS('cache/rf.fit.fuelType.rds')
# varImpPlot(rf.fit.ft)
rf.fit.canopy.h <- readRDS('cache/rf.fit.canopy.height.rds')
varImpPlot(rf.fit.canopy.h)
rf.fit.ns.h <- readRDS('cache/rf.fit.ns.height.rds')
# varImpPlot(rf.fit.ns.h)
# 
# model.path <- ('cache/rf.fit.hz.bark.rds')
rf.fit.ns.hz <- readRDS('cache/rf.fit.hz.ns.rds')
rf.fit.elevated.hz <- readRDS('cache/rf.fit.hs.elevated.rds')
# rf.fit.elevated.hz$confusion
rf.fit.bark.hz <- readRDS('cache/rf.fit.hz.bark.rds')
# rf.fit.bark.hz$confusion
rf.fit.surface.hz <- readRDS('cache/rf.fit.hz.surface.rds')
# rf.fit.surface.hz$confusion
varImpPlot(rf.fit.surface.hz,type=2)
# # check evaluation####
# plot.evaluation.func <- function(model.nm,var.nm){
#   # 
#   # model.nm <- 'cache/rf.fit.hz.bark.rds'
#   rf.fit.bark.hz <- readRDS(model.nm)
#   predic.hs.bark <- pred.rf.func(dat = input.df,fit.in = rf.fit.bark.hz,y.nm = var.nm)
#   return(predic.hs.bark)
# } 
# evaluation.ft <- plot.evaluation.func(model.nm = 'cache/rf.fit.hz.ns.rds',var.nm = 'fuelType_vicnsw' )

# rf.fit.surface.hz <- readRDS('cache/rf.fit.fuelType.rds')
# # rf.fit.surface.hz$confusion
# varImpPlot(rf.fit.surface.hz,type=2)

# record important drivers#####
get.import.order.func <- function(fit.nm){
  x <- as.data.frame(fit.nm$importance)
  x <- x[order(x[,ncol(x)],decreasing = T),]
  return(row.names(x)[1:5])
}
# fit.nm = rf.fit.ns.h
# get.import.order.func(fit.nm=rf.fit.canopy.h)
# 
import.df <- data.frame(Variable = NA,
                        x1 = NA,x2 = NA,x3=NA,x4 = NA,x5=NA)
import.df[1,] <- c('Fuel type',
                   get.import.order.func(rf.fit.ft))
import.df[2,] <- c('Canopy height',
                   get.import.order.func(rf.fit.canopy.h))
import.df[3,] <- c('Near surface height',
                   get.import.order.func(rf.fit.ns.h))
import.df[4,] <- c('Elevated HS',
                   get.import.order.func(rf.fit.elevated.hz))
import.df[5,] <- c('Near surface HS',
                   get.import.order.func(rf.fit.ns.hz))
import.df[6,] <- c('Bark HS',
                   get.import.order.func(rf.fit.bark.hz))
import.df[7,] <- c('Surface HS',
                   get.import.order.func(rf.fit.surface.hz))

import.df[import.df == 'pr.seaonality'] <- 'Rainfall seaonality'
import.df[import.df == 'map'] <- 'MAP'
import.df[import.df == 'tmax.mean'] <- 'Mean Tmax'
import.df[import.df == 'rh.min'] <- 'Minimum RH'
import.df[import.df == 'tmax'] <- 'Tmax'
import.df[import.df == 'rain'] <- 'Precipitation'
import.df[import.df == 'rad.short.jan'] <- 'Solar radiation (Jan)'
import.df[import.df == 'soil.density'] <- 'Soil bulk density'
import.df[import.df == 'lai.opt'] <- 'Optimal LAI'

write.csv(import.df,'cache/importance.csv',row.names=F)

# # 
# test.df <- input.df[1:100,]
# test.df <- test.df[test.df$elevated_hz==5,]
# 
# test.df$lai.opt <- 10
# test.df$tmax <- 20
# # test.df$map[1] <- 1000
# # test.df$rh[1] <- 40
# # test.df$tmax.mean[1] <- 20
# test.df$rain <- 20
# 
# predict(rf.fit.elevated.hz,test.df)
# range(input.df[1:100,'lai.opt'],na.rm=T)
# plot(rf.fit.bark.hz)


# # ValidSet[,y.nm]
# 
# predic.canopy.h <- pred.rf.func(dat = input.df,fit.in = rf.fit.canopy.h,y.nm = 'CNPY_TOP_hight_cm')
# 
# # confusionMatrix(predic.canopy.h$pred.value,predic.canopy.h$CNPY_TOP_hight_cm)
# 
# plot(CNPY_TOP_hight_cm~exp(pred.value),data = predic.canopy.h)
# abline(a=0,b=1)
# # plot(CNPY_TOP_hight_cm~map,data = predic.canopy.h,ylim=c(0,200))
# summary(lm(CNPY_TOP_hight_cm~exp(pred.value),data = predic.canopy.h))


# rmse.func(exp(predic.canopy.h$pred.value),predic.canopy.h$CNPY_TOP_hight_cm)
# rf.fit.canopy.h <- readRDS('cache/rf.fit.canopy.height.rds')

#function for plot fiting####
pred.rf.func <- function(dat,y.nm,fit.in, 
                         x.nm = c('soil.density'  , 'clay' , #soil attributes
                                  'rad.short.jan' ,'rad.short.jul', 'wi' ,'curvature_profile','curvature_plan',#topo
                                  'tmax' , 'rain' , 'rh.min', #climate
                                  'tmax.mean','map','pr.seaonality',#long term clim 
                                  'lai.opt'),#vegetation,,#vegetation,
                         ...){
  formula.use <- as.formula(paste0(y.nm,'~.'))
  test.df <- dat[,c(y.nm,#target
                    x.nm)]#vegetation
  
  
  # test.df <- test.df[complete.cases(test.df),]
  test.df <- test.df#[1000:5000,]
  set.seed(1935)
  train <- sample(nrow(test.df), 0.7*nrow(test.df), replace = FALSE)
  # TrainSet <- test.df[train,]
  ValidSet <- test.df[-train,]
  # 
  prediction_for_roc_curve <- predict(fit.in,ValidSet)
  # model.hieght <- predict(rf.fit.canopy.h,)
  # varImpPlot(model.hieght)
  # summary(TrainSet)
  ValidSet$pred.value <- prediction_for_roc_curve
  
  
  return(ValidSet)
}
# 
rmse.func = function(m, o){
  sqrt(mean((m - o)^2,na.rm=T))
}
plot.h.func <- function(model.path,var.nm){
  # var.nm <- 'NS_TOP_height_cm'
  # 2.a#
  # model.path <- 'cache/rf.fit.ns.height.rds'
  rf.fit.ns.h <- readRDS(model.path)
  train.c.h.pred <- rf.fit.ns.h$predicted
  train.c.h.obs <- rf.fit.ns.h$y
  # 
  plot(exp(train.c.h.obs)~exp(train.c.h.pred),ylim=c(0,200),
       pch=16,col='grey',
       xlab='Modelled height',ylab='Observed height',cex=0.5)
  abline(a=0,b=1)
  # plot(NS_TOP_height_cm~tmax,data = predic.ns.h,ylim=c(0,200))
  fit.lm <- (lm(exp(train.c.h.obs)~exp(train.c.h.pred)))
  mylabel = bquote(italic(R)^2 == .(format(summary(fit.lm)$r.squared, digits = 3)))
  abline(fit.lm,col='darkgrey',lty='dashed',lwd=3)
  # 
  legend('bottomright',legend = c(as.expression(mylabel),
                                  paste0('RSME = ',
                                         format(rmse.func(exp(train.c.h.pred),  
                                                          exp(train.c.h.obs)), 
                                                digits = 3))),
         bty='n')
  
  legend('topleft',legend = paste0('(a) training'),bty='n')
  # 2.b#
  predic.ns.h <- pred.rf.func(dat = input.df,fit.in = rf.fit.ns.h,y.nm = var.nm)
  # 
  plot(predic.ns.h[,var.nm]~exp(predic.ns.h$pred.value),ylim=c(0,200),
       pch=16,col='grey',
       xlab='Modelled height',ylab='Observed height',cex=0.5)
  abline(a=0,b=1)
  # plot(NS_TOP_height_cm~tmax,data = predic.ns.h,ylim=c(0,200))
  fit.lm <- (lm(predic.ns.h[,var.nm]~exp(predic.ns.h$pred.value),data = predic.ns.h))
  mylabel = bquote(italic(R)^2 == .(format(summary(fit.lm)$r.squared, digits = 3)))
  abline(fit.lm,col='darkgrey',lty='dashed',lwd=3)
  # 
  legend('bottomright',legend = c(as.expression(mylabel),
                                  paste0('RSME = ',
                                         format(rmse.func(exp(predic.ns.h$pred.value),  
                                                          predic.ns.h[,var.nm]), 
                                                digits = 3))),
         bty='n')
  
  legend('topleft',legend = paste0('(b) evaluation'),bty='n')
}
#plot fiting####
# 1.
png(filename = 'figures/c_matric/h_c.png',width = 600,height = 500*2)
par(mar=c(5,5,1,1),mfrow = c(2,1))
plot.h.func(model.path = 'cache/rf.fit.canopy.height.rds',
            var.nm = 'CNPY_TOP_hight_cm')
dev.off()
# 2.
png(filename = 'figures/c_matric/h_ns.png',width = 600,height = 500*2)
par(mar=c(5,5,1,1),mfrow = c(2,1))
plot.h.func(model.path = 'cache/rf.fit.ns.height.rds',
            var.nm = 'NS_TOP_height_cm')
dev.off()
# plot(CNPY_TOP_hight_cm~wi,data = input.df,ylim=c(0,40))

# hist(log(input.df$CNPY_TOP_hight_cm))

# plot ####
plot.confuMtraix.func <- function(model.nm,var.nm,var.plot.name){
  # 
  # model.nm <- 'cache/rf.fit.hz.bark.rds'
  rf.fit.bark.hz <- readRDS(model.nm)
  predic.hs.bark <- pred.rf.func(dat = input.df,fit.in = rf.fit.bark.hz,y.nm = var.nm)
  # predic.hs.bark$bark_hz <- as.numeric(as.character(predic.hs.bark$bark_hz))
  # predic.hs.bark$pred.value <- as.numeric(as.character(predic.hs.bark$pred.value))
  # plot(bark_hz~pred.value,data = predic.hs.bark,
  #      pch=16,col='grey')
  # pred3 = performance(predic.hs.bark, "tpr","fpr")
  confu.m.bark <- confusionMatrix(predic.hs.bark$pred.value, predic.hs.bark[,var.nm])
  # ac.num.overall <- unname(confu.m.bark$overall[1])
  
  confu.m.bark.table <- matrix(confu.m.bark$table,nrow = ncol(confu.m.bark$table))
  
  confu.m.bark.table <- confu.m.bark.table / rowSums(confu.m.bark.table)
  
  # plot
  par(mfrow = c(2,1),mar=c(2,4,2,4))
  brk.vec <- seq(0,1,by=0.2)
  # palette(c(col.df$beauty[c(4,3,2,1,5)],
  #           col.df))
  col.func <- colorRampPalette(c(col.df$beauty[5],col.df$cityNight[3]))
  # plot training
  num.vec <- as.numeric(colnames(confu.m.bark$table))
  confu.m.bark.train <- rf.fit.bark.hz$confusion[,1:length(num.vec)]
  confu.m.bark.train <- confu.m.bark.train / rowSums(confu.m.bark.train)
  plot(flip(raster(confu.m.bark.train,
                   xmn=min(num.vec), xmx=max(num.vec), 
                   ymn=min(num.vec), ymx=max(num.vec)),),
      breaks = brk.vec,col = col.func(5),
       interpolate=FALSE,xaxs = 'i',yaxs = 'i',axes=F,asp=FALSE)
  
  axis(side = 1,at = num.vec*length(num.vec) / (length(num.vec)+1)+0.3,labels = num.vec)
  axis(side = 2,at = num.vec*length(num.vec) / (length(num.vec)+1)+0.3,labels = num.vec)
  
  title(sprintf('Training - %s harzard Score',var.plot.name))
  # plot evaluation
  plot(flip(raster(confu.m.bark.table,
                   xmn=0, xmx=5, ymn=0, ymx=5)),
       xlim=c(0,5),ylim=c(0,5),breaks = brk.vec,col = col.func(5),
       interpolate=FALSE,xaxs = 'i',yaxs = 'i',axes=F,asp=FALSE)
  
  axis(side = 1,at = (0:5)*6/7+0.3,labels = 0:5)
  axis(side = 2,at = (0:5)*6/7+0.3,labels = 0:5)
  
  title(sprintf('Evaluation - %s harzard Score',var.plot.name))
}
# 
# 3.
png(filename = 'figures/c_matric/elevated.png',width = 600,height = 500*2)
plot.confuMtraix.func(model.nm = 'cache/rf.fit.hs.elevated.rds',
                      var.nm = 'elevated_hz',
                      var.plot.name = 'elevated')
dev.off()
# 4.
png(filename = 'figures/c_matric/ns.png',width = 600,height = 500*2)
plot.confuMtraix.func(model.nm = 'cache/rf.fit.hz.ns.rds',
                      var.nm = 'nearsurface_hz',
                      var.plot.name = 'near surface')
dev.off()
# 5.
png(filename = 'figures/c_matric/bark.png',width = 600,height = 500*2)
plot.confuMtraix.func(model.nm = 'cache/rf.fit.hz.bark.rds',
                      var.nm = 'bark_hz',
                      var.plot.name = 'bark')
dev.off()

# 6.
png(filename = 'figures/c_matric/surface.png',width = 600,height = 500*2)
plot.confuMtraix.func(model.nm = 'cache/rf.fit.hz.surface.rds',
                      var.nm = 'surface_hz',
                      var.plot.name = 'surface')
dev.off()
