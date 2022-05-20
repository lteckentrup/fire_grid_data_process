library(randomForest)
require(caTools)
library(caret)
# 
source('r/process_input.R')
# # read fits
rf.fit.canopy.h <- readRDS('cache/rf.fit.canopy.height.rds')
varImpPlot(rf.fit.canopy.h)


rf.fit.ns.h <- readRDS('cache/rf.fit.ns.height.rds')
varImpPlot(rf.fit.ns.h)
# 
# model.path <- ('cache/rf.fit.hz.bark.rds')
rf.fit.ns.hz <- readRDS('cache/rf.fit.hz.ns.rds')
rf.fit.elevated.hz <- readRDS('cache/rf.fit.hs.elevated.rds')
rf.fit.elevated.hz$confusion
rf.fit.bark.hz <- readRDS('cache/rf.fit.hz.bark.rds')
rf.fit.bark.hz$confusion
rf.fit.surface.hz <- readRDS('cache/rf.fit.hz.surface.rds')
rf.fit.surface.hz$confusion
varImpPlot(rf.fit.ns.hz,type = 2)
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
pred.rf.func <- function(dat,y.nm,fit.in, 
                         x.nm = c('soil.density' , 'ph' , 'clay' , #soil attributes
                                  'rad.short.jan' ,'rad.short.jul', 'wi' ,'curvature_profile','curvature_plan',#topo
                                  'tmax' , 'rain' , 'rh.min', #climate
                                  'tmax.mean','map',#long term clim 
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

# ValidSet[,y.nm]

predic.canopy.h <- pred.rf.func(dat = input.df,fit.in = rf.fit.canopy.h,y.nm = 'CNPY_TOP_hight_cm')

# confusionMatrix(predic.canopy.h$pred.value,predic.canopy.h$CNPY_TOP_hight_cm)

plot(CNPY_TOP_hight_cm~exp(pred.value),data = predic.canopy.h)
abline(a=0,b=1)
# plot(CNPY_TOP_hight_cm~map,data = predic.canopy.h,ylim=c(0,200))
summary(lm(CNPY_TOP_hight_cm~exp(pred.value),data = predic.canopy.h))

rmse.func = function(m, o){
  sqrt(mean((m - o)^2,na.rm=T))
}
rmse.func(exp(predic.canopy.h$pred.value),predic.canopy.h$CNPY_TOP_hight_cm)


# 
predic.ns.h <- pred.rf.func(dat = input.df,fit.in = rf.fit.ns.h,y.nm = 'NS_TOP_height_cm')

# confusionMatrix(predic.canopy.h$pred.value,predic.canopy.h$CNPY_TOP_hight_cm)

plot(NS_TOP_height_cm~exp(pred.value),data = predic.ns.h,ylim=c(0,200))
abline(a=0,b=1)
# plot(NS_TOP_height_cm~tmax,data = predic.ns.h,ylim=c(0,200))
summary(lm(NS_TOP_height_cm~exp(pred.value),data = predic.ns.h))

rmse.func(exp(predic.ns.h$pred.value),predic.ns.h$NS_TOP_height_cm)
# 
plot(CNPY_TOP_hight_cm~wi,data = input.df,ylim=c(0,40))

hist(log(input.df$CNPY_TOP_hight_cm))

# plot bark####
# 
predic.hs.bark <- pred.rf.func(dat = input.df,fit.in = rf.fit.bark.hz,y.nm = 'bark_hz')
predic.hs.bark$bark_hz <- as.numeric(as.character(predic.hs.bark$bark_hz))
# predic.hs.bark$pred.value <- as.numeric(as.character(predic.hs.bark$pred.value))
# plot(bark_hz~pred.value,data = predic.hs.bark,
#      pch=16,col='grey')
# pred3 = performance(predic.hs.bark, "tpr","fpr")
confu.m.bark <- confusionMatrix(predic.hs.bark$pred.value, predic.hs.bark$bark_hz)
ac.num.overall <- unname(confu.m.bark$overall[1])

confu.m.bark.table <- matrix(confu.m.bark$table,nrow = 6)

confu.m.bark.table <- confu.m.bark.table / colSums(confu.m.bark.table)

library(raster)
par(mar=c(3,3,3,3))
plot(raster(confu.m.bark.table,
            xmn=0, xmx=5, ymn=0, ymx=5))
axis(side = 1,at = (0:5)+0.1,labels = 0:5)
axis(side = 2,at = (0:5)+0.1,labels = 0:5)
