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
rf.fit.ns.hz <- readRDS('cache/rf.fit.hz.ns.rds')
rf.fit.elevated.hz <- readRDS('cache/rf.fit.hs.elevated.rds')
rf.fit.elevated.hz$confusion
rf.fit.bark.hz <- readRDS('cache/rf.fit.hz.bark.rds')
rf.fit.bark.hz$confusion
rf.fit.surface.hz <- readRDS('cache/rf.fit.hz.surface.rds')
rf.fit.surface.hz$confusion
varImpPlot(rf.fit.bark.hz)
# 
# plot(rf.fit.bark.hz)
pred.rf.func <- function(dat,y.nm,fit.in, 
                         x.nm = c('soil.density' , 'ph' , 'clay' , #soil attributes
                                  'rad.short.jan' ,'rad.short.jul', 'wi' ,'curvature_profile','curvature_plan',#topo
                                  'tmax' , 'rain' , 'vph15', #climate
                                  'pet','map',#long term clim 
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
