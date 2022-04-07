
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
rf.fit.bark.hz <- readRDS('cache/rf.fit.hz.bark.rds')
rf.fit.surface.hz <- readRDS('cache/rf.fit.hz.surface.rds')

varImpPlot(rf.fit.bark.hz)
# 

pred.rf.func <- function(dat,y.nm,fit.in,x.nm = c('soil.density' , 'ph' , 'clay' , #soil attributes
                                                 'rad.short.jan' ,'rad.short.jul', 'wi' ,#topo
                                                 'tmax' , 'rain' , 'vph15', #climate
                                                 'pet','map',#long term clim 
                                                 'lai.opt'),#vegetation,
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
  prediction_for_roc_curve <- predict(rf.fit.canopy.h,ValidSet)
  # model.hieght <- predict(rf.fit.canopy.h,)
  # varImpPlot(model.hieght)
  # summary(TrainSet)
  ValidSet$pred.value <- prediction_for_roc_curve
  
  
  return(ValidSet)
}

# ValidSet[,y.nm]

predic.canopy.h <- pred.rf.func(dat = input.df,y.nm = 'CNPY_TOP_hight_cm')

confusionMatrix(predic.canopy.h$pred.value,predic.canopy.h$CNPY_TOP_hight_cm)

plot(CNPY_TOP_hight_cm~pred.value,data = predic.canopy.h)
abline(a=0,b=1)

summary(lm(CNPY_TOP_hight_cm~pred.value,data = predic.canopy.h))

rmse.func = function(m, o){
  sqrt(mean((m - o)^2,na.rm=T))
}
rmse.func(predic.canopy.h$pred.value,predic.canopy.h$CNPY_TOP_hight_cm)
