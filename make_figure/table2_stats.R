rf.fit.ns.hz <- readRDS('cache/rf.fit.hz.ns.rds')
rf.fit.hz.elevated <- readRDS('cache/rf.fit.hs.elevated.rds')
# rf.fit.bark.hz$confusion
rf.fit.surface.hz <- readRDS('cache/rf.fit.hz.surface.rds')


library(randomForest)
require(caTools)
library(caret)

source('r/process_input.R')
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
predic.test <- pred.rf.all.func(dat = input.df,
                                fit.in = rf.fit.hz.elevated,
                                y.nm = 'elevated_hz')

confu.m.test <- confusionMatrix(predic.test$pred.value, 
                                predic.test[,'elevated_hz'])
overall.stat <- (confu.m.test$overall)
out.stat.df[out.stat.df$Strata == 'Elevated',2:4] <- overall.stat[c(1,5,2)]
# 
predic.test <- pred.rf.all.func(dat = input.df,
                                fit.in = rf.fit.ns.hz,
                                y.nm = 'nearsurface_hz')

confu.m.test <- confusionMatrix(predic.test$pred.value, 
                                predic.test[,'nearsurface_hz'])
overall.stat <- (confu.m.test$overall)
out.stat.df[out.stat.df$Strata == 'Near-sruface',2:4] <- overall.stat[c(1,5,2)]
# 
predic.test <- pred.rf.all.func(dat = input.df,
                                fit.in = rf.fit.surface.hz,
                                y.nm = 'surface_hz')

confu.m.test <- confusionMatrix(predic.test$pred.value, 
                                predic.test[,'surface_hz'])
overall.stat <- (confu.m.test$overall)
out.stat.df[out.stat.df$Strata == 'Surface',2:4] <- overall.stat[c(1,5,2)]
# confu.m.test$byClass
write.csv(out.stat.df,'cache/table2_stats.csv',row.names=F)
