# read fits
rf.fit.canopy.h <- readRDS('cache/rf.fit.canopy.height.rds')
varImpPlot(rf.fit.canopy.h)


rf.fit.ns.h <- readRDS('cache/rf.fit.ns.height.rds')
varImpPlot(rf.fit.ns.h)

rf.fit.ns.hz <- readRDS('cache/rf.fit.hz.ns.rds')
rf.fit.elevated.hz <- readRDS('cache/rf.fit.hs.elevated.rds')
# 

pred.rf.func <- function(dat,y.nm,fit.in){
  formula.use <- as.formula(paste0(y.nm,'~.'))
  test.df <- dat[,c(y.nm,#target
                    'soil.density' , 'ph' , 'clay' , #soil attributes
                    'rad.short.jan' ,'rad.short.jul', 'wi' ,#topo
                    'tmax' , 'rain' , 'vph15', #climate
                    'lai.opt')]#vegetation
  
  
  # test.df <- test.df[complete.cases(test.df),]
  test.df <- test.df#[1000:5000,]
  set.seed(1935)
  train <- sample(nrow(test.df), 0.7*nrow(test.df), replace = FALSE)
  TrainSet <- test.df[train,]
  ValidSet <- test.df[-train,]
  # 
  prediction_for_roc_curve <- predict(rf.fit.canopy.h,ValidSet)
  # model.hieght <- predict(rf.fit.canopy.h,)
  # varImpPlot(model.hieght)
  # summary(TrainSet)
  
  return(prediction_for_roc_curve)
}

ValidSet[,y.nm]
