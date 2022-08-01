# function to fit rf$$$$######
fit.rf.func <- function(dat,y.nm,
                        x.nm = c('soil.density' ,  'clay' ,#'ph' , #soil attributes
                                 'rad.short.jan' ,'rad.short.jul', 'wi' ,'curvature_profile','curvature_plan',#topo
                                 'tmax' , 'rain' , 'rh.min', #climate
                                 'tmax.mean','map','pr.seaonality',#long term clim 
                                 'lai.opt.mean'),#vegetation,
                        ...){
  # set fomular
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
  TrainSet <- test.df[train,]
  
  # ####
  # so what do we want to capture here 
  # all score or just high score
  # here we assume we need to predict each score well
  # not just one score
  # #####
  
  #split by score
  test.ls <- split(TrainSet,TrainSet[,y.nm])
  
  # sample within each score
  sample.func <- function(df){
    print(nrow(df))
    if(nrow(df)>5000){
      set.seed(1935)
      train <- sample(nrow(df), 5000, replace = F)
    }else{
      set.seed(1935)
      train <- sample(nrow(df), 5000, replace = T)
    }
    return(df[train,])
  }
  
  out.ls <- lapply(test.ls,sample.func)
  TrainSet <- do.call(rbind,out.ls)
  # fit
  # model.hieght <- randomForest(formula.use,
  #                              data = train.df,
  #                              mtry=5,
  #                              importance = TRUE,
  #                              na.action=na.omit)
  
  # varImpPlot(rf.fit.hz.elevated)
  
  
  # fit
  model.hieght <- randomForest(formula.use,
                               data = TrainSet, 
                               importance = TRUE,
                               na.action=na.omit,
                               ...)
  
  return(model.hieght)
}