install.packages("neuralnet")
require(neuralnet)
# 
source('r/process_input.R')

input.df$fuelType_vicnsw <- as.factor(input.df$fuelType_vicnsw )

normalize.func <- function(x) {
  return ((x - min(x,na.rm=T)) / (max(x,na.rm=T) - min(x,na.rm=T)))
}
input.df$canopy.h.norm <- normalize.func(input.df$CNPY_TOP_hight_cm)
# # fit neural network
fit.nn.func <- function(dat,y.nm,
                        x.nm = c('soil.density' , 'ph' , 'clay' , #soil attributes
                                 'rad.short.jan' ,'rad.short.jul', 'wi' ,'curvature_profile','curvature_plan',#topo
                                 'tmax' , 'rain' , 'vph15', #climate
                                 'pet','map',#long term clim
                                 'lai.opt'),...#vegetation,
  
){
  
  formula.use <- as.formula(paste0(y.nm,'~.'))
  test.df <- dat[,c(y.nm,#target
                    x.nm)]

  test.df <- test.df[complete.cases(test.df),]
  # test.df <- test.df[!is.na(test.df[,y.nm]),]#[1000:5000,]
  set.seed(1935)
  train <- sample(nrow(test.df), 0.7*nrow(test.df), replace = FALSE)
  TrainSet <- test.df[train,]
  
  nn=neuralnet(formula.use,
               data=TrainSet, hidden=c(2,6,4,3),
               linear.output = FALSE, threshold=0.01)

  results <- data.frame(actual.c_h = TrainSet[,y.nm],
                        # actual.e_h = test.df$NS_TOP_height_cm,
                        # prediction.c_h = nn$net.result[[1]][,1],
                        prediction.c_h = nn$net.result
                        
  )
  names(results) <- c('actual','prediction')
  
  predicted=results$prediction #* abs(diff(range(test.df$CNPY_TOP_hight_cm))) + min(test.df$CNPY_TOP_hight_cm)
  actual=results$actual #* abs(diff(range(test.df$CNPY_TOP_hight_cm))) + min(test.df$CNPY_TOP_hight_cm)
  # comparison=data.frame(predicted,actual)
  # deviation=((actual-predicted)/actual)
  # comparison=data.frame(predicted,actual,deviation)
  # comparison <- comparison[!is.infinite(comparison$deviation),]
  # accuracy=1-abs(mean(comparison$deviation))
  # accuracy
  plot(actual~predicted)
}




# # formula.use <- as.formula(paste0('fuelType_vicnsw','~.'))
# 
# # 
# nn.df <- input.df[!is.na(input.df$CNPY_TOP_hight_cm),]
# nn.df <- nn.df[1:4000,c('CNPY_TOP_hight_cm','NS_TOP_height_cm',#target
#                         x.nm)]
# # 
# test.df <- nn.df[1:2000,]
# test.df <- test.df[complete.cases(test.df),]
# 
# nn=neuralnet(CNPY_TOP_hight_cm ~pet + map + 
#                soil.density + ph +clay + 
#                lai.opt + 
#                wi + rad.short.jan + curvature_profile + curvature_plan +
#                tmax+rain+vph15 ,
#              data=test.df, hidden=c(6,4,3),act.fct = "logistic",
#              linear.output = TRUE)
# plot(nn)
# 
# 
# valid.df <- nn.df[2001:4000,]
# valid.df <- test[complete.cases(valid.df),]
# Predict=compute(nn,valid.df)
# # Predict$net.result
# 
# # 
# results <- data.frame(actual.c_h = test.df$CNPY_TOP_hight_cm,
#                       # actual.e_h = test.df$NS_TOP_height_cm,
#                       # prediction.c_h = nn$net.result[[1]][,1],
#                       prediction.c_h = nn$net.result
#                       
#                       )
# names(results) <- c('actual','prediction')
# 
# predicted=results$prediction #* abs(diff(range(test.df$CNPY_TOP_hight_cm))) + min(test.df$CNPY_TOP_hight_cm)
# actual=results$actual #* abs(diff(range(test.df$CNPY_TOP_hight_cm))) + min(test.df$CNPY_TOP_hight_cm)
# comparison=data.frame(predicted,actual)
# deviation=((actual-predicted)/actual)
# comparison=data.frame(predicted,actual,deviation)
# comparison <- comparison[!is.infinite(comparison$deviation),]
# accuracy=1-abs(mean(comparison$deviation))
# accuracy
