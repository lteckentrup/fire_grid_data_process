# read and clean data####
input.df <- readRDS('cache/hs.soil.topo.met.lai.rds')

for (i in 1:ncol(input.df)) {
  input.df[,i] <- replace(input.df[,i],list = which(input.df[,i]< -100),NA)
}

# summary(input.df)
# test.df <- input.df#[1:2000,]
# summary(test.df)
# test.df <- test.df[,c('overall_hz',#target
#                       'soil.density' , 'ph' , 'clay' , #soil attributes
#                       'rad.short.jan' ,'rad.short.jul', 'wi' ,#topo
#                       'tmax' , 'rain' , 'vph15' , #climate
#                       'lai.opt')]#vegetation
# test.df <- test.df[complete.cases(test.df),]
# test.df <- test.df[1:1000,]
# test.df$overall_hz <- as.factor(test.df$overall_hz)
# function to fit rf$$$$######
library(randomForest)
require(caTools)
fit.rf.func <- function(dat,y.nm){
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
  
  model.hieght <- randomForest(formula.use,
                               data = TrainSet, importance = TRUE,na.action=na.omit,
                               ntree = 500,mtry=9)
  # varImpPlot(model.hieght)
  # summary(TrainSet)
  
  return(model.hieght)
}

# fit rf models########
# highets
rf.fit.canopy.h <- fit.rf.func(dat = input.df,
                               y.nm = 'CNPY_TOP_hight_cm')

saveRDS(rf.fit.canopy.h,'cache/rf.fit.canopy.height.rds')
# 
rf.fit.ns.h <- fit.rf.func(dat = input.df,
                               y.nm = 'NS_TOP_height_cm')

saveRDS(rf.fit.ns.h,'cache/rf.fit.ns.height.rds')
# hz score
rf.fit.hz.elevated <- fit.rf.func(dat = input.df,
                           y.nm = 'elevated_hz')
saveRDS(rf.fit.hz.elevated,'cache/rf.fit.hs.elevated.rds')
# 
rf.fit.hz.ns <- fit.rf.func(dat = input.df,
                                  y.nm = 'nearsurface_hz')
saveRDS(rf.fit.hz.ns,'cache/rf.fit.hz.ns.rds')
# 
rf.fit.hz.bark <- fit.rf.func(dat = input.df,
                            y.nm = 'bark_hz')
saveRDS(rf.fit.hz.bark,'cache/rf.fit.hz.bark.rds')
# 
rf.fit.hz.surface <- fit.rf.func(dat = input.df,
                              y.nm = 'surface_hz')
saveRDS(rf.fit.hz.surface,'cache/rf.fit.hz.surface.rds')










































# try rf model fitting
library(randomForest)
require(caTools)
set.seed(1935)
train <- sample(nrow(test.df), 0.7*nrow(test.df), replace = FALSE)
TrainSet <- test.df[train,]
ValidSet <- test.df[-train,]
summary(TrainSet)

model1 <- randomForest(overall_hz 
                       # +  NS_TOP_height_cm + CNPY_TOP_hight_cm  +
                       #   bark_hz  + elevated_hz + nearsurface_hz + 
                       #   surface_hz  + sns_hz
                       ~.,
                         # soil.density + ph + clay + 
                         # rad.short.jan +rad.short.jul+ wi +
                         # tmax + rain + vph15 + 
                         # lai.opt,
                       data = TrainSet, importance = TRUE,na.action=na.omit,
                       ntree = 500,mtry=9,oob.prox=F)
summary(model1)
varImpPlot(model1)
importance(model1)
plot(model1)
# predTrain <- predict(model1, TrainSet, type = "class")
# Checking classification accuracy
# cm = table(TrainSet, predTrain)

hist(treesize(model1),
     main = "No. of Nodes for the Trees",
     col = "green")
varImpPlot(model1,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")
# 3333######
test.df <- input.df#[!is.na(input.df$LAST_FIRE_yr),]#[1:2000,]
# summary(test.df)
test.df <- test.df[,c('NS_TOP_height_cm',#target
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

model.hieght <- randomForest(NS_TOP_height_cm
                       # +  NS_TOP_height_cm + CNPY_TOP_hight_cm  +
                       #   bark_hz  + elevated_hz + nearsurface_hz + 
                       #   surface_hz  + sns_hz
                       ~.,
                       # soil.density + ph + clay + 
                       # rad.short.jan +rad.short.jul+ wi +
                       # tmax + rain + vph15 + 
                       # lai.opt,
                       data = TrainSet, importance = TRUE,na.action=na.omit,
                       ntree = 500,mtry=8)
varImpPlot(model.hieght)
summary(TrainSet)


# 
model.hieght <- randomForest(NS_TOP_height_cm
                             # +  NS_TOP_height_cm + CNPY_TOP_hight_cm  +
                             #   bark_hz  + elevated_hz + nearsurface_hz + 
                             #   surface_hz  + sns_hz
                             ~.,
                             # soil.density + ph + clay + 
                             # rad.short.jan +rad.short.jul+ wi +
                             # tmax + rain + vph15 + 
                             # lai.opt,
                             data = TrainSet, importance = TRUE,na.action=na.omit,
                             ntree = 500,mtry=8)
