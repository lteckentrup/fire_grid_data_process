# 
library(randomForest)
require(caTools)
library(caret)
source('r/process_input.R')

# function to fit rf$$$$######
fit.rf.func <- function(dat,y.nm,
                        x.nm = c('soil.density' ,  'clay' ,#'ph' , #soil attributes
                        'rad.short.jan' ,'rad.short.jul', 'wi' ,'curvature_profile','curvature_plan',#topo
                        'tmax' , 'rain' , 'rh.min', #climate
                        'tmax.mean','map','pr.seaonality',#long term clim 
                        'lai.opt'),#vegetation,
                        ...){
  # set fomular
  formula.use <- as.formula(paste0(y.nm,'~.'))
  test.df <- dat[,c(y.nm,#target
                    x.nm)]
  # separate training and validting
  set.seed(1935)
  train <- sample(nrow(test.df), 0.7*nrow(test.df), replace = FALSE)
  TrainSet <- test.df[train,]
  # fit 
  model.hieght <- randomForest(formula.use,
                               data = TrainSet, importance = TRUE,na.action=na.omit,
                               ...)

  return(model.hieght)
}
# fuel type#####
tmp.ft.df <- input.df[!is.na(input.df$fuelType_vicnsw),]
# unique(tmp.ft.df$VICSANSW.TYPE_NAME)
tmp.ft.df <- tmp.ft.df[tmp.ft.df$fuelType_vicnsw %in% names(which(table(tmp.ft.df$fuelType_vicnsw)>=100)),]
tmp.ft.df$fuelType_vicnsw <- factor(tmp.ft.df$fuelType_vicnsw)
rf.fit.ft <- fit.rf.func(dat = tmp.ft.df,
                         y.nm = 'fuelType_vicnsw',
                         x.nm = c('soil.density', 'clay' , #soil attributes
                                  'rad.short.jan' ,'rad.short.jul', 'wi' ,'curvature_profile','curvature_plan',#topo
                                  'tmax' , 'rain' , 'rh.min', #climate
                                  'tmax.mean','map','pr.seaonality',#long term clim 
                                  'lai.opt'))
varImpPlot(rf.fit.ft,type=1)
# previous.fit <- readRDS('cache/rf.fit.fuelType.rds')
saveRDS(rf.fit.ft,'cache/rf.fit.fuelType.rds')
# save  lut for fuel type
tmp.sub.df <- tmp.ft.df[!duplicated(tmp.ft.df$VICSANSW.TYPE_NAME),]
tmp.sub.df <- tmp.sub.df[,c('fuelType_vicnsw','VICSANSW.TYPE_NAME')]
write.csv(tmp.sub.df,'cache/fuelType_LUT.csv',row.names = F)
# fit rf models########
# 1. highets#####
height.df <- input.df
height.df$CNPY_TOP_hight_cm_log <- log(height.df$CNPY_TOP_hight_cm)
height.df <- height.df[!is.na(height.df$CNPY_TOP_hight_cm_log),]
height.df <- height.df[is.finite(height.df$CNPY_TOP_hight_cm_log),]

rf.fit.canopy.h <- fit.rf.func(dat = height.df,
                               y.nm = 'CNPY_TOP_hight_cm_log',mtry=5)

# rf.fit.canopy.h <- fit.rf.func(dat = input.df,
#                                y.nm = 'CNPY_TOP_hight_cm',mtry=5)
# varImpPlot(rf.fit.canopy.h.old,type=2)
# hist(input.df$CNPY_TOP_hight_cm)
# rf.fit.canopy.h.old <- readRDS('cache/rf.fit.canopy.height.rds')
saveRDS(rf.fit.canopy.h,'cache/rf.fit.canopy.height.rds')
# 2.

height.df <- input.df
height.df$NS_TOP_height_cm_log <- log(height.df$NS_TOP_height_cm)
height.df <- height.df[!is.na(height.df$NS_TOP_height_cm_log),]
height.df <- height.df[is.finite(height.df$NS_TOP_height_cm_log),]

rf.fit.ns.h <- fit.rf.func(dat = height.df,
                               y.nm = 'NS_TOP_height_cm_log',mtry=5)
# varImpPlot(rf.fit.ns.h)
saveRDS(rf.fit.ns.h,'cache/rf.fit.ns.height.rds')
# 3. hz score####
rf.fit.hz.elevated <- fit.rf.func(dat = input.df,
                                  y.nm = 'elevated_hz',mtry=5)
varImpPlot(rf.fit.hz.elevated)
# rf.fit.hz.elevated.old <- readRDS('cache/rf.fit.hs.elevated.rds')
# varImpPlot(rf.fit.hz.elevated.old)
saveRDS(rf.fit.hz.elevated,'cache/rf.fit.hs.elevated.rds')
# 4. 
input.df$nearsurface_hz[input.df$nearsurface_hz==0] <- 'NA'
ns.df <- input.df[!is.na(input.df$nearsurface_hz),]
ns.df$nearsurface_hz <- factor(ns.df$nearsurface_hz )

rf.fit.hz.ns <- fit.rf.func(dat = ns.df,
                            y.nm = 'nearsurface_hz',mtry=5)
# varImpPlot(rf.fit.hz.ns)
saveRDS(rf.fit.hz.ns,'cache/rf.fit.hz.ns.rds')
# 5 
rf.fit.hz.bark <- fit.rf.func(dat = input.df,
                            y.nm = 'bark_hz',mtry=5)
saveRDS(rf.fit.hz.bark,'cache/rf.fit.hz.bark.rds')
# 6
rf.fit.hz.surface <- fit.rf.func(dat = input.df,
                              y.nm = 'surface_hz',mtry=5)
saveRDS(rf.fit.hz.surface,'cache/rf.fit.hz.surface.rds')


# predict(rf.fit.canopy.h,)













# predict with fit####
randomForest(x = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
             y = c("Species"),
             training_frame = train,
             model_id = "our.rf",
             seed = 1234)


levels(input.df$surface_hz)
summary(tmp.ft.df$fuelType_vicnsw )
levels(ns.df$nearsurface_hz)
table(tmp.ft.df$fuelType_vicnsw)[,2]

































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
