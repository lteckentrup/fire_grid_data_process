library(h2o)
h2o.init()
iris.hex <- as.h2o(input.df)
# r_df <- as.data.frame(iris.hex)
splits <- h2o.splitFrame(data = iris.hex,
                         ratios = c(0.7),  #partition data into 80% and 20% chunks
                         seed = 1935)
train <- splits[[1]]
test <- splits[[2]]

rf.fit.canopy.h <- h2o.randomForest(x = c('soil.density' , 'ph' , 'clay' , #soil attributes
                                    'rad.short.jan' ,'rad.short.jul', 'wi' ,'curvature_profile','curvature_plan',#topo
                                    'tmax' , 'rain' , 'rh.min', #climate
                                    'tmax.mean','map',#long term clim 
                                    'lai.opt'),
                              y = 'elevated_hz',
                              training_frame = train,
                              model_id = "ch.rf",
                              seed = 1935,mtries=5,ntrees =500)
summary(rf.fit.canopy.h)
h2o.performance(model = rf.fit.canopy.h, newdata = test)

predictions <- h2o.predict(rf.fit.ft, test)
