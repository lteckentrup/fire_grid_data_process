source('r/functions_predict.R')

#function to predict with cmip clim#### 
predict.rf.cmip.noVeg.func <- function(path.nm,model.path,out.nm){
  # # read inputs####
  # read met
  tmax.ra <- readRDS(list.files(path = path.nm,pattern = '_monthly_tmax.rds',full.names = T))[[1]]
  pr.ra <- readRDS(list.files(path = path.nm,pattern = '_monthly_pr.rds',full.names = T))[[1]]
  rh.ra <- readRDS(list.files(path = path.nm,pattern = '_monthly_rh.rds',full.names = T))[[1]]
  
  tmax.mean.ra <- readRDS(list.files(path = path.nm,pattern = '_annual_tmax.rds',full.names = T))
  pr.mean.ra <- readRDS(list.files(path = path.nm,pattern = '_annual_pr.rds',full.names = T))
  
  # read lai
  lai.fn.vec <- list.files(path = dirname(path.nm),
                           pattern = '_lai_jan_5km.rds',
                           full.names = T,recursive = T)
  lai.path <- lai.fn.vec[grep(pattern = 'history',x = lai.fn.vec)]

  lai.ra <- readRDS(lai.path)
  model.rf <- readRDS(model.path)#'cache/rf.fit.hz.surface.rds'
  
  # get predicted probbility
  prob.m <- try(predict.rf.func(model.in = model.rf,
                                s.den=matrix(soil.den),s.ph=matrix(soil.ph),s.clay= matrix(soil.clay),
                                rad.jan = matrix(rad.jan),rad.jul = matrix(rad.jul),
                                wi = matrix(wi.ra),c.profile = matrix(c.small),c.plan = matrix(c.plan.ra),
                                tmax = matrix(tmax.ra),rain = matrix(pr.ra),rh.min = matrix(rh.ra),
                                tmax.mean = matrix(tmax.mean.ra),map = matrix(pr.mean.ra),
                                lai.opt = matrix(lai.ra),
                                giveProb = T))
  # get predicted value
  rf.m <- predict.rf.func(model.in = model.rf,
                          s.den=matrix(soil.den),s.ph=matrix(soil.ph),s.clay= matrix(soil.clay),
                          rad.jan = matrix(rad.jan),rad.jul = matrix(rad.jul),
                          wi = matrix(wi.ra),c.profile = matrix(c.small),c.plan = matrix(c.plan.ra),
                          tmax = matrix(tmax.ra),rain = matrix(pr.ra),rh.min = matrix(rh.ra),
                          tmax.mean = matrix(tmax.mean.ra),map = matrix(pr.mean.ra),
                          lai.opt = matrix(lai.ra),giveProb = F)
  # save prediction
  var.m <- matrix(as.numeric(rf.m),
                  ncol = ncol(soil.den),
                  nrow = nrow(soil.den),byrow = T)
  
  
  score.ra <-raster(var.m)
  extent(score.ra) <- extent(soil.den)

#   rf.m.future.lai <- predict.rf.func(model.in = model.rf,
#                                      s.den=matrix(soil.den),s.ph=matrix(soil.ph),s.clay= matrix(soil.clay),
#                                      rad.jan = matrix(rad.jan),rad.jul = matrix(rad.jul),
#                                      wi = matrix(wi.ra),c.profile = matrix(c.small),c.plan = matrix(c.plan.ra),
#                                      tmax = matrix(tmax.ra),rain = matrix(pr.ra),rh.min = matrix(rh.ra),
#                                      tmax.mean = matrix(tmax.mean.ra),map = matrix(pr.mean.ra),
#                                      lai.opt = matrix(readRDS('data/met/future/ACCESS1-0/rcp45_long/rcp45_20852100_lai_jan_5km.rds')),
#                                      giveProb = F)
#   
#   var.m.future.lai <- matrix(as.numeric(rf.m.future.lai),
#                              ncol = ncol(soil.den),
#                              nrow = nrow(soil.den),byrow = T)
# 
# 
# score.ra.future <-raster(var.m.future.lai)
# extent(score.ra.future) <- extent(soil.den)
#   
# plot(score.ra-score.ra.future)
  
  # save prob 
if(class(prob.m) != 'try-error'){
    
    layer.nm <- colnames(prob.m)
    prob.m.ls <- list()
    for (lay.i in seq_along(layer.nm)) {
      prob.m.i <- matrix(prob.m[,layer.nm[lay.i]],
                         ncol = ncol(soil.den),
                         nrow = nrow(soil.den),byrow = T)
      prob.ra <-raster((prob.m.i))
      extent(prob.ra) <- extent(soil.den)
      # plot(prob.ra)
      
      prob.m.ls[[lay.i]] <- prob.ra
      
    }
    names(prob.m.ls) <- layer.nm
    
  }else{
    prob.ra <- NA
  }

  saveRDS(list(val = score.ra,
               prob = prob.ra),paste0(path.nm,'/','noVeg',out.nm))
}
# 
met.path.vec <- list.dirs('data/met/future/',recursive = T)
rcp45.index <- grep('rcp45',met.path.vec)
rcp85.index <- grep('rcp85',met.path.vec)
# hist.index <- grep('history',met.path.vec)
fn.vec.chosen <- met.path.vec[c(rcp45.index,rcp85.index)]
# met.mode.fn <- sapply(met.path.vec, function(x)list.dirs(x,recursive = F))
library(randomForest)
sapply(fn.vec.chosen,wrap.predic.func,my.fun = predict.rf.cmip.noVeg.func)
