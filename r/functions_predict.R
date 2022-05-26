devtools::source_url("https://github.com/Jinyan-Yang/colors/blob/master/R/col.R?raw=TRUE")
# read inputs####
# read topo
rad.jan <- readRDS('cache/rad_jan_5km.rds')
rad.jul <- readRDS('cache/rad_jul_5km.rds')
c.plan.ra <- readRDS('cache/curvature_plan_vic_5km.rds')
c.small <- readRDS('cache/curvature_profile_vic_5km.rds')
wi.ra <- readRDS('cache/wi_vic_5km.rds')
# read soil
soil.den <- readRDS('cache/density_vic_5km.rds')
soil.ph <- readRDS('cache/ph_vic_5km.rds')
soil.clay <- readRDS('cache/clay_vic_5km.rds')

# function to predict#####
predict.rf.func <- function(model.in,
                            s.den,s.ph,s.clay,
                            rad.jan,rad.jul,wi,c.profile,c.plan,
                            tmax,rain,rh.min,
                            tmax.mean,map,pr.seaonality,
                            lai.opt,
                            giveProb){
  # model.in = readRDS('cache/rf.fit.hz.surface.rds')
  # make a df
  df <- data.frame(soil.density = s.den,ph = s.ph,clay = s.clay,
                   rad.short.jan = rad.jan, rad.short.jul = rad.jul,
                   wi = wi,curvature_profile = c.profile,curvature_plan=c.plan,
                   tmax = tmax,rain = rain,rh.min = rh.min,
                   tmax.mean = tmax.mean,map=map,pr.seaonality = pr.seaonality,
                   lai.opt=lai.opt)
  # predict prob and score
  if(giveProb){
    
    return(predict(object = model.in,newdata=df,type='prob'))
  }else{
    return( predict(object = model.in,newdata=df))
  }
  
  # }
}

# x.df <- predict(object = model.in,newdata=df,type='vote')
# x.prob.df <- predict(object = model.in,newdata=df,type='prob')
#function to predict with cmip clim#### 
predict.rf.cmip.func <- function(path.nm,model.path,out.nm){
  # # read inputs####
  # read met
  # tmax.ra <- readRDS('data/met/future/ACCESS1-0/rcp45_mid/rcp45_20452060_monthly_tmax.rds')[[1]]
  # pr.ra <- readRDS('data/met/future/ACCESS1-0/rcp45_mid/rcp45_20452060_monthly_pr.rds')[[1]]
  # rh.ra <- readRDS('data/met/future/ACCESS1-0/rcp45_mid/rcp45_20452060_monthly_rh.rds')[[1]]
  # path.nm <-  'data/met/future/ACCESS1-0/rcp45_mid/'
  
  tmax.ra <- readRDS(list.files(path = path.nm,pattern = '_monthly_tmax.rds',full.names = T))[[1]]
  pr.ra <- readRDS(list.files(path = path.nm,pattern = '_monthly_pr.rds',full.names = T))[[1]]
  rh.ra <- readRDS(list.files(path = path.nm,pattern = '_monthly_rh.rds',full.names = T))[[1]]
  
  tmax.mean.ra <- readRDS(list.files(path = path.nm,pattern = '_annual_tmax.rds',full.names = T))
  pr.mean.ra <- readRDS(list.files(path = path.nm,pattern = '_annual_pr.rds',full.names = T))
  
  # read proper rainfall seasonality
  is.his <- grep(x = path.nm,pattern = 'history')
  is.45.mid <- grep(x = path.nm,pattern = 'rcp45_mid')
  is.45.long <- grep(x = path.nm,pattern = 'rcp45_long')
  is.85.mid <- grep(x = path.nm,pattern = 'rcp85_mid')
  is.85.long <- grep(x = path.nm,pattern = 'rcp85_long')
  
  if(length(is.his)>0){
    pr.season.ra <- readRDS('cache/pr_seaonality_history.rds')
  }else if(length(is.45.mid)>0){
    pr.season.ra <- readRDS('cache/pr_seaonality_rcp45_mid.rds')
  }else if(length(is.45.long)>0){
    pr.season.ra <- readRDS('cache/pr_seaonality_rcp45_long.rds')
  }else if(length(is.85.long)>0){
    pr.season.ra <- readRDS('cache/pr_seaonality_rcp85_long.rds')
  }else if(length(is.85.mid)>0){
    pr.season.ra <- readRDS('cache/pr_seaonality_rcp85_mid.rds')
  }else{
    stop('did not find rainfall seasonality for the RCP')
  }
  
  # read lai
  # lai.ra <- readRDS(paste0(path.nm,'_lai_jan_5km.rds'))
  lai.ra <- readRDS(list.files(path = path.nm,pattern = '_lai_jan_5km.rds',full.names = T))
  model.rf <- readRDS(model.path)#'cache/rf.fit.hz.surface.rds'
  # model.rf <- readRDS('cache/rf.fit.hs.elevated.rds')#
  
  # get predicted probbility
  prob.m <- try(predict.rf.func(model.in = model.rf,
                                s.den=matrix(soil.den),s.ph=matrix(soil.ph),s.clay= matrix(soil.clay),
                                rad.jan = matrix(rad.jan),rad.jul = matrix(rad.jul),
                                wi = matrix(wi.ra),c.profile = matrix(c.small),c.plan = matrix(c.plan.ra),
                                tmax = matrix(tmax.ra),rain = matrix(pr.ra),rh.min = matrix(rh.ra),
                                tmax.mean = matrix(tmax.mean.ra),map = matrix(pr.mean.ra),pr.seaonality = matrix(pr.season.ra),
                                lai.opt = matrix(lai.ra),
                                giveProb = T))
  # get predicted value
  rf.m <- predict.rf.func(model.in = model.rf,
                          s.den=matrix(soil.den),s.ph=matrix(soil.ph),s.clay= matrix(soil.clay),
                          rad.jan = matrix(rad.jan),rad.jul = matrix(rad.jul),
                          wi = matrix(wi.ra),c.profile = matrix(c.small),c.plan = matrix(c.plan.ra),
                          tmax = matrix(tmax.ra),rain = matrix(pr.ra),rh.min = matrix(rh.ra),
                          tmax.mean = matrix(tmax.mean.ra),map = matrix(pr.mean.ra),pr.seaonality = matrix(pr.season.ra),
                          lai.opt = matrix(lai.ra),giveProb = F)
  
  # save prediction
  var.m <- matrix(as.numeric(as.character(rf.m)),
                  ncol = ncol(soil.den),
                  nrow = nrow(soil.den),byrow = T)
  
  
  score.ra <-raster(var.m)
  extent(score.ra) <- extent(soil.den)
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
    prob.m.ls <- NA
  }
  
  saveRDS(list(val = score.ra,
               prob = prob.m.ls),paste0(path.nm,'/',out.nm))
}
# wrap func####
wrap.predic.func <- function(where.is.data,my.fun = predict.rf.cmip.func){
  # where.is.data <- 'data/met/future/access/rcp45_20452060'
  # 1. canopy height
  my.fun(path.nm = where.is.data,
         model.path = 'cache/rf.fit.canopy.height.rds',
         out.nm = '_height_canopy.rds')
  # readRDS(model.path)
  # 2. ns height
  my.fun(path.nm = where.is.data,
         model.path = 'cache/rf.fit.ns.height.rds',
         out.nm = '_height_ns.rds')
  # 3. hz ele
  my.fun(path.nm = where.is.data,
         model.path = 'cache/rf.fit.hs.elevated.rds',
         out.nm = '_hz_elevated.rds')
  
  # 4. hz bark
  my.fun(path.nm = where.is.data,
         model.path = 'cache/rf.fit.hz.bark.rds',
         out.nm = '_hz_bark.rds')
  
  # 5. hz ns
  my.fun(path.nm = where.is.data,
         model.path = 'cache/rf.fit.hz.ns.rds',
         out.nm = '_hz_ns.rds')
  # 6. hz surface
  my.fun(path.nm = where.is.data,
         model.path = 'cache/rf.fit.hz.surface.rds',
         out.nm = '_hz_surface.rds')
}

