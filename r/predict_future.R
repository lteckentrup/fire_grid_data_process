source('r/functions_predict.R')
# devtools::source_url("https://github.com/Jinyan-Yang/colors/blob/master/R/col.R?raw=TRUE")

# # read topo
# rad.jan <- readRDS('cache/rad_jan_5km.rds')
# rad.jul <- readRDS('cache/rad_jul_5km.rds')
# c.plan.ra <- readRDS('cache/curvature_plan_vic_5km.rds')
# c.small <- readRDS('cache/curvature_profile_vic_5km.rds')
# wi.ra <- readRDS('cache/wi_vic_5km.rds')
# # read soil
# soil.den <- readRDS('cache/density_vic_5km.rds')
# soil.ph <- readRDS('cache/ph_vic_5km.rds')
# soil.clay <- readRDS('cache/clay_vic_5km.rds')

#function to predict with cmip clim#### 
predict.rf.cmip.func <- function(path.nm,model.path,out.nm){
  # # read inputs####
  # read met
  # tmax.ra <- readRDS('data/met/future/access/rcp45_20452060_monthly_tmax.rds')[[1]]
  # pr.ra <- readRDS('data/met/future/access/rcp45_20452060_monthly_pr.rds')[[1]]
  # rh.ra <- readRDS('data/met/future/access/rcp45_20452060_monthly_rh.rds')[[1]]
  # path.nm <-  'data/met/future/access/rcp45_20452060'
  tmax.ra <- readRDS(paste0(path.nm,'_monthly_tmax.rds'))[[1]]
  pr.ra <- readRDS(paste0(path.nm,'_monthly_pr.rds'))[[1]]
  rh.ra <- readRDS(paste0(path.nm,'_monthly_rh.rds'))[[1]]
  
  tmax.mean.ra <- readRDS(paste0(path.nm,'_annual_tmax.rds'))
  pr.mean.ra <- readRDS(paste0(path.nm,'_annual_pr.rds'))
  # read lai
  lai.ra <- readRDS(paste0(path.nm,'_lai_jan_5km.rds'))
  model.rf <- readRDS(model.path)#'cache/rf.fit.hz.surface.rds'
  prob.m <- try(predict.rf.func(model.in = model.rf,
                            s.den=matrix(soil.den),s.ph=matrix(soil.ph),s.clay= matrix(soil.clay),
                            rad.jan = matrix(rad.jan),rad.jul = matrix(rad.jul),
                            wi = matrix(wi.ra),c.profile = matrix(c.small),c.plan = matrix(c.plan.ra),
                            tmax = matrix(tmax.ra),rain = matrix(pr.ra),rh.min = matrix(rh.ra),
                            tmax.mean = matrix(tmax.mean.ra),map = matrix(pr.mean.ra),
                            lai.opt = matrix(lai.ra),
                            giveProb = T))
  
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
  # save prob
  if(class(prob.m) != 'try-error'){
    prob.m.m <- matrix(prob.m,
                       ncol = ncol(soil.den),
                       nrow = nrow(soil.den),byrow = T)
    
    
    prob.ra <-raster((prob.m.m))
    extent(prob.ra) <- extent(soil.den)
  }else{
    prob.ra <- NA
  }
  
  # plot(prob.ra)
  
  saveRDS(list(val = score.ra,
               prob = prob.ra),paste0(path.nm,out.nm))
}

# wrap func
wrap.predic.func <- function(where.is.data){
  # where.is.data <- 'data/met/future/access/rcp45_20452060'
  # 1. canopy height
  predict.rf.cmip.func(path.nm = where.is.data,
                       model.path = 'cache/rf.fit.canopy.height.rds',
                       out.nm = '_height_canopy.rds')
  # 2. ns height
  predict.rf.cmip.func(path.nm = where.is.data,
                       model.path = 'cache/rf.fit.ns.height.rds',
                       out.nm = '_height_ns.rds')
  # 3. hz ele
  predict.rf.cmip.func(path.nm = where.is.data,
                       model.path = 'cache/rf.fit.hs.elevated.rds',
                       out.nm = '_hz_elevated.rds')
  
  # 4. hz bark
  predict.rf.cmip.func(path.nm = where.is.data,
                       model.path = 'cache/rf.fit.hz.bark.rds',
                       out.nm = '_hz_bark.rds')
  
  # 5. hz ns
  predict.rf.cmip.func(path.nm = where.is.data,
                       model.path = 'cache/rf.fit.hz.ns.rds',
                       out.nm = '_hz_ns.rds')
  # 6. hz surface
  predict.rf.cmip.func(path.nm = where.is.data,
                       model.path = 'cache/rf.fit.hz.surface.rds',
                       out.nm = '_hz_surface.rds')
}

met.path.vec <- list.dirs('data/met/future/',recursive = T)
rcp45.index <- grep('rcp45',met.path.vec)
rcp85.index <- grep('rcp85',met.path.vec)
hist.index <- grep('history',met.path.vec)
fn.vec.chosen <- met.path.vec[c(rcp45.index,rcp85.index,hist.index)]
# met.mode.fn <- sapply(met.path.vec, function(x)list.dirs(x,recursive = F))
sapply(fn.vec.chosen,wrap.predic.func)

# wrap.predic.func('data/met/future/ACCESS1-0/maca_VIC_ACCESS1-0_historical/')

# hs.ele.ls <- readRDS('data/met/future/access/rcp45_20452060_hz_elevated.rds')
# h.can.ls <- readRDS('data/met/future/access/rcp45_20452060_height_ns.rds')
# plot(exp(h.can.ls[['val']]))

# # plot#####
# brks <- 0.5:5.5
# plot(hs.ele.ls[['val']],breaks=brks,col=col.df$cityNight[c(1,2,5,4,3)],lab.breaks=0:5)
# 
# colours <- colorRampPalette(c("grey","red"))
# plot(prob.ra,col=colours(20))








# library(raster)
# #make df for prediction####
# pr.df <- readRDS('data/met/future/access/rcp45_20452060_monthly_pr.rds')
# rh.df <- readRDS('data/met/future/access/rcp45_20452060_monthly_rh.rds')
# tmax.df <- readRDS('data/met/future/access/rcp45_20452060_monthly_tmax.rds')
# 
# # # plot(tmax.df[[1]])
# # # i=1
# # rad.small.jan.ra <- crop(rad.jan.ra, extent(pr.df[[1]]))
# # # rad.small.jan.ra.ag <- aggregate(rad.small.jan.ra)
# # # plot(as.raster(pr.df[[1]]))
# # crs(rad.small.jan.ra) <- crs(pr.df[[1]]) <- '+init=epsg:4326'
# # 
# # # get rad####
# # rad.jan.ra <- raster('data/topo/rad/SRADTotalShortwaveSlopingSurf_0115_lzw.tif')
# # rad.jul.ra <- raster('data/topo/rad/SRADTotalShortwaveSlopingSurf_0715_lzw.tif')
# # rad.small.jan.ra <- crop(rad.jan.ra, extent(pr.df[[1]]))
# # rad.small.jul.ra <- crop(rad.jul.ra, extent(pr.df[[1]]))
# # 
# # crs(rad.small.jan.ra) <- crs(rad.small.jul.ra)<- crs(pr.df[[1]]) <- '+init=epsg:4326'
# # rad.small.jan.ra.ag.clim <- projectRaster(from=rad.small.jan.ra,
# #                                           to=pr.df[[1]], method="bilinear")
# # 
# # rad.small.jul.ra.ag.clim <- projectRaster(from=rad.small.jul.ra,
# #                                           to=pr.df[[1]], method="bilinear")
# 
# rad.small.jan.ra.ag.clim <- readRDS('cache/rad_jan_5km.rds')
# rad.small.jul.ra.ag.clim <- readRDS('cache/rad_jul_5km.rds')
# 
# 
# # get par value####
# par_fraction <- 0.368
# 
# par.ra.5km <- (rad.small.jan.ra.ag.clim +
#                  rad.small.jul.ra.ag.clim)/2 *par_fraction
# 
# 
# 
# # plot(par.ra.5km)
# # get vpd####
# get.vp.from.t.func <- function(temperature){
#   # Magnus_pressure = 
#   0.61094 * exp((17.625 * temperature) / (temperature + 243.04))
# }
# 
# #model####
# # library("devtools")
# # install_bitbucket("Jinyan_Jim_Yang/g1.opt.package.git")
# n.days <- 365.25
# library(g1.opt.func)
# vpd.ra.5km <- get.vp.from.t.func((tmax.df[[1]] - 272.15)) * (1- rh.df[[1]]/100)
# jan.lai.m <- mapply(g1.lai.e.func,
#                     VPD = matrix(vpd.ra.5km),
#                     E = matrix(pr.df[[1]])*n.days,
#                     PAR = matrix(rad.small.jan.ra.ag.clim * par_fraction*n.days), 
#                     TMAX = matrix(tmax.df[[1]]- 272.15))
# 
# vpd.ra.5km.jul <- get.vp.from.t.func((tmax.df[[7]] - 272.15)) * (1- rh.df[[7]]/100)
# vpd.ra.5km.jul[vpd.ra.5km.jul<0.05] <- 0.05
# jul.lai.m <- mapply(g1.lai.e.func,
#                     VPD = matrix(vpd.ra.5km.jul),
#                     E = matrix(pr.df[[7]]*n.days),
#                     PAR = matrix(rad.small.jul.ra.ag.clim * par_fraction*n.days), 
#                     TMAX = matrix(tmax.df[[7]]- 272.15))
# 
# convert.fun <- function(lai.opt.vec){
#   var.vec.jul <- lai.opt.vec#jul.lai.m[2,]
#   var.m <- matrix(var.vec.jul,
#                   ncol = ncol(vpd.ra.5km),
#                   nrow = nrow(vpd.ra.5km),byrow = T)
#   
#   # lai.ra <- rasterize(x = ,y = ,var.m)
#   lai.ra <-raster(var.m)
#   extent(lai.ra) <- extent(vpd.ra.5km)
#   plot(lai.ra)
#   return(lai.ra)
# }
# 
# lai.jan.ra <- convert.fun(lai.opt.vec = jan.lai.m[2,])
# 
# saveRDS(lai.jan.ra,'data/met/future/access/lai_jan_5km.rds')
# lai.jul.ra <- convert.fun(lai.opt.vec = jul.lai.m[2,])

# var.vec.jul <- jul.lai.m[2,]
# var.m <- matrix(var.vec,
#                 ncol = ncol(vpd.ra.5km),
#                 nrow = nrow(vpd.ra.5km),byrow = T)
# 
# # lai.ra <- rasterize(x = ,y = ,var.m)
# lai.ra <-raster(var.m)
# extent(lai.ra) <- extent(vpd.ra.5km)
# # plot(lai.ra)
# # plot(rad.small.jul.ra.ag.clim * par_fraction*n.days)
# # plot(tmax.df[[1]]- 272.15)



# predict lai####
# # hs.soil.wi.met.df <- readRDS('cache/hs.soil.topo.met.rds')
# rad.jan.ra <- raster('data/topo/rad/SRADTotalShortwaveSlopingSurf_0115_lzw.tif')
# input.future.df$rad.short.jan <- extract(x = rad.jan.ra,
#                                          y =  cbind(input.future.df$lon,input.future.df$lat),
#                                          small=T,na.rm=T)
# # rm(rad.jan.ra)
# 
# rad.jul.ra <- raster('data/topo/rad/SRADTotalShortwaveSlopingSurf_0715_lzw.tif')
# input.future.df$rad.short.jul <- extract(x = rad.jul.ra,
#                                          y =  cbind(input.future.df$lon,input.future.df$lat),
#                                          small=T,na.rm=T)
# rm(rad.jul.ra)
par_fraction <- 0.368
# 
# library("devtools")
# install_bitbucket("Jinyan_Jim_Yang/g1.opt.package.git")

# input.future.df$PAR <- (input.future.df$rad.short.jan +
#                           input.future.df$rad.short.jul)/2 *par_fraction
get.vp.from.t.func <- function(temperature){
  # Magnus_pressure = 
  0.61094 * exp((17.625 * temperature) / (temperature + 243.04))
}


input.future.df$vpd <- get.vp.from.t.func(input.future.df$tmax) * (1- input.future.df$vph15/100)
input.future.df$vpd[input.future.df$vpd<0.05] <- 0.05
library(g1.opt.func)
opt.out.ls <- list()
n.days <- 365.25
for (i in 1:nrow(input.future.df)) {
  tmp <- try(g1.lai.e.func(VPD = input.future.df$vpd[i],
                           E = input.future.df$rain[i]*n.days,
                           PAR = input.future.df$PAR[i]*n.days,
                           TMAX = input.future.df$tmax[i],
                           # rcp4.5= 2017-2030: 419; 2045-2060:492;2085-2100: 585
                           # rcp8.5= 2017-2030: 427; 2045-2060:562;2085-2100: 877
                           Ca = 492))
  if(class(tmp)=='try-error'){
    opt.out.ls[[i]] <- NA
  }else{
    opt.out.ls[[i]] <- tmp
  }
  print(i)
}

opt.out.df <- do.call(rbind,opt.out.ls)
input.future.df$lai.opt <- opt.out.df[,'LAI']
