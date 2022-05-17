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

# func####
library(g1.opt.func)
library(raster)
get.lai.func <- function(pr.fn,tmax.fn,rh.fn,out.fn){
  
  # 
  # use cliamte to predict lai for vic
  # inputs: pr.fn= file name of precipitation,max temperature, relative humidity
  # output: out.fn= file name for optimal lai      
  # 
  
  
  #read climate####
  pr.df <- readRDS(pr.fn)
  rh.df <- readRDS(rh.fn)
  tmax.df <- readRDS(tmax.fn)
  # read rad
  rad.jan.ra <- readRDS('cache/rad_jan_5km.rds')
  # rad.small.jul.ra.ag.clim <- readRDS('cache/rad_jul_5km.rds')
  
  # get par value####
  par_fraction <- 0.368

  # get vpd####
  get.vp.from.t.func <- function(temperature){
    # Magnus_pressure = 
    0.61094 * exp((17.625 * temperature) / (temperature + 243.04))
  }
  
  #model####
  # library("devtools")
  # install_bitbucket("Jinyan_Jim_Yang/g1.opt.package.git")
  n.days <- 365.25

  vpd.ra.5km <- get.vp.from.t.func((tmax.df[[1]] - 272.15)) * (1- rh.df[[1]]/100)
  jan.lai.m <- mapply(g1.lai.e.func,
                      VPD = matrix(vpd.ra.5km),
                      E = matrix(pr.df[[1]])*n.days,
                      PAR = matrix(rad.jan.ra * par_fraction*n.days), 
                      TMAX = matrix(tmax.df[[1]]- 272.15))
  
  # vpd.ra.5km.jul <- get.vp.from.t.func((tmax.df[[7]] - 272.15)) * (1- rh.df[[7]]/100)
  # vpd.ra.5km.jul[vpd.ra.5km.jul<0.05] <- 0.05
  # jul.lai.m <- mapply(g1.lai.e.func,
  #                     VPD = matrix(vpd.ra.5km.jul),
  #                     E = matrix(pr.df[[7]]*n.days),
  #                     PAR = matrix(rad.small.jul.ra.ag.clim * par_fraction*n.days), 
  #                     TMAX = matrix(tmax.df[[7]]- 272.15))
  
  convert.fun <- function(lai.opt.vec){
    var.vec.jul <- lai.opt.vec#jul.lai.m[2,]
    var.m <- matrix(var.vec.jul,
                    ncol = ncol(vpd.ra.5km),
                    nrow = nrow(vpd.ra.5km),byrow = T)
    
    # lai.ra <- rasterize(x = ,y = ,var.m)
    lai.ra <-raster(var.m)
    extent(lai.ra) <- extent(vpd.ra.5km)
    plot(lai.ra)
    return(lai.ra)
  }
  
  lai.jan.ra <- convert.fun(lai.opt.vec = jan.lai.m[2,])
  
  saveRDS(lai.jan.ra,out.fn)
}

# do it for different climate model####
get.lai.func(pr.fn = 'data/met/future/access/rcp45_20452060_monthly_pr.rds',
             rh.fn = 'data/met/future/access/rcp45_20452060_monthly_rh.rds',
             tmax.fn= 'data/met/future/access/rcp45_20452060_monthly_tmax.rds',
             out.fn = 'data/met/future/access/lai_jan_5km.rds')
