# 
library(randomForest)
require(caTools)
library(caret)
source('r/process_input.R')
source('r/function_rf.R')
source('r/get_vic_shape.R')
# 
library(raster)
evc.df <- raster('data/EVC_fuelType/evc/VICSANSW161.tif')

evc.df[evc.df >= 4000] <- NA
evc.df[evc.df <= 2999] <- NA
# plot(evc.df)
# xxx <- projectRaster(evc.df, crs = "+proj=longlat +datum=WGS84")
# plot(xxx)
set.seed(1935)
sample.index <- sampleStratified(evc.df,5000, na.rm=TRUE)
saveRDS(sample.index,'cache/ft.sample.index.rds')
# plot(evc.df,breaks=c(0,2000,2001:2070,3001))
# atrribute.df <- evc.df@data@attributes[[1]]

# ############
sample.index <- readRDS('cache/ft.sample.index.rds')

sample.index <- sample.index[sample.index[,2]>2999,]
sample.index <- sample.index[sample.index[,2]<3999,]

# get sample details
evc.df <- raster('data/EVC_fuelType/evc/VICSANSW161.tif')

sample.df <- xyFromCell(evc.df,sample.index) 
sample.df <- as.data.frame(sample.df)
sample.df$ft <- sample.index[,2]

# 
library(rgdal)
# set the crs (GDA 94) that the data is in 
sincrs <- "+proj=lcc +lat_1=-36 +lat_2=-38 +lat_0=-37 +lon_0=145 +x_0=2500000 +y_0=2500000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs "
# GPS (WGS84)
lonlat <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0' 
# convert coords to spatial points
s <- SpatialPoints(cbind(sample.df$x, sample.df$y), 
                   proj4string=CRS(sincrs))
# transfor to gps
coords.new <- spTransform(s, lonlat)

sample.df$lat <- coords.new@coords[,2]
sample.df$long <- coords.new@coords[,1]

saveRDS(sample.df,'cache/sample.ft.rds')

# combisoil and met####

soil.density.ra <- raster('data/soil/BDW_000_005_EV_N_P_AU_TRN_N_20140801.tif')
# density
sample.df$soil.density <- extract(x = soil.density.ra,
                                  y =  cbind(sample.df$lon,sample.df$lat))

# clay
soil.clay.ra <- raster('data/soil/CLY_000_005_EV_N_P_AU_TRN_N_20140801.tif')
sample.df$clay <- extract(x = soil.clay.ra,
                          y =  cbind(sample.df$lon,sample.df$lat))

#get topo####
# shortwave radiation
rad.jan.ra <- raster('data/topo/rad/SRADTotalShortwaveSlopingSurf_0115_lzw.tif')
sample.df$rad.short.jan <- extract(x = rad.jan.ra,
                                   y =  cbind(sample.df$lon,
                                                             sample.df$lat))
rm(rad.jan.ra)

rad.jul.ra <- raster('data/topo/rad/SRADTotalShortwaveSlopingSurf_0715_lzw.tif')
sample.df$rad.short.jul <- extract(x = rad.jul.ra,
                                   y =  cbind(sample.df$lon,sample.df$lat))
rm(rad.jul.ra)

# get wi#####
wi.ra <- raster('data/topo/wetness_index/90m/twi_3s.tif')
sample.df$wi <- extract(x = wi.ra, 
                     y =  cbind(sample.df$lon,sample.df$lat))
rm(wi.ra)

# get profile_c
fn <- 'data/topo/curvature_profile/90m/profile_curvature_3s.tif'
c_profile.ra <- raster(fn)
sample.df$curvature_profile <- extract(x = c_profile.ra,
                                       y = cbind(sample.df$lon,sample.df$lat))

fn.c.plan <- 'data/topo/curvature_plan/90m/plan_curvature_3s.tif'
c_plan.ra <- raster(fn.c.plan)
sample.df$curvature_plan <- extract(x = c_plan.ra,
                                    y = cbind(sample.df$lon,sample.df$lat))

# add longterm climate######
pet.ls <- readRDS('data/met/pet.gps.rds')
library(raster)
# pet.ra <- raster(t(pet.ls[['pet']]),
#                  xmn=min(pet.ls[['lon']]), xmx=max(pet.ls[['lon']]), 
#                  ymn=min(pet.ls[['lat']]), ymx=max(pet.ls[['lat']]))
vpd.ra <- raster(t(readRDS('data/met/vpd.rds')),
                 xmn=min(pet.ls[['lon']]), xmx=max(pet.ls[['lon']]), 
                 ymn=min(pet.ls[['lat']]), ymx=max(pet.ls[['lat']]))

tmax.ra <- raster(t(readRDS('data/met/temperature.rds')),
                  xmn=min(pet.ls[['lon']]), xmx=max(pet.ls[['lon']]), 
                  ymn=min(pet.ls[['lat']]), ymx=max(pet.ls[['lat']]))
# plot(tmax.ra)
map.ra <- raster(t(pet.ls[['map']]),
                 xmn=min(pet.ls[['lon']]), xmx=max(pet.ls[['lon']]), 
                 ymn=min(pet.ls[['lat']]), ymx=max(pet.ls[['lat']]))

# 
sample.df$tmax.mean <- raster::extract(tmax.ra,cbind(sample.df$lon,sample.df$lat))
sample.df$map <- raster::extract(map.ra,cbind(sample.df$lon,sample.df$lat))
sample.df$vpd.mean <- raster::extract(vpd.ra,cbind(sample.df$lon,sample.df$lat))
# get rainfall seaonality
pr.seaon.ra <- readRDS('cache/pr_seaonality_silo.rds')

sample.df$pr.seaonality <- raster::extract(pr.seaon.ra,
                                        cbind(sample.df$lon,sample.df$lat))
# 
saveRDS(sample.df,'cache/ft.met.rds')

#####get lai for long term clim
# predict lai####
tmp.df <- readRDS('cache/ft.met.rds')
par_fraction <- 0.368
# 
# library("devtools")
# install_bitbucket("Jinyan_Jim_Yang/g1.opt.package.git")

tmp.df$PAR <- (tmp.df$rad.short.jan + tmp.df$rad.short.jul)/2 *par_fraction
get.vp.from.t.func <- function(temperature){
  # Magnus_pressure = 
  0.61094 * exp((17.625 * temperature) / (temperature + 243.04))
}


tmp.df$vpd <- tmp.df$vpd.mean
tmp.df$vpd[tmp.df$vpd<0.05] <- 0.05
library(g1.opt.func)
opt.out.ls <- list()
n.days <- 365.25
for (i in 1:nrow(tmp.df)) {
  tmp <- try(g1.lai.e.func(VPD = tmp.df$vpd[i],
                           E = tmp.df$map[i],
                           PAR = tmp.df$PAR[i]*n.days,
                           TMAX = tmp.df$tmax[i],
                           Ca = 400))
  if(class(tmp)=='try-error'){
    opt.out.ls[[i]] <- NA
  }else{
    opt.out.ls[[i]] <- tmp
  }
  print(i)
}

opt.out.df <- do.call(rbind,opt.out.ls)
tmp.df$lai.opt.mean <- opt.out.df[,'LAI']
saveRDS(tmp.df,'cache/ft.met.lai.rds')


# read soil depth####
tmp.df <- readRDS('cache/ft.met.lai.rds')

soil.depth.ra <- raster('data/soil/DER_000_999_EV_N_P_AU_NAT_C_20150601.tif')
# density
tmp.df$soil.depth <- extract(x = soil.depth.ra,
                                  y =  cbind(tmp.df$lon,tmp.df$lat))

saveRDS(tmp.df,'cache/ft.met.lai.rds')




