# library(raster)
# 
# fn.vec <- list.files(path = 'data/topo/curvature_plan/PlanCurvature_1_arc-second_resolution/tiles/',pattern = 'w001001.adf',
#                      full.names = T,recursive = T)
# 
# get.gps.func <- function(fn){
#   tile.ra <- raster(fn)
#   projection(tile.ra)
#   tile.ra@
# }
# 
# nc.

# lat <- seq(-40,-33,by = 0.0002777624*2)
#   
# lon <- seq(139,150,by = 0.0002777624*2)


library(raster)

soil.density.ra <- raster('data/soil/BDW_000_005_EV_N_P_AU_TRN_N_20140801.tif')
e <- as(extent(140,150, -39.2, -33), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
r <- crop(soil.density.ra, e)
plot(r)
df.soil <- as.data.frame(r, xy=TRUE)
saveRDS(df.soil,'density.rds')
rm(soil.density.ra)
# 
df.soil <- readRDS('density.rds')
names(df.soil) <- c('lon','lat','soil.density')

# df.soil$ <- paste0(floor(df.soil$lon),
#                         '~',
#                         ceiling(df.soil$lat))

# sites.lon.lat <- unique(sites.lon.lat)

# get soil strributes####

# density
# har.score.df.gps$soil.density <- extract(x = soil.density.ra,y =  cbind(har.score.df.gps$lon,har.score.df.gps$lat))

# ph
soil.ph.ra <- raster('data/soil/pHc_000_005_EV_N_P_AU_NAT_C_20140801.tif')
df.soil$ph <- extract(x = soil.ph.ra,y =  cbind(df.soil$lon,df.soil$lat))
rm(soil.ph.ra)
# clay
soil.clay.ra <- raster('data/soil/CLY_000_005_EV_N_P_AU_TRN_N_20140801.tif')
har.score.df.gps$clay <- extract(x = soil.clay.ra,y =  cbind(har.score.df.gps$lon,har.score.df.gps$lat))
rm(soil.clay.ra)

#get topo####
# shortwave radiation
rad.jan.ra <- raster('data/topo/rad/SRADTotalShortwaveSlopingSurf_0115_lzw.tif')
har.score.df.gps$rad.short.jan <- extract(x = rad.jan.ra,y =  cbind(har.score.df.gps$lon,har.score.df.gps$lat))
rm(rad.jan.ra)

rad.jul.ra <- raster('data/topo/rad/SRADTotalShortwaveSlopingSurf_0715_lzw.tif')
har.score.df.gps$rad.short.jul <- extract(x = rad.jul.ra,y =  cbind(har.score.df.gps$lon,har.score.df.gps$lat))
rm(rad.jul.ra)
saveRDS(har.score.df.gps,'cache/hazardScoreWithSoil.rds')

# get wi#####
library(ncdf4)
library(abind)
har.score.df.gps$east <- paste0('e',floor(har.score.df.gps$lon))
har.score.df.gps$south <- paste0('s',abs(floor(har.score.df.gps$lat)))

# remove duplicated sites
gps.df <- har.score.df.gps[,c('lon','lat','east','south')]
# gsp.m <-  cbind(gps.df$lon,har.score.df.gps$lat)

gps.df <- gps.df[!duplicated(gps.df),]
# wetness index calculated as specific catchment area / slope
gps.df$wi <- NA
for (i in 1:nrow(gps.df)) {
  fn <- file.path('data/topo/wetness_index/TopographicWetnessIndex_1_arcsecond_resolution/tiles',
                  gps.df$east[i],
                  gps.df$south[i],
                  paste0(gps.df$east[i],gps.df$south[i]),
                  'w001001.adf')
  wi.ra <- raster(fn)
  print(i)
  gps.df$wi[i] <- extract(x = wi.ra,y = cbind(gps.df$lon[i],gps.df$lat[i]))
  
  
}
saveRDS(gps.df,'cache/wetnessByGPs.rds')

# add curvature####
# get profile_c
fn <- 'data/topo/curvature_profile/profile_curvature_3s.tif'
c_profile.ra <- raster(fn)
gps.df$curvature_profile <- extract(x = c_profile.ra,y = cbind(gps.df$lon,gps.df$lat))
# get plan_c
for (i in 1:nrow(gps.df)) {
  fn <- file.path('data/topo/curvature_plan/PlanCurvature_1_arc-second_resolution/tiles/',
                  gps.df$east[i],
                  gps.df$south[i],
                  paste0(gps.df$east[i],gps.df$south[i]),
                  'w001001.adf')
  c_plan.ra <- raster(fn)
  print(i)
  gps.df$wi[i] <- extract(x = c_plan.ra,y = cbind(gps.df$lon[i],gps.df$lat[i]))
  
  
  
}

saveRDS(gps.df,'cache/wetness_curvature_ByGPs.rds')