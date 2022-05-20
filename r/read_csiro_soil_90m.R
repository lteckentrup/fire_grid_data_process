har.score.df.gps <- read.csv('cache/hazard.score.gps.csv')
# range(har.score.df.gps$lon)
# range(har.score.df.gps$lat)
har.score.df.gps <- har.score.df.gps[order(har.score.df.gps$lon),]

sites.lon.lat <- paste0(floor(har.score.df.gps$lon),
                        '~',
                        ceiling(har.score.df.gps$lat))

# sites.lon.lat <- unique(sites.lon.lat)

# get soil strributes####
library(raster)
soil.density.ra <- raster('data/soil/BDW_000_005_EV_N_P_AU_TRN_N_20140801.tif')
# density
har.score.df.gps$soil.density <- extract(x = soil.density.ra,y =  cbind(har.score.df.gps$lon,har.score.df.gps$lat))
rm(soil.density.ra)
# ph
soil.ph.ra <- raster('data/soil/pHc_000_005_EV_N_P_AU_NAT_C_20140801.tif')
har.score.df.gps$ph <- extract(x = soil.ph.ra,y =  cbind(har.score.df.gps$lon,har.score.df.gps$lat))
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
wi.ra <- raster('data/topo/wetness_index/90m/twi_3s.tif')
gps.df$wi <- extract(x = wi.ra, 
                     y =  cbind(gps.df$lon,gps.df$lat))
rm(wi.ra)
# saveRDS(har.score.df.gps,'cache/wetnessByGPs.rds')
# get profile_c
fn <- 'data/topo/curvature_profile/profile_curvature_3s.tif'
c_profile.ra <- raster(fn)
gps.df$curvature_profile <- extract(x = c_profile.ra,y = cbind(gps.df$lon,gps.df$lat))

fn.c.plan <- 'data/topo/curvature_plan/90m/plan_curvature_3s.tif'
c_plan.ra <- raster(fn.c.plan)
gps.df$curvature_plan <- extract(x = c_plan.ra,y = cbind(gps.df$lon,gps.df$lat))
saveRDS(gps.df,'cache/wetness_curvature_ByGPs.rds')
# saveRDS(gps.df,'cache/wetness_curvature_ByGPs.rds')
# gps.df$wi <- NA
# for (i in 1:nrow(gps.df)) {
#   fn <- file.path('data/topo/wetness_index/TopographicWetnessIndex_1_arcsecond_resolution/tiles',
#                   gps.df$east[i],
#                   gps.df$south[i],
#                   paste0(gps.df$east[i],gps.df$south[i]),
#                   'w001001.adf')
#   wi.ra <- raster(fn)
#   print(i)
#   gps.df$wi[i] <- extract(x = wi.ra,y = cbind(gps.df$lon[i],gps.df$lat[i]))
#   
#   
# }
# saveRDS(gps.df,'cache/wetnessByGPs.rds')

# add curvature####
# get profile_c
# fn <- 'data/topo/curvature_profile/profile_curvature_3s.tif'
# c_profile.ra <- raster(fn)
# gps.df$curvature_profile <- extract(x = c_profile.ra,y = cbind(gps.df$lon,gps.df$lat))
# # get plan_c
# for (i in 1:nrow(gps.df)) {
#   fn <- file.path('data/topo/curvature_plan/PlanCurvature_1_arc-second_resolution/tiles/',
#                   gps.df$east[i],
#                   gps.df$south[i],
#                   paste0(gps.df$east[i],gps.df$south[i]),
#                   'w001001.adf')
#   c_plan.ra <- raster(fn)
#   print(i)
#   gps.df$wi[i] <- extract(x = c_plan.ra,y = cbind(gps.df$lon[i],gps.df$lat[i]))
#   
#   
#   
# }























#########################################################################
# use the slga package to read data directly (90m)
# issue is doesn't work with large volumn
#########################################################################

# install package###
# list.dirs('data/topo/Slope/')
# devtools::install_github("obrl-soil/slga")

library(raster)
library(slga)
# library(ggplot2)

# get surface clay content for King Island####
aoi <- c(143.12587026293875, -37.00582938760682, 
         143.39778188724188, -37.218137343957784)

bne_surface_clay <- get_soils_data(product = 'NAT', attribute = 'CLY',
                                  component = 'ALL', depth = 1,
                                  aoi = aoi, write_out = FALSE)

bne_surface_clay <- get_soils_point(product = 'TSJAN ', attribute = 'CLY',
                                   component = 'VAL', depth = 1,
                                   poi  = c(145.36842674400233,-36.98995581408727))

aoi <- c(152.95, -27.55, 153.87, -27.45)

aoi <- c(143.12587026293875, -37.20582938760682, 
         143.39778188724188, -37.018137343957784)

# bne_surface_swr <- get_lscape_data(product = 'TSJAN', 
#                                    aoi = aoi, 
#                                    write_out = FALSE)

gsp.m <-  cbind(har.score.df.gps$lon,har.score.df.gps$lat)

gsp.m <- gsp.m[!duplicated(gsp.m),]

swr.jan.df <- get_lscape_data(product = 'TSJAN',aoi = c(min(har.score.df.gps$lon,na.rm = T),
                                          min(har.score.df.gps$lat,na.rm = T),
                                          max(har.score.df.gps$lon,na.rm = T),
                                          max(har.score.df.gps$lat,na.rm = T)),write_out = F)

swr.jan.ls <- list()
swr.jul.ls <- list()
for (i in 1:nrow(har.score.df.gps)) {
  swr.jan.ls[[i]] <- get_lscape_point(product = 'TSJAN',poi =  cbind(har.score.df.gps$lon,har.score.df.gps$lat)[i,])
  swr.jul.ls[[i]] <- get_lscape_point(product = 'TSJUL',poi =  cbind(har.score.df.gps$lon,har.score.df.gps$lat)[i,])
}

# swr.jan.ls <- apply(X = cbind(har.score.df.gps$lon,har.score.df.gps$lat)[,],FUN = get_lscape_point,product = 'TSJAN',MARGIN = 1)
# 
# swr.jul.ls <- apply(X = cbind(har.score.df.gps$lon,har.score.df.gps$lat),FUN = get_lscape_point,product = 'TSJUL',MARGIN = 1)


plot(bne_surface_swr)
slga_product_info()
slga_attribute_info 

