library(raster)
#read future climate###
pr.df <- readRDS('data/met/future/access/rcp45_20452060_monthly_pr.rds')

# func to reporoject soil and topo to climate resolotion
proj.func <- function(ra.in,ra.target,fn.out,ifSave=TRUE){
  
  crs(ra.in)<- crs(ra.target) <- '+init=epsg:4326'
  
  rad.small.jan.ra <- crop(ra.in, extent(ra.target))
 
  rad.small.jan.ra.ag.clim <- projectRaster(from=rad.small.jan.ra,
                                            to=ra.target, method="bilinear")
  if(ifSave){
    saveRDS(rad.small.jan.ra.ag.clim,fn.out)
  }else{
    return(rad.small.jan.ra.ag.clim)
  }
  
}

# get rad####
rad.jan.ra <- raster('data/topo/rad/SRADTotalShortwaveSlopingSurf_0115_lzw.tif')
rad.jul.ra <- raster('data/topo/rad/SRADTotalShortwaveSlopingSurf_0715_lzw.tif')
# get vic subset first#
rad.small.jan.ra <- crop(rad.jan.ra, extent(pr.df[[1]]))
rad.small.jul.ra <- crop(rad.jul.ra, extent(pr.df[[1]]))
# ser crs
crs(rad.small.jan.ra) <- crs(rad.small.jul.ra)<- crs(pr.df[[1]]) <- '+init=epsg:4326'

# repoject
rad.small.jan.ra.ag.clim <- projectRaster(from=rad.small.jan.ra,
                                          to=pr.df[[1]], method="bilinear")

rad.small.jul.ra.ag.clim <- projectRaster(from=rad.small.jul.ra,
                                          to=pr.df[[1]], method="bilinear")
# save data
saveRDS(rad.small.jan.ra.ag.clim,'cache/rad_jan_5km.rds')
saveRDS(rad.small.jul.ra.ag.clim,'cache/rad_jul_5km.rds')

#get c plan#########
fn.vec <- list.files(path = 'data/topo/curvature_plan/PlanCurvature_1_arc-second_resolution/tiles/',
                 pattern = 'w001001.adf',recursive = T,full.names = T)


tmp.ra.ls <- list()
for (ra.i in seq_along(fn.vec)) {
  c_plan.ra <- raster(fn.vec[ra.i])
  tmp.ra.check <- try(proj.func(ra.in = c_plan.ra,
                                                ra.target = pr.df[[1]],
                                                fn.out = '',ifSave = F))
  
  if(class(tmp.ra.check) != 'try-error'){
    tmp.ra.ls[[length(tmp.ra.ls)+1]] <- tmp.ra.check
  }
  
}
# 
c.plan.ra <- do.call(merge,tmp.ra.ls)

saveRDS(c.plan.ra,'cache/curvature_plan_vic_5km.rds')

#get wi#########
fn.vec <- list.files(path = 'data/topo/wetness_index/',
                     pattern = 'w001001.adf',recursive = T,full.names = T)


tmp.ra.ls <- list()
for (ra.i in seq_along(fn.vec)) {
  c_plan.ra <- raster(fn.vec[ra.i])
  tmp.ra.check <- try(proj.func(ra.in = c_plan.ra,
                                ra.target = pr.df[[1]],
                                fn.out = '',ifSave = F))
  
  if(class(tmp.ra.check) != 'try-error'){
    tmp.ra.ls[[length(tmp.ra.ls)+1]] <- tmp.ra.check
  }
  
}
# 
wi.ra <- do.call(merge,tmp.ra.ls)

saveRDS(wi.ra,'cache/wi_vic_5km.rds')

# get c profile#####
c.profile.nm <- raster('data/topo/curvature_profile/profile_curvature_3s.tif')

c.small <- proj.func(ra.in = c.profile.nm,
                     ra.target = pr.df[[1]],
                     fn.out = '',ifSave = F)

saveRDS(c.small,'cache/curvature_profile_vic_5km.rds')

# get soil den#####
soil.den <-  raster('data/soil/BDW_000_005_EV_N_P_AU_TRN_N_20140801.tif')

soil.den.small <- proj.func(ra.in = soil.den,
                     ra.target = pr.df[[1]],
                     fn.out = '',ifSave = F)

saveRDS(soil.den.small,'cache/density_vic_5km.rds')
plot(soil.den.small)

# get soil ph#####
soil.ph <-  raster('data/soil/pHc_000_005_EV_N_P_AU_NAT_C_20140801.tif')

soil.ph.small <- proj.func(ra.in = soil.ph,
                            ra.target = pr.df[[1]],
                            fn.out = '',ifSave = F)

saveRDS(soil.ph.small,'cache/ph_vic_5km.rds')

# get soil ph#####
soil.clay.ra <- raster('data/soil/CLY_000_005_EV_N_P_AU_TRN_N_20140801.tif')

soil.clay.small <- proj.func(ra.in = soil.clay.ra,
                           ra.target = pr.df[[1]],
                           fn.out = '',ifSave = F)

saveRDS(soil.clay.small,'cache/clay_vic_5km.rds')






























###############
library(oz)
vic.lines <- ozRegion(sections=5,states=T)
vic.shape <- Polygon(as.matrix(data.frame(x = vic.lines$lines[[1]]$x,
                                                  y = vic.lines$lines[[1]]$y)))


plot(vic.shape)
plot(crop(x = wi.ra,y = vic.shape))
crop(x = wi.ra,y = vic.shape)