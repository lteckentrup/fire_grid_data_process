
library(raster)
# topo####
c.small <- raster('data/topo/curvature_profile/90m/profile_curvature_3s.tif')
c.plan.ra.i <- raster('data/topo/curvature_plan/90m/plan_curvature_3s.tif')

c.small.vic <- get.small.area.func(c.small,p = shape.vic)
c.plan.vic <- get.small.area.func(c.plan.ra.i,p = shape.vic)
# 
wi.ra <- raster('data/topo/wetness_index/90m/twi_3s.tif')
wi.vic <- get.small.area.func(wi.ra,p = shape.vic)
# 
rad.jan <- raster('data/topo/rad/SRADTotalShortwaveSlopingSurf_01_3s_lzw.tif')
rad.jan.vic <- get.small.area.func(rad.jan,p = shape.vic)
# 
rad.jul <- raster('data/topo/rad/SRADTotalShortwaveSlopingSurf_07_3s_lzw.tif')
rad.jul.vic <- get.small.area.func(rad.jul,p = shape.vic)
# read soil####
soil.den <- raster('data/soil/BDW_000_005_EV_N_P_AU_TRN_N_20140801.tif')
soil.den.vic <- get.small.area.func(soil.den,p = shape.vic)
soil.clay <- raster('data/soil/CLY_000_005_EV_N_P_AU_TRN_N_20140801.tif')
soil.clay.vic <- get.small.area.func(soil.clay,p = shape.vic)
soil.depth <- raster('data/soil/DER_000_999_EV_N_P_AU_NAT_C_20150601.tif')
soil.depth.vic <- get.small.area.func(soil.depth,p = shape.vic)
