
library(raster)
# topo####
c.small <- raster('data/topo/curvature_profile/90m/profile_curvature_3s.tif')
c.plan.ra.i <- raster('data/topo/curvature_plan/90m/plan_curvature_3s.tif')
# 
wi.ra <- raster('data/topo/wetness_index/90m/twi_3s.tif')
# 

rad.jan <- raster('data/topo/rad/SRADTotalShortwaveSlopingSurf_0115_lzw.tif')

# 
rad.jul <- raster('data/topo/rad/SRADTotalShortwaveSlopingSurf_0715_lzw.tif')

# read soil####
soil.den <- raster('data/soil/BDW_000_005_EV_N_P_AU_TRN_N_20140801.tif')

soil.clay <- raster('data/soil/CLY_000_005_EV_N_P_AU_TRN_N_20140801.tif')

