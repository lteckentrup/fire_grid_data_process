library(raster)

get.small.area.func <- function(ra.in,p = p){
  r2 <- crop(ra.in, extent(p))
  r3 <- mask(r2, p)
  # crs(r3) <- crs(ra.in)
  # aggregate(r3, fact=3)
  return(r3)
}

# functions####
source('r/functions_predict.R')
source('r/get_vic_shape.R')
# read topo####
# 
c.small <- raster('data/topo/curvature_profile/90m/profile_curvature_3s.tif')
c.vic <- get.small.area.func(c.small,shape.vic)
# plot(c.vic)

lcm.ra <- raster('E:/storage/equilibrium-lai-for-australia/downloads/LCM/luav4g9abll07811a02egigeo___/lu05v4ag/w001001.adf')
lcm.vic <- get.small.area.func(lcm.ra,shape.vic)
lcm.lut <- lcm.vic@data@attributes[[1]]
lcm.lut.natrua <- lcm.lut$ID[lcm.lut$LU_DESC %in% c('CONSERVATION AND NATURAL ENVIRONMENTS',
                                                    'PRODUCTION FROM RELATIVELY NATURAL ENVIRONMENTS')]


lcm.vic[!(lcm.vic %in% lcm.lut.natrua)] <- NA
plot(lcm.vic)


# 
lcm.vic.fine <- resample(lcm.vic,c.vic)
saveRDS(lcm.vic.fine,'cache/lcm.fine.rds')
# 
lcm.vic.fine <- readRDS('cache/lcm.fine.rds')
plot(lcm.vic.fine)

c.vic.sub <- mask(c.vic,lcm.vic.fine)
plot(c.vic.sub)
# vic(add=T)
cellStats(nonNA_raster, sum)
