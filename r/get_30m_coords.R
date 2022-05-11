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

lat <- seq(-40,-33,by = 0.0002777624)
  
lon <- seq(139,150,by = 0.0002777624)