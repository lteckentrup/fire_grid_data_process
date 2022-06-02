source('r/functions_plot.R')
source('r/get_vic_shape.R')
library(raster)
# 
var.in.nm = '_hz_elevated.rds'
future_s = 'history'

save.tif.func <- function(var.in.nm,future_s){
  # read predictions
  hz.ele.his <- read.future.all.func(var.in.nm = var.in.nm,
                                     future_s = future_s,
                                     exclude.nm = 'noVeg')
  # get mode
  ra.mode.his <- calc(hz.ele.his, fun=Mode.func)
  ra.mode.his <- mask(ra.mode.his, shape.vic)

  # save
  tmp.var.nm <- gsub(pattern = '.rds',replacement = '',var.in.nm)
  out.nm <- sprintf('outputs/%s/%s/%s.tif',
                    future_s,
                    tmp.var.nm,
                    tmp.var.nm)
  # create folers for outputs
  dir.create('outputs')
  dir.create(gsub(pattern = paste0(tmp.var.nm,'.tif'),
                  replacement = '',x = out.nm))
  # dir.create('outputs/rcp45_mid')
  # dir.create('outputs/rcp45_long')
  # dir.create('outputs/rcp85_mid')
  # dir.create('outputs/rcp85_long')
  
  print(out.nm)
  writeRaster(ra.mode.his,out.nm,options=c('TFW=YES'))
}
# 'data/met/future/BNU-ESM/rcp45_mid/_fuelType.rds'
wrap.save.func <- function(future_s){
  save.tif.func(var.in.nm = 'hz_elevated.rds',future_s = future_s)
  save.tif.func(var.in.nm = 'hz_ns.rds',future_s = future_s)
  save.tif.func(var.in.nm = 'hz_surface.rds',future_s = future_s)
  save.tif.func(var.in.nm = 'hz_bark.rds',future_s = future_s)
  save.tif.func(var.in.nm = 'height_canopy.rds',future_s = future_s)
  save.tif.func(var.in.nm = 'height_ns.rds',future_s = future_s)
  save.tif.func(var.in.nm = 'fuelType.rds',future_s = future_s)
}
wrap.save.func('history')

wrap.save.func('rcp45_mid')
wrap.save.func('rcp45_long')
wrap.save.func('rcp85_mid')
wrap.save.func('rcp85_long')
