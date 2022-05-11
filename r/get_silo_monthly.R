# fn <- list.files('data/met/tmax_silo/',full.names = T)[1]

library(readr)
library(raster)

# get tmax monthly######
fn.vec <- list.files('data/met/tmax_silo/',full.names = T,pattern = '.nc')
for (fn.i in seq_along(fn.vec)) {
  
  fn <- fn.vec[fn.i]
  
  nc.tmp <- nc_open(fn)
  names(nc.tmp$var)
  tmax <- ncvar_get(nc.tmp,'max_temp')
  # tmax[tmax=='NAN'] <- NA
  
  lon.vec <-nc.tmp$dim$lon$vals
  lat.vec <-nc.tmp$dim$lat$vals
  
  date.vec <-as.Date(nc.tmp$dim$time$vals,
                       origin = gsub(pattern = 'days since ','',x = nc.tmp$dim$time$units),tz = 'GMT')
  # 
  # tmax[tmax=='NAN'] <- NA
  # 
  # tmax.df <- tmax[,,1:3]
  # rain.raster <- raster(t(tmax.df),
  #                       xmn=min(lon.vec), xmx=max(lon.vec), ymn=min(lat.vec), ymx=max(lat.vec))
  # met.raster.right <- flip(rain.raster, direction='y')
  # 
  # e <- extent(139, 150, -40, -33)
  # rc <- crop(met.raster.right, e)
  
  # 
select.lon <- which(lon.vec>132 & lon.vec < 150)
select.lat <- which(lat.vec> -40&lat.vec< -29)

tmax.vic <- tmax[select.lon,select.lat,]
  

  yr.in <- parse_number(fn)*10000
  
  if(yr.in%%4 == 0){
    mon.days <- c(31,29,31,30,31,30,
                  31,31,30,31,30,31)
  }else{
    mon.days <- c(31,28,31,30,31,30,
                  31,31,30,31,30,31)
  }
  
  mon.tmax <- array(data = NA,dim = c(219, 139,12))
  
  
  # mon.tmax[,,1] <-  apply(tmax.vic[,,1:31], MARGIN = c(1,2), max, na.rm = TRUE)

  mon.tmax <- list()
  for(i in seq_along(mon.days)){
    
    end.d <- sum(mon.days[1:i])
    star.d <- end.d - mon.days[i]+1
   tmp <-  apply(tmax.vic[,,star.d:end.d], MARGIN = c(1,2), max, na.rm = TRUE)
   tmp <- raster(t(tmp),
                xmn=132, xmx=150, ymn=-40, ymx=-29)
   met.raster.right <- flip(tmp, direction='y')
   # plot(met.raster.right)
   mon.tmax[[i]] <- met.raster.right
  }
  
  fn.out <- sprintf('data/met/tmax_silo/tmax.%s.rds',yr.in)
  
  saveRDS(mon.tmax,fn.out)
}
#monthly rh#####
# 
fn.vec <- list.files('data/met/rh_silo/',full.names = T,pattern = '.nc')
for (fn.i in seq_along(fn.vec)) {
  
  fn <- fn.vec[fn.i]
  
  nc.tmp <- nc_open(fn)
  names(nc.tmp$var)
  tmax <- ncvar_get(nc.tmp,'rh_tmax')
  # tmax[tmax=='NAN'] <- NA
  
  lon.vec <-nc.tmp$dim$lon$vals
  lat.vec <-nc.tmp$dim$lat$vals
  
  date.vec <-as.Date(nc.tmp$dim$time$vals,
                     origin = gsub(pattern = 'days since ','',x = nc.tmp$dim$time$units),tz = 'GMT')
  # 
  # tmax[tmax=='NAN'] <- NA
  # 
  # tmax.df <- tmax[,,1:3]
  # rain.raster <- raster(t(tmax.df),
  #                       xmn=min(lon.vec), xmx=max(lon.vec), ymn=min(lat.vec), ymx=max(lat.vec))
  # met.raster.right <- flip(rain.raster, direction='y')
  # 
  # e <- extent(139, 150, -40, -33)
  # rc <- crop(met.raster.right, e)
  
  # 
  select.lon <- which(lon.vec>132 & lon.vec < 150)
  select.lat <- which(lat.vec> -40&lat.vec< -29)
  
  tmax.vic <- tmax[select.lon,select.lat,]
  
  
  yr.in <- parse_number(fn)*10000
  
  if(yr.in%%4 == 0){
    mon.days <- c(31,29,31,30,31,30,
                  31,31,30,31,30,31)
  }else{
    mon.days <- c(31,28,31,30,31,30,
                  31,31,30,31,30,31)
  }
  
  # mon.tmax <- array(data = NA,dim = c(219, 139,12))
  
  
  # mon.tmax[,,1] <-  apply(tmax.vic[,,1:31], MARGIN = c(1,2), max, na.rm = TRUE)
  
  mon.tmax <- list()
  for(i in seq_along(mon.days)){
    
    end.d <- sum(mon.days[1:i])
    star.d <- end.d - mon.days[i]+1
    tmp <-  apply(tmax.vic[,,star.d:end.d], MARGIN = c(1,2), min, na.rm = TRUE)
    tmp <- raster(t(tmp),
                  xmn=132, xmx=150, ymn=-40, ymx=-29)
    met.raster.right <- flip(tmp, direction='y')
    # plot(met.raster.right)
    mon.tmax[[i]] <- met.raster.right
  }
  
  fn.out <- sprintf('data/met/rh_silo/rh.%s.rds',yr.in)
  
  saveRDS(mon.tmax,fn.out)
  
}
