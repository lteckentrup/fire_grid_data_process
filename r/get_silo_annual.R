# get annual######
get.annual.slio.func <- function(fn.path,var.nm,my.fun){
  # # 
  # 
  # fn.path = 'data/met/tmax_silo/',
  # var.nm = 'max_temp',
  # my.fun = mean
  # # 
  
  fn.vec <- list.files(fn.path,full.names = T,pattern = '.nc')
  met.ls <- list()
  for (fn.i in seq_along(fn.vec)) {
    
    fn <- fn.vec[fn.i]
    
    nc.tmp <- nc_open(fn)
    names(nc.tmp$var)
    tmax <- ncvar_get(nc.tmp,var.nm)
    # 
    lon.vec <-nc.tmp$dim$lon$vals
    lat.vec <-nc.tmp$dim$lat$vals
    # 
    select.lon <- which(lon.vec>132 & lon.vec < 150)
    select.lat <- which(lat.vec> -40&lat.vec< -29)
    # 
    tmax.vic <- tmax[select.lon,select.lat,]
    tmp <- apply(tmax.vic, MARGIN = c(1,2), max, na.rm = TRUE)
    # 
    tmp.ra <- flip(t(raster((tmp))))
    extent(tmp.ra) <- c(xmn=132, xmx=150, ymn=-40, ymx=-29)
    
    met.ls[[fn.i]] <- tmp.ra
  }
  
  met.ra.stack <- stack(met.ls)
  met.ra.mean <- mean(met.ra.stack)
 return(met.ra.mean)
}
tmax.ra <- get.annual.slio.func(fn.path = 'data/met/tmax_silo/',
                                var.nm = 'max_temp',
                                my.fun = mean)


# rh.ra <- get.annual.slio.func(fn.path = 'data/met/rh_silo/',
#                                 var.nm = 'rh_tmax',
#                                 my.fun = mean)

prcp.ra <- get.annual.slio.func(fn.path = 'data/met/precip_silo/',
                              var.nm = 'monthly_rain',
                              my.fun = mean)

vpd.ra <- get.annual.slio.func(fn.path = 'data/met/vpd_silo/',
                                var.nm = 'vp_deficit',
                                my.fun = mean)
plot(vpd.ra/10)
saveRDS(stack(list(tmax = tmax.ra,
                   # rh = rh.ra,
                   prcp=prcp.ra*12,#month mean to annual
                   vpd = vpd.ra/10)),#hPa to kPa
        'data/met/slio.met.annual.rds')
