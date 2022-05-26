# calculating rainfall seasonality follwoing Feng et al 2013
# 10.1038/nclimate1907

library(raster)
get.rain.seaonality.func <- function(fn.in){
  # read file
  # fn.in <- 'data/met/future/ACCESS1-0/history/history_20002015_monthly_pr.rds'
  
  pr.ra.ls <- readRDS(fn.in)
  # stack rasters
  pr.stack <- stack(pr.ra.ls)
  # get mean
  pr.mean <- calc(pr.stack, fun=mean)
  # get probability for each month
  pr.p.stack <- (pr.stack*365.25/12)/(pr.mean *365.25)
  # get the entropy
  d.stack <- pr.p.stack * log(pr.p.stack*12)
  
  d.sum <- calc(d.stack, fun=sum)
  # get index
  pr.seasonality.index <- d.sum * pr.mean / maxValue(pr.mean)
  
  # save outputs
  fn.out <- gsub(pattern = 'monthly_pr',replacement = 'pr_seasonality',x = fn.in)
  saveRDS(pr.seasonality.index,fn.out)
}
# do it for all rainfall data###
fn.vec <- list.files(path = 'data/met/future',
                     pattern = 'monthly_pr.rds',
                     full.names = T,recursive = T)
get.rain.seaonality.func

sapply(fn.vec,get.rain.seaonality.func)
## example plot
# fn.in <- 'data/met/future/ACCESS1-0/history/history_20002015_pr_seasonality.rds'
# plot(readRDS(fn.in))

# get mean among models####
fn.vec <- list.files(path = 'data/met/future',
                     pattern = 'pr_seasonality.rds',
                     full.names = T,recursive = T)

# 
get.seasonality.mean.func <- function(rcp.in){
  fn.hist <- fn.vec[grep(pattern = rcp.in,x = fn.vec)]
  
  pr.seaonality.ls <- sapply(fn.hist, readRDS)
  pr.seaonality.stack <- stack(pr.seaonality.ls)
  
  pr.seaonality.mean <- mean(pr.seaonality.stack)
  saveRDS(pr.seaonality.mean,sprintf('cache/pr_seaonality_%s.rds',rcp.in))
}

# history####
# fn.hist <- fn.vec[grep(pattern = 'history',x = fn.vec)]
# 
# pr.seaonality.ls <- sapply(fn.hist, readRDS)
# pr.seaonality.stack <- stack(pr.seaonality.ls)
# 
# pr.seaonality.mean <- mean(pr.seaonality.stack)
# saveRDS(pr.seaonality.mean,'cache/pr_seaonality_history.rds')

get.seasonality.mean.func(rcp.in = 'history')
# rcp####
get.seasonality.mean.func(rcp.in = 'rcp45_mid')
get.seasonality.mean.func(rcp.in = 'rcp45_long')
get.seasonality.mean.func(rcp.in = 'rcp85_long')
get.seasonality.mean.func(rcp.in = 'rcp85_mid')


# get silo seaonality####
# get file
met.nm <-  list.files(path = 'data/met/precip_silo/',
                      pattern = '.nc',full.names = T,recursive = F)  

get.rain.ra.func <- function(met.nm.in,mon.in){
  rain.nc <- nc_open(met.nm.in)
  # get gps
  lon.vec <- ncvar_get(rain.nc, 'lon')
  lat.vec <- ncvar_get(rain.nc, 'lat')
  # get rain
  rain.df <- try(ncvar_get(rain.nc, 'monthly_rain'))
  
  rain.mon.df <- rain.df[,,mon.in]
  rain.raster <- raster(t(rain.mon.df),
                        xmn=min(lon.vec), xmx=max(lon.vec), ymn=min(lat.vec), ymx=max(lat.vec))
  met.raster.right <- flip(rain.raster, direction='y')
  met.raster.right[is.infinite(met.raster.right)] <- NA

  return(met.mean)
}

# 
get.mean.func <- function(mon.in){
  rain.silo.1 <- sapply(met.nm,get.rain.ra.func,mon.in=mon.in)
  
  rain.silo.1.mean <- mean(stack(rain.silo.1))
  
  return(rain.silo.1.mean)
}
# 
rain.silo.seasnoality.ls <- sapply(1:12,get.mean.func)
pr.stack <- stack(rain.silo.seasnoality.ls)
# get mean
pr.mean <- calc(pr.stack, fun=sum)
# get probability for each month
pr.p.stack <- (pr.stack)/(pr.mean)
# get the entropy
d.stack <- pr.p.stack * log(pr.p.stack*12)

d.sum <- calc(d.stack, fun=sum)
# get index
pr.seasonality.index <- d.sum * pr.mean / maxValue(pr.mean)
# plot(pr.seasonality.index)

# ######

saveRDS(pr.seasonality.index,'cache/pr_seaonality_silo.rds')
