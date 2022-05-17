library(ncdf4)
library(lubridate)
library(raster)
# get monthly data for cmip####
get.mon.clim.func <- function(fn.in,par.nm,out.nm,yr.in,fun.im ){
 # read in climate
   # cfa.clim.file <- nc_open('data/met/future/access/maca_VIC_ACCESS1-0_rcp45_pr2017-2060.nc')
  cfa.clim.file <- nc_open(fn.in)
  # names(cfa.clim.file$var)
  cfa.clim.file$var$pr
  pr.array <- ncvar_get(cfa.clim.file,par.nm)
  lat <- ncvar_get(cfa.clim.file,'lat')
  lon <- ncvar_get(cfa.clim.file,'lon')
  days <- as.Date(ncvar_get(cfa.clim.file,'time'),
                  origin = gsub(pattern = 'days since ',replacement = '',x = cfa.clim.file$dim$time$units),
                  tz = 'GMT')
  
  
  # choose only yr of interest
  # choice.yr <- which(year(days) %in% 2045:2060)
  choice.yr <- which(year(days) %in% yr.in)

  pr.sub <- pr.array[,,choice.yr]
  days.sub <- days[choice.yr]
  # subset by month and get mean
  mon.pr<- list()
  for (mon.i in 1:12) {
    mon.choice <- which(month(days.sub) ==mon.i )
    
    tmp <-  apply(pr.sub[,,mon.choice], MARGIN = c(1,2), fun.im, na.rm = TRUE)
    tmp <- raster(t(tmp),
                  xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat))
    met.raster.right <- flip(tmp, direction='y')
    # plot(met.raster.right)
    mon.pr[[mon.i]] <- met.raster.right
  }
  plot(met.raster.right)
  # save output
  saveRDS(mon.pr,out.nm)
  # saveRDS(mon.pr,'data/met/future/access/rcp45_20452060_monthly_pr.rds')
}


# use functions to get monthly data###
# aceess 
get.mon.clim.func(fn.in = 'data/met/future/access/maca_VIC_ACCESS1-0_rcp45_pr2017-2060.nc',
                  par.nm ='pr',
                  out.nm = 'data/met/future/access/rcp45_20452060_monthly_pr.rds',
                  yr.in = 2045:2060,fun.im = mean)

get.mon.clim.func(fn.in = 'data/met/future/access/maca_VIC_ACCESS1-0_rcp45_rhsmin2017-2060.nc',
                  par.nm ='rhsmin',
                  out.nm = 'data/met/future/access/rcp45_20452060_monthly_rh.rds',
                  yr.in = 2045:2060,fun.im = mean)

get.mon.clim.func(fn.in = 'data/met/future/access/maca_VIC_ACCESS1-0_rcp45_tasmax2017-2060.nc',
                  par.nm ='tasmax',
                  out.nm = 'data/met/future/access/rcp45_20452060_monthly_tmax.rds',
                  yr.in = 2045:2060,fun.im = mean)

















# ########

cfa.clim.file <- nc_open('data/met/future/access/maca_VIC_ACCESS1-0_rcp45_pr2017-2060.nc')
# cfa.clim.file$dim$time$vals

pr.array <- ncvar_get(cfa.clim.file,'pr')
lat <- ncvar_get(cfa.clim.file,'lat')
lon <- ncvar_get(cfa.clim.file,'lon')
days <- as.Date(ncvar_get(cfa.clim.file,'time'),
                origin = gsub(pattern = 'days since ',replacement = '',x = cfa.clim.file$dim$time$units),
                tz = 'GMT')

choice.yr <- which(year(days) %in% 2045:2060)

# plot(raster(pr.array[,,1]))
# which(year(days) == 2030)

pr.sub <- pr.array[,,choice.yr]
days.sub <- days[choice.yr]

mon.pr<- list()
for (mon.i in 1:12) {
  mon.choice <- which(month(days.sub) ==mon.i )
  
  tmp <-  apply(pr.sub[,,mon.choice], MARGIN = c(1,2), mean, na.rm = TRUE)
  tmp <- raster(t(tmp),
                xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat))
  met.raster.right <- flip(tmp, direction='y')
  plot(met.raster.right)
  mon.pr[[i]] <- met.raster.right
}

saveRDS(mon.pr,'data/met/future/access/rcp45_20452060_monthly_pr.rds')

# # 
# yr.in <-2030
# if(yr.in%%4 == 0){
#   mon.days <- c(31,29,31,30,31,30,
#                 31,31,30,31,30,31)
# }else{
#   mon.days <- c(31,28,31,30,31,30,
#                 31,31,30,31,30,31)
# }


# mon.tmax <- list()
# for(i in seq_along(mon.days)){
#   
#   end.d <- sum(mon.days[1:i])
#   star.d <- end.d - mon.days[i]+1
#   tmp <-  apply(pr.array[,,star.d:end.d], MARGIN = c(1,2), sum, na.rm = TRUE)
#   # 
#   # tmp <-  apply(pr.array[,,1:365], MARGIN = c(1,2), sum, na.rm = TRUE)
# 
#   
#   # met.raster.right <- flip(raster(t(tmp)), direction='y')
#   # plot(met.raster.right)
#   tmp <- raster(t(tmp),
#                 xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat))
#   met.raster.right <- flip(tmp, direction='y')
#   # plot(met.raster.right)
#   mon.tmax[[i]] <- met.raster.right
#   
#   names(mon.tmax)[i] <- yr.in
# }
# mon.tmax$`2030`
# # 








# 
# 
# 
# 
# # mon.tmax[,,1] <-  apply(tmax.vic[,,1:31], MARGIN = c(1,2), max, na.rm = TRUE)
# 
# mon.tmax <- list()
# for(i in seq_along(mon.days)){
#   
#   end.d <- sum(mon.days[1:i])
#   star.d <- end.d - mon.days[i]+1
#   tmp <-  apply(tmax.vic[,,star.d:end.d], MARGIN = c(1,2), max, na.rm = TRUE)
#   tmp <- raster(t(tmp),
#                 xmn=132, xmx=150, ymn=-40, ymx=-29)
#   met.raster.right <- flip(tmp, direction='y')
#   # plot(met.raster.right)
#   mon.tmax[[i]] <- met.raster.right
# }
# 
# 
# temp.array <- abind::abind(pr.array, along=3)
# # Annual average  for every x,y location
# average <- rowMeans(temp.array, dims=2)*12*10
# 
# pr.ra <- raster(t(average),
#                 xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat))