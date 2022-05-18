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
wrap.get.mon.clim.func <- function(model.path){
  # model.path <- 'data/met/future/BNU-ESM/'
 
  # 
  dir.vec <- list.dirs(model.path,recursive = F)
  
  rcp.file.vec <- list.files(path = model.path,
                             pattern = '.nc',recursive = T,full.names = T)
  tmp.1 <- rcp.file.vec[grep('_pr',x = rcp.file.vec)]
  tmp.2 <- rcp.file.vec[grep('_rhsmin',x = rcp.file.vec)]
  tmp.3 <- rcp.file.vec[grep('_tasmax',x = rcp.file.vec)]
  fn.vec <- c(tmp.1,tmp.2,tmp.3)
  
  # history
    fn.sub.his <- fn.vec[grep(pattern = 'historical',x = fn.vec)]
    dir.his <- dirname(fn.sub.his[1])
    dir.create(paste0(model.path,'history/'))
    get.mon.clim.func(fn.in = fn.sub.his[grep(pattern = '_pr',x = fn.sub.his)],
                      par.nm ='pr',
                      out.nm = paste0(dir.his,'/history/history_20002015_monthly_pr.rds'),
                      yr.in = 2000:2015,fun.im = mean)
    get.mon.clim.func(fn.in = fn.sub.his[grep(pattern = '_rhsmin',x = fn.sub.his)],
                      par.nm ='rhsmin',
                      out.nm = paste0(dir.his,'/history/history_20002015_monthly_rh.rds'),
                      yr.in = 2000:2015,fun.im = mean)
    get.mon.clim.func(fn.in = fn.sub.his[grep(pattern = '_tasmax',x = fn.sub.his)],
                      par.nm ='tasmax',
                      out.nm = paste0(dir.his,'/history/history_20002015_monthly_tmax.rds'),
                      yr.in = 2000:2015,fun.im = mean)

   # rcp45 and 85
    get4rcp.func <- function(rcp.in){
      dir.create(paste0(model.path,rcp.in,'_long/'))
      dir.create(paste0(model.path,rcp.in,'_mid/'))
      fn.sub.rcp45 <- fn.vec[grep(pattern = rcp.in,x = fn.vec)]
      
      for (rcp.45.i in seq_along(fn.sub.rcp45)) {
        
        fn.i <- fn.sub.rcp45[rcp.45.i]
        dir.i <- dirname(fn.i)
        if(length(grep(pattern = '2060',x = fn.i))>0){
          print('mid')
          yr.in = 2045:2060
          second.nm <- paste0('/',rcp.in,'_mid/',rcp.in,'_20452060')
          
        }else{
          print('long')
          yr.in = 2085:2100
          second.nm <- paste0('/',rcp.in,'_long/',rcp.in,'_20852100')
        }
        # 
        if(length(grep(pattern = '_tasmax',x = fn.i))>0){
          par.nm <- 'tasmax'
          par.out.nm <- 'tmax'
        }else if(length(grep(pattern = '_pr',x = fn.i))>0){
          par.nm <- 'pr'
          par.out.nm <- 'pr'
        }else if(length(grep(pattern = '_rhsmin',x = fn.i))>0){
          par.nm <- 'rhsmin'
          par.out.nm <- 'rh'
        }else{
          warning('input wrong?')
        }
        
        get.mon.clim.func(fn.in = fn.i,
                          par.nm =par.nm,
                          out.nm = paste0(dir.i,'/',second.nm,'_monthly_',par.out.nm,'.rds'),
                          yr.in = yr.in,fun.im = mean)
    }

    }
    
    get4rcp.func('rcp45')
    get4rcp.func('rcp85')

}
wrap.get.mon.clim.func('data/met/future/BNU-ESM/')
wrap.get.mon.clim.func(model.path = 'data/met/future/ACCESS1-0/')

wrap.get.mon.clim.func(model.path = 'data/met/future/CSIRO-Mk3-6-0/')
wrap.get.mon.clim.func(model.path = 'data/met/future/GFDL-CM3/')

wrap.get.mon.clim.func(model.path = 'data/met/future/GFDL-ESM2G/')
wrap.get.mon.clim.func(model.path = 'data/met/future/GFDL-ESM2M/')
wrap.get.mon.clim.func(model.path = 'data/met/future/inmcm4/')
wrap.get.mon.clim.func(model.path = 'data/met/future/IPSL-CM5A-LR/')
wrap.get.mon.clim.func(model.path = 'data/met/future/MRI-CGCM3/')

# use functions to get monthly data###
# # aceess 
# get.mon.clim.func(fn.in = 'data/met/future/access/maca_VIC_ACCESS1-0_rcp45_pr2017-2060.nc',
#                   par.nm ='pr',
#                   out.nm = 'data/met/future/access/rcp45_20452060_monthly_pr.rds',
#                   yr.in = 2045:2060,fun.im = mean)
# 
# get.mon.clim.func(fn.in = 'data/met/future/access/maca_VIC_ACCESS1-0_rcp45_rhsmin2017-2060.nc',
#                   par.nm ='rhsmin',
#                   out.nm = 'data/met/future/access/rcp45_20452060_monthly_rh.rds',
#                   yr.in = 2045:2060,fun.im = mean)
# 
# get.mon.clim.func(fn.in = 'data/met/future/access/maca_VIC_ACCESS1-0_rcp45_tasmax2017-2060.nc',
#                   par.nm ='tasmax',
#                   out.nm = 'data/met/future/access/rcp45_20452060_monthly_tmax.rds',
#                   yr.in = 2045:2060,fun.im = mean)
# ACCESS1-0 ####
# history
get.mon.clim.func(fn.in = 'data/met/future/ACCESS1-0/maca_VIC_ACCESS1-0_historical/maca_VIC_ACCESS1-0_historical_pr1972-2016.nc',
                  par.nm ='pr',
                  out.nm = 'data/met/future/ACCESS1-0/maca_VIC_ACCESS1-0_historical/history_20002015_monthly_pr.rds',
                  yr.in = 2000:2015,fun.im = mean)

get.mon.clim.func(fn.in = 'data/met/future/ACCESS1-0/maca_VIC_ACCESS1-0_historical/maca_VIC_ACCESS1-0_historical_tasmax1972-2016.nc',
                  par.nm ='tasmax',
                  out.nm = 'data/met/future/ACCESS1-0/maca_VIC_ACCESS1-0_historical/history_20002015_monthly_tmax.rds',
                  yr.in = 2000:2015,fun.im = mean)

get.mon.clim.func(fn.in = 'data/met/future/ACCESS1-0/maca_VIC_ACCESS1-0_historical/maca_VIC_ACCESS1-0_historical_rhsmin1972-2016.nc',
                  par.nm ='rhsmin',
                  out.nm = 'data/met/future/ACCESS1-0/maca_VIC_ACCESS1-0_historical/history_20002015_monthly_rh.rds',
                  yr.in = 2000:2015,fun.im = mean)

# rcp4.5 mid
get.mon.clim.func(fn.in = 'data/met/future/ACCESS1-0/maca_VIC_ACCESS1-0_rcp45_2017-2060/maca_VIC_ACCESS1-0_rcp45_pr2017-2060.nc',
                  par.nm ='pr',
                  out.nm = 'data/met/future/ACCESS1-0/rcp45_20452060_monthly_pr.rds',
                  yr.in = 2045:2060,fun.im = mean)

get.mon.clim.func(fn.in = 'data/met/future/ACCESS1-0/maca_VIC_ACCESS1-0_rcp45_2017-2060/maca_VIC_ACCESS1-0_rcp45_tasmax2017-2060.nc',
                  par.nm ='tasmax',
                  out.nm = 'data/met/future/ACCESS1-0/maca_VIC_ACCESS1-0_rcp45_2017-2060/rcp45_20452060_monthly_tmax.rds',
                  yr.in = 2045:2060,fun.im = mean)

get.mon.clim.func(fn.in = 'data/met/future/ACCESS1-0/maca_VIC_ACCESS1-0_rcp45_2017-2060/maca_VIC_ACCESS1-0_rcp45_rhsmin2017-2060.nc',
                  par.nm ='rhsmin',
                  out.nm = 'data/met/future/ACCESS1-0/maca_VIC_ACCESS1-0_rcp45_2017-2060/rcp45_20452060_monthly_rh.rds',
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