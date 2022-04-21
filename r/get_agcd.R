# get met for coord#####
har.score.df.gps <- read.csv('cache/hazard.score.gps.csv')
# range(har.score.df.gps$lon)
# range(har.score.df.gps$lat)
har.score.df.gps <- har.score.df.gps[order(har.score.df.gps$lon),]

sites.lon.lat <- paste0(floor(har.score.df.gps$lon),
                        '~',
                        ceiling(har.score.df.gps$lat))
har.score.df.gps$east <- paste0('e',floor(har.score.df.gps$lon))
har.score.df.gps$south <- paste0('s',abs(floor(har.score.df.gps$lat)))

# remove duplicated sites
gps.date.df <- har.score.df.gps[,c('lon','lat','east','south','Year','month')]
gps.date.df <- gps.date.df[!duplicated(gps.date.df),]

# extract met 
gps.date.df$previous.month <- as.Date(paste0(gps.date.df$Year,'-',
                                             gps.date.df$month,'-1')) -1

gps.date.df$previous.month <- format(gps.date.df$previous.month,format = '%Y.%m')
gps.date.df <- gps.date.df[!is.na(gps.date.df$previous.month),]

# get met from chelsa####
# library(lubridate)
library(raster)
library(ncdf4)
get.agcd.met.func <- function(dat,met.nm){
  
  if(met.nm == "rain"){
    met.var = 'precip'
    lat.nm <- 'lat'
    lon.nm <- 'lon'
  }else if(met.nm == "vph15"){
    met.var <- 'vapourpres'
    lat.nm <- 'lat'
    lon.nm <- 'lon'
  }else if (met.nm == "tmax"){
    met.var <- 'tmax'
    lat.nm <- 'lat'
    lon.nm <- 'lon'
  }else{
    stop('met var not in given list')
  }
  file.nm <- sprintf("data/met/%s/%s.%s.nc",
                         met.nm,met.nm,
                         substr(dat$previous.month[1],1,4))
  # # 
  # fn <- sprintf("data/met/%s/%s.%s.nc",
  #                    'vph15','vph15',
  #                   2000)

  # 
  read.func <- function(fn){
    
    nc_data <- nc_open(fn)
    # names(nc_data$var)
    lon.vec <- try(ncvar_get(nc_data, lon.nm))
    
    if(class(lon.vec) == 'try-error'){
      lon.vec <- ncvar_get(nc_data, 'longitude')
      lat.vec <- ncvar_get(nc_data, 'latitude')
    }else{
      lat.vec <- ncvar_get(nc_data, lat.nm)
    }
    
    # 
    met.df <- try(ncvar_get(nc_data, met.var))
    
    
    if(class(met.df) != 'try-error'){
      # array <- abind(met.df, along=3)
      # Annual average VP for every x,y location
      # mean.month <- rowMeans(array, dims=2,na.rm=T)
      
      
      met.raster <- raster(t(met.df[,,unique(dat$month)]),
                           xmn=min(lon.vec), xmx=max(lon.vec), ymn=min(lat.vec), ymx=max(lat.vec))
      
      # fix the projection issue
      met.raster.right <- flip(met.raster, direction='y')
     

      value.tar <- extract(x = met.raster.right,y = cbind(dat$lon,dat$lat),method = 'bilinear')
      # plot((met.raster))
      # points(cbind(dat$lon,dat$lat))
    }else{
      value.tar <- NA
    }
    
    # rm(met.df)
    return(value.tar)
  }
  
  return(read.func(fn=file.nm))
}
# 
get.rain.silo.func <- function(lat,lon,yr.mon){
  # separate yr and mon
  yr.in <- strsplit(x = yr.mon,split = '[.]')[[1]][1]
  mon.in <- as.numeric(strsplit(x = yr.mon,split = '[.]')[[1]][2])
  # get file
  met.nm <-  sprintf('data/met/precip_silo/precip.%s.nc',yr.in)
  
  rain.nc <- nc_open(met.nm)
# get gps
  lon.vec <- ncvar_get(rain.nc, 'lon')
  lat.vec <- ncvar_get(rain.nc, 'lat')
  # get rain
  rain.df <- try(ncvar_get(rain.nc, 'monthly_rain'))
  
  rain.mon.df <- rain.df[,,mon.in]
  rain.raster <- raster(t(rain.mon.df),
                       xmn=min(lon.vec), xmx=max(lon.vec), ymn=min(lat.vec), ymx=max(lat.vec))
  met.raster.right <- flip(rain.raster, direction='y')

  value.tar <- extract(x = met.raster.right,y = cbind(lon,lat),method = 'bilinear',small=T)
  return(value.tar)
}
# get the unique dates
date.vec <- unique(gps.date.df$previous.month)
gps.date.ls <- list()
# 

func2apply <- function(x,met.nm,dat){
  tmp.df <- gps.date.df[dat$previous.month == x,]
  
  tmax.vec <- try(get.agcd.met.func(dat = tmp.df,met.nm = met.nm))
  return(tmax.vec)
}

# x <- sapply(date.vec, func2apply,met.nm = 'tmax',dat = gps.date.df[1:10,])

for(i in 193:length(date.vec)#seq_along(date.vec)
    ){
  tmp.df <- gps.date.df[gps.date.df$previous.month == date.vec[i],]
  
  tmax.vec <- try(get.agcd.met.func(dat = tmp.df,met.nm = 'tmax'))
  if(class(tmax.vec) != 'try-error'){
    tmp.df$tmax <- tmax.vec
  }else{
    tmp.df$tmax <- NA
  }
  
  rain.vec <- try(get.rain.silo.func(lat = tmp.df$lat,lon = tmp.df$lon,yr.mon = date.vec[i]))#try(get.agcd.met.func(tmp.df,met.nm = 'rain'))
  if(class(rain.vec) != 'try-error'){
    tmp.df$rain <-rain.vec
  }else{
    tmp.df$rain <- NA
  }
  vph15.vec <- try(get.agcd.met.func(tmp.df,met.nm = 'vph15'))
  if(class(vph15.vec) != 'try-error'){
    tmp.df$vph15 <-vph15.vec
  }else{
    tmp.df$vph15 <- NA
  }
  # rad.vec <- try(get.agcd.met.func(tmp.df,met.nm = 'rad'))
  # if(class(rad.vec) != 'try-error'){
  #   tmp.df$rad <-rad.vec
  # }else{
  #   tmp.df$rad <- NA
  # }
  # tmp.df$tmin <- get.met.coords.ns.func(tmp.df,met.nm = 'tmin')
  # tmp.df$rad <- get.met.coords.ns.func(tmp.df,met.nm = 'rad')
  # tmp.df$rain <- get.met.coords.ns.func(tmp.df,met.nm = 'rain')
  
  gps.date.ls[[i]] <- tmp.df
  print(date.vec[i])
}

gps.date.met.df <- do.call(rbind,gps.date.ls)
saveRDS(gps.date.met.df,'cache/met.rds')
# gps.date.met.df.old <- readRDS('cache/met.rds')
# 
# 
# plot(gps.date.met.df.old$rain,gps.date.met.df$rain)
# 
# abline(a=0,b=1)

















###########################################################
# old script working with nc data probably from awap#####
###########################################################
get.met.coords.ns.func <- function(dat,met.nm){
  
  if(met.nm == "rain"){
    met.var = 'lwe_thickness_of_precipitation_amount'
    lat.nm <- 'lat'
    lon.nm <- 'lon'
  }else{
    met.var <- paste0(met.nm,'_day')
    lat.nm <- 'latitude'
    lon.nm <- 'longitude'
  }
  file.nm.vec <- sprintf("data/met/%s/%s.%s.nc",
                         met.nm,met.nm,
                         dat$previous.month[1])
  # 
  read.func <- function(fn){
    
    nc_data <- nc_open(fn)
    
    lon.vec <- try(ncvar_get(nc_data, lon.nm))
    
    if(class(lon.vec) == 'try-error'){
      lon.vec <- ncvar_get(nc_data, 'longitude')
      lat.vec <- ncvar_get(nc_data, 'latitude')
    }
    
    # 
    met.df <- try(ncvar_get(nc_data, met.var))
    
    
    if(class(met.df) != 'try-error'){
      array <- abind(met.df, along=3)
      # Annual average VP for every x,y location
      mean.month <- rowMeans(array, dims=2,na.rm=T)
      
      met.raster <- raster(t(mean.month),
                           xmn=min(lon.vec), xmx=max(lon.vec), ymn=min(lat.vec), ymx=max(lat.vec))
      value.tar <- extract(x = met.raster,y = cbind(dat$lon,dat$lat),method = 'bilinear')
      # plot((met.raster))
      # points(cbind(dat$lon,dat$lat))
    }else{
      value.tar <- NA
    }
    
    rm(met.df)
    return(value.tar)
  }
  
  return(read.func(fn=file.nm.vec))
}

# 
date.vec <- unique(gps.date.df$previous.month)
gps.date.ls <- list()
# 
# evi.ym.df.tmax <- get.met.coords.func(year.in,met.nm='tmax',lat = lat ,lon=lon)
# evi.ym.df.tmin <- get.met.coords.func(year.in,met.nm='tmin',lat = lat ,lon=lon)
# evi.ym.df.rad <- get.met.coords.func(year.in,met.nm='rad',lat = lat ,lon=lon)
# evi.ym.df.rain <- get.met.coords.func(year.in,met.nm='rain',lat = lat ,lon=lon)

for(i in seq_along(date.vec)){
  tmp.df <- gps.date.df[gps.date.df$previous.month == date.vec[i],]
  
  tmax.vec <- try(get.met.coords.ns.func(tmp.df,met.nm = 'tmax'))
  if(class(tmax.vec) != 'try-error'){
    tmp.df$tmax <- tmax.vec
  }else{
    tmp.df$tmax <- NA
  }
  
  rain.vec <- try(get.met.coords.ns.func(tmp.df,met.nm = 'rain'))
  if(class(rain.vec) != 'try-error'){
    tmp.df$rain <-rain.vec
  }else{
    tmp.df$rain <- NA
  }
  tmin.vec <- try(get.met.coords.ns.func(tmp.df,met.nm = 'tmin'))
  if(class(tmin.vec) != 'try-error'){
    tmp.df$tmin <-tmin.vec
  }else{
    tmp.df$tmin <- NA
  }
  rad.vec <- try(get.met.coords.ns.func(tmp.df,met.nm = 'rad'))
  if(class(rad.vec) != 'try-error'){
    tmp.df$rad <-rad.vec
  }else{
    tmp.df$rad <- NA
  }
  # tmp.df$tmin <- get.met.coords.ns.func(tmp.df,met.nm = 'tmin')
  # tmp.df$rad <- get.met.coords.ns.func(tmp.df,met.nm = 'rad')
  # tmp.df$rain <- get.met.coords.ns.func(tmp.df,met.nm = 'rain')
  
  gps.date.ls[[i]] <- tmp.df
}

gps.date.met.df <- do.call(rbind,gps.date.ls)
saveRDS(gps.date.met.df,'cache/met.rds')




gps.date.met.df <- readRDS('cache/met.rds')


















