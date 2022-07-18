get.small.area.func <- function(ra.in,p = p){
  r2 <- crop(ra.in, extent(p))
  r3 <- mask(r2, p)
  # crs(r3) <- crs(ra.in)
  # aggregate(r3, fact=3)
  return(r3)
}
read.future.prob.func <- function(var.in.nm,future_s,exclude.nm = 'noVeg'){
  # read file names
  rcp45_mid.fn <- list.files(path = 'data/met/future/',
                             pattern = var.in.nm,recursive = T,full.names = T)
  # filter files
  if(exclude.nm =='noVeg'){
    rcp45_mid.fn <- rcp45_mid.fn[!rcp45_mid.fn %in% 
                                   rcp45_mid.fn[grep(exclude.nm, rcp45_mid.fn)]]
  }else{
    rcp45_mid.fn <- rcp45_mid.fn[grep('noVeg', rcp45_mid.fn)]
  }
  
  rcp45_mid.fn <- rcp45_mid.fn[grep(future_s, rcp45_mid.fn)]
  # read in probes
  rcp45.mid.h.c <- sapply(rcp45_mid.fn, function(fn.in){
    
    x <- readRDS(fn.in)
    x.prob <- x[['prob']]
    high.risk.4 <- x.prob[['4']]
    high.risk.5 <- x.prob[['5']]
    
    
    high.total.ra <- high.risk.4 + high.risk.5
    
    return(high.total.ra)
  })
  # get model names
  model.names <- list.dirs('data/met/future/',full.names = F,recursive = F)
  # stack and save
  print(rcp45_mid.fn)
  raster.new = stack(rcp45.mid.h.c)
  names(raster.new) <- model.names
  
  return(raster.new)
}

# functions####
source('r/functions_predict.R')
source('r/functions_plot.R')
source('r/get_vic_shape.R')

# 

library(raster)
pet.ls <- readRDS('data/met/pet.gps.rds')

pet.ra <- raster(t(pet.ls[['pet']]),
                 xmn=min(pet.ls[['lon']]), xmx=max(pet.ls[['lon']]), 
                 ymn=min(pet.ls[['lat']]), ymx=max(pet.ls[['lat']]))
map.ra <- raster(t(pet.ls[['map']]),
                 xmn=min(pet.ls[['lon']]), xmx=max(pet.ls[['lon']]), 
                 ymn=min(pet.ls[['lat']]), ymx=max(pet.ls[['lat']]))

wi.ra <- map.ra / pet.ra

wi.vic <- get.small.area.func(wi.ra,shape.vic)
plot(1/wi.vic)

# 
hz.ele.his <- read.future.prob.func(var.in.nm = '_hz_elevated.rds',
                                    future_s = 'history',
                                    exclude.nm = 'noVeg')
hist.ra <- mean(hz.ele.his,na.rm=T)

# 
hz.ele.future <- read.future.prob.func(var.in.nm = '_hz_elevated.rds',
                                    future_s = 'rcp85_long',
                                    exclude.nm = 'noVeg')
future.ra <- mean(hz.ele.future,na.rm=T)

change.ra <- future.ra - hist.ra
change.ra.vic <- get.small.area.func(change.ra,shape.vic)
# plot(change.ra)

wi.vic.fine <- resample(wi.vic,change.ra.vic)
plot(wi.vic.fine)

plot(as.vector(change.ra.vic)~as.vector(1/wi.vic.fine),
     pch=16,col=t_col('grey',90))

# get lcm
lcm.ra <- raster('E:/storage/equilibrium-lai-for-australia/downloads/LCM/luav4g9abll07811a02egigeo___/lu05v4ag/w001001.adf')
lcm.vic <- get.small.area.func(lcm.ra,shape.vic)
lcm.lut <- lcm.vic@data@attributes[[1]]
lcm.lut.natrua <- lcm.lut$ID[lcm.lut$LU_DESC %in% c('CONSERVATION AND NATURAL ENVIRONMENTS',
                                                    'PRODUCTION FROM RELATIVELY NATURAL ENVIRONMENTS')]


lcm.vic[!(lcm.vic %in% lcm.lut.natrua)] <- NA
# filter out non-natural
lcm.vic.fine <- resample(lcm.vic,wi.vic.fine)
wi.vic.lcm <- mask(wi.vic.fine,lcm.vic.fine)
change.ra.vic.lcm <- mask(change.ra.vic,lcm.vic.fine)

plot.df <- data.frame(Change = as.vector(change.ra.vic.lcm),
                      DI = as.vector((1/wi.vic.lcm)))
plot.df <- plot.df[order(plot.df$DI),]

library(mgcv)
gam.fit <- gam(Change~s(DI,k=5),data = plot.df)

plot(Change~DI,data = plot.df,
     pch=16,col=t_col('grey',90))
points(gam.fit$fitted.values~gam.fit$model$DI,type='l',col='red')
abline(h=0)
