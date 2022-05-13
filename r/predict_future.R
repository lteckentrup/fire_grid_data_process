library(raster)
#make df for prediction####
pr.df <- readRDS('data/met/future/access/rcp45_20452060_monthly_pr.rds')
rh.df <- readRDS('data/met/future/access/rcp45_20452060_monthly_rh.rds')
tmax.df <- readRDS('data/met/future/access/rcp45_20452060_monthly_tmax.rds')


i=1
input.future.df <- as.data.frame(pr.df[[i]], xy=TRUE) 
names(input.future.df) <- c('lon','lat','rain')


input.future.df$tmax.k <- raster::extract(x = tmax.df[[i]],
                                          y = cbind(input.future.df$lon,input.future.df$lat),
                                          method = 'bilinear',small=T,na.rm=T)

input.future.df$tmax <- input.future.df$tmax.k -272.15

input.future.df$vph15 <- raster::extract(x = rh.df[[i]],
                                         y = cbind(input.future.df$lon,input.future.df$lat),
                                         method = 'bilinear',small=T,na.rm=T)
# soil 
input.future.df$vph15 <- raster::extract(x = rh.df[[i]],
                                         y = cbind(input.future.df$lon,input.future.df$lat),
                                         method = 'bilinear',small=T,na.rm=T)








# predict lai####
hs.soil.wi.met.df <- readRDS('cache/hs.soil.topo.met.rds')
par_fraction <- 0.368
# 
# library("devtools")
# install_bitbucket("Jinyan_Jim_Yang/g1.opt.package.git")

hs.soil.wi.met.df$PAR <- (hs.soil.wi.met.df$rad.short.jan + hs.soil.wi.met.df$rad.short.jul)/2 *par_fraction
get.vp.from.t.func <- function(temperature){
  # Magnus_pressure = 
  0.61094 * exp((17.625 * temperature) / (temperature + 243.04))
}


hs.soil.wi.met.df$vpd <- get.vp.from.t.func(hs.soil.wi.met.df$tmax) * (1- hs.soil.wi.met.df$vph15/100)
hs.soil.wi.met.df$vpd[hs.soil.wi.met.df$vpd<0.05] <- 0.05
library(g1.opt.func)
opt.out.ls <- list()
n.days <- 365.25
for (i in 1:nrow(hs.soil.wi.met.df)) {
  tmp <- try(g1.lai.e.func(VPD = hs.soil.wi.met.df$vpd[i],
                           E = hs.soil.wi.met.df$rain[i]*n.days,
                           PAR = hs.soil.wi.met.df$PAR[i]*n.days,
                           TMAX = hs.soil.wi.met.df$tmax[i],
                           Ca = 400))
  if(class(tmp)=='try-error'){
    opt.out.ls[[i]] <- NA
  }else{
    opt.out.ls[[i]] <- tmp
  }
  print(i)
}

opt.out.df <- do.call(rbind,opt.out.ls)
hs.soil.wi.met.df$lai.opt <- opt.out.df[,'LAI']