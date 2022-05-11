# combisoil and met####
met.df <- readRDS('cache/met.rds')
hs.soil.df <- readRDS('cache/hazardScoreWithSoil.rds')
wi.df <- readRDS('cache/wetness_curvature_ByGPs.rds')
fuelType.df <- readRDS('cache/fuelType.rds')

hs.soil.wi.df <- merge(hs.soil.df,wi.df,all.x=T)
hs.soil.wi.met.df<- merge(hs.soil.wi.df,met.df,all.x=T)
hs.soil.wi.met.df.fuel.df <-  merge(hs.soil.wi.met.df,fuelType.df,all.x=T)
saveRDS(hs.soil.wi.met.df.fuel.df,'cache/hs.soil.topo.met.rds')

# # read opt lai####
# library(raster)
# lai.full <- readRDS("data/LAI1991_2011.rds")
# lai.ra <- raster(t(lai.full),
#                  xmn=112.905, xmx=153.995, ymn=-43.375, ymx=-9.005)
# 
# # get opt lai for sites
# hs.soil.wi.met.df <- readRDS('cache/hs.soil.topo.met.rds')
# 
# hs.soil.wi.met.df$lai.opt <- extract(x = lai.ra,
#                                      y = cbind(hs.soil.wi.met.df$lon,
#                                                hs.soil.wi.met.df$lat))

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
# plot(hs.soil.wi.met.df$lai.opt)
saveRDS(hs.soil.wi.met.df,'cache/hs.soil.topo.met.lai.rds')
# lai <- readRDS('cache/hs.soil.topo.met.lai.rds')
# hs.soil.wi.met.df.fuel.df$lai.opt <- lai$lai.opt$lai.opt
# lai.df <- readRDS('cache/hs.soil.topo.met.lai.rds')
# hs.soil.wi.met.df$lai.opt <-lai.df$lai.opt
# add longterm climate######
pet.ls <- readRDS('data/met/pet.gps.rds')
library(raster)
pet.ra <- raster(t(pet.ls[['pet']]),
                 xmn=min(pet.ls[['lon']]), xmx=max(pet.ls[['lon']]), 
                 ymn=min(pet.ls[['lat']]), ymx=max(pet.ls[['lat']]))

map.ra <- raster(t(pet.ls[['map']]),
                 xmn=min(pet.ls[['lon']]), xmx=max(pet.ls[['lon']]), 
                 ymn=min(pet.ls[['lat']]), ymx=max(pet.ls[['lat']]))

tmp.df <- readRDS('cache/hs.soil.topo.met.lai.rds')
tmp.df$pet <- raster::extract(pet.ra,cbind(tmp.df$lon,tmp.df$lat))
tmp.df$map <- raster::extract(map.ra,cbind(tmp.df$lon,tmp.df$lat))
# 
saveRDS(tmp.df,'cache/hs.soil.topo.met.lai.rds')































# 
summary(hs.soil.wi.met.df)
# 
mumol_j = 4.56
par_fraction <- 0.368
# 
library("devtools")
install_bitbucket("Jinyan_Jim_Yang/g1.opt.package.git")
library("g1.opt.func")
hs.soil.wi.met.df$PAR <- (hs.soil.wi.met.df$rad.short.jan + hs.soil.wi.met.df$rad.short.jul)/2 *par_fraction
get.vp.from.t.func <- function(temperature){
  # Magnus_pressure = 
    0.61094 * exp((17.625 * temperature) / (temperature + 243.04))
}


hs.soil.wi.met.df$vpd <- get.vp.from.t.func(hs.soil.wi.met.df$tmax) - get.vp.from.t.func(hs.soil.wi.met.df$tmin)

opt.out.ls <- list()
for (i in 1:nrow(hs.soil.wi.met.df)) {
  tmp <- try(g1.lai.e.func(VPD = hs.soil.wi.met.df$vpd[i],
                           E = hs.soil.wi.met.df$rain[i],
                           PAR = hs.soil.wi.met.df$PAR[i],
                           TMAX = hs.soil.wi.met.df$tmax[i]))
  if(class(tmp)=='try-error'){
    opt.out.ls[[i]] <- NA
  }else{
    opt.out.ls[[i]] <- tmp
  }
  
}

opt.out.df <- do.call(rbind,opt.out.ls)
hs.soil.wi.met.df$lai.opt