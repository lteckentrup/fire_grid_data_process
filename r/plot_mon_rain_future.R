#####
read.climate.func <- function(var.in.nm,future_s,exclude.nm = 'noVeg'){
  rcp45_mid.fn <- list.files(path = 'data/met/future/',
                             pattern = var.in.nm,recursive = T,full.names = T)
  
  rcp45_mid.fn <- rcp45_mid.fn[!rcp45_mid.fn %in% grep(exclude.nm, rcp45_mid.fn)]
  rcp45_mid.fn <- rcp45_mid.fn[grep(future_s, rcp45_mid.fn)]
  
  rcp45.mid.h.c <- sapply(rcp45_mid.fn, function(fn.in){
    
    x <- readRDS(fn.in)
    return(x[[1]])
  })
  
  print(rcp45_mid.fn)
  raster.new = stack(rcp45.mid.h.c)
  return(raster.new)
}
# ####
pr.hist <- read.climate.func(var.in.nm = 'monthly_pr.rds',
                             future_s =  'history',
                             exclude.nm = '')
mean.ra.hist <- calc(pr.hist, fun = mean)


# 
pr.rcp45.mid <- read.climate.func(var.in.nm = 'monthly_pr.rds',
                                     future_s =  'rcp45_mid',
                                     exclude.nm = '')
mean.ra.45.mid <- calc(pr.rcp45.mid, fun = mean)
# plot(mean.ra.2 - mean.ra.hist)
# 
pr.rcp45.long <- read.climate.func(var.in.nm = 'monthly_pr.rds',
                                  future_s =  'rcp45_long',
                                  exclude.nm = '')
mean.ra.45.long<- calc(pr.rcp45.long, fun = mean)
# plot(mean.ra.long - mean.ra.hist)
# 
pr.rcp85.mid <- read.climate.func(var.in.nm = 'monthly_pr.rds',
                                  future_s =  'rcp85_mid',
                                  exclude.nm = '')
mean.ra.85.mid <- calc(pr.rcp45.mid, fun = mean)
# 
pr.rcp85.long <- read.climate.func(var.in.nm = 'monthly_pr.rds',
                                   future_s =  'rcp85_long',
                                   exclude.nm = '')
mean.ra.85.long<- calc(pr.rcp85.long, fun = mean)
# plot((mean.ra.85.long - mean.ra.hist))
# 

pdf('figures/future_rainfall_jan.pdf',width = 8,height = 7)

# 
break.vec <- seq(20,160,by=20)
library(raster)
plot(mean.ra.hist*30,main = ' Current',breaks = break.vec,col=(topo.colors(length(break.vec)-1)))
library(oz)
vic(add=T,col='grey',lwd=3)

# 
break.vec <- seq(-20,40,by=10)
library(raster)
plot((mean.ra.45.mid-mean.ra.hist)*30,main = ' RCP4.5 (2045-2060) - Current',
     breaks = break.vec,col=(topo.colors(length(break.vec)-1)))
library(oz)
vic(add=T,col='grey',lwd=3)
# 
# break.vec <- seq(20,160,by=20)
library(raster)
plot((mean.ra.45.long-mean.ra.hist)*30,main = ' RCP4.5 (2085-2100) - Current',
     breaks = break.vec,col=(topo.colors(length(break.vec)-1)))
library(oz)
vic(add=T,col='grey',lwd=3)
# 
# break.vec <- seq(20,160,by=20)
library(raster)
plot((mean.ra.85.mid-mean.ra.hist)*30,main = ' RCP8.5 (2045-2060) - Current',
     breaks = break.vec,col=(topo.colors(length(break.vec)-1)))
library(oz)
vic(add=T,col='grey',lwd=3)
# 
# break.vec <- seq(20,160,by=20)
library(raster)
plot((mean.ra.85.long-mean.ra.hist)*30,main = ' RCP8.5 (2085-2100) - Current',
     breaks = break.vec,col=(topo.colors(length(break.vec)-1)))
library(oz)
vic(add=T,col='grey',lwd=3)
dev.off()