source('r/get_vic_shape.R')
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
pr.hist <- read.climate.func(var.in.nm = 'pr.rds',
                             future_s =  'history',
                             exclude.nm = '')
mean.ra.hist <- calc(pr.hist, fun = mean)
mean.ra.hist <- get.small.area.func(mean.ra.hist,shape.vic)

# # 
# pr.rcp45.mid <- read.climate.func(var.in.nm = 'pr.rds',
#                                   future_s =  'rcp45_mid',
#                                   exclude.nm = '')
# mean.ra.45.mid <- calc(pr.rcp45.mid, fun = mean)
# # plot(mean.ra.2 - mean.ra.hist)
# # 
# pr.rcp45.long <- read.climate.func(var.in.nm = 'monthly_pr.rds',
#                                    future_s =  'rcp45_long',
#                                    exclude.nm = '')
# mean.ra.45.long<- calc(pr.rcp45.long, fun = mean)
# # plot(mean.ra.long - mean.ra.hist)
# # 
# pr.rcp85.mid <- read.climate.func(var.in.nm = 'monthly_pr.rds',
#                                   future_s =  'rcp85_mid',
#                                   exclude.nm = '')
# mean.ra.85.mid <- calc(pr.rcp45.mid, fun = mean)
# 
pr.rcp85.long <- read.climate.func(var.in.nm = 'pr.rds',
                                   future_s =  'rcp85_long',
                                   exclude.nm = '')
mean.ra.85.long<- calc(pr.rcp85.long, fun = mean)
mean.ra.85.long <- get.small.area.func(mean.ra.85.long,shape.vic)

# plot only the end of century#######
png('figures/future_rain.png',width = 800,height = 300)
par(mfrow=c(1,2),mar=c(3,3,1,5))
# a
break.vec <-seq(100,1200,by=200)

plot(mean.ra.hist,
     breaks = break.vec,
     col=(topo.colors(length(break.vec)-1)),
     box=FALSE,bty='n',
     xaxt='n')

legend('topleft',legend = '(c) Rainfall-current',bty='n')
# b
break.vec <- seq(-200,0,by=40)

plot(mean.ra.85.long - mean.ra.hist,main = '',
     breaks = break.vec,
     col=c('red','coral','grey','lightskyblue','navy'),
     box=FALSE,bty='n',
     xaxt='n')

legend('topleft',legend = '(d) Rainfall-2100 RCP8.5',bty='n')
dev.off()
