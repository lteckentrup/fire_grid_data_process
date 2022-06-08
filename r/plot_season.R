pr.season.hist <- readRDS('cache/pr_seaonality_history.rds')
pr.season.rcp45.mid <- readRDS('cache/pr_seaonality_rcp45_mid.rds')
pr.season.rcp45.long <- readRDS('cache/pr_seaonality_rcp45_long.rds')
pr.season.rcp85.mid <- readRDS('cache/pr_seaonality_rcp85_mid.rds')
pr.season.rcp85.long <- readRDS('cache/pr_seaonality_rcp85_long.rds')
# 

pdf('figures/future_rainfall_seasonality.pdf',height = 8,width = 9)
break.vec <- seq(-0.01,0.06,by=0.01)
library(raster)
plot(pr.season.hist,main = ' Current',breaks = break.vec,col=(topo.colors(length(break.vec)-1)))
library(oz)
vic(add=T,col='grey',lwd=3)
# 
library(raster)
plot(pr.season.rcp45.mid - pr.season.hist,main = 'RCP4.5 (2045-2060) - Current',
     breaks = break.vec,col=(topo.colors(length(break.vec)-1)))
library(oz)
vic(add=T,col='grey',lwd=3)
# 
library(raster)
plot(pr.season.rcp45.long - pr.season.hist,main = 'RCP4.5 (2085-2100) - Current'
     ,breaks = break.vec,col=(topo.colors(length(break.vec)-1)))
library(oz)
vic(add=T,col='grey',lwd=3)
# 
library(raster)
plot(pr.season.rcp85.mid - pr.season.hist,main = 'RCP8.5 (2045-2060) - Current'
     ,breaks = break.vec,col=(topo.colors(length(break.vec)-1)))
library(oz)
vic(add=T,col='grey',lwd=3)
# 
library(raster)
plot(pr.season.rcp85.long - pr.season.hist,main = 'RCP8.5 (2085-2100) - Current'
     ,breaks = break.vec,col=topo.colors((length(break.vec)-1)))
library(oz)
vic(add=T,col='grey',lwd=3)
dev.off()

# plot only the end of century#######
png('figures/future_rain_seasonality.png',width = 800,height = 300)
par(mfrow=c(1,2),mar=c(3,3,1,5))

# a
pr.season.hist <- mask(pr.season.hist, shape.vic)
break.vec <- seq(0,0.05,by=0.01)

plot(pr.season.hist,
     breaks = break.vec,
     col=(topo.colors(length(break.vec)-1)),
     box=FALSE,bty='n',
     xaxt='n')

legend('topleft',legend = '(e) Rainfall seasonality-current',bty='n')
# b
break.vec <- seq(-0.01,0.03,by=0.01)
pr.season.rcp85.long <- mask(pr.season.rcp85.long, shape.vic)

plot((pr.season.rcp85.long-pr.season.hist),main = '',
     breaks = break.vec,
     col=c('lightskyblue',rev(heat.colors(length(break.vec)-2))),
     box=FALSE,bty='n',
     xaxt='n')

legend('topleft',legend = '(f) Rainfall seasonality-2100 RCP8.5',bty='n')
dev.off()