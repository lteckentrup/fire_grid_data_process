# plot(readRDS('data/met/future/ACCESS1-0/history/history_20002015_lai_jan_5km.rds'),
#      colNA="lightskyblue")
# 
# 
# 
# library(oz)
# vic(add=T,lwd=1,col='black',coast = F)

source('r/functions_plot.R')
######
pdf('figures/hz_ele_rcp.pdf',width = 5.5*5,height = 3.7*2)
par(mar=c(2,2,1,6))
layout(matrix(1:10, 2, 5, byrow = T))
# par(mfcol=c(5,2))
wrap.plot.rcp.func(var.in.nm = '_hz_elevated.rds',
                   exclude.nm = 'noVeg',
                   legend.in = letters[1:5])
wrap.plot.rcp.func(var.in.nm = 'noVeg_hz_elevated.rds',
                   exclude.nm = '',
                   legend.in = letters[6:10])
dev.off()
# 
pdf('figures/hz_ns_rcp.pdf',width = 5.5*5,height = 3.7*2)
par(mar=c(2,2,1,6)) 
layout(matrix(1:10, 2, 5, byrow = T))
# par(mfcol=c(5,2))
wrap.plot.rcp.func(var.in.nm = '_hz_ns.rds',
                   exclude.nm = 'noVeg',
                   legend.in = letters[1:5])
wrap.plot.rcp.func(var.in.nm = 'noVeg_hz_ns.rds',
                   exclude.nm = '',
                   legend.in = letters[6:10])
dev.off()
# 
pdf('figures/hz_surface_rcp.pdf',width = 5.5*5,height = 3.7*2)
par(mar=c(2,2,1,6)) 
layout(matrix(1:10, 2, 5, byrow = T))
# par(mfcol=c(5,2))
wrap.plot.rcp.func(var.in.nm = '_hz_surface.rds',
                   exclude.nm = 'noVeg',
                   legend.in = letters[1:5])
wrap.plot.rcp.func(var.in.nm = 'noVeg_hz_surface.rds',
                   exclude.nm = '',
                   legend.in = letters[6:10])
dev.off()
# 
pdf('figures/hz_bark_rcp.pdf',width = 5.5*5,height = 3.7*2)
par(mar=c(2,2,1,6)) 
layout(matrix(1:10, 2, 5, byrow = T))
# par(mfcol=c(5,2))
wrap.plot.rcp.func(var.in.nm = '_hz_bark.rds',
                   exclude.nm = 'noVeg',
                   legend.in = letters[1:5])
wrap.plot.rcp.func(var.in.nm = 'noVeg_hz_bark.rds',
                   exclude.nm = '',
                   legend.in = letters[6:10])
dev.off()

# #######
pdf('figures/height_canopy_rcp.pdf',width = 5.5*5,height = 3.7*2)
layout(matrix(1:10, 2, 5, byrow = T))
par(mar=c(2,2,1,6)) 
wrap.plot.rcp.height.func(var.in.nm = '_height_canopy.rds',
                   exclude.nm = 'noVeg',
                   legend.in = letters[1:5])
wrap.plot.rcp.height.func(var.in.nm = 'noVeg_height_canopy.rds',
                   exclude.nm = '',
                   legend.in = letters[6:10])
dev.off()
# 
# 
pdf('figures/height_ns_rcp.pdf',width = 5.5*5,height = 3.7*2)
layout(matrix(1:10, 2, 5, byrow = T))
par(mar=c(2,2,1,6)) 
wrap.plot.rcp.height.func(var.in.nm = '_height_ns.rds',
                   exclude.nm = 'noVeg',
                   legend.in = letters[1:5])
wrap.plot.rcp.height.func(var.in.nm = 'noVeg_height_ns.rds',
                   exclude.nm = '',
                   legend.in = letters[6:10])
dev.off()
# 




png('figures/example_canopy_height.png',height = 800,width = 1200)
par(mar=c(2,2,1,3))
h.ra <- readRDS('data/met/future/ACCESS1-0/history/_height_canopy.rds')
col.func <- colorRampPalette(c('burlywood','darkgreen'))
brk.vec <- seq(4,18,by=1)
at.val <- seq(4.5,18.5,by=2)
lab.val <- seq(4,18,by=2)
plot(exp(h.ra[['val']]),breaks = brk.vec,col=col.func(length(brk.vec)-1),
     colNA = 'lightskyblue',
     axis.args=list(at=at.val,
                    labels=lab.val,
                    cex.axis=1),legend.width=1.5)
dev.off()
# ####
png('figures/example_canopy_hs.png',height = 800,width = 1200)
par(mar=c(2,2,1,3))
h.ra <- readRDS('data/met/future/ACCESS1-0/history/_hz_elevated.rds')
col.func <- colorRampPalette(c('darkgreen','red'))
brk.vec <- seq(0,6,by=1)
at.val <- head(brk.vec, -1)+0.5
lab.val <- head(brk.vec, -1)
plot(h.ra[['val']],breaks = brk.vec,col=col.func(length(brk.vec)-1),
     colNA = 'lightskyblue',
     axis.args=list(at=at.val,
                    labels=lab.val,
                    cex.axis=1),legend.width=1.5)
library(oz)
vic(add=T,lwd=1,col='black',coast = F)

dev.off()


# plot fuel type####
# change fuel type to smaller numbers for plot
# htis is not necessary but a leftover from previous analysis
evc.df <- raster('data/EVC_fuelType/evc/VICSANSW161.tif')
# plot(evc.df,breaks=c(0,2000,2001:2070,3001))
atrribute.df <- evc.df@data@attributes[[1]]
atrribute.df <- atrribute.df[,c('VICSANSW.FUEL_TYPE' , 'VICSANSW.TYPE_NAME')]
atrribute.df <- atrribute.df[!duplicated(atrribute.df),]
ft.no.vec <- atrribute.df$VICSANSW.FUEL_TYPE#read.csv('cache/fuelType_LUT.csv')$fuelType_vicnsw
ft.df <- data.frame(ft.no = ft.no.vec,
                    ft.new.num = seq_along(ft.no.vec),
                    nsmes = atrribute.df$VICSANSW.TYPE_NAME)
# function to change levels
put.ft.no.to.sense.func <- function(ra.in){
  # ft.no.in.ra <- unique(ra.in)
  for (i in seq_along(ft.df$ft.no)) {
    ra.in[ra.in==ft.df$ft.no[i]] <- ft.df$ft.new.num[i]
  }
  return(ra.in)
}
# read history ft
hz.ft.hist <- read.future.all.func(var.in.nm = 'fuelType',
                                         future_s = 'history',
                                         exclude.nm = '')

hz.ft.hist.mod <- calc(hz.ft.hist, fun=Mode.func)
hz.ft.hist.mod <- put.ft.no.to.sense.func(hz.ft.hist.mod)
# plot(hz.ft.hist.mod)
# read rcp8.5 end century
hz.ele.rcp45_mid <- read.future.all.func(var.in.nm = 'fuelType',
                                         future_s = 'rcp85_long',
                                         exclude.nm = 'noVeg')
# get mode
ra.rcp45_mid.mode.his <- calc(hz.ele.rcp45_mid, fun=Mode.func)

ra.rcp45_mid.mode.his <- put.ft.no.to.sense.func(ra.rcp45_mid.mode.his)
# make plotof fuel type ####
png('figures/fuelType_pred.png',width = 600,height = 550)
par(mar=c(3,3,1,1),mfrow=c(2,2),xpd=T)
# plot history ft
green.col <- colorRampPalette(c('forestgreen','yellowgreen'))
yellow.col <- colorRampPalette(c('yellow3','tan'))
red.col <- colorRampPalette(c('tomato','red'))
col.vec <- c(green.col(4),yellow.col(6),red.col(5))
# 
hz.ft.hist.mod <- mask(hz.ft.hist.mod, shape.vic)
# a history####
plot(hz.ft.hist.mod,
     breaks = c(unique(hz.ft.hist.mod),102),col=col.vec,
     # axis.args=list(at=0:1,
     #                labels=c('No','Yes'),
     # cex.axis=1),
     legend=F,
bty='n',box=F)
legend('topleft',legend = '(a)',bty='n')
# b empty with legendE####
present.ft <- unique(hz.ft.hist.mod)
par(xpd=TRUE)
plot(0,col='white',ann=F,axes=F)
legend('topleft',
       legend = (ft.df$nsmes[present.ft]),
       col=col.vec,pch=15,bty='n',ncol=1)

# c plot rcp8,5#####
ra.rcp45_mid.mode.his <- mask(ra.rcp45_mid.mode.his, shape.vic)
# 
plot(ra.rcp45_mid.mode.his,
     breaks = c(unique(present.ft),102),col=col.vec,
     # axis.args=list(at=0:1,
     #                labels=c('No','Yes'),
     # cex.axis=1),
     legend=F,
     bty='n',box=F)
legend('topleft',legend = '(b)',bty='n')
#####d difference#####
# get difference
diff.ra <- ra.rcp45_mid.mode.his - hz.ft.hist.mod
diff.ra[diff.ra !=0] <- 1
diff.ra <- mask(diff.ra, shape.vic)
par(mar=c(3,3,1,3))
plot(diff.ra,breaks = c(-0.5,0.5,1.5),col=c('grey','red'),
     axis.args=list(at=0:1,
                    labels=c('No','Yes'),
                    cex.axis=1),bty='n',box=F)
legend('topleft',legend = '(c)',bty='n')
dev.off()