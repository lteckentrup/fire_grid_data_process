
hs.ele.ls <- readRDS('data/met/future/BNU-ESM/rcp45_mid/_hz_elevated.rds')
hs.ele.noveg.ls <- readRDS('data/met/future/BNU-ESM/rcp45_mid/noVeg_hz_elevated.rds')

hs.ele.ls.rcp85.mid <- readRDS('data/met/future/BNU-ESM/rcp85_mid/_hz_elevated.rds')

# fn.vec <- list.files(path = 'data/met/future/ACCESS1-0/',pattern = '.rds',
#                      full.names = T,recursive = T)
# 
# fn.vec <- fn.vec[grep(pattern = 'rcp45_mid',x = fn.vec)]
# fn.vec <- fn.vec[-grep(pattern = 'noVeg',x = fn.vec)]

pdf('figures/example-future.pdf',width = 8,height = 8*.618)
# 
plot((hs.ele.ls[['val']]),
     main = 'Elevated Fuel RCP4.5 2045-2060')
# 
plot((hs.ele.noveg.ls[['val']]),
     main = 'Elevated Fuel RCP4.5 2045-2060 without veg')
# 
plot((hs.ele.ls[['val']] - hs.ele.noveg.ls[['val']]),
     main = 'Impacts of vegetation response')
# 
plot((hs.ele.ls[['val']] - hs.ele.ls.rcp85.mid[['val']]),
     main = 'RCP4.5 - RCP8.5')
# 
plot((hs.ele.ls[['prob']][[4]]),
     main = paste0('Probability of hazard score=',
                   names(hs.ele.ls[['prob']])[4]))
dev.off()



plot((hs.ele.ls[['prob']][[5]]))
names(hs.ele.ls[['prob']])


plot((hs.ele.noveg.ls[['val']]))
plot((hs.ele.noveg.ls[['prob']]))


# 
lai.ls <- readRDS('data/met/future/BNU-ESM/rcp85_long/rcp85_20852100_lai_jan_5km.rds')
lai.ls.hist <- readRDS('data/met/future/BNU-ESM/histrory/history_20002015_lai_jan_5km.rds')

# plot(lai.ls)

c.h <- readRDS('data/met/future/ACCESS1-0/history/_height_ns.rds')
brks <- seq(4,18,by=2)
brks <- seq(20,34,by=1)
colours <- colorRampPalette(c("grey","red"))
plot(exp(c.h[['val']]),breaks=brks,col=colours(length(brks)-1))

# brks <- 0.5:5.5
# plot(hs.ele.ls[['val']],breaks=brks,col=col.df$cityNight[c(1,2,5,4,3)],lab.breaks=0:5)
# 
# colours <- colorRampPalette(c("grey","red"))
# plot(prob.ra,col=colours(20))

# $##########

hs.ele.ls <- readRDS('data/met/future/BNU-ESM/rcp45_mid/_hz_elevated.rds')

brk.vec <- seq(0,6,by=1)
col.func <- colorRampPalette(c('darkseagreen','red'))
plot(hs.ele.ls[['val']],breaks = brk.vec,col = col.func(6), 
     axis.args=list(at=head(brk.vec, -1)+0.5,
                    labels=head(brk.vec, -1),
                    cex.axis=0.6),
     main = 'Elevated Fuel RCP4.5 2045-2060',colNA="lightskyblue")
# mask upper corner
# upper.corner <- crop(hs.ele.ls[['val']],extent(c(146,150, -36.1, -33)))
# upper.corner[is.na(upper.corner)] <- 1
# plot(upper.corner,add=T,col='white',ann=F,axes=F,legend=FALSE)

library(oz)
vic(add=T,lwd=1,col='black')




# 
raster.new = stack(hs.ele.ls[["prob"]])

plot(max(raster.new))
plot(hs.ele.ls[['val']])

