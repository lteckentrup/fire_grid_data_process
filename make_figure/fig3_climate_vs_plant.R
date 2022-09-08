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
source('r/process_input.R')

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
# 
hz.ele.future.noveg <- read.future.prob.func(var.in.nm = '_hz_elevated.rds',
                                       future_s = 'rcp85_long',
                                       exclude.nm = '')
future.ra.noveg <- mean(hz.ele.future.noveg,na.rm=T)
# 
change.ra <- future.ra - hist.ra
change.ra.noveg <- future.ra - future.ra.noveg 
# change.ra.noveg <- (hz.ele.future.noveg-hz.ele.future)
# change.ra.noveg <- range(change.ra.noveg,na.rm=T)
# as.vector(change.ra.noveg)
# as.vector(change.ra)
change.ra.vic <- get.small.area.func(change.ra,shape.vic)
change.ra.vic.noveg <- get.small.area.func(change.ra.noveg,shape.vic)
# plot(change.ra)

wi.vic.fine <- resample(wi.vic,change.ra.vic)
# plot(wi.vic.fine)

# plot(as.vector(change.ra.vic)~as.vector(1/wi.vic.fine),
#      pch=16,col=t_col('grey',90))

# get lcm
lcm.ra <- raster('D:/e drive back up/storage/equilibrium-lai-for-australia/downloads/LCM/luav4g9abll07811a02egigeo___/lu05v4ag/w001001.adf')
lcm.vic <- get.small.area.func(lcm.ra,shape.vic)
lcm.lut <- lcm.vic@data@attributes[[1]]
lcm.lut.natrua <- lcm.lut$ID[lcm.lut$LU_DESC %in% c('CONSERVATION AND NATURAL ENVIRONMENTS',
                                                    'PRODUCTION FROM RELATIVELY NATURAL ENVIRONMENTS')]


lcm.vic[!(lcm.vic %in% lcm.lut.natrua)] <- NA
# filter out non-natural
lcm.vic.fine <- resample(lcm.vic,wi.vic.fine)
wi.vic.lcm <- mask(wi.vic.fine,lcm.vic.fine)
change.ra.vic.lcm <- mask(change.ra.vic,lcm.vic.fine)
change.ra.vic.lcm.ng <- mask(change.ra.vic.noveg,lcm.vic.fine)
hist.ra.ng <-  mask(get.small.area.func(hist.ra,shape.vic),change.ra.vic.lcm.ng)
plot.df <- data.frame(Change = as.vector(change.ra.vic.lcm),
                      Change.ng = as.vector(change.ra.vic.lcm.ng),
                      current.val = as.vector(hist.ra.ng),
                      DI = as.vector((1/wi.vic.lcm)))
plot.df <- plot.df[order(plot.df$DI),]

# library(mgcv)
# gam.fit <- gam(Change~s(DI,k=5),data = plot.df)
# 
# plot(Change~DI,data = plot.df,
#      pch=16,col=t_col('grey',90))
# points(gam.fit$fitted.values~gam.fit$model$DI,type='l',col='red')
# abline(h=0)

# 
plot.df$DI.bin <- cut(plot.df$DI,breaks = seq(0,7,by=0.1))
levels(plot.df$DI.bin) <- seq(0.1,7,by=0.1)

# new.threshold <- boxplot.stats(plot.df$Change[plot.df$DI.bin==1])$stats[5]
# # 
# library(vioplot)
plot.df <- plot.df[!is.na(plot.df$Change),]
plot.df$DI.level <- as.character(plot.df$DI.bin)
plot.df$DI.level <- as.factor(plot.df$DI.level)
# vioplot(Change~DI.bin,data = plot.df)
# vioplot(plot.df$Change[plot.df$DI.bin==6])

elevate.df <- input.df[!is.na(input.df$elevated_hz),]
elevate.df$pet <- extract(pet.ra,cbind(elevate.df$lon,elevate.df$lat))
elevate.df$DI <- elevate.df$pet / elevate.df$map 
elevate.df$DI.bin <- cut(elevate.df$DI,breaks = seq(0,7,by=0.5))
levels(elevate.df$DI.bin) <- seq(0,7,by=0.5)
elevate.df$DI.bin <- droplevels(elevate.df$DI.bin)

elevate.df$hz.ele <- as.numeric(as.character(elevate.df$elevated_hz))
elevate.df <- elevate.df[elevate.df$hz.ele != 0,]
elevate.df <- elevate.df[,c("hz.ele","DI.bin",'DI')]
plot(hz.ele~DI.bin,data = elevate.df,pch='.',xlab='DI',
     ylab='Current probability')
plot(hz.ele~DI,data = elevate.df,pch='.',xlab='DI',
     ylab='Current probability',xlim=c(0,7))


vioplot(hz.ele~DI.bin,data = elevate.df[!is.na(elevate.df$DI.bin),])
# 
plot.df$DI.level <- droplevels(plot.df$DI.level)
plot.df$p.val <- NA 
for (f.i in seq_along(levels(plot.df$DI.bin))) {
  plot.sub.df <- plot.df[
    plot.df$DI.bin==levels(plot.df$DI.bin)[f.i],]

  
  if(nrow(plot.sub.df)>3){
    test.t.out <- t.test(plot.sub.df$Change)
    
    plot.df$p.val[plot.df$DI.bin ==
                    levels(plot.df$DI.bin)[f.i]]<- test.t.out$p.value
  }
  
}

sig.df <- plot.df[,c('DI.bin','p.val')]
sig.df <- sig.df[!duplicated(sig.df),]
sig.df <- sig.df[complete.cases(sig.df),]

# TukeyHSD(aov(Change~DI.level,data = plot.df))
# 

boxplot.stats(plot.df$Change.ng[plot.df$DI<2& plot.df$DI>0.75])$stats
dens=density(1/wi.vic.lcm,from=0, to=7)

pdf('figures/Climate and veg.pdf',width = 8,height = 2*6*.618)
par(mfrow=c(2,1),mar=c(5,5,1,1))
par(mar=c(1,5,4,1))
plot(current.val~DI.bin,data = plot.df,pch='.',xaxt='n',xlab=' ',
     ylab='Current probability',
     xlim=c(0,70))
legend('topleft',legend = '(a)',bty='n')
# 
par(mar=c(4,5,1,1))
plot(Change~DI.bin,data = plot.df,pch='.',xaxt='n',xlab='DI',
     ylim=c(-0.2,0.3),xlim=c(0,70))
axis(side = 1,at = seq(0.5,6.5,by=0.5) * 10 ,
     labels = seq(0.5,6.5,by=0.5))
abline(h=0,col='grey10')
# 

plot(Change.ng~DI.bin,
     data = plot.df,ylab='Change of probability',
     pch='.',xaxt='n',xlab='DI',add=T,col='forestgreen')
legend('topleft',legend = '(b)',bty='n')
par(new=T)
plot(dens$x * 10,
     dens$y,
     type="l",col='coral',lwd=2,
     xlim=c(0,70),
     ann=F,axes=F,ylim=c(0,5))
dev.off()
# par(new=T)
# hist(wi.vic.lcm,xlim=c(0.5,6.5),main='',xlab='',
#      freq =F,breaks = seq(0,7,by=0.1) )
# # sig.df$DI.bin <- as.numeric(as.character(sig.df$DI.bin))*10
# sig.df$position <- 0.3
# points(position~(DI.bin),data = sig.df[sig.df$p.val<0.01,],
#        pch='*')



