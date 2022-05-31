source('r/get_vic_shape.r')

library(raster)
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

wrap.plot.rcp.prob.func <- function(var.in.nm,
                                    rcp.in,
                                    legend.in = letters[1:5],
                                    brk.in = seq(-0.4,0.4,by=0.1),
                                    lab.in = seq(-0.4,0.4,by=0.1),
                                    if.x.axis=FALSE,
                                    col.in = c('navy','darkgreen','palegreen3','lightgreen',
                                               # 'grey',
                                               'burlywood','coral','darkorange4','red')){
  # read model outputs####
  # 
  hz.ele.his <- read.future.prob.func(var.in.nm = var.in.nm,
                                      future_s = 'history',
                                      exclude.nm = 'noVeg')
  hist.ra <- mean(hz.ele.his,na.rm=T)
  # read for different rcps
  hz.ele.rcp45_mid <- read.future.prob.func(var.in.nm = var.in.nm,
                                            future_s = rcp.in,
                                            exclude.nm = 'noVeg')
  hz.ele.rcp45_mid.noVeg <- read.future.prob.func(var.in.nm = var.in.nm,
                                                  future_s = rcp.in,
                                                  exclude.nm = '')
  
  # hz.ele.rcp45_long <- read.future.prob.func(var.in.nm = var.in.nm,
  #                                           future_s = 'rcp45_long',
  #                                           exclude.nm = 'noVeg')
  # 
  # hz.ele.rcp85_mid <- read.future.prob.func(var.in.nm = var.in.nm,
  #                                           future_s = 'rcp85_mid',
  #                                           exclude.nm = 'noVeg')
  # hz.ele.rcp85_long <- read.future.prob.func(var.in.nm = var.in.nm,
  #                                            future_s = 'rcp85_long',
  #                                            exclude.nm = 'noVeg')
  # loop through the model
  # diff.rcp85_long <- hz.ele.rcp85_long - hz.ele.his
  # diff.rcp85_mid <- hz.ele.rcp85_mid - hz.ele.his
  # diff.rcp45_long <- hz.ele.rcp45_long - hz.ele.his
  diff.rcp45_mid <- hz.ele.rcp45_mid - hz.ele.his
  diff.rcp45_noVeg <- hz.ele.rcp45_mid.noVeg - hz.ele.rcp45_mid
  # # get mean probe among models
  # diff.rcp85_long.mean <- mean(diff.rcp85_long,na.rm=T)
  # diff.rcp85_mid.mean <- mean(diff.rcp85_mid,na.rm=T)
  # diff.rcp45_long.mean <- mean(diff.rcp45_long,na.rm=T)
  diff.rcp45_mid.mean <- mean(diff.rcp45_mid,na.rm=T)
  diff.rcp45_mid.mean_noVeg  <- mean(diff.rcp45_noVeg,na.rm=T)
  # plot(diff.rcp45_mid.mean)
  # target.ra<- ra.mean.his
  
  # # 
  # col.func <- colorRampPalette(c('darkseagreen','red'))
  # brk.hist <- seq(0,0.6,by=0.1)
  # hist.ra <- mask(hist.ra, shape.vic)
  # raster::plot(hist.ra,breaks = brk.hist,col = topo.colors(length(brk.hist)-1),#col.func(6), 
  #              axis.args=list(at=brk.hist,
  #                             labels=brk.hist),
  #              legend.width=1.5,
  #              main = '',xlab='',ylab='')
  # legend('topleft',legend = sprintf('(%s) History',legend.in[1]),bty='n')
  # a
  par(mar=c(1,3,1,1))
  diff.rcp45_mid.mean <- mask(diff.rcp45_mid.mean, shape.vic)
  raster::plot(diff.rcp45_mid.mean,breaks = brk.in,col = col.in, 
               # axis.args=list(at=brk.in,
               #                labels=lab.in),
               # legend.width=1.5,
               legend=FALSE,axes=F,bty="n", box=FALSE,
               main = '',xlab='',ylab='')
  axis(side = 2,at =seq(-40,-30,by=1),labels = seq(-40,-30,by=1))
  if(if.x.axis){
    axis(side = 1,at =seq(140,150,by=1),labels = seq(140,150,by=1))
  }
  legend('topleft',legend = sprintf('(%s)',legend.in[1]),bty='n')
  # b
  par(mar=c(1,1,1,3))
  diff.rcp45_mid.mean_noVeg <- mask(diff.rcp45_mid.mean_noVeg, shape.vic)
  raster::plot(diff.rcp45_mid.mean_noVeg,breaks = brk.in,col = col.in, 
               axis.args=list(at=brk.in,
                              labels=lab.in),
               legend.width=1.5,axes=F,bty="n", box=FALSE,
               main = '',xlab='',ylab='')
  legend('topleft',legend = sprintf('(%s)',legend.in[2]),bty='n')
  if(if.x.axis){
    axis(side = 1,at =seq(140,150,by=1),labels = seq(140,150,by=1))
  }
  
}

plot.prob.future.func <- function(par.name){
  par(mfrow=c(5,2)#,mar=c(2,2,1,3)
      )
  # plot history maps
  hz.ele.his <- read.future.prob.func(var.in.nm = par.name,
                                      future_s = 'history',
                                      exclude.nm = 'noVeg')
  hist.ra <- mean(hz.ele.his,na.rm=T)
  # set up plot
  par(mar=c(1,3,1,1))
  col.func <- colorRampPalette(c('darkseagreen','red'))
  brk.hist <- seq(0,0.6,by=0.1)
  hist.ra <- mask(hist.ra, shape.vic)
  raster::plot(hist.ra,breaks = brk.hist,col = topo.colors(length(brk.hist)-1),#col.func(6),
               axis.args=list(at=brk.hist,
                              labels=brk.hist),
               legend.width=1.5,axes=F,bty="n", box=FALSE,
               main = '',xlab='',ylab='')
  axis(side = 2,at =seq(-40,-30,by=1),labels = seq(-40,-30,by=1))
  legend('topleft',legend = sprintf('(%s)',letters[1]),bty='n')
  # plot empty map
  plot(0,col='white',ann=F,axes=F)
  # rcp45-Mid
  wrap.plot.rcp.prob.func(var.in.nm = par.name,
                          rcp.in= 'rcp45_mid',
                          legend.in = letters[2:3])
  # rcp45-long
  wrap.plot.rcp.prob.func(var.in.nm = par.name,
                          rcp.in= 'rcp45_long',
                          legend.in = letters[4:5])
  # rcp85-Mid
  wrap.plot.rcp.prob.func(var.in.nm = par.name,
                          rcp.in= 'rcp85_mid',
                          legend.in = letters[6:7])
  # rcp85-long
  wrap.plot.rcp.prob.func(var.in.nm = par.name,
                          rcp.in= 'rcp85_long',
                          legend.in = letters[8:9],if.x.axis = T)
}
####plot different scores
png('figures/future_prob_hz_ele.png',width = 300 * 2,200*5)
# par(cex=10)
par(oma=c(1,1,1,1))
plot.prob.future.func(par.name = '_hz_elevated.rds')

dev.off()

# 
png('figures/future_prob_hz_ns.png',width = 300 * 2,200*5)
# par(cex=10)
par(oma=c(1,1,1,1))
plot.prob.future.func(par.name = '_hz_ns.rds')

dev.off()

# 
png('figures/future_prob_hz_surface.png',width = 300 * 2,200*5)
# par(cex=10)
par(oma=c(1,1,1,1))
plot.prob.future.func(par.name = '_hz_surface.rds')

dev.off()

# 
png('figures/future_prob_hz_bark.png',width = 300 * 2,200*5)
# par(cex=10)
par(oma=c(1,1,1,1))
plot.prob.future.func(par.name = '_hz_bark.rds')

dev.off()





# dir.create('figures/future')
# 
# dir.names <- list.dirs('data/met/future/',recursive = F,full.names =T)
# for (i in seq_along(dir.names)) {
#   targ.path <- file.path('figures/future',dir.names[i])
#   if(!dir.exists(targ.path)){
#     dir.create(targ.path)            
#   }
#   
# }
# 
# # out.title <- 'Elevated Fule RCP4.5 204502060'
# 
# # base.nm <- basename(pred.fn)
# out.val.fn <- gsub(pattern = '.rds',replacement = '.png',x = pred.fn)
# out.prob.fn <- gsub(pattern = '.rds',replacement = '_prob.png',x = pred.fn)
# 
# out.val.fn <- gsub(pattern = 'data/met/future/',
#                    replacement = 'figures/future/',x = out.val.fn)
# dir.create(dirname(out.val.fn))
# out.prob.fn <- gsub(pattern = 'data/met/future/',
#                     replacement = 'figures/future/',x = out.prob.fn)
# 
pred.fn <- 'data/met/future/BNU-ESM/rcp45_mid/_hz_elevated.rds'
hs.ele.ls <- readRDS(pred.fn)
prob.ls <- hs.ele.ls[['prob']]
high.risk.4 <- prob.ls[['4']]
high.risk.5 <- prob.ls[['5']]


high.total.ra.mid <- high.risk.4 + high.risk.5
plot(high.total.ra.long - high.total.ra.mid)

