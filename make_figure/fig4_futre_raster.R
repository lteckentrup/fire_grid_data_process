source('r/get_vic_shape.r')
library(wesanderson)
col.vec <- c(wes_palette("GrandBudapest1", n = 4)[c(3,4,2)],
             'grey80','grey70',
             wes_palette("Zissou1", n = 4)[1:3])
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
  rcp45_mid.fn <- rcp45_mid.fn[!rcp45_mid.fn %in% 
                                 rcp45_mid.fn[grep('rain', rcp45_mid.fn)]]
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
                                    if.y.axis=FALSE,
                                    col.in = col.vec){
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
  # hz.ele.rcp45_mid.noVeg <- read.future.prob.func(var.in.nm = var.in.nm,
  #                                                 future_s = rcp.in,
  #                                                 exclude.nm = '')
  
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
  # diff.rcp45_noVeg <- hz.ele.rcp45_mid.noVeg - hz.ele.rcp45_mid
  # # get mean probe among models
  # diff.rcp85_long.mean <- mean(diff.rcp85_long,na.rm=T)
  # diff.rcp85_mid.mean <- mean(diff.rcp85_mid,na.rm=T)
  # diff.rcp45_long.mean <- mean(diff.rcp45_long,na.rm=T)
  diff.rcp45_mid.mean <- mean(diff.rcp45_mid,na.rm=T)
  # diff.rcp45_mid.mean_noVeg  <- mean(diff.rcp45_noVeg,na.rm=T)
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
  
  if(if.y.axis){
    axis(side = 2,at =seq(-40,-30,by=1),labels = seq(-40,-30,by=1))
  }
  
  
  if(if.x.axis){
    axis(side = 1,at =seq(140,150,by=1),labels = seq(140,150,by=1))
  }
  legend('topleft',legend = sprintf('(%s)',legend.in[1]),bty='n')
  # # b
  # par(mar=c(1,1,1,3))
  # diff.rcp45_mid.mean_noVeg <- mask(diff.rcp45_mid.mean_noVeg, shape.vic)
  # raster::plot(diff.rcp45_mid.mean_noVeg,breaks = brk.in,col = col.in, 
  #              axis.args=list(at=brk.in,
  #                             labels=lab.in),
  #              legend.width=1.5,axes=F,bty="n", box=FALSE,
  #              main = '',xlab='',ylab='')
  # legend('topleft',legend = sprintf('%s',legend.in[2]),bty='n')
  # if(if.x.axis){
  #   axis(side = 1,at =seq(140,150,by=1),labels = seq(140,150,by=1))
  # }
  
}

plot.prob.future.change.func <- function(par.name,l.in,if.x.axis){
  # par(mfrow=c(1,2)#,mar=c(2,2,1,3)
  # )
  # plot history maps
  hz.ele.his <- read.future.prob.func(var.in.nm = par.name,
                                      future_s = 'history',
                                      exclude.nm = 'noVeg')
  hist.ra <- mean(hz.ele.his,na.rm=T)
  # set up plot
  par(mar=c(3,3,1,1))
  col.func <- colorRampPalette(c('darkseagreen','red'))
  brk.hist <- seq(0,0.6,by=0.1)
  hist.ra <- mask(hist.ra, shape.vic)
  # raster::plot(hist.ra,breaks = brk.hist,col = topo.colors(length(brk.hist)-1),#col.func(6),
  #              axis.args=list(at=brk.hist,
  #                             labels=brk.hist),
  #              legend.width=1.5,axes=F,bty="n", box=FALSE,
  #              main = '',xlab='',ylab='')
  # axis(side = 2,at =seq(-40,-30,by=1),labels = seq(-40,-30,by=1))
  # legend('topleft',legend = sprintf('(%s)',letters[1]),bty='n')
  # # plot empty map
  # plot(0,col='white',ann=F,axes=F)
  # rcp45-Mid
  wrap.plot.rcp.prob.func(var.in.nm = par.name,
                          rcp.in= 'rcp45_mid',
                          legend.in = letters[1],if.y.axis = T)
  # rcp45-long
  wrap.plot.rcp.prob.func(var.in.nm = par.name,
                          rcp.in= 'rcp45_long',
                          legend.in = letters[2])
  # rcp85-Mid
  wrap.plot.rcp.prob.func(var.in.nm = par.name,
                          rcp.in= 'rcp85_mid',
                          legend.in = letters[3],
                          if.y.axis = T,
                          if.x.axis = if.x.axis)
  # rcp85-long
  wrap.plot.rcp.prob.func(var.in.nm = par.name,
                          rcp.in= 'rcp85_long',
                          legend.in = letters[4],if.x.axis = if.x.axis)
}
####plot different scores
# png('figures/future_prob_hz_rcp85_2100.png',width = 300 * 2*2,200*4*2)
pdf('figures/fig4_future_prob_hz_rcp85_2100.pdf',width = 4*2,height=3*2)
# par(cex=10)
par(mfrow=c(2,2))
par(oma=c(1,1,1,1),mar=c(4,3,1,0))
plot.prob.future.change.func(par.name = '_hz_elevated.rds',
                             l.in=c('(a) Elevated fuel score',
                                    '(b) Elevated fuel score'),
                             if.x.axis = T)


# lab.in = seq(-0.4,0.4,by=0.1)
# col.in = c('navy','darkgreen','palegreen3','lightgreen',
#            # 'grey',
#            'burlywood','coral','darkorange4','red')
par(new=T)
par(mfrow=c(1,1))
plot(0,pch=NA,ann=F,axes=F)
legend(x = 0.6,y=1.25,
       legend = c('-0.2 - 0.1','-0.1 - 0','0 - 0.1',
                        '0.1 - 0.2','0.2 - 0.3',
                        '0.3 - 0.4'),
       col = col.vec[3:8],
       pch=15,bty='n',horiz = T,xpd=T)
# plot.prob.future.change.func(par.name = '_hz_bark.rds',
#                              l.in=c('(c) Bark fuel score',
#                                     '(d) Bark fuel score'),
#                              if.x.axis = F)

######
par(mfrow=c(2,2))
par(oma=c(1,1,1,1),mar=c(4,3,1,0))

plot.prob.future.change.func(par.name = '_hz_ns.rds',
                             l.in=c('(e) Near surface fuel score',
                                    '(f) Near surface fuel score'),
                             if.x.axis = T)
par(new=T)
par(mfrow=c(1,1))
plot(0,pch=NA,ann=F,axes=F)
legend(x = 0.6,y=1.25,
       legend = c('-0.2 - 0.1','-0.1 - 0','0 - 0.1',
                  '0.1 - 0.2'),
       col = col.vec[3:8],
       pch=15,bty='n',horiz = T,xpd=T)
##########
par(mfrow=c(2,2))
par(oma=c(1,1,1,1),mar=c(4,3,1,0))
plot.prob.future.change.func(par.name = '_hz_surface.rds',
                             l.in=c('(g) Surface fuel score',
                                    '(h) Surface fuel score'),
                             if.x.axis = T)
par(new=T)
par(mfrow=c(1,1))
plot(0,pch=NA,ann=F,axes=F)
legend(x = 0.6,y=1.25,
       legend = c('-0.2 - 0.1','-0.1 - 0','0 - 0.1'),
       col = col.vec[3:8],
       pch=15,bty='n',horiz = T,xpd=T)
dev.off()

