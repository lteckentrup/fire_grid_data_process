# plot(readRDS('data/met/future/ACCESS1-0/history/history_20002015_lai_jan_5km.rds'),
#      colNA="lightskyblue")
# 
# 
# 
# library(oz)
# vic(add=T,lwd=1,col='black',coast = F)

# make colot transprent##############
t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  
  ## Save the color
  invisible(t.col)
}

# #####
read.future.all.func <- function(var.in.nm,future_s,exclude.nm = 'noVeg'){
  rcp45_mid.fn <- list.files(path = 'data/met/future/',
                             pattern = var.in.nm,recursive = T,full.names = T)
  
  if(exclude.nm =='noVeg'){
    rcp45_mid.fn <- rcp45_mid.fn[!rcp45_mid.fn %in% 
                                   rcp45_mid.fn[grep(exclude.nm, rcp45_mid.fn)]]
  }else{
    rcp45_mid.fn <- rcp45_mid.fn[grep(exclude.nm, rcp45_mid.fn)]
  }
  rcp45_mid.fn <- rcp45_mid.fn[grep(future_s, rcp45_mid.fn)]
  
  rcp45.mid.h.c <- sapply(rcp45_mid.fn, function(fn.in){
   
    x <- readRDS(fn.in)
    return(x[['val']])
  })
  
  print(rcp45_mid.fn)
  raster.new = stack(rcp45.mid.h.c)
  return(raster.new)
}

# brk.vec <- seq(0.2,.6,by=0.1)
# col.func <- colorRampPalette(c('grey80','black'))
# function to get mode
Mode.func <- function(x) {
  ux <- unique(x)
  ux=ux[!is.na(ux)]
  ux[which.max(tabulate(match(x, ux)))]
}

# # read data
# hz.ele.rcp45.mid <- read.future.all.func(var.in.nm = 'noVeg_hz_elevated.rds',
#                                          future_s = 'rcp45_mid',
#                                          exclude.nm = '')
# 
# hz.ele.rcp45.long <- read.future.all.func(var.in.nm = 'noVeg_hz_elevated.rds',
#                                           future_s = 'rcp85_long',
#                                           exclude.nm = '')
# 
# # hist(calc(hz.ele.rcp45.mid, fun=Mode.func),probability  = T,breaks = (0:5)+0.5,
# #      col=t_col('grey'))
# # hist(calc(hz.ele.rcp45.long, fun=Mode.func),probability  = T,breaks = (0:5)+0.5,
# #      add=T,col=t_col('red'))


# # plot diff
# mean.ra <- calc(hz.ele.rcp45.mid, fun=Mode.func)
# mean.ra.2 <- calc(hz.ele.rcp45.long, fun=Mode.func)
# diff.ra <- (mean.ra.2 - mean.ra)
# plot(diff.ra)
# diff.ra[diff.ra>0]
# 
# diff.df <- as.data.frame(diff.ra, xy=TRUE)
# diff.df <- diff.df[diff.df$layer !=0,]
# diff.df <- diff.df[complete.cases(diff.df),]

# functions####
plot.1rcp.func <- function(future_s,var.in.nm,exclude.nm,ra.ele.mode.his,
                           brk.in = seq(-4.5,4.5,by=1),
                           lab.in = seq(-4,4,by=1),
                           col.in = c('navy','darkgreen','palegreen3','lightgreen',
                                   'grey',
                                   'burlywood','coral','darkorange4','red')){
  # read data
  hz.ele.rcp45_mid <- read.future.all.func(var.in.nm = var.in.nm,
                                           future_s = future_s,
                                           exclude.nm = exclude.nm)
  # get mode
  ra.rcp45_mid.mode.his <- calc(hz.ele.rcp45_mid, fun=Mode.func)
  # 
  plot(ra.rcp45_mid.mode.his - ra.ele.mode.his,breaks = brk.in,
       col=col.in,colNA = 'lightskyblue',
       axis.args=list(at=lab.in,
                      labels=lab.in,
                      cex.axis=1),legend.width=1.5,
       xlab='',ylab='')
}
# plot(ra.rcp45_mid.mode.his,col=heat.colors(7))
# 
wrap.plot.rcp.func <- function(var.in.nm,
                               exclude.nm,
                               legend.in = letters[1:5],
                               brk.in = seq(0,6,by=1),
                               col.in = c('navy','darkgreen','palegreen3','lightgreen',
                                          'grey',
                                          'burlywood','coral','darkorange4','red')){
  # history ele hz####
 if(exclude.nm!='noVeg'){
   plot(0,ann=F,axes=F,col='white')
 }else{
   hz.ele.his <- read.future.all.func(var.in.nm = var.in.nm,
                                      future_s = 'history',
                                      exclude.nm = exclude.nm)
   ra.mode.his <- calc(hz.ele.his, fun=Mode.func)
   
   target.ra<- ra.mode.his
   brk.vec <- brk.in   
   at.val <- head(brk.vec, -1)+0.5
   lab.val <- head(brk.vec, -1)
   
   col.func <- colorRampPalette(c('darkseagreen','red'))
   
   raster::plot(target.ra,breaks = brk.vec,col = col.func(length(brk.vec)-1), 
                 axis.args=list(at=at.val,
                                labels=lab.val),
                 legend.width=1.5,
                 main = '',colNA="lightskyblue",xlab='',ylab='')
   legend('topleft',legend = sprintf('(%s) History',legend.in[1]),bty='n')
 }

  # #rcp45 mid####
  if(exclude.nm!='noVeg'){
    hz.ele.his <- read.future.all.func(var.in.nm = gsub(pattern = 'noVeg',
                                                        replacement = '',
                                                        x = var.in.nm),
                                       future_s = 'rcp45_mid',
                                       exclude.nm = 'noVeg')
    ra.mode.his <- calc(hz.ele.his, fun=Mode.func)
  }
  
  plot.1rcp.func(future_s = 'rcp45_mid',
                 var.in.nm = var.in.nm,
                 exclude.nm = exclude.nm,
                 ra.ele.mode.his = ra.mode.his,col.in = col.in)
  legend('topleft',legend = sprintf('(%s) RCP4.5 2045-2060',legend.in[2]),bty='n')
  
  # ##rcp45 long#
  if(exclude.nm!='noVeg'){
    hz.ele.his <- read.future.all.func(var.in.nm = gsub(pattern = 'noVeg',
                                                        replacement = '',
                                                        x = var.in.nm),
                                       future_s = 'rcp45_long',
                                       exclude.nm = 'noVeg')
    ra.mode.his <- calc(hz.ele.his, fun=Mode.func)
  }
  plot.1rcp.func(future_s = 'rcp45_long',
                 var.in.nm = var.in.nm,
                 exclude.nm = exclude.nm,
                 ra.ele.mode.his = ra.mode.his,col.in = col.in)
  legend('topleft',legend = sprintf('(%s) RCP4.5 2085-2100',legend.in[3]),bty='n')
  # rcp85 mid
  if(exclude.nm!='noVeg'){
    hz.ele.his <- read.future.all.func(var.in.nm = gsub(pattern = 'noVeg',
                                                        replacement = '',
                                                        x = var.in.nm),
                                       future_s = 'rcp85_mid',
                                       exclude.nm = 'noVeg')
    ra.mode.his <- calc(hz.ele.his, fun=Mode.func)
  }
  plot.1rcp.func(future_s = 'rcp85_mid',
                 var.in.nm = var.in.nm,
                 exclude.nm = exclude.nm,
                 ra.ele.mode.his = ra.mode.his,col.in = col.in)
  legend('topleft',legend = sprintf('(%s) RCP8.5 2045-2060',legend.in[4]),bty='n')
  # rcp85 long
  if(exclude.nm!='noVeg'){
    hz.ele.his <- read.future.all.func(var.in.nm = gsub(pattern = 'noVeg',
                                                        replacement = '',
                                                        x = var.in.nm),
                                       future_s = 'rcp85_long',
                                       exclude.nm = 'noVeg')
    ra.mode.his <- calc(hz.ele.his, fun=Mode.func)
  }
  # 
  plot.1rcp.func(future_s = 'rcp85_long',
                 var.in.nm = var.in.nm,
                 exclude.nm = exclude.nm,
                 ra.ele.mode.his = ra.mode.his,col.in = col.in)
  legend('topleft',legend = sprintf('(%s) RCP8.5 2085-2100',legend.in[5]),bty='n')
  # hz.ele.rcp45_mid <- read.future.all.func(var.in.nm = '_hz_elevated.rds',
  #                                    future_s = 'rcp45_mid',
  #                                    exclude.nm = 'noVeg')
  # ra.rcp45_mid.mode.his <- calc(hz.ele.rcp45_mid, fun=Mode.func)
  # # 
  # plot(ra.rcp45_mid.mode.his - ra.ele.mode.his,breaks = seq(-3.5,3.5,by=1),
  #      col=c('darkgreen','palegreen3','lightgreen',
  #            'grey',
  #            'lightpink','coral','red'),colNA = 'lightskyblue',
  #      axis.args=list(at=seq(-3,3,by=1),
  #                     labels=seq(-3,3,by=1),
  #                     cex.axis=0.6))

}
# functions height####
plot.1rcp.height.func <- function(future_s,var.in.nm,exclude.nm,ra.ele.mode.his,
                           brk.in = seq(-5.5,5.5,by=1),
                           lab.in = seq(-5,5,by=1),
                           col.in = c('navy','darkgreen','palegreen3','lightgreen',
                                      'grey',
                                      'burlywood','coral','darkorange4','red')){
  # read data
  hz.ele.rcp45_mid <- read.future.all.func(var.in.nm = var.in.nm,
                                           future_s = future_s,
                                           exclude.nm = exclude.nm)
  # get mode
  ra.rcp45_mid.mode.his <- calc(hz.ele.rcp45_mid, fun=mean,na.rm=T)
  # 
  col.func <- colorRampPalette(c('navy','red'))
  plot(exp(ra.rcp45_mid.mode.his) - exp(ra.ele.mode.his),breaks = brk.in,
       col=col.func(length(brk.in)-1),colNA = 'lightskyblue',
       axis.args=list(at=lab.in,
                      labels=lab.in,
                      cex.axis=1),legend.width=1.5,
       xlab='',ylab='')
}
# plot(ra.rcp45_mid.mode.his,col=heat.colors(7))
# 
wrap.plot.rcp.height.func <- function(var.in.nm,
                               exclude.nm,
                               legend.in = letters[1:5],
                               # brk.in = seq(0,30,by=5),
                               col.in = heat.colors(5)){
  # history ele hz####
  if(exclude.nm!='noVeg'){
    plot(0,ann=F,axes=F,col='white')
  }else{
    hz.ele.his <- read.future.all.func(var.in.nm = var.in.nm,
                                       future_s = 'history',
                                       exclude.nm = exclude.nm)
    ra.mode.his <- calc(hz.ele.his, fun=mean,na.rm=T)
    
    target.ra<- ra.mode.his
    
    # 
    col.func <- colorRampPalette(c('darkseagreen','red'))
    
    if(cellStats(exp(target.ra), max)>25){
      brk.vec <- seq(15,40,by=5)  

    }else{
      brk.vec <- seq(0,25,by=5)   

    }
    
    at.val <- head(brk.vec, -1)
    lab.val <- head(brk.vec, -1)
    
    raster::plot(exp(target.ra),breaks = brk.vec,col = col.func(length(brk.vec)-1), 
                 axis.args=list(at=at.val,
                                labels=lab.val),
                 legend.width=1.5,
                 main = '',colNA="lightskyblue",xlab='',ylab='')
    legend('topleft',legend = sprintf('(%s) History',legend.in[1]),bty='n')
  }
  
  # #rcp45 mid####
  if(exclude.nm!='noVeg'){
    hz.ele.his <- read.future.all.func(var.in.nm = gsub(pattern = 'noVeg',
                                                        replacement = '',
                                                        x = var.in.nm),
                                       future_s = 'rcp45_mid',
                                       exclude.nm = 'noVeg')
    ra.mode.his <- calc(hz.ele.his, fun=mean,na.rm=T)
  }
  
  plot.1rcp.height.func(future_s = 'rcp45_mid',
                 var.in.nm = var.in.nm,
                 exclude.nm = exclude.nm,
                 ra.ele.mode.his = ra.mode.his,col.in = col.in)
  legend('topleft',legend = sprintf('(%s) RCP4.5 2045-2060',legend.in[2]),bty='n')
  
  # ##rcp45 long#
  if(exclude.nm!='noVeg'){
    hz.ele.his <- read.future.all.func(var.in.nm = gsub(pattern = 'noVeg',
                                                        replacement = '',
                                                        x = var.in.nm),
                                       future_s = 'rcp45_long',
                                       exclude.nm = 'noVeg')
    ra.mode.his <- calc(hz.ele.his, fun=mean,na.rm=T)
  }
  plot.1rcp.height.func(future_s = 'rcp45_long',
                 var.in.nm = var.in.nm,
                 exclude.nm = exclude.nm,
                 ra.ele.mode.his = ra.mode.his,col.in = col.in)
  legend('topleft',legend = sprintf('(%s) RCP4.5 2085-2100',legend.in[3]),bty='n')
  # rcp85 mid
  if(exclude.nm!='noVeg'){
    hz.ele.his <- read.future.all.func(var.in.nm = gsub(pattern = 'noVeg',
                                                        replacement = '',
                                                        x = var.in.nm),
                                       future_s = 'rcp85_mid',
                                       exclude.nm = 'noVeg')
    ra.mode.his <- calc(hz.ele.his, fun=mean)
  }
  plot.1rcp.height.func(future_s = 'rcp85_mid',
                 var.in.nm = var.in.nm,
                 exclude.nm = exclude.nm,
                 ra.ele.mode.his = ra.mode.his,col.in = col.in)
  legend('topleft',legend = sprintf('(%s) RCP8.5 2045-2060',legend.in[4]),bty='n')
  # rcp85 long
  if(exclude.nm!='noVeg'){
    hz.ele.his <- read.future.all.func(var.in.nm = gsub(pattern = 'noVeg',
                                                        replacement = '',
                                                        x = var.in.nm),
                                       future_s = 'rcp85_long',
                                       exclude.nm = 'noVeg')
    ra.mode.his <- calc(hz.ele.his, fun=mean,na.rm=T)
  }
  # 
  plot.1rcp.height.func(future_s = 'rcp85_long',
                 var.in.nm = var.in.nm,
                 exclude.nm = exclude.nm,
                 ra.ele.mode.his = ra.mode.his,col.in = col.in)
  legend('topleft',legend = sprintf('(%s) RCP8.5 2085-2100',legend.in[5]),bty='n')
  # hz.ele.rcp45_mid <- read.future.all.func(var.in.nm = '_hz_elevated.rds',
  #                                    future_s = 'rcp45_mid',
  #                                    exclude.nm = 'noVeg')
  # ra.rcp45_mid.mode.his <- calc(hz.ele.rcp45_mid, fun=Mode.func)
  # # 
  # plot(ra.rcp45_mid.mode.his - ra.ele.mode.his,breaks = seq(-3.5,3.5,by=1),
  #      col=c('darkgreen','palegreen3','lightgreen',
  #            'grey',
  #            'lightpink','coral','red'),colNA = 'lightskyblue',
  #      axis.args=list(at=seq(-3,3,by=1),
  #                     labels=seq(-3,3,by=1),
  #                     cex.axis=0.6))
  
}
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