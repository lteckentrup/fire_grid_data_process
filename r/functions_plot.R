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
  print(rcp45_mid.fn)
  rcp45.mid.h.c <- sapply(rcp45_mid.fn, function(fn.in){
    
    x <- readRDS(fn.in)
    return(x[['val']])
  })
  
  # print(rcp45_mid.fn)
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
  # legend('topleft',legend = sprintf('(%s)',legend.in[1]),bty='n')
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
  legend('topleft',legend = c('(a) RCP4.5: 2045-2060 minus 2000-2015'),bty='n')
  # rcp45-long
  wrap.plot.rcp.prob.func(var.in.nm = par.name,
                          rcp.in= 'rcp45_long',
                          legend.in = letters[2])
  legend('topleft',legend = c('(b) RCP4.5: 2085-2100 minus 2000-2015'),bty='n')
  # rcp85-Mid
  wrap.plot.rcp.prob.func(var.in.nm = par.name,
                          rcp.in= 'rcp85_mid',
                          legend.in = letters[3],
                          if.y.axis = T,
                          if.x.axis = if.x.axis)
  legend('topleft',legend = c('(c) RCP8.5: 2045-2060 minus 2000-2015'),bty='n')
  # rcp85-long
  wrap.plot.rcp.prob.func(var.in.nm = par.name,
                          rcp.in= 'rcp85_long',
                          legend.in = letters[4],if.x.axis = if.x.axis)
  legend('topleft',legend = c('(d) RCP8.5: 2085-2100 minus 2000-2015'),bty='n')
}
