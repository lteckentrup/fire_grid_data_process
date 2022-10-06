source('r/get_vic_shape.r')
library(wesanderson)
library(raster)
source('r/functions_plot.R')
# wes_palette("Zissou1", n = 4)
# wes_palette("GrandBudapest1", n = 4)[c(1,2,4,3)]
# c('navy','darkgreen','palegreen3','grey40',
#   # 'grey',
#   'grey60','coral','darkorange4','red')
read.future.prob.old.func <- function(var.in.nm,future_s,exclude.nm = 'noVeg'){
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

par.name <-  "rain.season._hz_elevated"
hz.ele.ref <- read.future.prob.old.func(var.in.nm = par.name,
                                        future_s = 'hist',
                                        exclude.nm = 'noVeg')
hist.ra <- mean(hz.ele.ref,na.rm=T)

hz.ele.real <- read.future.prob.old.func(var.in.nm = par.name,
                                        future_s = 'rcp85_long',
                                        exclude.nm = 'noVeg')
read.ra <- mean(hz.ele.real,na.rm=T)

real.change.ra <- read.ra - hist.ra

hz.ele.his.orignal <- read.future.prob.func(var.in.nm = '_hz_elevated.rds',
                                            future_s = 'rcp85_long',
                                            exclude.nm = 'noVeg')
hist.ra.ori <- mean(hz.ele.his.orignal,na.rm=T)

ori.change.ra <- hist.ra.ori - hist.ra

pdf('figures/hist_rain_seasonality_impact.pdf',width = 8,height = 8*.618)

  c1 <- rgb(173,216,230,max = 255, alpha = 90, names = "lt.blue")
  c2 <- rgb(255,192,203, max = 255, alpha = 90, names = "lt.pink")
  
  brk.vec <- seq(-0.15,0.4,by=0.01)
  hgA <- hist(ori.change.ra, breaks = brk.vec, plot = FALSE) # Save first histogram data
  hgB <- hist(real.change.ra, breaks = brk.vec, plot = FALSE) # Save 2nd histogram data
  
  plot(hgB, border = NA,col = c1,main='',xlab='Change of Probability') # Plot 1st histogram using a transparent color
  plot(hgA, border = NA,col = c2, add = TRUE)
  
  legend('topright',legend = c('Constant',
                               'Predicted'),pch=15,col=c(c1,c2),
         bty='n',title = 'Rainfall seasonality')
  
dev.off()

