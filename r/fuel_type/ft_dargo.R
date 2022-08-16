library(RColorBrewer)

n <- 38
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector.all = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
col_vector <- sample(col_vector.all, n)
devtools::source_url("https://github.com/Jinyan-Yang/colors/blob/master/R/col.R?raw=TRUE")
library(raster)
# set up dargo gps###
lat.vec <- -c(37.33588,37.38554,37.38554,37.33588)
lon.vec <-  c(147.27566,147.27566,147.32566,147.32566)
p = spPolygons(matrix(c(lon.vec,lat.vec), ncol=2, byrow = F))
# p = spPolygons(list(Polygons(list(p), "p")))

##subset map#####
evc.df.sub.clean <- readRDS('cache/evc.lon.lat.masked.rds')
evc.dargo <- get.small.area.func(evc.df.sub.clean,p)
# 
centre.gps <- c(mean(lat.vec), mean(lon.vec))

# check dargo area###
# library(oz)
# vic(col='grey',lwd=3)
# polygon(x = lon.vec,y=lat.vec,col='red')
get.small.area.func <- function(ra.in,p = p){
  r2 <- crop(ra.in, extent(p))
  r3 <- mask(r2, p)
  # crs(r3) <- crs(ra.in)
  # aggregate(r3, fact=3)
  return(r3)
}

# functions####
source('r/functions_predict.R')
# read topo####
# 
c.small <- raster('data/topo/curvature_profile/90m/profile_curvature_3s.tif')
c.small <- get.small.area.func(c.small,p=p)
c.small <- disaggregate(c.small,fact=3)
# 
c.plan.ra.i <- raster('data/topo/curvature_plan/PlanCurvature_1_arc-second_resolution/tiles/e147/s38/e147s38/w001001.adf')
# c.plan.ra <- get.small.area.func(c.plan.ra.i,p=p)
c.plan.ra <- get.small.area.func(c.plan.ra.i,p=c.small)
# 
wi.ra <- raster('data/topo/wetness_index/TopographicWetnessIndex_1_arcsecond_resolution/tiles/e147/s38/e147s38/w001001.adf')
# wi.ra <- raster::extract(x = wi.ra,
#                            y = p)[[1]]
# wi.ra <- get.small.area.func(wi.ra,p=p)
wi.ra <- get.small.area.func(wi.ra,p=c.small)
# wi.ra <- disaggregate(wi.ra,fact=3)
# 

rad.jan <- raster('data/topo/rad/SRADTotalShortwaveSlopingSurf_0115_lzw.tif')

# r2 <- crop(rad.jan, extent(p))
# r3 <- mask(r2, p)
# crs(r3) <- crs(c.plan.ra.i)
# r4 <- aggregate(r3, fact=3)

# rad.jan <- get.small.area.func(rad.jan,p=p)#raster::extract(x = r4,y = p)[[1]]
rad.jan <- get.small.area.func(rad.jan,p=c.small)
# 
rad.jul <- raster('data/topo/rad/SRADTotalShortwaveSlopingSurf_0715_lzw.tif')
# r2 <- crop(rad.jul, extent(p))
# r3 <- mask(r2, p)
# crs(r3) <- crs(c.plan.ra.i)
# r4 <- aggregate(r3, fact=3)

# rad.jul <- get.small.area.func(rad.jul,p=p)#raster::extract(x = r4,y = p)[[1]]
rad.jul <- get.small.area.func(rad.jul,p=c.small)

# read soil####
soil.den <- raster('data/soil/BDW_000_005_EV_N_P_AU_TRN_N_20140801.tif')
# soil.den <- raster::extract(x = soil.den,y = p)[[1]]
soil.den <- get.small.area.func(soil.den,
                                p=spPolygons(matrix(c(147,148,148,147,
                                                      -37,-37,-38,-38), 
                                                    ncol=2, byrow = F)))
soil.den <- disaggregate(soil.den,fact=3)
# soil.den <-  extend(soil.den, extent(c.plan.ra))
soil.den <- get.small.area.func(soil.den,p=c.small)

# plot(soil.den,xlim=c(147.24,147.4),ylim=c(-37.38,-37.3))
# soil.ph <- raster('data/soil/BDW_000_005_EV_N_P_AU_TRN_N_20140801.tif')
# soil.den <- raster::extract(x = soil.den,
#                             y = p)
soil.clay <- raster('data/soil/CLY_000_005_EV_N_P_AU_TRN_N_20140801.tif')
soil.clay <- get.small.area.func(soil.clay,
                                 p=spPolygons(matrix(c(147,148,148,147,
                                                       -37,-37,-38,-38), 
                                                     ncol=2, byrow = F)))
soil.clay <- disaggregate(soil.clay,fact=3)
# soil.clay <-  extend(soil.clay, extent(c.small))
soil.clay <- get.small.area.func(soil.clay,p=c.small)
# soil.den <- get.small.area.func(soil.den,p=c.small)


#function to predict with cmip clim#### 
predict.rf.cmip.fine.reso.func <- function(path.nm,model.path,out.nm,area.in){
  # # read inputs####
  # read met
  # tmax.ra <- readRDS('data/met/future/ACCESS1-0/rcp45_mid/rcp45_20452060_monthly_tmax.rds')[[1]]
  # pr.ra <- readRDS('data/met/future/ACCESS1-0/rcp45_mid/rcp45_20452060_monthly_pr.rds')[[1]]
  # rh.ra <- readRDS('data/met/future/ACCESS1-0/rcp45_mid/rcp45_20452060_monthly_rh.rds')[[1]]
  # path.nm <-  'data/met/future/ACCESS1-0/rcp45_mid/'
  # path.nm <-  "data/met/future//ACCESS1-0/history"
  
  tmax.ra <- readRDS(list.files(path = path.nm,pattern = '_monthly_tmax.rds',full.names = T))[[1]]
  # plot(tmax.ra)
  # points(x = centre.gps[2],y=centre.gps[1])
  tmax.ra <- raster::extract(x = tmax.ra,
                             y = rbind(centre.gps[c(2,1)]),
                             buffer = 1000,na.rm=T)[[1]]
  
  pr.ra <- readRDS(list.files(path = path.nm,pattern = '_monthly_pr.rds',full.names = T))[[1]]
  pr.ra <- raster::extract(x = pr.ra,
                           y = rbind(centre.gps[c(2,1)]),
                           buffer = 1000,na.rm=T)[[1]]
  rh.ra <- readRDS(list.files(path = path.nm,pattern = '_monthly_rh.rds',full.names = T))[[1]]
  rh.ra <- raster::extract(x = rh.ra,
                           y = rbind(centre.gps[c(2,1)]),
                           buffer = 1000,na.rm=T)[[1]]
  
  tmax.mean.ra <- readRDS(list.files(path = path.nm,pattern = '_annual_tmax.rds',full.names = T))
  tmax.mean.ra <- raster::extract(x = tmax.mean.ra,
                                  y = rbind(centre.gps[c(2,1)]),
                                  buffer = 1000,na.rm=T)[[1]]
  
  pr.mean.ra <- readRDS(list.files(path = path.nm,pattern = '_annual_pr.rds',full.names = T))
  pr.mean.ra <- raster::extract(x = pr.mean.ra,
                                y = rbind(centre.gps[c(2,1)]),
                                buffer = 1000,na.rm=T)[[1]]
  
  # read proper rainfall seasonality
  is.his <- grep(x = path.nm,pattern = 'history')
  is.45.mid <- grep(x = path.nm,pattern = 'rcp45_mid')
  is.45.long <- grep(x = path.nm,pattern = 'rcp45_long')
  is.85.mid <- grep(x = path.nm,pattern = 'rcp85_mid')
  is.85.long <- grep(x = path.nm,pattern = 'rcp85_long')
  
  if(length(is.his)>0){
    pr.season.ra <- readRDS('cache/pr_seaonality_history.rds')
  }else if(length(is.45.mid)>0){
    pr.season.ra <- readRDS('cache/pr_seaonality_rcp45_mid.rds')
  }else if(length(is.45.long)>0){
    pr.season.ra <- readRDS('cache/pr_seaonality_rcp45_long.rds')
  }else if(length(is.85.long)>0){
    pr.season.ra <- readRDS('cache/pr_seaonality_rcp85_long.rds')
  }else if(length(is.85.mid)>0){
    pr.season.ra <- readRDS('cache/pr_seaonality_rcp85_mid.rds')
  }else{
    stop('did not find rainfall seasonality for the RCP')
  }
  pr.season.ra <- raster::extract(x = pr.season.ra,
                                  y = rbind(centre.gps[c(2,1)]),
                                  buffer = 1000,na.rm=T)[[1]]
  # read lai
  # lai.ra <- readRDS(paste0(path.nm,'_lai_jan_5km.rds'))
  lai.ra <- readRDS(list.files(path = path.nm,pattern = '_lai_jan_5km.rds',full.names = T))
  lai.ra <- raster::extract(x = lai.ra,
                            y = rbind(centre.gps[c(2,1)]),
                            buffer = 1000,na.rm=T)[[1]]
  
  model.rf <- readRDS(model.path)#'cache/rf.fit.hz.surface.rds'
  # model.rf <- readRDS('cache/rf.fit.hs.elevated.rds')#
  
  # get predicted probbility
  size.others <- length(soil.den)
  prob.m <- try(predict.rf.func(model.in = model.rf,
                                s.den=matrix(soil.den),s.clay= matrix(soil.clay),
                                rad.jan = matrix(rad.jan),rad.jul = matrix(rad.jul),
                                wi = matrix(wi.ra),c.profile = matrix(c.small),c.plan = matrix(c.plan.ra),
                                tmax = matrix(rep(tmax.ra,size.others)),
                                rain = matrix(rep(pr.ra,size.others)),
                                rh.min = matrix(rep(rh.ra,size.others)),
                                tmax.mean = matrix(rep(tmax.mean.ra,size.others)),
                                map = matrix(rep(pr.mean.ra,size.others)),
                                pr.seaonality = matrix(rep(pr.season.ra,size.others)),
                                lai.opt = matrix(rep(lai.ra,size.others)),
                                giveProb = T))
  # get predicted value
  rf.m <- predict.rf.func(model.in = model.rf,
                          s.den=matrix(soil.den),s.clay= matrix(soil.clay),
                          rad.jan = matrix(rad.jan),rad.jul = matrix(rad.jul),
                          wi = matrix(wi.ra),c.profile = matrix(c.small),c.plan = matrix(c.plan.ra),
                          tmax = matrix(rep(tmax.ra,size.others)),
                          rain = matrix(rep(pr.ra,size.others)),
                          rh.min = matrix(rep(rh.ra,size.others)),
                          tmax.mean = matrix(rep(tmax.mean.ra,size.others)),
                          map = matrix(rep(pr.mean.ra,size.others)),
                          pr.seaonality = matrix(rep(pr.season.ra,size.others)),
                          lai.opt = matrix(rep(lai.ra,size.others)),
                          giveProb = F)
  
  # save prediction
  var.m <- matrix(as.numeric(as.character(rf.m)),
                  ncol = ncol(c.small),
                  nrow = nrow(c.small),byrow = T)
  
  
  score.ra <-raster(var.m)
  
  extent(score.ra) <- extent(soil.den)
  # save prob
  if(class(prob.m) != 'try-error'){
    
    layer.nm <- colnames(prob.m)
    prob.m.ls <- list()
    for (lay.i in seq_along(layer.nm)) {
      prob.m.i <- matrix(prob.m[,layer.nm[lay.i]],
                         ncol = ncol(soil.den),
                         nrow = nrow(soil.den),byrow = T)
      prob.ra <-raster((prob.m.i))
      extent(prob.ra) <- extent(soil.den)
      # plot(prob.ra)
      
      prob.m.ls[[lay.i]] <- prob.ra
      
    }
    names(prob.m.ls) <- layer.nm
    
  }else{
    prob.m.ls <- NA
  }
  # path.nm<- "data/met/future//ACCESS1-0/history"
  model.nm.folder <- basename(dirname(path.nm))
  
  dir.create(paste0('cache/fineScale/',model.nm.folder))
  dir.create(paste0('cache/fineScale/',model.nm.folder,'/history'))
  
  out.file.full.nm <- paste0('cache/fineScale/',model.nm.folder,'/history/',out.nm)
  print(out.file.full.nm)
  saveRDS(list(val = score.ra,
               prob = prob.m.ls),out.file.full.nm)
}
# read#####
met.path.vec <- list.dirs('data/met/future/',recursive = T)
rcp45.index <- NULL#grep('rcp45',met.path.vec)
rcp85.index <- NULL#grep('rcp85',met.path.vec)
hist.index <- grep('history',met.path.vec)
fn.vec.chosen <- met.path.vec[c(rcp45.index,rcp85.index,hist.index)]
# fn.vec.chosen <- met.path.vec[c(hist.index)]
# fn.vec.chosen <- fn.vec.chosen[grep('BNU-ESM',fn.vec.chosen)]
# met.mode.fn <- sapply(met.path.vec, function(x)list.dirs(x,recursive = F))
library(randomForest)
# sapply(fn.vec.chosen[1],wrap.predic.func,my.fun = predict.rf.cmip.fine.reso.func)
# fuelType
predict.rf.cmip.fine.reso.func(path.nm = fn.vec.chosen[1],
                               model.path = 'cache/rf.fit.fuelType.new.rds',
                               out.nm = 'fuelType.rds')
# 
ft.lut <- read.csv('cache/fuelType_LUT.csv')
ft.lut.sub <- ft.lut[ft.lut$fuelType_vicnsw %in% 3005:3009,]
ft.lut.sub <- ft.lut.sub[order(ft.lut.sub$fuelType_vicnsw),]
# 
fuelType.pred <-readRDS('cache/fineScale/ACCESS1-0/history/fuelType.rds') 
# curretn fuel type####
# evc.df <- raster('data/EVC_fuelType/evc/VICSANSW161.tif')
# put.ft.no.to.sense.func <- function(ra.in,ft.df){
#   # ft.no.in.ra <- unique(ra.in)
#   for (i in seq_along(ft.df$ft.no)) {
#     ra.in[ra.in==ft.df$ft.no[i]] <- ft.df$ft.new.num[i]
#   }
#   return(ra.in)
# }
# ft.no.vec <- atrribute.df$VICSANSW.FUEL_TYPE#read.csv('cache/fuelType_LUT.csv')$fuelType_vicnsw
# ft.df <- data.frame(ft.no = ft.no.vec,
#                     ft.new.num = seq_along(ft.no.vec),
#                     nsmes = atrribute.df$VICSANSW.TYPE_NAME)
# # 
# crs(fuelType.pred[['val']]) <- '+init=epsg:4326'
# evc.ra.vic <- projectRaster(from = evc.df,
#                             to = fuelType.pred[['val']],
#                             method = 'ngb')
# # get rid of none present types
# ft.df.sub <- ft.df[ft.df$ft.no %in% unique(evc.ra.vic), ]
# ft.df.sub$ft.new.num <- seq_along(ft.df.sub$ft.no)
# evc.ra.vic <- put.ft.no.to.sense.func(evc.ra.vic,ft.df.sub)

#####
ft.in.df <- readRDS('cache/ft.met.lai.rds')
######remove non natual types##########
evc.df <- raster('data/EVC_fuelType/evc/VICSANSW161.tif')

atrribute.df <- evc.df@data@attributes[[1]]
atrribute.df <- atrribute.df[,c('VICSANSW.FUEL_TYPE' , 'VICSANSW.TYPE_NAME')]
nm.vec <- atrribute.df$VICSANSW.TYPE_NAME[atrribute.df$VICSANSW.FUEL_TYPE %in%
                                            unique(ft.in.df$ft)]
ft.nm.df <- data.frame(nm = atrribute.df$VICSANSW.TYPE_NAME[atrribute.df$VICSANSW.FUEL_TYPE %in%
                                                              unique(ft.in.df$ft)],
                       ID = unique(ft.in.df$ft))

ID.natue <- ft.nm.df$ID[!ft.nm.df$nm %in% c('Eaten Out Grass','Orchard / Vineyard',
                                            'Softwood Plantation','Hardwood Plantation',
                                            'Water, sand, no vegetation')]
# ft.nm.df <- ft.nm.df[ft.nm.df$ID %in% ID.natue,]
ft.nm.df$ID.factor <- as.factor(ft.nm.df$ID)
# 
evc.dargo.nature <- evc.dargo

evc.dargo.nature[!evc.dargo.nature %in% ID.natue] <- NA
# $##############
pdf('figures/dargo.fuelType.stratified.pdf',height = 4*3,width = 5)

par(mar=c(3,3,0,0),mfrow=c(3,1))
# 
plot(evc.dargo.nature,
     breaks = c(ft.nm.df$ID[1]-0.1,ft.nm.df$ID+0.1),
     col=c(col_vector[ft.nm.df$ID.factor]),legend=F)
# hist(fuelType.pred[['val']])
# hist(evc.dargo.nature)
# fuelType.pred[['val']][fuelType.pred[['val']]>3033]
plot(fuelType.pred[['val']],
     breaks = c(ft.nm.df$ID[1]-0.1,ft.nm.df$ID+0.1),
     col=c(col_vector[ft.nm.df$ID.factor]),legend=F)
plot(0,pch=NA,ann=F,axes=F)
legend('top',legend = ft.nm.df$nm,col = col_vector[ft.nm.df$ID.factor],
       pch=15,ncol=3,bty='n',xpd=T,cex=0.8)

dev.off()
# # plot out####
# pdf('figures/dargo.fuelType.pdf',height = 8*0.8,width = 7.5*0.8)
# par(mar=c(2,2,5,1))
# plot(fuelType.pred[['val']],breaks=3004.5:3009.5,col=col.df$iris,
#      # axis.args=list(at=3005:3009,
#      #                labels=ft.lut.sub$VICSANSW.TYPE_NAME,
#      #                line = -3),
#      # legend.args=list(text='', side = 4, line = 1),legend.width=1,
#      legend = F,
#      bty="n", box=FALSE)
# plot(wi.ra,col=gray.colors(12,alpha=0.3),add=T,
#      # axis.args=list(at=3005:3009,
#      #                labels=ft.lut.sub$VICSANSW.TYPE_NAME,
#      #                line = -3),
#      # legend.args=list(text='', side = 4, line = 1),legend.width=1,
#      legend = F,
#      bty="n", box=FALSE)
# par(xpd = TRUE)
# legend('bottom', legend = ft.lut.sub$VICSANSW.TYPE_NAME, 
#        fill = col.df$iris, 
#        cex = 1, inset = 1,ncol = 2,bty='n')
# dev.off()

# write.csv(x,'fit CM.csv')
# write.csv(ft.nm.df,'fuelName.csv',row.names = F)
