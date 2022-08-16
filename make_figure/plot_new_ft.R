#color for plotting#####
library(RColorBrewer)
library(raster)
n <- 21
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector.all = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
col_vector <- sample(col_vector.all, n)
# get land use type ####
lcm.ra <- raster('E:/storage/equilibrium-lai-for-australia/downloads/LCM/luav4g9abll07811a02egigeo___/lu05v4ag/w001001.adf')
lcm.vic <- get.small.area.func(lcm.ra,shape.vic)
lcm.lut <- lcm.vic@data@attributes[[1]]
lcm.lut.natrua <- lcm.lut$ID[lcm.lut$LU_DESC %in% c('CONSERVATION AND NATURAL ENVIRONMENTS',
                                                    'PRODUCTION FROM RELATIVELY NATURAL ENVIRONMENTS')]


lcm.vic[!(lcm.vic %in% lcm.lut.natrua)] <- NA


# ##########
source('r/functions_plot.R')
source('r/get_vic_shape.R')

ft.hist <- read.future.all.func(var.in.nm = 'fuelType',
                                         future_s = 'history',
                                         exclude.nm = 'noVeg')
ft.hist.mod <- calc(ft.hist, fun=Mode.func)

# ft.hist.mod <- readRDS('data/met/future/ACCESS1-0/history/_fuelType.rds')
# ft.hist.mod <- ft.hist.mod[['val']]

ft.hist.mod.vic <- get.small.area.func(ft.hist.mod,shape.vic)
# 
lcm.vic.corse <- resample(lcm.vic,ft.hist.mod.vic,method='ngb')

# get a subset of inputs
ft.sub <- mask(ft.hist.mod.vic,lcm.vic.corse)
#get real fuel type ########
evc.df <- raster('data/EVC_fuelType/evc/VICSANSW161.tif')

atrribute.df <- evc.df@data@attributes[[1]]
atrribute.df <- atrribute.df[,c('VICSANSW.FUEL_TYPE' , 'VICSANSW.TYPE_NAME')]
# nm.vec <- atrribute.df$VICSANSW.TYPE_NAME[atrribute.df$VICSANSW.FUEL_TYPE %in% 
#                                             unique(ft.hist.mod.vic)]
lonlat <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0' 

evc.df.lonlat <- projectRaster(evc.df,
                               crs = lonlat,
                               method='ngb')
saveRDS(evc.df.lonlat,'cache/evc.lon.lat.rds')
# 
evc.df.sub <- get.small.area.func(evc.df.lonlat,shape.vic)
unique(evc.df.lonlat)
lcm.vic.evc <- resample(lcm.vic,evc.df.sub,method='ngb')

evc.df.sub.clean <- mask(evc.df.sub,lcm.vic.evc)
saveRDS(evc.df.sub.clean,'cache/evc.lon.lat.masked.rds')

# evc.df.sub.clean <- readRDS('cache/evc.lon.lat.masked.rds')
# evc.df.sub.clean <- readRDS('cache/evc.lon.lat.rds')
# 
ft.in.vc.vec <- unique(evc.df.sub.clean)
nm.vec <- atrribute.df$VICSANSW.TYPE_NAME[atrribute.df$VICSANSW.FUEL_TYPE %in% 
                                            unique(evc.df.sub.clean)]

ft.nm.df <- data.frame(nm = atrribute.df$VICSANSW.TYPE_NAME[atrribute.df$VICSANSW.FUEL_TYPE %in% 
                                                              unique(ft.sub)],
                       ID = unique(ft.sub))

ft.nm.df$ID.factor <- as.factor(ft.nm.df$ID)
# plot(evc.df.sub)
# unique(evc.df.sub)
# evc.df.sub[!evc.df.sub %in% ft.nm.df$ID] <- NA


# 
png('figures/fuelType_detailed.pdf',width = 8,height = 8)
par(mar=c(3,3,0,0),mfrow=c(1,1))
# 
plot(evc.df.sub.clean,
     breaks = c(ft.nm.df$ID[1]-0.1,ft.nm.df$ID+0.1),
     col=rev(col_vector[ft.nm.df$ID.factor]),legend=F)

plot(0,pch=NA,ann=F,axes=F)
legend('top',legend = ft.nm.df$nm,col = col_vector[ft.nm.df$ID.factor],
       pch=15,ncol=2,bty='n',xpd=T,cex=0.8)

# 

plot(ft.sub,
     breaks = c(ft.nm.df$ID[1]-0.1,ft.nm.df$ID+0.1),
     col=rev(col_vector[ft.nm.df$ID.factor]),legend=F)

plot(0,pch=NA,ann=F,axes=F)
legend('top',legend = ft.nm.df$nm,col = col_vector[ft.nm.df$ID.factor],
       pch=15,ncol=2,bty='n',xpd=T,cex=0.8)

dev.off()



x.df <- readRDS('data/met/future/ACCESS1-0/history/_fuelType.rds')
plot(x.df[['prob']][[2]])
