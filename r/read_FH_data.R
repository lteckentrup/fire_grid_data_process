# check spatial distribution of FH data####
library(rgdal)
# set the crs (GDA 94) that the data is in 
sincrs <- "+proj=lcc +lat_1=-36 +lat_2=-38 +lat_0=-37 +lon_0=145 +x_0=2500000 +y_0=2500000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs "
# GPS (WGS84)
lonlat <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0' 
# read fire hazard score data
har.score.df <- read.csv('data/combdata.csv')
# convert coords to spatial points
s <- SpatialPoints(cbind(har.score.df$coords.x1, har.score.df$coords.x2), proj4string=CRS(sincrs))
# transfor to gps
coords.repro.tmp <- spTransform(s, lonlat)
coords.repro.df <- as.data.frame(coords.repro.tmp)
names(coords.repro.df) <- c('lon','lat')

# combine data and gps
har.score.df.gps <- cbind(har.score.df,coords.repro.df)
# get rid of uesless columns
har.score.df.gps <- har.score.df.gps[,c('lon','lat',
                                        'coords.x1','coords.x2',
                                        'DATE','DATE_YEAR','DATE_MONTH',
                                        'S_DPTH','S_CVR','NS_CVR','NS_TOP',
                                        'NS_DEAD','E_CVR','E_TOP',
                                        'CNPY_CVR','CNPY_BASE','CNPY_TOP',
                                        'LAST_FIRE','tsf',
                                        'b.ffb.hz','b.rib.hz','b.oth.hz',
                                        'b.hz','e.hz','ns.hz','s.hz','sns.hz','ofh.hz')]

# b_ffb_hz	Bark hazard for fibrous bark
# b_rib_hz	Bark hazard for ribbon bark
# b_oth_hz	Bark hazard for other bark
# b_hz	Bark hazard
# e_hz	Elevated hazard
# ns_hz	Near surface hazard
# s_hz	surface hazard
# sns_hz	Combined surface / near surface hazard
# ofh_hz	Overall hazard

names(har.score.df.gps) <- c('lon','lat',
                             'coords.x1','coords.x2',
                             'Date','Year','month',
                             'S_fuel_DPTH_mm','S_CVR_percent','NS_CVR_precent','NS_TOP_height_cm',
                             'NS_DEAD_percent','E_CVR_percent','E_TOP_hight_cm',
                             'CNPY_CVR','CNPY_BASE_hight_cm','CNPY_TOP_hight_cm',
                             'LAST_FIRE_yr','tsf',
                             'b_ffb_hz','b_rib_hz','b_oth_hz',
                             'bark_hz','elevated_hz','nearsurface_hz','surface_hz','sns_hz','overall_hz')
# save
write.csv(har.score.df.gps,'cache/hazard.score.gps.csv',row.names = F)

# plot sites on a map####
library(maps)
# plot maps
# show map with Latitude 200 as center
pdf('FH.location.pdf',width = 8,height = 8*20/25)
maps::map('world', xlim = c(130, 155),ylim=c(-40,-30))
# add axes
maps::map.axes()
library(oz)
vic(coast = F,add=T)
points(x = coords.repro.df$lon,y=coords.repro.df$lat,pch=16,cex=0.5,col='red')
dev.off()

# now check how to get a score#####
score.gps.df <- read.csv('cache/hazard.score.gps.csv')
# fix wrong year in 2011
score.gps.df$Date <- gsub(pattern = '2111',replacement = '2011',score.gps.df$Date)
score.gps.df$Date.formated <- as.Date(score.gps.df$Date)
# 
score.gps.df$site <- paste0(score.gps.df$lon,score.gps.df$lat)
length(unique(score.gps.df$site))


plot(lat~Date.formated,data = score.gps.df )

summary(score.gps.df)

library(spdep)
library(sp)
library(sf)
library(raster)
example(columbus)
spsample(columbus,n=12,"random")


dpath <- 'data/ibra51_reg/ibra7_regions.shp'
aerm.original <-  raster(dpath)

shape <- read_sf(dsn = dpath, layer = "SHAPEFILE")
library(maptools)
area <- readShapePoly(dpath)
x <- area@polygons[[1]]
y <- x@Polygons[1]
y[[1]]@coords


library(rgdal)
data.shape<-readOGR(dsn=dpath,layer="SHAPEFILE")

