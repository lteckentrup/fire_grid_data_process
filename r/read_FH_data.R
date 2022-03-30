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
# range(coords.repro.df$coords.x2)
har.score.df.gps <- cbind(har.score.df,coords.repro.df)

har.score.df.gps <- har.score.df.gps[,c('lon','lat',
                                        'coords.x1','coords.x2',
                                        'DATE',
                                        'S_DPTH','S_CVR','NS_CVR','NS_TOP',
                                        'NS_DEAD','E_CVR','E_TOP',
                                        'CNPY_CVR','CNPY_BASE','CNPY_TOP',
                                        'LAST_FIRE','tsf')]

names(har.score.df.gps) <- c('lon','lat',
                             'coords.x1','coords.x2',
                             'Date',
                             'S_fuel_DPTH_mm','S_CVR_percent','NS_CVR_precent','NS_TOP_height_cm',
                             'NS_DEAD_percent','E_CVR_percent','E_TOP_hight_cm',
                             'CNPY_CVR','CNPY_BASE_hight_cm','CNPY_TOP_hight_cm',
                             'LAST_FIRE_yr','tsf')

write.csv(har.score.df.gps,'cache/hazard.score.gps.csv',row.names = F)

# plot on a map####
library(maps)
# plot maps
# show map with Latitude 200 as center
pdf('FH.location.pdf',width = 8,height = 8*20/25)
maps::map('world', xlim = c(130, 155),ylim=c(-45,-25))
# add axes
maps::map.axes()
points(x = coords.repro.df$coords.x1,y=coords.repro.df$coords.x2,pch=16,cex=0.5,col='red')
dev.off()

# now check how to get a score#####
