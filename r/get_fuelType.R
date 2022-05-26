library(raster)
evc.df <- raster('data/EVC_fuelType/evc/VICSANSW161.tif')
# plot(evc.df,breaks=c(0,2000,2001:2070,3001))
atrribute.df <- evc.df@data@attributes[[1]]
atrribute.df <- atrribute.df[,c('VICSANSW.FUEL_TYPE' , 'VICSANSW.TYPE_NAME')]
# points(x = har.score.df$coords.x1[1],y=har.score.df$coords.x2[1])

har.score.df <- read.csv('data/combdata.csv')
har.score.df <- har.score.df[,c('coords.x1','coords.x2')]
har.score.df <- har.score.df[!duplicated(har.score.df),]
# 
har.score.df$fuelType_vicnsw <- extract(evc.df,y = cbind(har.score.df$coords.x1,har.score.df$coords.x2))

har.score.df.type <- merge(har.score.df,atrribute.df,all.x =T,
                           by.x = 'fuelType_vicnsw',by.y = 'VICSANSW.FUEL_TYPE')

# har.score.df.type <- har.score.df.type[,c('coords.x1', 'coords.x2','VICSANSW.TYPE_NAME')]
saveRDS(har.score.df.type,'cache/fuelType.rds')
# 
ft.fq.df <- (table(as.vector(evc.df)))
ft.vec <- as.vector(evc.df)
ft.fq.df <- aggregate(data.frame(count = ft.vec), list(value = ft.vec), length)
rm(ft.vec)

ft.fq.nm.df <- merge(atrribute.df,ft.fq.df,by.x = 'VICSANSW.FUEL_TYPE',by.y ='value'  )

write.csv(ft.fq.nm.df,'fuelTypeCount.csv',row.names = F)

