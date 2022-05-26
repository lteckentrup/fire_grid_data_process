library(raster)
evc.df <- raster('data/EVC_fuelType/evc/VICSANSW161.tif')
plot(evc.df,breaks=c(0,2000,2001:2070,3001))
atrribute.df <- evc.df@data@attributes[[1]]
atrribute.df <- atrribute.df[,c('VICSANSW.FUEL_TYPE' , 'VICSANSW.TYPE_NAME')]
plot(evc.df)
sub.ra <- evc.df[evc.df==atrribute.df$VICSANSW.FUEL_TYPE[1]]
write.csv(atrribute.df,'fuelType.csv')
# 
set.seed(1935)
sub.df <- sample(1:length(evc.df), 100000, replace = FALSE)
train <- sample(1:length(evc.df), 0.7*length(evc.df), replace = FALSE)
TrainSet <- test.df[train,]
# ValidSet <- test.df[-train,]

names(evc.df@data@attributes[[1]])
