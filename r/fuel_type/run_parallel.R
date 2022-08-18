# prepare#########
library(raster)
library(randomForest)
library(doParallel)
library(foreach)
# model fit#############
# read inputs
model.input.df <- readRDS('cache/input_access.rds')
# splite inputs by no. of cpu core
model.input.df <- model.input.df[complete.cases(model.input.df),]
sample.d <- model.input.df#[1:1000,]
n.split <- 4
data.ls <- split(sample.d, rep(1:n.split, 
                    length.out = nrow(sample.d), 
                    each = ceiling(nrow(sample.d)/n.split)))
# save memory
rm(model.input.df)
rm(sample.d)
#  rf model####
model.rf <- readRDS('cache/rf.fit.fuelType.new.rds')

# set up parallel computing#

cores=n.split#use multicore, set to the number of our cores
cl <- makeCluster(cores[1]) 
registerDoParallel(cl)
# record time used
s.time <- Sys.time()

out.list <- foreach (i=1:cores, .packages = 'randomForest') %dopar% {
  predict(object = model.rf,newdata=data.ls[[i]])
}
stopImplicitCluster()

e.time <- Sys.time()

time.use <- e.time - s.time
# get out put####
out.list <- lapply(X = out.list,as.character)

rm(model.rf)
rm(data.ls)
rm(cl)
# put input output together
sample.d <- do.call(rbind,data.ls)
sample.d$pred <- do.call(c,out.list)
# make a raster output #####
spg <- sample.d[,c('x','y','pred')]
spg$pred <- as.numeric(spg$pred)
coordinates(spg) <- ~ x + y
gridded(spg) <- TRUE
out.ra <- raster(spg)
# plot(rasterDF)

saveRDS(out.ra,'out.access.rds')

# make plots#########
ft.in.df <- readRDS('cache/ft.met.lai.rds')
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
ft.nm.df <- ft.nm.df[ft.nm.df$ID %in% ID.natue,]
ft.nm.df$ID.factor <- as.factor(ft.nm.df$ID)

dev.new() 
pdf('test.pdf',width = 15,height = 15)
# 
par(mar=c(3,3,0,0),mfrow=c(2,1))
# 
plot(out.ra,
     breaks = c(ft.nm.df$ID[1]-0.1,ft.nm.df$ID+0.1),
     col=c(col_vector[ft.nm.df$ID.factor]),legend=F)
plot(0,pch=NA,ann=F,axes=F)
legend('top',legend = as.character(ft.nm.df$nm),col = col_vector[ft.nm.df$ID.factor],
       pch=15,ncol=2,bty='n',xpd=T,cex=0.8)
dev.off()
