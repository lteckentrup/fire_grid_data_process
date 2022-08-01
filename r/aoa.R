library(CAST)
library(raster)
library(virtualspecies)
library(caret)

library(viridis)
library(latticeExtra)
library(gridExtra)

# get.small.area.func <- function(ra.in,p = p){
#   r2 <- crop(ra.in, extent(p))
#   r3 <- mask(r2, p)
#   # crs(r3) <- crs(ra.in)
#   # aggregate(r3, fact=3)
#   return(r3)
# }

# functions####
source('r/functions_predict.R')
source('r/get_vic_shape.R')

# s.den=matrix(soil.den),s.clay= matrix(soil.clay),
# rad.jan = matrix(rad.jan),rad.jul = matrix(rad.jul),
# wi = matrix(wi.ra),c.profile = matrix(c.small),c.plan = matrix(c.plan.ra),
# tmax = matrix(tmax.ra),rain = matrix(pr.ra),rh.min = matrix(rh.ra),
# tmax.mean = matrix(tmax.mean.ra),map = matrix(pr.mean.ra),pr.seaonality = matrix(pr.season.ra),
# lai.opt = matrix(lai.ra)

# read topo
rad.jan <- readRDS('cache/rad_jan_5km.rds')
rad.jul <- readRDS('cache/rad_jul_5km.rds')
c.plan <- readRDS('cache/curvature_plan_vic_5km.rds')
c.profile <- readRDS('cache/curvature_profile_vic_5km.rds')
wi <- readRDS('cache/wi_vic_5km.rds')
# read soil
s.den <- readRDS('cache/density_vic_5km.rds')
# soil.ph <- readRDS('cache/ph_vic_5km.rds')
s.clay <- readRDS('cache/clay_vic_5km.rds')
# read climate
pr.seaonality <- readRDS('cache/pr_seaonality_history.rds')

tmax <- readRDS('data/met/future/ACCESS1-0/history/history_20002015_monthly_tmax.rds')[[1]]
rain <- readRDS('data/met/future/ACCESS1-0/history/history_20002015_monthly_pr.rds')[[1]]
rh.min <- readRDS('data/met/future/ACCESS1-0/history/history_20002015_annual_rh.rds')[[1]]

tmax.mean <- readRDS('data/met/future/ACCESS1-0/history/history_20002015_annual_tmax.rds')
map <- readRDS('data/met/future/ACCESS1-0/history/history_20002015_annual_pr.rds')

lai.opt <- readRDS('data/met/future/ACCESS1-0/history/history_20002015_lai_jan_5km.rds')

x.nm = c('rad.short.jan' ,'rad.short.jul','curvature_plan', 'curvature_profile','wi' ,
         'soil.density' ,  'clay' ,#'ph' , #soil attributes
         #topo
         # 'tmax' , 'rain' , 'rh.min', #climate
         'pr.seaonality','tmax.mean','map',#long term clim
         'lai.opt.mean')

# 
predictors <- stack(c(rad.jan,rad.jul,c.plan,c.profile,wi,
                      s.den,s.clay,
                      # tmax,rain,rh.min,
                      pr.seaonality,tmax.mean,map,
                      lai.opt))
names(predictors) <- x.nm
# 
lcm.ra <- raster('E:/storage/equilibrium-lai-for-australia/downloads/LCM/luav4g9abll07811a02egigeo___/lu05v4ag/w001001.adf')
lcm.vic <- get.small.area.func(lcm.ra,shape.vic)
lcm.lut <- lcm.vic@data@attributes[[1]]
lcm.lut.natrua <- lcm.lut$ID[lcm.lut$LU_DESC %in% c('CONSERVATION AND NATURAL ENVIRONMENTS',
                                                    'PRODUCTION FROM RELATIVELY NATURAL ENVIRONMENTS')]


lcm.vic[!(lcm.vic %in% lcm.lut.natrua)] <- NA

lcm.vic.corse <- resample(lcm.vic,predictors[[1]])

predictors.sub <- mask(predictors,lcm.vic.corse)

# response <- generateSpFromPCA(predictors,
#                               means = c(3,1),sds = c(2,2), plot=F)$suitab.raster
# 
# response.sub <- generateSpFromPCA(predictors.sub,
#                               means = c(3,1),sds = c(2,2), plot=F)$suitab.raster

# 
source('r/process_input.R')
set.seed(1935)


# 
test.df <- input.df[,c('lon','lat')]

train <- sample(nrow(test.df), 0.7*nrow(test.df), replace = FALSE)
TrainSet <- test.df[train,]
samplepoints <- cbind(TrainSet$lon,TrainSet$lat)

trainDat <- extract(predictors,samplepoints,df=TRUE)
# trainDat$response <- extract (response,samplepoints)
# trainDat <- merge(trainDat,samplepoints,by.x="ID",by.y="ID")

sp.df <- data.frame(lon = samplepoints[,1],
                    lat = samplepoints[,2])
# sp.df


trainDat <- trainDat[complete.cases(trainDat),]
# # 
# model_random <- train(trainDat[,names(predictors)],
#                       trainDat$response,
#                       method="rf",
#                       importance=TRUE,
#                       trControl = trainControl(method="cv"))
# di.train <- trainDI(train = subset(trainDat,select= -ID),
#                     variables = x.nm)
# di.train$trainDI
# # 
# x <- trainDI(train = subset(trainDat[100:1500,],select= -ID),
#              variables = x.nm)

# # boxplot.stats(x$trainDI)$stats[5]
# # vec.di <- x$trainDI
# # vec.di[is.infinite(vec.di)] <- 10
# range(vec.di[is.finite(vec.di)])
# boxplot.stats(vec.di[is.finite(vec.di)])$stats[5]
# x$threshold
# quantile(vec.di[is.finite(vec.di)],probs = .75)

# test <- trainDat[c(100:200,1400:1500),]

# plot(test$lai.opt.mean)
# prediction_random <- predict(predictors,model_random)
# mask.1 <- predictors[[1]]
# csample(mask.1,75,15,maxdist=0.20,seed=15)
# 
# k.val <- length(samplepoints[!duplicated(samplepoints)])
# folds <- CreateSpacetimeFolds(trainDat, spacevar="ID",k=30)
# model_random <- train(trainDat[,names(predictors)],
#                trainDat$response,
#                method="rf",
#                importance=TRUE,
#                tuneGrid = expand.grid(mtry = c(2:length(names(predictors)))),
#                trControl = trainControl(method="cv",index=folds$index))
library(doParallel)
library(parallel)
cl <- makeCluster(4)
registerDoParallel(cl)

AOA_spatial <- aoa(newdata = predictors, 
                   model = model_random,
                   cl=cl)

saveRDS(AOA_spatial,'cache/aoa.rds')
plot(AOA_spatial)
# 
AOA_sub <- aoa(newdata = predictors.sub, 
               model = model_random,
               cl=cl)
AOA_sub <- aoa(newdata = predictors.sub, 
               train = trainDat[,names(predictors)],
               cl=cl)
saveRDS(AOA_sub,'cache/aoa.natrual.land.rds')

plot(AOA_sub)
plot(AOA_sub$DI)
plot(AOA_sub$AOA,col=c('grey','red'))
points(samplepoints,pch=15,cex=0.05)

# 
di.train <- trainDI(train = subset(trainDat,select= -ID),
                    variables = x.nm)
# 
hist(di.train$trainDI[di.train$trainDI>0])
length(di.train$trainDI[di.train$trainDI>0])
boxplot.stats(di.train$trainDI[di.train$trainDI>0])$stats[5]
AOA_sub <- readRDS('cache/aoa.natrual.land.rds')
plot(AOA_sub$DI)

# di.extract <- extract(AOA_sub$DI,samplepoints,df=TRUE)

# hist(di.extract$DI)
# range(di.extract$DI,na.rm=T)
AOA_sub.di <- AOA_sub$DI
new.threshold <- boxplot.stats(AOA_sub.di[is.finite(AOA_sub.di)])$stats[5]

AOA_sub$AOA.new <- AOA_sub$DI
AOA_sub$AOA.new[AOA_sub$DI <= new.threshold] <- 1
AOA_sub$AOA.new[AOA_sub$DI > new.threshold] <- 0

plot(AOA_sub$DI)
plot(AOA_sub$parameters)
# 
png('figures/aoa.png',width = 100*8,height = 100*5)
par(mar=c(4,4,1,1))
plot(AOA_sub$AOA.new,
     breaks = c(-0.9,0.1,1.1),col=c('blue','red'),
     legend=F)
legend('topright',
       legend = c('Covered','Not covered'),
       pch=15,col=c('red','blue'),bty='n')

dev.off()