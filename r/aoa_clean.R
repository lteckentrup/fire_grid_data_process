library(CAST)
library(raster)
library(virtualspecies)
library(caret)

library(viridis)
library(latticeExtra)
library(gridExtra)


# functions####
source('r/functions_predict.R')
source('r/get_vic_shape.R')

# read inputs####
# read topo
rad.jan <- readRDS('cache/rad_jan_5km.rds')
rad.jul <- readRDS('cache/rad_jul_5km.rds')
c.plan <- readRDS('cache/curvature_plan_vic_5km.rds')
c.profile <- readRDS('cache/curvature_profile_vic_5km.rds')
wi <- readRDS('cache/wi_vic_5km.rds')
# read soil
s.den <- readRDS('cache/density_vic_5km.rds')
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

# put all into one raster####
predictors <- stack(c(rad.jan,rad.jul,c.plan,c.profile,wi,
                      s.den,s.clay,
                      # tmax,rain,rh.min,
                      pr.seaonality,tmax.mean,map,
                      lai.opt))
names(predictors) <- x.nm
# get land use type ####
lcm.ra <- raster('E:/storage/equilibrium-lai-for-australia/downloads/LCM/luav4g9abll07811a02egigeo___/lu05v4ag/w001001.adf')
lcm.vic <- get.small.area.func(lcm.ra,shape.vic)
lcm.lut <- lcm.vic@data@attributes[[1]]
lcm.lut.natrua <- lcm.lut$ID[lcm.lut$LU_DESC %in% c('CONSERVATION AND NATURAL ENVIRONMENTS',
                                                    'PRODUCTION FROM RELATIVELY NATURAL ENVIRONMENTS')]


lcm.vic[!(lcm.vic %in% lcm.lut.natrua)] <- NA

lcm.vic.corse <- resample(lcm.vic,predictors[[1]])
# get a subset of inputs
predictors.sub <- mask(predictors,lcm.vic.corse)

# get groubd fire data####
source('r/process_input.R')
set.seed(1935)
# 
test.df <- input.df[,c('lon','lat')]

train <- sample(nrow(test.df), 0.7*nrow(test.df), replace = FALSE)
TrainSet <- test.df[train,]

samplepoints <- cbind(TrainSet$lon,TrainSet$lat)

trainDat <- extract(predictors,samplepoints,df=TRUE)


trainDat <- trainDat[complete.cases(trainDat),]

# calculated aoa####
AOA_sub <- aoa(newdata = predictors.sub, 
               train = trainDat[,names(predictors)])
saveRDS(AOA_sub,'cache/aoa.natrual.land.rds')

# correct for infinite data####
AOA_sub.di <- AOA_sub$DI
new.threshold <- boxplot.stats(AOA_sub.di[is.finite(AOA_sub.di)])$stats[5]

AOA_sub$AOA.new <- AOA_sub$DI
AOA_sub$AOA.new[AOA_sub$DI <= new.threshold] <- 1
AOA_sub$AOA.new[AOA_sub$DI > new.threshold] <- 0

# make plots####
png('figures/aoa.png',width = 100*8,height = 100*5)
par(mar=c(4,4,1,1))
plot(AOA_sub$AOA.new,
     breaks = c(-0.9,0.1,1.1),col=c('blue','red'),
     legend=F)
legend('topright',
       legend = c('Covered','Not covered'),
       pch=15,col=c('red','blue'),bty='n')

dev.off()

















# trash##############
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