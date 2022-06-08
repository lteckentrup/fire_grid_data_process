library(raster)
library(randomForest)
source('r/read_soil_topo.R')
source('r/read_met_run_parallel.R')

# c.curvature.ra.vic <- mask(c.small,shape.vic)
get.small.area.func <- function(ra.in,p = p){
  r2 <- crop(ra.in, extent(p))
  r3 <- mask(r2, p)
  # crs(r3) <- crs(ra.in)
  # aggregate(r3, fact=3)
  return(r3)
}
# 
c.curvature.ra.vic <- get.small.area.func(c.small,shape.vic)

saveRDS(c.curvature.ra.vic,'cache/curvature_vic_ra.rds')

# 

c.curvature.ra.vic <- readRDS('cache/curvature_vic_ra.rds')
num.cell.vec <- seq_along(c.curvature.ra.vic)
# length(num.cell.vec)/4
# split(num.cell.vec,             # Applying split() function
#       ceiling(seq_along(num.cell.vec) / 4))


# curvature <- c.curvature.ra.vic[3e7]
# gps.site <- xyFromCell(c.curvature.ra.vic,cell = 3e7,spatial=T)
quart.num <- length(num.cell.vec)/4

# read rf model####
model.rf <- readRDS('cache/rf.fit.hs.elevated.rds')

# 
pred.1.point.func <- function(cell.num,model.rf){
  # read curvature
  curvature <- c.curvature.ra.vic[cell.num]
  # save time not to run any NA
  if(is.na(curvature)){
    return(NA)
  }else{
    # readGPs
    gps.site <- xyFromCell(c.curvature.ra.vic,cell = cell.num,spatial=T)
    
    # get topo inputs####
    c.plan.points <- raster::extract(c.plan.ra.i,gps.site)
    
    wi.points <- raster::extract(wi.ra,gps.site)
    
    rad.jan.point <- raster::extract(rad.jan,gps.site)
    
    rad.jul.point <- raster::extract(rad.jul,gps.site)
    
    # read soil####
    soil.den.point <- raster::extract(soil.den,gps.site)
    
    soil.clay.point <- raster::extract(soil.clay,gps.site)
    
    # read met
    tmax.ra.point <- raster::extract(tmax.ra,gps.site)
    pr.ra.point <- raster::extract(pr.ra,gps.site)
    rh.ra.point <-raster::extract(rh.ra,gps.site)
    
    tmax.mean.ra.point <- raster::extract(tmax.mean.ra,gps.site)
    pr.mean.ra.point <- raster::extract(pr.mean.ra,gps.site)
    pr.season.ra.point <- raster::extract(pr.season.ra,gps.site)
    
    lai.ra.point <- raster::extract(lai.ra,gps.site)
    
    
    # make a df
    df <- data.frame(soil.density = soil.den.point,clay = soil.clay.point,
                     rad.short.jan = rad.jan.point, rad.short.jul = rad.jul.point,
                     wi = wi.points,curvature_profile = curvature,curvature_plan=c.plan.points,
                     tmax = tmax.ra.point,rain = pr.ra.point,rh.min = rh.ra.point,
                     tmax.mean = tmax.mean.ra.point,map=pr.mean.ra.point,pr.seaonality = pr.season.ra.point,
                     lai.opt=lai.ra.point)
    
    
    # predict prob and score
    return(predict(object = model.rf,newdata=df))
  }
}

do.pre.for.points.func <- function(num.part){
  # chose the cell for parallel split
  chose.vec <- (((num.part-1) * quart.num)+1):(num.part * quart.num)
  
  cell.vec <- num.cell.vec[chose.vec]
  
  # 
  system.time(sapply(cell.vec[1:20], pred.1.point.func,model.rf = model.rf))
  x <- sapply(cell.vec[1:4], pred.1.point.func,model.rf = model.rf)
  
}

# # 
# x <- matrix(runif(500), 100)
# y <- gl(2, 50)
# library(foreach)
# rf <- foreach(ntree=rep(250, 4), .packages=c('randomForest','raster'),.combine='c') %dopar%
#   randomForest(x, y, ntree=ntree)

library(doParallel)
library(foreach)

registerDoParallel(4)  # use multicore, set to the number of our cores
foreach (i=1:4) %dopar% {
  mean(c.curvature.ra.vic)
}
stopImplicitCluster()

