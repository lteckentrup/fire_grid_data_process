library(raster)
# library(randomForest)
source('r/get_vic_shape.R')
source('r/read_soil_topo.R')
source('r/read_met_run_parallel.R')
# read
lcm.vic <- readRDS('cache/lcm.vic.rds')
lcm.vic.fine <- resample(lcm.vic,rad.jan.vic,method='ngb')
# 
t.s.ra <- stack(c.small.vic,c.plan.vic,wi.vic,
                soil.den.vic,soil.clay.vic,soil.depth.vic,
                rad.jan.vic,rad.jan.vic)
saveRDS(t.s.ra,'cache/topo_soil_90m_vic.rds')

# 
t.s.ra.nature <- mask(t.s.ra,lcm.vic.fine)
rad.nature <- mask(rad.jan.vic,lcm.vic.fine)

names(t.s.ra.nature) <- c('curvature_profile','curvature_plan','wi' ,
                          'soil.density'  , 'clay' ,'soil.depth', #soil attributes
                          'rad.short.jan' ,'rad.short.jul')

saveRDS(t.s.ra.nature,'cache/topo_soil_90m_vic_nature.rds')

# 
t.s.ra.nature <- readRDS('cache/topo_soil_90m_vic_nature.rds')

x.df <- as.data.frame(t.s.ra.nature, xy=TRUE)
x.df <- x.df[complete.cases(x.df),]
saveRDS(x.df,'cache/df_topo_soil_90m_vic_nature.rds')

# C
# 
met.ra <- stack(tmax.mean.vic,pr.mean.vic,
                pr.season.vic,lai.vic)
names(met.ra) <- c('tmax.mean','map','pr.seaonality',#long term clim
                   'lai.opt.mean')
samplepoints <- cbind(x.df$x,x.df$y)
met.df <- extract(met.ra,samplepoints,df=TRUE)

model.input.df <- cbind(x.df,met.df)
saveRDS(model.input.df,'cache/input_access.rds')