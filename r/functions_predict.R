devtools::source_url("https://github.com/Jinyan-Yang/colors/blob/master/R/col.R?raw=TRUE")
# read inputs####
# read topo
rad.jan <- readRDS('cache/rad_jan_5km.rds')
rad.jul <- readRDS('cache/rad_jul_5km.rds')
c.plan.ra <- readRDS('cache/curvature_plan_vic_5km.rds')
c.small <- readRDS('cache/curvature_profile_vic_5km.rds')
wi.ra <- readRDS('cache/wi_vic_5km.rds')
# read soil
soil.den <- readRDS('cache/density_vic_5km.rds')
soil.ph <- readRDS('cache/ph_vic_5km.rds')
soil.clay <- readRDS('cache/clay_vic_5km.rds')

# function to predict
predict.rf.func <- function(model.in,
                            s.den,s.ph,s.clay,
                            rad.jan,rad.jul,wi,c.profile,c.plan,
                            tmax,rain,rh.min,
                            tmax.mean,map,
                            lai.opt,
                            giveProb){
  # model.in = readRDS('cache/rf.fit.hz.surface.rds')
  # make a df
  df <- data.frame(soil.density = s.den,ph = s.ph,clay = s.clay,
                   rad.short.jan = rad.jan, rad.short.jul = rad.jul,
                   wi = wi,curvature_profile = c.profile,curvature_plan=c.plan,
                   tmax = tmax,rain = rain,rh.min = rh.min,
                   tmax.mean = tmax.mean,map=map,
                   lai.opt=lai.opt)
  # predict prob and score
  if(giveProb){
    
    return(predict(object = model.in,newdata=df,type='prob'))
  }else{
    return( predict(object = model.in,newdata=df))
  }
  
  # }
}
