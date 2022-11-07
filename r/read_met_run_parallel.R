# tmax.ra <- readRDS(file = 'data/met/future/ACCESS1-0/history/history_20002015_annual_tmax.rds')
# pr.ra <- readRDS(file = 'data/met/future/ACCESS1-0/history/history_20002015_annual_pr.rds')
# rh.ra <- readRDS(file = 'data/met/future/ACCESS1-0/history/history_20002015_annual_rh.rds')

t.path <- 'data/met/future/ACCESS1-0/rcp85_long/rcp85_20852100_annual_tmax.rds'
map.path <- 'data/met/future/ACCESS1-0/rcp85_long/rcp85_20852100_annual_pr.rds'
pr.season.path <- 'data/met/future/ACCESS1-0/rcp85_long/rcp85_20852100_pr_seasonality.rds'
lai.path <- 'data/met/future/ACCESS1-0/rcp85_long/rcp85_20852100_lai_jan_5km.rds'

tmax.mean.ra <- readRDS(file = t.path)
pr.mean.ra <- readRDS(file = map.path)
pr.season.ra <- readRDS(pr.season.path)
lai.ra <- readRDS(file = lai.path)


# 
tmax.mean.vic <- get.small.area.func(tmax.mean.ra,p = shape.vic)

pr.mean.vic <- get.small.area.func(pr.mean.ra,p = shape.vic)

pr.season.vic <- get.small.area.func(pr.season.ra,p = shape.vic)

lai.vic <- get.small.area.func(lai.ra,p = shape.vic)