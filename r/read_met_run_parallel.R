# tmax.ra <- readRDS(file = 'data/met/future/ACCESS1-0/history/history_20002015_annual_tmax.rds')
# pr.ra <- readRDS(file = 'data/met/future/ACCESS1-0/history/history_20002015_annual_pr.rds')
# rh.ra <- readRDS(file = 'data/met/future/ACCESS1-0/history/history_20002015_annual_rh.rds')

tmax.mean.ra <- readRDS(file = 'data/met/future/ACCESS1-0/history/history_20002015_annual_tmax.rds')
pr.mean.ra <- readRDS(file = 'data/met/future/ACCESS1-0/history/history_20002015_annual_pr.rds')
pr.season.ra <- readRDS('cache/pr_seaonality_history.rds')

lai.ra <- readRDS(file = 'data/met/future/ACCESS1-0/history/history_20002015_lai_jan_5km.rds')


# 
tmax.mean.vic <- get.small.area.func(tmax.mean.ra,p = shape.vic)

pr.mean.vic <- get.small.area.func(pr.mean.ra,p = shape.vic)

pr.season.vic <- get.small.area.func(pr.season.ra,p = shape.vic)

lai.vic <- get.small.area.func(lai.ra,p = shape.vic)