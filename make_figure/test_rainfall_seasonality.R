source('r/get_vic_shape.R')
pr.season.ra.hist <- readRDS('cache/pr_seaonality_history.rds')

pr.season.ra.rcp45.mid <- readRDS('cache/pr_seaonality_rcp45_mid.rds')

pr.season.ra.rcp45.long <- readRDS('cache/pr_seaonality_rcp45_long.rds')

pr.season.ra.rcp85.long <- readRDS('cache/pr_seaonality_rcp85_long.rds')

pr.season.ra.rcp85.mid <- readRDS('cache/pr_seaonality_rcp85_mid.rds')

hist.vic <- get.small.area.func(pr.season.ra.hist,shape.vic)
plot(hist.vic)
break.vec <- seq(-0.01,0.025,by=0.005)
plot(get.small.area.func(pr.season.ra.rcp45.mid,shape.vic) - hist.vic,
     breaks = break.vec,col=topo.colors(7))
plot(get.small.area.func(pr.season.ra.rcp45.long,shape.vic) - hist.vic,
     breaks = break.vec,col=topo.colors(7))
plot(get.small.area.func(pr.season.ra.rcp85.mid,shape.vic) - hist.vic,
     breaks = break.vec,col=topo.colors(7))
plot(get.small.area.func(pr.season.ra.rcp85.long,shape.vic) - hist.vic,
     breaks = break.vec,col=topo.colors(7))
