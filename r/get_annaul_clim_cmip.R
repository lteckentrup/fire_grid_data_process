# get monthly data for cmip####
get.annual.clim.func <- function(fn.in,par.nm,out.nm){
  # read in climate
  # cfa.clim.file <- nc_open('data/met/future/access/maca_VIC_ACCESS1-0_rcp45_pr2017-2060.nc')
  # fn.in <- 'data/met/future/access/rcp45_20452060_monthly_pr.rds'
  cfa.clim.file <- readRDS(fn.in)
  # names(cfa.clim.file$var)
  
  
  rs <- stack(cfa.clim.file)
  
  if(par.nm=='pr'){conver.day=365.25}else{conver.day=1}

  rs1 <- calc(rs, mean)*conver.day
  # plot(rs1)
 
  # met.df <- do.call(rbind,cfa.clim.file)
  # 
  # # pr.array <- ncvar_get(cfa.clim.file,par.nm)
  # lat <- ncvar_get(cfa.clim.file,'lat')
  # lon <- ncvar_get(cfa.clim.file,'lon')
  # days <- as.Date(ncvar_get(cfa.clim.file,'time'),
  #                 origin = gsub(pattern = 'days since ',replacement = '',x = cfa.clim.file$dim$time$units),
  #                 tz = 'GMT')
  # 
  # 
  # # choose only yr of interest
  # # choice.yr <- which(year(days) %in% 2045:2060)
  # choice.yr <- which(year(days) %in% yr.in)
  # 
  # pr.sub <- pr.array[,,choice.yr]
  # days.sub <- days[choice.yr]
  # # subset by month and get mean
  # mon.pr<- list()
  # for (mon.i in 1:12) {
  #   mon.choice <- which(month(days.sub) ==mon.i )
  #   
  #   tmp <-  apply(pr.sub[,,mon.choice], MARGIN = c(1,2), mean, na.rm = TRUE)
  #   tmp <- raster(t(tmp),
  #                 xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat))
  #   met.raster.right <- flip(tmp, direction='y')
  #   # plot(met.raster.right)
  #   mon.pr[[mon.i]] <- met.raster.right
  # }
  # plot(met.raster.right)
  # # save output
  saveRDS(rs1,out.nm)
  # saveRDS(mon.pr,'data/met/future/access/rcp45_20452060_monthly_pr.rds')
}


# use functions to get monthly data###
# # aceess 
# get.annual.clim.func(fn.in = 'data/met/future/access/rcp45_20452060_monthly_pr.rds',
#                   par.nm ='pr',
#                   out.nm = 'data/met/future/access/rcp45_20452060_annual_pr.rds')
# 
# get.annual.clim.func(fn.in = 'data/met/future/access/rcp45_20452060_monthly_tmax.rds',
#                      par.nm ='tmax',
#                      out.nm = 'data/met/future/access/rcp45_20452060_annual_tmax.rds')

# list met files
pr.fn.vec <- list.files(pattern = '_monthly_pr.rds',
                        path = 'data/met/future/',
                        full.names= T,recursive = T)
for(i in seq_along(pr.fn.vec)){
  get.annual.clim.func(fn.in = pr.fn.vec[i],
                       par.nm ='pr',
                       out.nm = gsub('monthly','annual',pr.fn.vec[i]))
}
# 
rh.fn.vec <- list.files(pattern = '_monthly_rh.rds',
                        path = 'data/met/future/',
                        full.names= T,recursive = T)
for(i in seq_along(rh.fn.vec)){
  get.annual.clim.func(fn.in = rh.fn.vec[i],
                       par.nm ='rh',
                       out.nm = gsub('monthly','annual',rh.fn.vec[i]))
}
# 
tmax.fn.vec <- list.files(pattern = '_monthly_tmax.rds',
                          path = 'data/met/future/',
                          full.names= T,recursive = T)
for(i in seq_along(tmax.fn.vec)){
  get.annual.clim.func(fn.in = tmax.fn.vec[i],
                       par.nm ='tmax',
                       out.nm = gsub('monthly','annual',tmax.fn.vec[i]))
}

