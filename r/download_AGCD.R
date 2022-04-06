# download agcd data###
# get precip
for (year.i in 1995:2018){
  tmax.fn <- sprintf('https://dapds00.nci.org.au/thredds/fileServer/zv2/agcd/v1/precip/total/r005/01month/agcd_v1_precip_total_r005_monthly_%s.nc',
                     year.i)
  tmax.fn.out <- sprintf('data/met/rain/rain.%s.nc',year.i)
  download.file(tmax.fn,
                destfile = tmax.fn.out,cacheOK=FALSE,method = 'curl')
}
# get tmax
for (year.i in 1995:2018){
  tmax.fn <- sprintf('https://dapds00.nci.org.au/thredds/fileServer/zv2/agcd/v1/tmax/mean/r005/01month/agcd_v1_tmax_mean_r005_monthly_%s.nc',
                     year.i)
  tmax.fn.out <- sprintf('data/met/tmax/tmax.%s.nc',year.i)
  download.file(tmax.fn,
                destfile = tmax.fn.out,cacheOK=FALSE,method = 'curl')
}
# get vph15
for (year.i in 1995:2018){
  tmax.fn <- sprintf('https://dapds00.nci.org.au/thredds/fileServer/zv2/agcd/v1/vapourpres_h15/mean/r005/01month/agcd_v1_vapourpres_h15_mean_r005_monthly_%s.nc',
                     year.i)
  tmax.fn.out <- sprintf('data/met/vph15/vph15.%s.nc',year.i)
  download.file(tmax.fn,
                destfile = tmax.fn.out,cacheOK=FALSE,method = 'curl')
}
