# https://s3-ap-southeast-2.amazonaws.com/silo-open-data/annual/evap_pan/2015.evap_pan.nc
# https://www.longpaddock.qld.gov.au/silo/about/climate-variables/

dir.create('data/met/precip_silo/')
for (year.i in 1994:2018){
  tmax.fn <- sprintf('https://s3-ap-southeast-2.amazonaws.com/silo-open-data/annual/monthly_rain/%s.monthly_rain.nc',
                     year.i)
  tmax.fn.out <- sprintf('data/met/precip_silo/precip.%s.nc',year.i)
  download.file(tmax.fn,
                destfile = tmax.fn.out,cacheOK=FALSE,method = 'curl')
}

dir.create('data/met/tmax_silo/')
for (year.i in 1994:2018){
  tmax.fn <- sprintf('https://s3-ap-southeast-2.amazonaws.com/silo-open-data/annual/max_temp/%s.max_temp.nc',
                     year.i)
  tmax.fn.out <- sprintf('data/met/tmax_silo/tmax.%s.nc',year.i)
  download.file(tmax.fn,
                destfile = tmax.fn.out,cacheOK=FALSE,method = 'curl')
}

dir.create('data/met/rh_silo/')
for (year.i in 1994:2018){
  tmax.fn <- sprintf('https://s3-ap-southeast-2.amazonaws.com/silo-open-data/annual/rh_tmax/%s.rh_tmax.nc',
                     year.i)
  tmax.fn.out <- sprintf('data/met/rh_silo/rh.%s.nc',year.i)
  download.file(tmax.fn,
                destfile = tmax.fn.out,cacheOK=FALSE,method = 'curl')
}

# for (year.i in 1995:2018){
#   dir.create('data/met/precip_silo/')
#   tmax.fn <- sprintf('https://s3-ap-southeast-2.amazonaws.com/silo-open-data/annual/monthly_rain/%s.monthly_rain.nc',
#                      year.i)
#   tmax.fn.out <- sprintf('data/met/precip_silo/precip.%s.nc',year.i)
#   download.file(tmax.fn,
#                 destfile = tmax.fn.out,cacheOK=FALSE,method = 'curl')
# }
# 
# for (year.i in 1995:2018){
#   dir.create('data/met/precip_silo/')
#   tmax.fn <- sprintf('https://s3-ap-southeast-2.amazonaws.com/silo-open-data/annual/monthly_rain/%s.monthly_rain.nc',
#                      year.i)
#   tmax.fn.out <- sprintf('data/met/precip_silo/precip.%s.nc',year.i)
#   download.file(tmax.fn,
#                 destfile = tmax.fn.out,cacheOK=FALSE,method = 'curl')
# }


# library(ncdf4)
# 
# nc.file <- nc_open('test.nc')
# names(nc.file$var)
# rain.df <- ncvar_get(nc.file,'monthly_rain')
