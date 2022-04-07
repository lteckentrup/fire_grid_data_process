# download.file('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/monthly/tasmax/CHELSA_tasmax_01_1995_V.2.1.tif ',
#               destfile = 'data/met/tmax.tif')
# 
for (year.i in 1995:2018){
  tmax.fn <- sprintf('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/monthly/tasmax/CHELSA_tasmax_01_%s_V.2.1.tif ',year.i)
  tmax.fn.out <- sprintf('data/met/tmax/tmax.%s.tif',year.i)
  download.file(tmax.fn,
                destfile = tmax.fn.out)
}
# 
for (year.i in 1995:2018){
  vpd.fn <- sprintf('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/monthly/vpd/CHELSA_vpd_01_%s_V.2.1.tif',year.i)
  vpd.fn.out <- sprintf('data/met/vpd/vpd.%s.tif',year.i)
  download.file(vpd.fn,
                destfile = vpd.fn.out)
}
# 
for (year.i in 1995:2018){
  rain.fn <- sprintf('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/monthly/pr/CHELSA_pr_01_%s_V.2.1.tif',year.i)
  rain.fn.out <- sprintf('data/met/rain/rain.%s.tif',year.i)
  download.file(rain.fn,
                destfile = rain.fn.out)
}
