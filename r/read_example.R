library(ncdf4)
library(raster)
cfa.clim.file <- nc_open('data/met/sample/maca_VIC_ACCESS1-0_rcp45_pr2061-2100.nc')
names(cfa.clim.file$var$pr$units)
pr.array <- ncvar_get(cfa.clim.file,'pr')
lat <- ncvar_get(cfa.clim.file,'lat')
lon <- ncvar_get(cfa.clim.file,'lon')
temp.array <- abind::abind(pr.array, along=3)
# Annual average  for every x,y location
average <- rowMeans(temp.array, dims=2)*12*10

pr.ra <- raster(t(average),
                xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat))

plot(flip((pr.ra), direction='y'))


  
  
  diff(lat)
  