library(ncdf4)
library(raster)

# Define function to get annual mean of a meteorological variable from SILO reanalysis
get.annual.slio.func <- function(fn.path, var.nm, my.fun){
  # Get list of file names
  fn.vec <- list.files(fn.path, full.names = TRUE, pattern = '.nc')

  # Use lapply to apply a function over the list of file names
  met.ls <- lapply(fn.vec, function(fn){
    # Open NetCDF file
    nc.tmp <- nc_open(fn)

    # Extract meteorological variable from NetCDF file
    tmax <- ncvar_get(nc.tmp, var.nm)

    # Longitude and latitude
    lon.vec <- nc.tmp$dim$lon$vals
    lat.vec <- nc.tmp$dim$lat$vals

    # Longitude and latitude indices for Victoria
    select.lon <- which(lon.vec > 132 & lon.vec < 150)
    select.lat <- which(lat.vec > -40 & lat.vec < -29)

    # Select data for Victoria
    tmax.vic <- tmax[select.lon, select.lat, ]

    # Get maximum value for each grid cell in Victoria
    tmp <- apply(tmax.vic, MARGIN = c(1, 2), max, na.rm = TRUE)

    # Convert maximum values to raster object
    tmp.ra <- flip(t(raster((tmp))))

    # Set extent of raster object to Victoria
    extent(tmp.ra) <- c(xmn = 132, xmx = 150, ymn = -40, ymx = -29)

    # Return raster object
    tmp.ra
  })

  # Stack raster objects into a single object
  met.ra.stack <- stack(met.ls)

  # Calculate mean of stacked raster object
  met.ra.mean <- mean(met.ra.stack)

  # Return mean raster object
  return(met.ra.mean)
}

# Maximum temperature
tmax.ra <- get.annual.slio.func(fn.path = '../data/met/tmax_silo/',
                                var.nm = 'max_temp',
                                my.fun = mean)

# Precipitation
prcp.ra <- get.annual.slio.func(fn.path = '../data/met/precip_silo/',
                                var.nm = 'monthly_rain',
                                my.fun = mean)

# Vapor pressure deficit
vpd.ra <- get.annual.slio.func(fn.path = '../data/met/vpd_silo/',
                               var.nm = 'vp_deficit',
                               my.fun = mean)

# Save stacked raster object with annual mean values of maximum temperature,
# precipitation, and vapor pressure deficit to RDS

saveRDS(stack(list(tmax = tmax.ra,
                   prcp = prcp.ra * 12, # month mean to annual
                   vpd = vpd.ra / 10)), # hPa to kPa
        '../cache/slio.met.annual.rds')
