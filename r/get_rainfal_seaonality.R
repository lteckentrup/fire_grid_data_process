# Calculates rainfall seasonality follwoing Feng et al 2013
# 10.1038/nclimate1907

library(raster)
library(ncdf4)

get.rain.seaonality.func <- function(fn.in){
  # Read in RDS file
  pr.ra.ls <- readRDS(fn.in)

  # stack rasters
  pr.stack <- stack(pr.ra.ls)

  # get mean
  pr.mean <- calc(pr.stack, fun=mean)

  # get probability for each month
  pr.p.stack <- (pr.stack*365.25/12)/(pr.mean *365.25)

  # get the entropy
  d.stack <- pr.p.stack * log(pr.p.stack*12)
  d.sum <- calc(d.stack, fun=sum)

  # get index
  pr.seasonality.index <- d.sum * pr.mean / maxValue(pr.mean)

  # save outputs
  fn.out <- gsub(pattern = 'monthly_pr',replacement = 'pr_seasonality',x = fn.in)
  saveRDS(pr.seasonality.index,fn.out)
}


# Get list of file names with monthly rainfall data
fn_vec <- list.files(path = '../data/met/future',
                     pattern = 'monthly_pr.rds',
                     full.names = TRUE, recursive = TRUE)

# Apply the get.rain.seaonality.func function to each file in fn_vec
sapply(fn_vec, get.rain.seaonality.func)

# Get list of file names with seasonal rainfall data
fn_vec <- list.files(path = '../data/met/future',
                     pattern = 'pr_seasonality.rds',
                     full.names = TRUE, recursive = TRUE)

# Define function to calculate mean of seasonal rainfall data for a given RCP scenario
get_seasonality_mean_func <- function(rcp) {
  # Get the file names for the given RCP scenario
  fn_hist <- fn_vec[grep(pattern = rcp, x = fn_vec)]

  # Read in the seasonal rainfall data for each file
  pr_seasonality_ls <- sapply(fn_hist, readRDS)

  # Stack the data and calculate the mean
  pr_seasonality_stack <- stack(pr_seasonality_ls)
  pr_seasonality_mean <- mean(pr_seasonality_stack)

  # Save the mean data to a file
  saveRDS(pr_seasonality_mean, sprintf('../cache/pr_seasonality_%s.rds', rcp))
}

get_seasonality_mean_func(rcp = 'history')
get_seasonality_mean_func(rcp = 'rcp45_mid')
get_seasonality_mean_func(rcp = 'rcp45_long')
get_seasonality_mean_func(rcp = 'rcp85_long')
get_seasonality_mean_func(rcp = 'rcp85_mid')

### Seasonality in SILO reanalysis
met.nm <-  list.files(path = '../data/met/precip_silo/',
                      pattern = '.nc',full.names = T,recursive = F)

get_rain_ra_func <- function(met_nm, mon) {
  # Open netCDF file
  rain_nc <- nc_open(met_nm)

  # Get the longitude and latitude
  lon_vec <- ncvar_get(rain_nc, 'lon')
  lat_vec <- ncvar_get(rain_nc, 'lat')

  # Get the rainfall data
  rain_df <- try(ncvar_get(rain_nc, 'monthly_rain'))

  # Extract the desired month's data
  rain_mon_df <- rain_df[,,mon]

  # Convert the data to a raster object
  rain_raster <- raster(t(rain_mon_df),
                        xmn = min(lon_vec), xmx = max(lon_vec),
                        ymn = min(lat_vec), ymx = max(lat_vec))

  # Flip the raster vertically and replace infinite values with NA
  met_raster_right <- flip(rain_raster, direction = 'y')
  met_raster_right[is.infinite(met_raster_right)] <- NA

  # Return the mean value of the raster
  return(mean(met_raster_right, na.rm = TRUE))
}

get_mean_func <- function(mon) {
  # Apply get_rain_ra_func function to each element in the met_nm vector
  rain_silo_1 <- sapply(met_nm, get_rain_ra_func, mon_in = mon)

  # Calculate mean value of the stacked data
  rain_silo_1_mean <- mean(stack(rain_silo_1))

  # Return the mean value
  return(rain_silo_1_mean)
}
