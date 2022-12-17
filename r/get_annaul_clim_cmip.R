library(raster)

# Define function to calculate annual values of met predictors
get.annual.clim.func <- function(fn.in,par.nm,out.nm){

  # Read in RDS file
  cfa.clim.file <- readRDS(fn.in)

  # Stack rasters
  rs <- stack(cfa.clim.file)

  # Precipitation has daily values: adjust leap year
  if(par.nm=='pr'){conver.day=365.25}else{conver.day=1}

  # Calculate mean annual values
  rs1 <- calc(rs, mean)*conver.day

  # Save annual RDS file
  saveRDS(rs1,out.nm)
}

# List met variables
met_vars <- c("pr", "rh", "tmax")

# Define pathway
path <- "../data/met/future/"

# Loop through met variables
for(met_var in met_vars) {
  print(met_var)
  pattern <- paste0("_monthly_", met_var, ".rds")
  fn.vec <- list.files(pattern = pattern,
                       path = path,
                       full.names = T,
                       recursive = T)
  for(i in seq_along(fn.vec)) {
    print(fn.vec[i])
    get.annual.clim.func(fn.in = fn.vec[i],
                         par.nm = met_var,
                         out.nm = gsub("monthly", "annual", fn.vec[i])
                       )
  }
}
