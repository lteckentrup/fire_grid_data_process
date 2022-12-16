library(raster)
library(oz)

read.climate.func <- function(var.in.nm, future_s, exclude.nm = 'noVeg') {
  # Generate list of file names
  file_names <- list.files(path = '../data/met/future/',
                           pattern = var.in.nm,
                           recursive = T,
                           full.names = T)

  # Exclude file names based on exclude.nm
  file_names <- file_names[!file_names %in% grep(exclude.nm, file_names)]

  # Filter for file names that match future_s
  file_names <- file_names[grep(future_s, file_names)]

  # Read RDS data
  rasters <- lapply(file_names, readRDS)

  # Generate single raster stack
  raster.new <- stack(rasters)

  return(raster.new)
}

# Read in the data for historical
pr.hist <- read.climate.func(var.in.nm = 'lai_jan_5km.rds',
                             future_s = 'history',
                             exclude.nm = '')

# Calculate the mean value for each raster in the stack
mean.ra.hist <- calc(pr.hist, fun = mean)

# Read in the data for the RCP4.5 mid-century scenario
pr.rcp45.mid <- read.climate.func(var.in.nm = 'lai_jan_5km.rds',
                                  future_s = 'rcp45_mid',
                                  exclude.nm = '')

# Calculate the mean value for each raster in the stack
mean.ra.45.mid <- calc(pr.rcp45.mid, fun = mean)

# Read in the data for the RCP4.5 end-of-century scenario
pr.rcp45.long <- read.climate.func(var.in.nm = 'lai_jan_5km.rds',
                                   future_s = 'rcp45_long',
                                   exclude.nm = '')

# Calculate the mean value for each raster in the stack
mean.ra.45.long <- calc(pr.rcp45.long, fun = mean)

# Read in the data for the RCP8.5 mid-century scenario
pr.rcp85.mid <- read.climate.func(var.in.nm = 'lai_jan_5km.rds',
                                  future_s = 'rcp85_mid',
                                  exclude.nm = '')

# Calculate the mean value for each raster in the stack
mean.ra.85.mid <- calc(pr.rcp85.mid, fun = mean)

# Read in the data for the RCP8.5 end-of-century scenario
pr.rcp85.long <- read.climate.func(var.in.nm = 'lai_jan_5km.rds',
                                   future_s = 'rcp85_long',
                                   exclude.nm = '')

# Calculate the mean value for each raster in the stack
mean.ra.85.long <- calc(pr.rcp85.long, fun = mean)

# Define a vector of scenarios to plot
scenarios <- c("RCP4.5 (2045-2060)",
               "RCP4.5 (2085-2100)",
               "RCP8.5 (2045-2060)",
               "RCP8.5 (2085-2100)")

# Define a vector of rasters to plot
rasters <- list(mean.ra.45.mid - mean.ra.hist, mean.ra.45.long - mean.ra.hist,
                mean.ra.85.mid - mean.ra.hist, mean.ra.85.long - mean.ra.hist)

# Define a vector of color breaks for historical
hist.breaks <- seq(0, 4.5, by = 0.5)

# Define a vector of color breaks for future scenarios
fut.breaks <- seq(-0.6, 1.6, by = 0.3)

# Open a PDF file
pdf("future_LAI_new.pdf", width = 8, height = 7)

# Plot historical scenario
plot(mean.ra.hist, main = "Historical", breaks = hist.breaks,
     col = topo.colors(length(hist.breaks) - 1))

vic(add = TRUE, col = "grey", lwd = 3)

# Loop through the scenarios and rasters
for (i in seq_along(scenarios)) {
  # Plot the difference between future scenarios and historical
  plot(rasters[[i]], main = paste(scenarios[i], " - Historical"),
       breaks = fut.breaks, col = topo.colors(length(fut.breaks) - 1))
  vic(add = TRUE, col = "grey", lwd = 3)
}

# Close the PDF file
dev.off()
