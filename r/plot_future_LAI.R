library(raster)
library(oz)
library(rnaturalearth)

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

# Scenarios to read in
scenarios <- c("history", "rcp45_mid", "rcp45_long", "rcp85_mid", "rcp85_long")

# Names for output variables
names <- c("mean.ra.hist", "mean.ra.45.mid", "mean.ra.45.long",
           "mean.ra.85.mid", "mean.ra.85.long")

# Loop through scenarios
for (i in seq_along(scenarios)) {

  # Read in the data
  raster <- read.climate.func(var.in.nm = 'lai_jan_5km.rds',
                              future_s = scenarios[i],
                              exclude.nm = '')

  # Calculate the mean value for each raster in the stack
  mean <- mean(raster)

  # Assign the mean raster to the output variable
  assign(names[i], mean)
}

# Define title names
scenarios <- c("RCP4.5 (2045-2060)",
               "RCP4.5 (2085-2100)",
               "RCP8.5 (2045-2060)",
               "RCP8.5 (2085-2100)")

# Rasters to plot
rasters <- list(mean.ra.45.mid - mean.ra.hist, mean.ra.45.long - mean.ra.hist,
                mean.ra.85.mid - mean.ra.hist, mean.ra.85.long - mean.ra.hist)

# Color breaks for historical
hist.breaks <- seq(0, 4.5, by = 0.5)

# Color breaks for future scenarios
fut.breaks <- seq(-0.6, 1.6, by = 0.3)

# Open a PDF file
pdf("future_LAI_new.pdf", width = 8, height = 7)

# Plot historical
plot(mean.ra.hist, main = "Historical", breaks = hist.breaks,
     col = topo.colors(length(hist.breaks) - 1))

vic(add = TRUE, col = "grey", lwd = 3)

# Loop through the future scenarios and rasters
for (i in seq_along(scenarios)) {

  # Plot the difference between future scenarios and historical
  plot(rasters[[i]], main = paste(scenarios[i], " - Historical"),
       breaks = fut.breaks, col = topo.colors(length(fut.breaks) - 1))
  vic(add = TRUE, col = "grey", lwd = 3)
}

# Close the PDF file
dev.off()

#### Plot only historical LAI and the difference at the end of the century

# Get shape of Victoria
au_map <- ne_states(country = "Australia",
                    returnclass = "sf")
vic_map <- au_map[7,]$geometry
shape.vic<- sf::as_Spatial(vic_map)

png('future_LAI.png',width = 800,height = 300)
par(mfrow=c(1,2),mar=c(3,3,1,5))

### Mask Victoria
mean.ra.hist <- mask(mean.ra.hist, shape.vic)
mean.ra.85.long <- mask(mean.ra.85.long, shape.vic)

### First subplot: Historical
break.vec <- seq(0,3,by=0.5)
plot(mean.ra.hist,
     breaks = break.vec,
     col=rev(topo.colors(length(break.vec)-1)),
     box=FALSE,bty='n')
myText = bquote((g)~Historical~LAI~(m^2*m^-2))
legend('topleft',legend = myText,bty='n')

### Second subplot: Different at end of century for RCP8.5
break.vec <- seq(0,1.2,by=0.2)
plot((mean.ra.85.long-mean.ra.hist),main = '',
     breaks = break.vec,
     col=rev(hcl.colors(length(break.vec)-1)),
     box=FALSE,bty='n')
legend('topleft',legend = '(h) Change in the future',bty='n')

dev.off()
