library(raster)
source('../R/get_vic_shape.R')

read.climate.func <- function(var.in.nm,future_s,exclude.nm = 'noVeg'){
  rcp45_mid.fn <- list.files(path = '../data/met/future/',
                             pattern = var.in.nm,recursive = T,full.names = T)

  rcp45_mid.fn <- rcp45_mid.fn[!rcp45_mid.fn %in% grep(exclude.nm, rcp45_mid.fn)]
  rcp45_mid.fn <- rcp45_mid.fn[grep(future_s, rcp45_mid.fn)]

  rcp45.mid.h.c <- sapply(rcp45_mid.fn, function(fn.in){

    x <- readRDS(fn.in)
    return(x[[1]])
  })

  raster.new = stack(rcp45.mid.h.c)
  return(raster.new)
}

# Set up a vector with the values for future_s
future_vec <- c('history', 
                'rcp45_mid', 
                'rcp45_long', 
                'rcp85_mid', 
                'rcp85_long')

# Set up a vector with the plot titles
plot_titles <- c('Historical', 
                 'RCP4.5 (2045 - 2060)', 
                 'RCP4.5 (2085 - 2100)', 
                 'RCP8.5 (2045 - 2060)', 
                 'RCP8.5 (2085 - 2100)')

# Initialize a list to store the output from calc
mean_ra_list <- list()

# Loop through the values in future_vec
for(i in 1:length(future_vec)) {

  # Read data
  pr <- read.climate.func(var.in.nm = 'monthly_tmax.rds',
                         future_s = future_vec[i],
                         exclude.nm = '')

  # Calculate the mean
  mean_ra <- calc(pr, fun = mean)

  # Mask data and add to data list
  mean_ra_list[[future_vec[i]]] <- mask(mean_ra, shape.vic)
}

# Set up breaks for the plots
break_vec <- seq(10,35,by=5)

# Open the PDF file
pdf('future_mon_tmax_new.pdf',width = 8,height = 7)

# Plot the current data
plot(mean_ra_list[['history']] - 273.15, main = plot_titles[1],
     breaks = break_vec, col = topo.colors(length(break_vec)-1))

# Loop through the remaining values in future_vec
for(i in 2:length(future_vec)) {

  # Set breaks for each plot
  if(future_vec[i] %in% c('rcp45_mid', 'rcp45_long', 'rcp85_mid')) {
    break_vec <- seq(0.5,2.5,by=0.5)
  } else {
    break_vec <- seq(2,4,by=0.5)
  }

  # Plot the data
  plot((mean_ra_list[[future_vec[i]]] - mean_ra_list[['history']]),
        main = paste(plot_titles[i], ' - Historical'), breaks = break_vec,
        col = topo.colors(length(break_vec)-1))
}

# Close the PDF file
dev.off()

### Only plot historical and end of century RCP8.5
png('future_tmax_new.png', width=800, height=300)
par(mfrow=c(1,2), mar=c(3,3,1,5))

# Set up breaks for historical
break.vec <- seq(10, 35, by=5)

# Plot historical data
plot(mean_ra_list[['history']] - 273.15,
     breaks=break.vec,
     col=topo.colors(length(break.vec)-1),
     box=FALSE, bty='n',
     xaxt='n')

legend('topleft', legend=bquote((a)~Current~mean~T[max]~(degree*C)), bty='n')

# Set up breaks for RCP8.5
break.vec <- seq(2, 4, by=0.5)

# Plot change at the end of the century
plot((mean_ra_list[['rcp85_long']] - mean_ra_list[['history']]),
      main='',
      breaks=break.vec,
      col=heat.colors(length(break.vec)-1),
      box=FALSE, bty='n',
      xaxt='n')

legend('topleft', legend='(b) Change in the future', bty='n')
dev.off()
