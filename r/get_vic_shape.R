library(rnaturalearth)
au_map <- ne_states(country = "Australia", 
                    returnclass = "sf")
vic_map <- au_map[7,]$geometry
# plot(raster(vic_map))
shape.vic<- sf::as_Spatial(vic_map)


# 
get.small.area.func <- function(ra.in,p = p){
  r2 <- crop(ra.in, extent(p))
  r3 <- mask(r2, p,method='ngb')
  # crs(r3) <- crs(ra.in)
  # aggregate(r3, fact=3)
  return(r3)
}



# crs(diff.rcp45_mid.mean) <- crs(shape.vic)
# only.vic<- crop(y = extent(shape.vic),x =diff.rcp45_mid.mean )
# plot(only.vic)
# r3 <- mask(diff.rcp45_mid.mean, shape.vic)
# plot(r3)
