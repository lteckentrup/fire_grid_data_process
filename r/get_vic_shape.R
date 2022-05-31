library(rnaturalearth)
au_map <- ne_states(country = "Australia", 
                    returnclass = "sf")
vic_map <- au_map[7,]$geometry
# plot(raster(vic_map))
shape.vic<- sf::as_Spatial(vic_map)





# crs(diff.rcp45_mid.mean) <- crs(shape.vic)
# only.vic<- crop(y = extent(shape.vic),x =diff.rcp45_mid.mean )
# plot(only.vic)
# r3 <- mask(diff.rcp45_mid.mean, shape.vic)
# plot(r3)
