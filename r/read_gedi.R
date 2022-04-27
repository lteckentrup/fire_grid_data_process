# library(devtools)
# devtools::install_git("https://github.com/carlos-alberto-silva/rGEDI", dependencies = TRUE)
# library(rGEDI)

# install.packages("BiocManager")
# BiocManager::install("rhdf5")

library(rhdf5)

# file <- h5file('data/gedi/GEDI02_B_2021350082749_O17051_04_T10525_02_003_01_V002.h5')
# data.sets<-list.datasets(file)
# preprocess.GEDI(input.file=file, 
#                 filename='test.txt')

read_gedi.func <- function(fn,
                           beams=c("0000","0001","0010","0011",
                                        "0101","0110", "1000", "1011")){
  # testSubset <- h5read(file = fn, 
  #                      name = "BEAM0000/geolocation/lat_highestreturn")
  # fn <- "data/gedi/GEDI02_B_2021350082749_O17051_04_T10525_02_003_01_V002.h5"
  
  # D = H5Fopen(fn)

  # file <- list.datasets(D)

  # data.sets<-h5ls(file)
  
  beams_ls<-lapply(1:8, function(i){
   
    lat<-h5read(file = fn, 
                name = paste("/BEAM",beams[i],"/geolocation/lat_highestreturn", sep=""))
      
     
    if(is.null(nrow(lat))) next
    lon<-h5read(file = fn, 
                name = paste("/BEAM",beams[i],"/geolocation/lon_highestreturn", sep=""))

    elev_lowestmode<-h5read(file = fn, 
                            name = paste("/BEAM",beams[i],"/geolocation/elev_lowestmode", sep=""))
      

    elev_TDX<-h5read(file = fn, 
                     name = paste("/BEAM",beams[i],"/geolocation/digital_elevation_model", sep=""))
    
    rh100<-h5read(file = fn, 
                  name = paste("/BEAM",beams[i],"/rh100", sep=""))


    cover<-h5read(file = fn, 
                  name = paste("/BEAM",beams[i],"/cover", sep=""))
    
    pai<-h5read(file = fn, 
                name = paste("/BEAM",beams[i],"/pai", sep=""))
    
    fhd<-h5read(file = fn, 
                name = paste("/BEAM",beams[i],"/fhd_normal", sep=""))
  
    num_detectedmodes<-h5read(file = fn, 
                              name = paste("/BEAM",beams[i],"/num_detectedmodes", sep=""))
    # PAVD<-h5read(file = fn, 
    #          name = paste("/BEAM",beams[i],"/pavd_z", sep=""))
    x <- h5ls(file = fn)
    modis_treecover<-h5read(file = fn, 
                            name = paste("/BEAM",beams[i],"/land_cover_data/modis_treecover", sep=""))
    
    q1<-h5read(file = fn, 
               name = paste("/BEAM",beams[i],"/l2a_quality_flag", sep=""))
      
    surface_flag<-h5read(file = fn, 
                         name = paste("/BEAM",beams[i],"/surface_flag", sep=""))
    
 
    sensitivity<-h5read(file = fn, 
                        name = paste("/BEAM",beams[i],"/sensitivity", sep=""))
      
  
    shot_number<-h5read(file = fn, 
                        name = paste("/BEAM",beams[i],"/shot_number", sep=""))
    
    delta_time<-h5read(file = fn, 
                       name = paste("/BEAM",beams[i],"/geolocation/delta_time", sep=""))
    
    return(data.frame(lon=lon[1:nrow(lon)],
                      lat=lat[1:nrow(lat)],
                      elev_lowestmode=elev_lowestmode[1:nrow(elev_lowestmode)],
                      elev_TDX = elev_TDX[1:nrow(elev_TDX)],
                      rh100=rh100[1:nrow(rh100)]/100,
                      cover=cover[1:nrow(cover)],
                      pai=pai[1:nrow(pai)],
                      fhd=fhd[1:nrow(fhd)],
                      # PAVD = PAVD,
                      num_detectedmodes=num_detectedmodes[1:nrow(num_detectedmodes)],
                      modis_treecover=modis_treecover[1:nrow(modis_treecover)],
                      q1=q1[1:nrow(q1)],
                      surface_flag=surface_flag[1:nrow(surface_flag)],
                      sensitivity=round(sensitivity[1:nrow(sensitivity)],1),
                      shot_number=shot_number[1:nrow(shot_number)],
                      delta_time=delta_time[1:nrow(delta_time)],
                      beam = as.character(beams[i]))
    )
    
  })
  
  # 
  return(do.call(rbind,beams_ls))
  
}


gedi.df <- read_gedi.func('data/gedi/GEDI02_B_2021350082749_O17051_04_T10525_02_003_01_V002.h5')
unique(gedi.df$beam)

# library(raster)
# r <- raster(ncol=length((gedi.df$lon)), nrow=length((gedi.df$lat)),
#             xmn = min(gedi.df$lon),xmx=max(gedi.df$lon), ymn=min(gedi.df$lat), ymx=max(gedi.df$lat) )
# values(r) <- gedi.df$pai
# plot(r)
# GEDI_sp<-SpatialPointsDataFrame(coords=cbind(gedi.df$lon,gedi.df$lat),
#                                 data=gedi.df[,c('lon','lat','pai')])

