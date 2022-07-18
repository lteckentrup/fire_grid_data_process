########
ftLUT.df <- read.csv('FT2Group_lut.csv')
length(unique(ftLUT.df$Fuel_Type))
unique(ftLUT.df$GROUPNAME)
unique(ftLUT.df$SUBGROUPNA)

evd.df <- read.csv('BioEVCEVD_VitalAtt_fh01_20120116.csv')
unique(evd.df$evd)

evg.df <- read.csv('evc2evgLUT.csv')
unique(evg.df$EVC_GPNAME)


library(raster)
evc.df <- raster('data/EVC_fuelType/evc/VICSANSW161.tif')
# plot(evc.df,breaks=c(0,2000,2001:2070,3001))
atrribute.df <- evc.df@data@attributes[[1]]
atrribute.df <- atrribute.df[,c('VICSANSW.FUEL_TYPE' , 'VICSANSW.TYPE_NAME')]
x.df <- atrribute.df[atrribute.df$VICSANSW.FUEL_TYPE>=3000,]

# get evc evd
evc2evd.df <- read.csv('evc2evdLUT.csv')

replace.func <- function(input.string,tar.string,replace.string=''){
  gsub(pattern = tar.string,
       replacement = replace.string,
       x = input.string)
}
replace.func(evc2evd.df$evc_no[12],tar.string = '_61')
evc2evd.df$evc_no <- sapply(evc2evd.df$evc_no,replace.func,simplify =T,tar.string = '_61')
evc2evd.df$evc_no <- sapply(evc2evd.df$evc_no,replace.func,simplify =T,tar.string = '_62')
evc2evd.df$evc_no <- as.numeric(evc2evd.df$evc_no)
evc2evd.df <- evc2evd.df[complete.cases(evc2evd.df),]
names(evc2evd.df)
# get evc fuel type
evc2ft.df <- read.csv('evc2ftLUTt.csv')
evc2ft.df <- evc2ft.df[,c('EVC','EVCNAME',"FUEL_TYPE",'X')]
names(evc2ft.df) <- c('evc_no','EVCNAME','FUEL_TYPE','fuelName')

# 
evc.ft.evd.df <- merge(evc2evd.df,evc2ft.df,all=T)
evc.ft.evd.df <- evc.ft.evd.df[,c("evc_no","commonname","evc_fullname","EVCNAME",
                                  "evd","FUEL_TYPE","fuelName")]
names(evc.ft.evd.df) <- c("evc_no","commonname","EVCNAME_1","EVCNAME_2","EVD","FUEL_TYPE","fuelName")
write.csv(evc.ft.evd.df,'evc_evd_fuelType.LUT.csv',row.names = F)




