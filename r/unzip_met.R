zip.fn.vec <- list.files(path = 'E:/download/CFA_VIC_MET/',
           full.names = T,recursive = T,pattern = '.zip')

library(utils)

for(i in seq_along(zip.fn.vec)){
  # read file content
  zip.fn.content <- unzip(zip.fn.vec[i], 
                          files = NULL, 
                          list = T)
  dir.out <- dirname(zip.fn.vec[i])
  # select relevent met
  pr.fn <- grep(x = zip.fn.content$Name,
                pattern = '_pr' )
  tm.fn <- grep(x = zip.fn.content$Name,
                pattern = '_tasmax')
  rh.fn <- grep(x = zip.fn.content$Name,
                pattern = '_rhsmin')
  met.fn <-  zip.fn.content$Name[c(pr.fn,tm.fn,rh.fn)]
  # unzip
  unzip(zip.fn.vec[i], 
        files = met.fn, 
        list = F,exdir = dir.out)
}

# delet zip files
for(i in seq_along(zip.fn.vec)){
unlink(zip.fn.vec[i])
}

