# hz.ele.ls <- readRDS('data/met/future/ACCESS1-0/history/_hz_elevated.rds')
# plot(hs.ele.ls.rcp85.mid[['val']])
# raster.new = stack(hz.ele.ls[["prob"]])
# plot(max(raster.new))
# 
# 
# 
# hs.ele.ls <- readRDS('data/met/future/ACCESS1-0/rcp45_mid/_hz_elevated.rds')
# hs.ele.noveg.ls <- readRDS('data/met/future/BNU-ESM/rcp45_mid/noVeg_hz_elevated.rds')
# 
# hs.ele.ls.rcp85.mid <- readRDS('data/met/future/BNU-ESM/rcp85_mid/_hz_bark.rds')
# hs.ele.ls.rcp85.mid <- readRDS('data/met/future/BNU-ESM/rcp45_mid/noVeg_hz_bark.rds')
# plot(hs.ele.ls.rcp85.mid[['val']])
# x <- hs.ele.ls.rcp85.mid[['prob']]
# names(x)
# 
# # hs.ele.ls.rcp85.mid[['prob']][[2]]
#   # fn.vec <- list.files(path = 'data/met/future/ACCESS1-0/',pattern = '.rds',
# #                      full.names = T,recursive = T)
# # 
# # fn.vec <- fn.vec[grep(pattern = 'rcp45_mid',x = fn.vec)]
# # fn.vec <- fn.vec[-grep(pattern = 'noVeg',x = fn.vec)]
# 
# pdf('figures/example-future.pdf',width = 8,height = 8*.618)
# # 
# plot((hs.ele.ls[['val']]),
#      main = 'Elevated Fuel RCP4.5 2045-2060')
# # 
# plot((hs.ele.noveg.ls[['val']]),
#      main = 'Elevated Fuel RCP4.5 2045-2060 without veg')
# # 
# plot((hs.ele.ls[['val']] - hs.ele.noveg.ls[['val']]),
#      main = 'Impacts of vegetation response')
# # 
# plot((hs.ele.ls[['val']] - hs.ele.ls.rcp85.mid[['val']]),
#      main = 'RCP4.5 - RCP8.5')
# # 
# plot((hs.ele.ls[['prob']][[4]]),
#      main = paste0('Probability of hazard score=',
#                    names(hs.ele.ls[['prob']])[4]))
# dev.off()
# 
# 
# 
# plot((hs.ele.ls[['prob']][[5]]))
# names(hs.ele.ls[['prob']])
# 
# 
# plot((hs.ele.noveg.ls[['val']]))
# plot((hs.ele.noveg.ls[['prob']]))
# 
# 
# # 
# lai.ls <- readRDS('data/met/future/BNU-ESM/rcp85_long/rcp85_20852100_lai_jan_5km.rds')
# lai.ls.hist <- readRDS('data/met/future/BNU-ESM/histrory/history_20002015_lai_jan_5km.rds')
# 
# # plot(lai.ls)
# 
# c.h <- readRDS('data/met/future/ACCESS1-0/history/_height_ns.rds')
# brks <- seq(4,18,by=2)
# brks <- seq(20,34,by=1)
# colours <- colorRampPalette(c("grey","red"))
# plot(exp(c.h[['val']]),breaks=brks,col=colours(length(brks)-1))
# 
# # brks <- 0.5:5.5
# # plot(hs.ele.ls[['val']],breaks=brks,col=col.df$cityNight[c(1,2,5,4,3)],lab.breaks=0:5)
# # 
# # colours <- colorRampPalette(c("grey","red"))
# # plot(prob.ra,col=colours(20))

# $##########
# plot score 
plot.map.func <- function(pred.fn,out.title,add.Borad=FALSE,plot.height=FALSE){
        dir.create('figures/future')
        
        dir.names <- list.dirs('data/met/future/',recursive = F,full.names =T)
        for (i in seq_along(dir.names)) {
                targ.path <- file.path('figures/future',dir.names[i])
                if(!dir.exists(targ.path)){
                        dir.create(targ.path)            
                }
                
        }
        
        # out.title <- 'Elevated Fule RCP4.5 204502060'
        # pred.fn <- 'data/met/future/BNU-ESM/rcp45_mid/_hz_elevated.rds'
        # base.nm <- basename(pred.fn)
        out.val.fn <- gsub(pattern = '.rds',replacement = '.png',x = pred.fn)
        out.prob.fn <- gsub(pattern = '.rds',replacement = '_prob.png',x = pred.fn)
        
        out.val.fn <- gsub(pattern = 'data/met/future/',
                           replacement = 'figures/future/',x = out.val.fn)
        dir.create(dirname(out.val.fn))
        out.prob.fn <- gsub(pattern = 'data/met/future/',
                            replacement = 'figures/future/',x = out.prob.fn)
        # 
        hs.ele.ls <- readRDS(pred.fn)
        print(out.val.fn)
        
        # plot val
        png(out.val.fn,width = 500,height = 500*0.77)
        if(plot.height){
                
                target.ra <- exp(hs.ele.ls[['val']])
                # brk.vec <- seq(max(c(0,round(cellStats(target.ra, range)[1],digits = -1)-10)),
                #                round(cellStats(target.ra, range)[2],digits = -1)+10,
                #                length.out = 5)
                if(cellStats(target.ra, max)>25){
                        brk.vec <- seq(15,40,by=5)  
                }else{
                        brk.vec <- seq(0,25,by=5)   
                }
                
                print(cellStats(target.ra, range))  
                at.val <- head(brk.vec, -1)
                lab.val <- head(brk.vec, -1)
        }else{
                target.ra<- (hs.ele.ls[['val']])
                brk.vec <- seq(0,6,by=1)    
                at.val <- head(brk.vec, -1)+0.5
                lab.val <- head(brk.vec, -1)
        }
        
        col.func <- colorRampPalette(c('darkseagreen','red'))
        plot(target.ra,breaks = brk.vec,col = col.func(length(brk.vec)-1), 
             axis.args=list(at=at.val,
                            labels=lab.val,
                            cex.axis=0.6),
             main = out.title,colNA="lightskyblue")
        
        # mask upper corner
        # upper.corner <- crop(hs.ele.ls[['val']],extent(c(146,150, -36.1, -33)))
        # upper.corner[is.na(upper.corner)] <- 1
        # plot(upper.corner,add=T,col='white',ann=F,axes=F,legend=FALSE)
        # 
        if(add.Borad){
                library(oz)
                vic(add=T,lwd=1,col='black',coast = F)
        }
        
        dev.off()
        # 
        if(!plot.height){
                png(out.prob.fn,width = 500,height = 500*0.77)
                raster.new = stack(hs.ele.ls[["prob"]])
                
                brk.vec <- seq(0.2,.6,by=0.1)
                col.func <- colorRampPalette(c('grey80','black'))
                
                plot(max(raster.new),
                     breaks = brk.vec,col = col.func(4),
                     main = 'Probability',colNA="lightskyblue")
                dev.off()
        }
}

# get file list
fn.vec.ns <- list.files(path = 'data/met/future',pattern = 'ns.rds',recursive = T,full.names = T)
fn.vec.el <- list.files(path = 'data/met/future',pattern = 'elevated.rds',recursive = T,full.names = T)
fn.vec.ca <- list.files(path = 'data/met/future',pattern = 'canopy.rds',recursive = T,full.names = T)
fn.vec.ba <- list.files(path = 'data/met/future',pattern = 'bark.rds',recursive = T,full.names = T)
fn.vec.su <- list.files(path = 'data/met/future',pattern = 'surface.rds',recursive = T,full.names = T)

fn.vec <- c(fn.vec.ns,fn.vec.el,fn.vec.ca,
            fn.vec.ba,fn.vec.su)

fn.vec.sub <- fn.vec[!fn.vec %in% grep('height', fn.vec, value = T)]
# plot.map.func(pred.fn = fn.vec[1],out.title = '')

sapply(fn.vec.sub,plot.map.func,out.title = '')

# plot height
fn.vec.ht <- fn.vec[grep('height', fn.vec)]
sapply(fn.vec.ht,plot.map.func,out.title = '',plot.height = T)

# plot.map.func(pred.fn = fn.vec.ht[105],out.title = '',plot.height = T)
# plot.map.func(pred.fn = fn.vec.ht[1],out.title = '',plot.height = T)
