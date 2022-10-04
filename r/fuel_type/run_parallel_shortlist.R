# prepare#########
library(raster)
library(randomForest)
library(doParallel)
library(foreach)
# model fit#############
# read inputs
model.input.df <- readRDS('cache/input_access.rds')
# splite inputs by no. of cpu core
model.input.df <- model.input.df[complete.cases(model.input.df),]
sample.d <- model.input.df#[1:1000,]
n.split <- 4
data.ls <- split(sample.d, rep(1:n.split, 
                               length.out = nrow(sample.d), 
                               each = ceiling(nrow(sample.d)/n.split)))
# save memory
rm(model.input.df)
# rm(sample.d)
#  rf model####
# model.rf <- readRDS('cache/rf.fit.fuelType.new.rds')
model.rf <- readRDS('cache/rf.fit.fuelType.new.short.rds')


# set up parallel computing#
cores=n.split#use multicore, set to the number of our cores
cl <- makeCluster(cores[1]) 
registerDoParallel(cl)
# record time used
s.time <- Sys.time()

out.list <- foreach (i=1:cores, .packages = 'randomForest') %dopar% {
  predict(object = model.rf,newdata=data.ls[[i]])
}
stopImplicitCluster()

e.time <- Sys.time()

time.use <- e.time - s.time
print(time.use)
# get out put####
out.list <- lapply(X = out.list,as.character)

rm(model.rf)
rm(cl)
# put input output together
# sample.d <- do.call(rbind,data.ls)
rm(data.ls)
sample.d$pred <- do.call(c,out.list)
# make a raster output #####
spg <- sample.d[,c('x','y','pred')]
spg$pred <- as.numeric(spg$pred)
coordinates(spg) <- ~ x + y
gridded(spg) <- TRUE
out.ra <- raster(spg)
# plot(out.ra)

saveRDS(out.ra,'out.access.short.rds')

# make plots#########
tmp.ft.df <- readRDS('ft.train.evaluation.rds')
ft.df <- tmp.ft.df[,c('ft.new','ft.new.num')]
ft.df <- ft.df[!duplicated(ft.df),]
# 
library(RColorBrewer)

n <- nrow(ft.df)
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector.all = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
col_vector <- sample(col_vector.all, n)
# dev.new()
pdf('test.pdf',width = 15,height = 15)
# 
par(mar=c(3,3,1,1),mfrow=c(2,1))
# 
plot(out.ra,
     # breaks = c(ft.nm.df$ID[1]-0.1,ft.nm.df$ID+0.1),
     col=c(col_vector[ft.df$ft.new.num]),legend=F)
plot(0,pch=NA,ann=F,axes=F)
legend('top',legend = levels(ft.df$ft.new),col = col_vector[ft.df$ft.new.num],
       pch=15,ncol=2,bty='n',xpd=T,cex=0.8)
dev.off()

