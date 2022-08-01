# check dargo.r first###

sapply('data/met/future/ACCESS1-0/rcp85_long/',
       wrap.predic.func,
       my.fun = predict.rf.cmip.fine.reso.func)

predict.rf.cmip.fine.reso.func(path.nm = 'data/met/future/ACCESS1-0/rcp85_long/',
                               model.path = 'cache/rf.fit.hs.elevated.rds',
                               out.nm = 'access_rcp85_2100_elevated.rds')
# 1
# hz.ele.ra <- readRDS('cache/fineScale/ACCESS1-0/history/_hz_elevated.rds')
# plot(hz.ele.ra[['val']],breaks = 0:5,col=topo.colors(5),
#      axis.args=list(at=0:5,labels=0:5),
#      legend.width=1.5,axes=F,bty="n", box=FALSE)
# axis(side = 2,at =seq(-37.39,-37.33,by=.01),labels =seq(-37.39,-37.33,by=.01))
# axis(side = 1,at =seq(147.27,147.33,by=.01),labels =seq(147.27,147.33,by=.01))
# 
hz.ele.ra.85 <- readRDS('cache/fineScale/ACCESS1-0/history/access_rcp85_2100_elevated.rds')
# plot(hz.ele.ra.85[['val']],breaks = 0:5,col=topo.colors(5),
#      axis.args=list(at=0:5,labels=0:5),
#      legend.width=1.5,axes=F,bty="n", box=FALSE)
# axis(side = 2,at =seq(-37.39,-37.33,by=.01),labels =seq(-37.39,-37.33,by=.01))
# axis(side = 1,at =seq(147.27,147.33,by=.01),labels =seq(147.27,147.33,by=.01))
# 
diff.ra <- hz.ele.ra.85[['val']] - hz.ele.ra[['val']]
# plot(diff.ra,breaks = -1:3,col=topo.colors(5),
#      axis.args=list(at=-1:3,labels= -1:3),
#      legend.width=1.5,axes=F,bty="n", box=FALSE)
# 

pdf('figures/fig8_dargo.pdf',width = 6,height = 5)
par(mfrow=c(2,2),mar=c(3,3,0,0),oma=c(0,0,1,0))
# 
prob.hist.ls <- hz.ele.ra[['prob']]
high.hist.ra <- prob.hist.ls[[4]] + prob.hist.ls[[5]]
plot(high.hist.ra,
     breaks = seq(0.1,0.7,by=0.05),col=topo.colors(12),
     axis.args=list(at=seq(0.1,0.6,by=0.2),labels= seq(0.1,0.6,by=0.2)),
     legend.width=1.5,axes=F,bty="n", box=FALSE)   
axis(side = 2,at =seq(-37.39,-37.33,by=.01),labels =seq(-37.39,-37.33,by=.01))
axis(side = 1,at =seq(147.27,147.33,by=.01),labels =seq(147.27,147.33,by=.01))
legend('topleft',legend = '(a)',bty='n')
# 
prob.85.ls <- hz.ele.ra.85[['prob']]
high.85.ra <- prob.85.ls[[4]] + prob.85.ls[[5]]
plot(high.85.ra,
     breaks = seq(0.1,0.7,by=0.05),col=topo.colors(12),
     axis.args=list(at=seq(0.3,0.7,by=0.2),labels= seq(0.3,0.7,by=0.2)),
     legend.width=1.5,axes=F,bty="n", box=FALSE)   
axis(side = 2,at =seq(-37.39,-37.33,by=.01),labels =seq(-37.39,-37.33,by=.01))
axis(side = 1,at =seq(147.27,147.33,by=.01),labels =seq(147.27,147.33,by=.01))
legend('topleft',legend = '(b)',bty='n')
# 
plot(high.85.ra - high.hist.ra,
     breaks = seq(0,0.35,by=0.01),col=rev(heat.colors(37)),
     axis.args=list(at=seq(0,0.3,by=0.1),labels= seq(0,0.3,by=0.1)),
     legend.width=1.5,axes=F,bty="n", box=FALSE)

axis(side = 2,at =seq(-37.39,-37.33,by=.01),labels =seq(-37.39,-37.33,by=.01))
axis(side = 1,at =seq(147.27,147.33,by=.01),labels =seq(147.27,147.33,by=.01))
legend('topleft',legend = '(c)',bty='n')
# 
plot(rad.jan,axes=F,bty="n", box=FALSE)
axis(side = 2,at =seq(-37.39,-37.33,by=.01),labels =seq(-37.39,-37.33,by=.01))
axis(side = 1,at =seq(147.27,147.33,by=.01),labels =seq(147.27,147.33,by=.01))
legend('topleft',legend = '(d)',bty='n')
dev.off()