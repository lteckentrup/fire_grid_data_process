source('r/get_vic_shape.r')
library(wesanderson)
col.vec <- c(wes_palette("GrandBudapest1", n = 4)[c(3,4,2)],
             'grey80','grey70',
             wes_palette("Zissou1", n = 4)[1:3])
library(raster)
source('r/functions_plot.R')
####plot different scores
# png('figures/future_prob_hz_rcp85_2100.png',width = 300 * 2*2,200*4*2)
pdf('figures/fig4_future_prob_hz_rcp85_2100.pdf',width = 4*2,height=3*2)
# par(cex=10)
par(mfrow=c(2,2))
par(oma=c(1,1,1,1),mar=c(4,3,1,0))
plot.prob.future.change.func(par.name = '_hz_elevated.rds',
                             l.in=c('(a) Elevated fuel score',
                                    '(b) Elevated fuel score'),
                             if.x.axis = T)


# lab.in = seq(-0.4,0.4,by=0.1)
# col.in = c('navy','darkgreen','palegreen3','lightgreen',
#            # 'grey',
#            'burlywood','coral','darkorange4','red')
par(new=T)
par(mfrow=c(1,1))
plot(0,pch=NA,ann=F,axes=F,xlab='lon',ylab='lat')
legend(x = 0.6,y=1.25,
       legend = c('-0.2 - 0.1','-0.1 - 0','0 - 0.1',
                        '0.1 - 0.2','0.2 - 0.3',
                        '0.3 - 0.4'),
       col = col.vec[3:8],
       pch=15,bty='n',horiz = T,xpd=T)
# plot.prob.future.change.func(par.name = '_hz_bark.rds',
#                              l.in=c('(c) Bark fuel score',
#                                     '(d) Bark fuel score'),
#                              if.x.axis = F)

######
par(mfrow=c(2,2))
par(oma=c(1,1,1,1),mar=c(4,3,1,0))

plot.prob.future.change.func(par.name = '_hz_ns.rds',
                             l.in=c('(e) Near surface fuel score',
                                    '(f) Near surface fuel score'),
                             if.x.axis = T)
par(new=T)
par(mfrow=c(1,1))
plot(0,pch=NA,ann=F,axes=F)
legend(x = 0.6,y=1.25,
       legend = c('-0.2 - 0.1','-0.1 - 0','0 - 0.1',
                  '0.1 - 0.2'),
       col = col.vec[3:8],
       pch=15,bty='n',horiz = T,xpd=T)
##########
par(mfrow=c(2,2))
par(oma=c(1,1,1,1),mar=c(4,3,1,0))
plot.prob.future.change.func(par.name = '_hz_surface.rds',
                             l.in=c('(g) Surface fuel score',
                                    '(h) Surface fuel score'),
                             if.x.axis = T)
par(new=T)
par(mfrow=c(1,1))
plot(0,pch=NA,ann=F,axes=F)
legend(x = 0.6,y=1.25,
       legend = c('-0.2 - 0.1','-0.1 - 0','0 - 0.1'),
       col = col.vec[3:8],
       pch=15,bty='n',horiz = T,xpd=T)
dev.off()

