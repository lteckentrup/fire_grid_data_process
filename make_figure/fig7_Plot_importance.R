rf.fit.ft <- readRDS('cache/rf.fit.fuelType.rds')
rf.fit.canopy.h <- readRDS('cache/rf.fit.canopy.height.rds')
varImpPlot(rf.fit.canopy.h,type=1)
rf.fit.ns.h <- readRDS('cache/rf.fit.ns.height.rds')
# varImpPlot(rf.fit.ns.h)
# 
# model.path <- ('cache/rf.fit.hz.bark.rds')
rf.fit.ns.hz <- readRDS('cache/rf.fit.hz.ns.rds')
rf.fit.elevated.hz <- readRDS('cache/rf.fit.hs.elevated.rds')
# rf.fit.elevated.hz$confusion
rf.fit.bark.hz <- readRDS('cache/rf.fit.hz.bark.rds')
# rf.fit.bark.hz$confusion
rf.fit.surface.hz <- readRDS('cache/rf.fit.hz.surface.rds')

# 
varImpPlot <- function (x, sort = TRUE, n.var = min(30, nrow(x$importance)), 
          type = NULL, class = NULL, scale = TRUE, main = deparse(substitute(x)), 
          ...) 
{
  if (!inherits(x, "randomForest")) 
    stop("This function only works for objects of class `randomForest'")
  imp <- importance(x, class = class, scale = scale, type = type, 
                    ...)
  if (ncol(imp) > 2) 
    imp <- imp[, -(1:(ncol(imp) - 2))]
  nmeas <- ncol(imp)
  if (nmeas > 1) {
    op <- par(mfrow = c(1, 2), mar = c(4, 5, 4, 1), mgp = c(2, 
                                                            0.8, 0), oma = c(0, 0, 2, 0), no.readonly = TRUE)
    on.exit(par(op))
  }
  for (i in 1:nmeas) {
    ord <- if (sort) 
      rev(order(imp[, i], decreasing = TRUE)[1:n.var])
    else 1:n.var
    xmin <- if (colnames(imp)[i] %in% c("IncNodePurity", 
                                        "MeanDecreaseGini")) 
      0
    else min(imp[ord, i])
    dotchart(imp[ord, i], ylab = "", 
             main = if (nmeas == 1) 
               main
             else NULL, xlim = c(xmin, max(imp[, i])), ...)
  }
  if (nmeas > 1) 
    mtext(outer = TRUE, side = 3, text = main, cex = 1.2)
  invisible(imp)
}

get.import.order.func <- function(fit.nm){
  x <- as.data.frame(fit.nm$importance)
  x <- x[order(x[,ncol(x)],decreasing = T),]
  x.2 <- x[order(x[,ncol(x)-1],decreasing = T),]
  return(data.frame(gini = row.names(x),
                    acu = row.names(x.2)))
}

plot.importance.func <- function(rf.fit.ft,
                                 l.out.1,
                                 l.out.2){
  # readdata
  x <- get.import.order.func(rf.fit.ft)
  
  # change names
  x[x == 'pr.seaonality'] <- 'Rainfall seaonality'
  x[x == 'map'] <- 'MAP'
  x[x == 'tmax.mean'] <- 'Mean Tmax'
  x[x == 'rh.min'] <- 'Minimum RH'
  x[x == 'tmax'] <- 'Tmax'
  x[x == 'rain'] <- 'Precipitation'
  x[x == 'rad.short.jan'] <- 'Solar radiation (Jan)'
  x[x == 'soil.density'] <- 'Soil bulk density'
  x[x == 'rad.short.jul'] <- 'Solar radiation (Jul)'
  x[x == 'lai.opt.mean'] <- 'Optimal LAI'
  x[x == 'curvature_plan'] <- 'Plan curvature'
  x[x == 'curvature_profile'] <- 'Profile curvature'
  x[x == 'wi'] <- 'Wetness index'
  x[x == 'clay'] <- '% clay'
  # plot
  varImpPlot(rf.fit.ft,main = '',pch=16,labels  = rev(x[,1]),type=1,
             xlab='Mean decrease in accuracy')
  legend('topleft',legend = l.out.1,bty='n')
  varImpPlot(rf.fit.ft,main = '',pch=16,labels  = rev(x[,2]),type=2,
             xlab='Mean decrease in Gini Index')
  legend('topleft',legend = l.out.2,bty='n')
}

pdf('figures/importance.pdf',width = 3*2,height = 3*3)
par(mfrow=c(3,2),mar=c(5,5,1,1))
# # 1
# plot.importance.func(rf.fit.ft = rf.fit.ft,
#                      l.out.1 = '(a) Fuel type',
#                      l.out.2 = "(b)")
# # 2
# plot.importance.func(rf.fit.ft = rf.fit.canopy.h,
#                      l.out.1 = '(c) Elevated height',
#                      l.out.2 = "(d)")

# # 3
# plot.importance.func(rf.fit.ft = rf.fit.ns.h,
#                      l.out.1 = '(e) Near surface height',
#                      l.out.2 = "(f)")

# 4
plot.importance.func(rf.fit.ft = rf.fit.elevated.hz,
                     l.out.1 = '(a) Elevated',
                     l.out.2 = "(b)")

# 5
plot.importance.func(rf.fit.ft = rf.fit.ns.hz,
                     l.out.1 = '(c) Near surface',
                     l.out.2 = "(d)")

# # 6
# plot.importance.func(rf.fit.ft = rf.fit.bark.hz,
#                      l.out.1 = '(k) Bark hazard score',
#                      l.out.2 = "(l)")

# 7
plot.importance.func(rf.fit.ft = rf.fit.bark.hz,
                     l.out.1 = '(e) Surface',
                     l.out.2 = "(f)")
dev.off()
