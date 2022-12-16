library(randomForest)

# Read the random forest model object
rf.fit.ft <- readRDS('../cache/rf.fit.fuelType.new.rds')

# Define a function to plot predictor importance of RF model
plot.importance.func <- function(rf.fit.ft) {

  # Grab importance metrics
  imp <- importance(rf.fit.ft, scale = TRUE)

  # Select last two columns which contain importance metrics
  imp <- imp[, -(1:(ncol(imp) - 2))]

  # Change names for metrics
  colnames(imp) <- c("Gini Importance", "Accuracy Importance")

  # Change predictor names
  row.names(imp) <- c('Rainfall seaonality', 'MAP', 'Mean Tmax', 'Minimum RH',
                        'Tmax', 'Precipitation', 'Solar radiation (Jan)',
                        'Soil bulk density', 'Solar radiation (Jul)',
                        'Optimal LAI', 'Plan curvature', 'Profile curvature',
                        'Wetness index', '% clay')[1:nrow(imp)]

  # Plot the importance of each variable; sort values
  varImpPlot(rf.fit.ft, main = paste(colnames(imp)[1], "vs", colnames(imp)[2]),
             labels = rev(row.names(imp)), sort = TRUE)
}

# Call function to plot importance
plot.importance.func(rf.fit.ft)

dev.off()
