# inpuprocess
# read and clean data####
input.df <- readRDS('cache/hs.soil.topo.met.lai.rds')

for (i in 1:ncol(input.df)) {
  input.df[,i] <- replace(input.df[,i],list = which(input.df[,i]< -100),NA)
}

input.df$overall_hz <- as.factor(input.df$overall_hz)
input.df$nearsurface_hz <- as.factor(input.df$nearsurface_hz)
input.df$bark_hz <- as.factor(input.df$bark_hz)
input.df$surface_hz <- as.factor(input.df$surface_hz)
input.df$elevated_hz <- as.factor(input.df$elevated_hz)