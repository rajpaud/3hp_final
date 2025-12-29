# requires packages "lhs" and "EnvStats"
# install.packages("lhs")
# install.packages("EnvStats")

library(lhs)
library(EnvStats)

set.seed(20240410)

# setting number of samples
nsmpl <- 10000

# importing the parameters range
par.range <- read.csv("data/par_range.csv", row.names = 1)

fu.pars <- au.pars <- randomLHS(nsmpl, ncol(par.range))

for (j in 1:ncol(par.range)) {
  fu.pars[, j] <- qunif(au.pars[, j], min = par.range["low", j], max = par.range["high", j])
  }

colnames(fu.pars) <- colnames(par.range) 
head(fu.pars)

# save fu.pars to be used in calibration later 


# end