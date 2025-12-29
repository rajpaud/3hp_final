# here a hypothetical scale up scenario described in the manuscript is simulated
# requires packates "deSolve" and "tidyverse"
# install.packages("deSolve")
# install.packages("tidyverse")

library(deSolve)
library(tidyverse)

source("scripts/01_model_setup")

# load results selected from calibration for each districts (demonstrated only for chitwan here)
ch_select_result <- 

# load selected parameters for chitwan
select_pars <- 
select_pars <- select_pars[,3:19]

cureProbability3HP <- 0.81

index <- 1
scale_up_int <- vector('list', nrow(select_pars))

cascade <- read.csv("cascade_for_model.csv", sep = ",", header = TRUE, row.names = 1)

# for chitwan
cascade <- cascade[,1] # [,2] for pyuthan

indicators <- function(x) {
  x %>%
    mutate(
      t_pop = S + EL + LL + ATB + STB + R,
      t_cases = ATB + STB,
      prev = t_cases/t_pop * 100000,
      new_cases = c(0, diff(inc)),
      incidence = (new_cases/t_pop * 100000)*nt,
      t_deaths = c(0, diff(deaths)),
      mortality = (t_deaths/t_pop * 100000)*nt,
      ltbi_prev = (EL + LL) / t_pop * 100,
      recent = c(0, diff(rec)),
      remote = c(0, diff(rem)),
      prop_recent = recent/(recent + remote) * 100,
      StoAratio = STB/ATB
    )
}

efficacy3hp <- 0.91
txsuccess <- 0.92
set.seed(12345)

nns <- runif(min = 30, max = 60, n = length(ch_select_result)) #number needed to screen

tic()
for(js in 1:length(ch_select_result)) {
  
  #updating parameters
  parameters[1] <- as.numeric(select_pars[js,'beta'])
  parameters[2] <- as.numeric(select_pars[js,'beta_dec'])
  parameters[3] <- as.numeric(select_pars[js,'omega'])
  parameters[4] <- as.numeric(select_pars[js,'sigma1'])
  parameters[5] <- as.numeric(select_pars[js,'sigma2'])
  parameters[6] <- as.numeric(select_pars[js,'gamma'])
  parameters[7] <- as.numeric(select_pars[js,'rho1'])
  parameters[8] <- as.numeric(select_pars[js,'phi'])
  parameters[9] <- as.numeric(select_pars[js,'r1'])
  parameters[10] <- as.numeric(select_pars[js,'LE'])
  parameters[11] <- as.numeric(select_pars[js,'b'])
  parameters[12] <- as.numeric(select_pars[js,'mu'])
  parameters[13] <- as.numeric(select_pars[js,'mu.atb'])
  parameters[14] <- as.numeric(select_pars[js,'mu.stb'])
  parameters[15] <- as.numeric(select_pars[js,'rho2'])
  parameters[16] <- as.numeric(select_pars[js, 'w1'])
  parameters[17] <- as.numeric(select_pars[js, 'w2'])
  
  nt <- 10
  h <- 1/nt
  tmax <- 22
  ll <- (tmax)/h + 1
  tint <- 5 # 5 years long intervention as scale up
  
  y <- ch_select_result[[js]][1:11]
  
  z <- zstart <- as.numeric(c((y[ll,2:7]),0,0,0,0,0,0))
  
  names(z) <- names(zstart) <- c(names(y[c(2:11)]), "ELR", "LLR")
  
  pop <- sum(z[1:6])

  propEL <- 0.8 # proportion of EL cases in the population
  propATB <- 0.6
  # adding the intervention effect
  # all of the household contacts are identified and treated 
  ntreated <- prod(cascade) * efficacy3hp * pop / 100000

  parameters[18] <- ntreated / (y[ll,3] * propEL + y[ll,4] * (1 - propEL)) 

  nacf <- prod(cascade[1:2])/nns[js] * txsuccess * pop / 100000
  parameters[19] <- nacf/(y[ll,5] * propATB + y[ll,6] * (1 - propATB)) 
 
  parameters[20] <- nns[js]

  names(parameters)[18:20] <- c("tau", "kappa", "nns")
 
  ystart_int <- z
  
  # running 20  years
  tmax <- 20
  
  parameters[1] <- parameters[1] * exp(parameters[2] * (-22))
  
  y_int <- scale_up_model_ode(h = h, tmax = tmax, parms = parameters, ystart = ystart_int)
  
  y_int <- indicators(y_int)
  y <- indicators(y)

  y <- y %>%
    add_column(ELR = rep(0, nrow(y)), .after = "R") %>%
    add_column(LLR = rep(0, nrow(y)), .after = "ELR")

  y_int <- rbind(y, y_int[-1,])

  
  
  scale_up_int[[index]] <- list(y = y_int, ystart = ystart_int, pars = parameters)
  
  index <- index + 1
  
}

# save scale_up_int and use it later to calculate the cases and deaths averted and for plots.
# repeat the same for pyuthan
# repeat the same for no intervention scenario, i.e., ntreated = 0, nacf = 0 and save as scale_up_noint

# end