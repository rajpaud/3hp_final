# requires packages "deSolve"
# install.packages("deSolve")

# library(deSolve)
# library(dplyr)

# importing the model
source("scripts/01_model_setup.R")

# importing the parameter sets

sim.pars <- # import fu.pars from parameters sampling here 
base_par <- parameters

# a null list to store results
results <- vector('list', nrow(sim.pars))

index <- 1

# to add the indicators into the final list

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
  
for (js in 1:nrow(sim.pars)) {
  parameters <- base_par
  parameters[1] <- as.numeric(sim.pars[js,'beta'])
  #parameters[2] <- as.numeric(sim.pars[js,'beta_dec'])      
  parameters[3] <- as.numeric(sim.pars[js,'omega'])
  parameters[4] <- as.numeric(sim.pars[js,'sigma1'])
  parameters[5] <- as.numeric(sim.pars[js,'sigma2'])
  parameters[6] <- as.numeric(sim.pars[js,'gamma'])
  parameters[7] <- as.numeric(sim.pars[js,'rho1'])
  parameters[8] <- as.numeric(sim.pars[js,'phi'])
  parameters[9] <- as.numeric(sim.pars[js,'r1'])
  parameters[10] <- as.numeric(sim.pars[js,'LE'])
  parameters[11] <- as.numeric(sim.pars[js,'b'])
  parameters[12] <- as.numeric(sim.pars[js,'mu'])
  parameters[13] <- as.numeric(sim.pars[js,'mu.ATB'])
  parameters[14] <- as.numeric(sim.pars[js,'mu.STB'])
  parameters[15] <- as.numeric(sim.pars[js,'rho2'])
  parameters[16] <- as.numeric(sim.pars[js, 'w1'])
  parameters[17] <- as.numeric(sim.pars[js, 'w2'])
  
  # running the transience
  
  nt <- 5 
  h <- 1/nt 
  tmax <- 100 # maximum simulation time
  ll <- tmax/h + 1 
  
  # using the model
  ytrans <- ltbi_model_ode(h = h, tmax = tmax, parms = parameters, ystart = ystart)
  
  Ntrans <- rowSums(ytrans[ll, 2:7])
  
  # running for final 22 years from 2000 to 2022
  
  nt <- 10
  h <- 1/nt
  tmax <- 22
  
  # changing beta_dec now
  
  parameters[2] <- as.numeric(sim.pars[js, 'beta_dec'])
    
  ystart.cont <- as.numeric(c(ytrans[ll,2:7], 0, 0, 0, 0))
  
  names(ystart.cont) <- names(ystart)
  
  calibration_output <- ltbi_model_ode(h = h,
                               tmax = tmax,
                               parms = parameters,
                               ystart = ystart.cont)
  
  calibration_output <- indicators(calibration_output)
  
  annual_decline = -1/10 * log(calibration_output$incidence[calibration_output$time == 22]/calibration_output$incidence[calibration_output$time == 12])
  
  results[[index]] <- list(y = calibration_output, r = annual_decline, ystart = ystart.cont, parameters = parameters)
  
  index = index + 1
}

# save "results"

# selecting only the parameters at year 2022

cal_output <- list()

for (i in seq_along(results)){
  
  cal_y <- (tail(results[[i]]$y, 1))
  cal_parms <- t(data.frame((results[[i]]$parameters)))
  cal_r <- results[[i]]$r
  cal_set <- cbind(cal_parms, cal_y, cal_r)
  
  cal_output[[i]] <- c(i, cal_set)
}

final_cal <- do.call(rbind, cal_output)

# save "final_cal"

# end