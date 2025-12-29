# Requires package "deSolve". 
# install("deSolve")

library(deSolve)

# Set up initial parameters and state sizes

beta <- 20 #contact rate (transmission coefficient)
beta_dec <- 0 #rate of decline in transmission
omega <- 0.25 #rate of progression from EL to LL #stabilization
sigma1 <- 0.042 #rate of progression from EL to ATB #early progression
sigma2 <- 0.0025 #rate of progression from LL to ATB  #Late progression
gamma <- 1.5 #rate of progression from ATB to STB #taken 10 months 
rho1 <- 1.4 #recovery rate #taken 6 months #could be larger
phi <- 0.1 #protection against reinfection
r1 <- 1.4 #regression from STB to ATB 
LE <- 75 #Life expectancy in years
b <- 0.025
mu <- 1/LE #death rate
mu.atb <- 0.02 #death rate (due to ATB) #could be smaller - 
mu.stb <- 0.2 #death rate (due to STB)
rho2 <- 0.3 #self recovery from ATB to Recovered #spontaneous resolution
w1 <- 0.018 #self-clearance 
w2 <- 0.028 #life-time self-clearance rate 


# New vector with all the parameters
parameters <- c(beta = beta,
                beta_dec = beta_dec,
                omega = omega,
                sigma1 = sigma1,
                sigma2 = sigma2,
                gamma = gamma,
                rho1 = rho1,
                phi = phi,
                r1 = r1,
                LE = LE,
                b = b,
                mu = mu,
                mu.atb = mu.atb,
                mu.stb = mu.stb,
                rho2 = rho2,
                w1 = w1,
                w2 = w2)


# state sizes

i_s <- 100000 - 100
i_el <- 0
i_ll <- 0
i_atb <- 0
i_stb <- 100
i_r <- 0

ini_val <- c(S = i_s,
             EL = i_el,
             LL = i_ll,
             ATB = i_atb,
             STB = i_stb,
             R = i_r,
             inc = 0, #incidence
             rec = 0, #recent transmission
             rem = 0, #remote transmission
             deaths = 0) #total deaths due to TB
             
ystart <- ini_val

# time
times <- seq(0, 200, by = 1)

# developing the model function

ltbi_model <- function (t, y, parms) {
  with(as.list(c(y, parms)), {
    
    # total     
    N <- S + EL + LL + ATB + STB + R
    
    lambda <- beta * exp(-t * beta_dec) * (ATB + STB)/N
    phi0 <- phi * lambda
    
    # movement around the compartments
    
    # susceptible
    dS <- b * N - lambda * S - mu * S
    # early latent
    dEL <- lambda * S + phi0 * LL + phi0 * R - omega * EL - sigma1 * EL - w1 * EL - mu * EL 
    # late latent
    dLL <- omega * EL - sigma2 * LL - phi0 * LL - w2 * LL - mu * LL 
    # asymptomatic active TB
    dATB <- sigma2 * LL + sigma1 * EL + r1 * STB - gamma * ATB - rho2 * ATB - mu.atb * ATB - mu * ATB
    # symptomatic active TB
    dSTB <- gamma * ATB - r1 * STB - mu.stb * STB - rho1 * STB - mu * STB
    # recovered
    dR <- rho1 * STB + rho2 * ATB + w1 * EL + w2 * LL - phi0 * R - mu * R 
    # infected
    dinc <- sigma2 * LL + sigma1 * EL
    # recent transmission
    drec <- sigma1 * EL
    # remote transmission
    drem <- sigma2 * LL
    # total deaths due to TB
    dtd <- mu.atb * ATB + mu.stb * STB
    
    # returning all the outputs
    return(list(c(dS, dEL, dLL, dATB, dSTB, dR, dinc, drec, drem, dtd)))
  })
}

# model

ltbi_model_ode <- function (h, tmax, parms, ystart){
  sol <- as.data.frame(
    lsoda(
      ystart,
      times=seq(0,tmax,by=h),
      func=ltbi_model,
      parms=parms,
      rtol = 1e-8,
      atol = 1e-8
    )
  )  
}

###############################

# mode for scale_up
# "tint "is intervention time for scale-up

scale_up_model <- function (t, y, parms) {
  with(as.list(c(y, parms)), {
    
    #total     
    N <- S + EL + LL + ATB + STB + R
    
    lambda <- beta * exp(-t * beta_dec) * (ATB + STB)/N
    phi0 <- phi * lambda

    # limiting movement from EL to R for the first 5 years only
    tau_current <- ifelse(t <= tint, tau, 0)
    
    # also limiting active case finding for the first 5 years only
    kappa_current <- ifelse(t <= tint, kappa, 0)
    
    # movement around the compartments
    
    # susceptible
    dS <- b * N - lambda * S - mu * S
    # early latent
    dEL <- lambda * S + phi0 * LL + phi0 * R - omega * EL - sigma1 * EL - w1 * EL - mu * EL - tau_current * propEL * EL
    # late latent
    dLL <- omega * EL - sigma2 * LL - phi0 * LL - w2 * LL - mu * LL - tau_current * (1-propEL) * LL
    # asymptomatic active TB
    dATB <- sigma2 * LL + sigma1 * EL + r1 * STB - gamma * ATB - rho2 * ATB - mu.atb * ATB - mu * ATB - kappa_current * propATB * ATB 
    # symptomatic active TB
    dSTB <- gamma * ATB - r1 * STB - mu.stb * STB - rho1 * STB - mu * STB - kappa_current * (1 - propATB) * STB
    # recovered
    dR <- rho1 * STB + rho2 * ATB + w1 * EL + w2 * LL - phi0 * R - mu * R + tau_current * propEL * EL + tau_current * (1-propEL) * LL + kappa_current * (1 - propATB) * STB + kappa_current * propATB * ATB 
    # infected
    dinc <- sigma2 * LL + sigma1 * EL
    # recent transmission
    drec <- sigma1 * EL
    # remote transmission
    drem <- sigma2 * LL
    # total deaths due to TB
    dtd <- mu.atb * ATB + mu.stb * STB
    # number EL to R
    dELR <- tau_current * propEL * EL
    # number LL to R
    dLLR <- tau_current * (1-propEL) * LL
    
    # returning all the outputs
    return(list(c(dS, dEL, dLL, dATB, dSTB, dR, dinc, drec, drem, dtd, dELR, dLLR)))
  })
}


scale_up_model_ode <- function (h, tmax, parms, ystart){
  sol <- as.data.frame(
    lsoda(
      ystart,
      times=seq(0,tmax,by=h),
      func=scale_up_model,
      parms=parms,
      rtol = 1e-8,
      atol = 1e-8
    )
  )  
}


# end