# library(dplyr)
# library(tictoc)

# import model
# import selected chitwan and pyuthan results
# import selected parameters as "select_pars[,3:19]"
# the following script produces results only for chitwan with both 3HP with ACF as intervention

cureProbability3HP <- 0.81

index <- 1
int_both_results <- vector('list', nrow(select_pars))

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

# adding new parameters for intervention, i.e., proportion of early latent, proportion of asymptomatic active TB cases and yield of household contact tracing.

set.seed(123)
propEL <- runif(n = length(ch_select_result), min = 0.5, max = 00.8)
propATB <- runif(n = length(ch_select_result), min = 0.5, max = 0.8)
yield_hh <- runif(n = length(ch_select_result), min = 0.02, max = 0.06)

parameters <- c(parameters, 0,0,0)

names(parameters)[18:20] <- c("propEL", "propATB", "yield_hh")

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
  parameters[18] <- as.numeric(propEL[js]) 
  parameters[19] <- as.numeric(propATB[js])
  parameters[20] <- as.numeric(yield_hh[js])

  nt <- 10
  h <- 1/nt
  tmax <- 22
  ll <- (tmax)/h + 1
  
  y <- ch_select_result[[js]][1:11]
  
  z <- zstart <- as.numeric(c((y[ll,2:7]),0,0,0,0))
  
  names(z) <- names(zstart) <- names(y[c(2:11)])
  
  pop <- sum(z[1:6])
  pop_chitwan <- 719859 # 232019 for pyuthan
  
  # information from LTBI project cascade
  txchitwan <- 307 # 168 for pyuthan # 0 for no intervention
  
  numberTreatmentCompleted = txchitwan/pop_chitwan * pop
 
  # calculating proportions moved to cured
  
  # c(AA,SA) to R
  
  contactschitwan <- 1000 # 711 for pyuthan # 0 for no intervention
  contacts = contactschitwan/pop_chitwan * pop
  
  treatment_success <- 0.91

  numberAtoR <- contacts * parameters[20] * treatment_success
  
  # c(EL,LL) to R
  numberLtoR <- numberTreatmentCompleted * cureProbability3HP 
  
  # now moving these proportions to recovered
  z["ATB"] <- z["ATB"] - numberAtoR * (parameters[19])
  z["STB"] <- z["STB"] - numberAtoR * (1 - parameters[19])
  z["R"] <- z["R"] + numberAtoR
  
  # now moving individuals from EL and LL to R
  z["EL"] <- z["EL"] - numberLtoR * parameters[18]  
  z["LL"] <- z["LL"] - numberLtoR * (1 - parameters[18])       
  z["R"] <- z["R"] + numberLtoR
  
  ystart_int <- z
  
  # running 20  years
  tmax <- 20
  
  parameters[1] <- parameters[1] * exp(parameters[2] * (-22))
  
  y_int <- ltbi_model_ode(h = h, tmax = tmax, parms = parameters, ystart = ystart_int)
  
  y_int <- indicators(y_int)
  y <- indicators(y)

  y_int <- rbind(y, y_int[-1,])
  
  int_both_results[[index]] <- list(y = y_int, ystart = ystart_int, pars = parameters, trans = c(numberAtoR, numberLtoR))
  
  index <- index + 1
  
}

# save int_both_results
# run a no intervention scenario with txchitwan = 0, and save results as no_intervention

z_noint <- lapply(no_int_results, function (x) x$y)
z_both_int <- lapply(int_both_results, function (x) x$y)

trans_both_int <- lapply(int_both_results, function(x) x$trans)
trans_both_df <- do.call(rbind, lapply(trans_both_int, function(y) as.data.frame(t(y))))

h_trans_both <- trans_both_df / sapply(z_both_int, function(x) x$t_pop) * 719859

colnames(ch_trans_both) <- c("AtoR", "LtoR")

quantile(ch_trans_both[,1], probs = c(0.025, 0.5, 0.975))

# summarizing results

# cumulative cases cases

cum_cases_noint <- sapply(z_noint, function(x) ((x$new_cases[-(1:221)]/x$t_pop[-(1:221)])*719859))
cum_cases_int <- sapply(z_both_int, function (x) ((x$new_cases[-(1:221)]/x$t_pop[-(1:221)])*719859))

# cases averted
cases_averted_both <- cum_cases_noint - cum_cases_int
cum_cases_averted_both <- apply(cases_averted_both,2,cumsum)
m_cca_both <- apply(cum_cases_averted_both, 1, median)
low_95_cca_both <- apply(cum_cases_averted_both, 1, function(x) quantile(x, 0.025))
up_95_cca_both <- apply(cum_cases_averted_both, 1, function(x) quantile(x, 0.975))

# deaths
cum_deaths_noint <- sapply(z_noint, function(x) ((x$t_deaths[-(1:221)]/x$t_pop[-(1:221)])*719859))
cum_deaths_int <- sapply(z_both_int, function (x) ((x$t_deaths[-(1:221)]/x$t_pop[-(1:221)])*719859))

# deaths averted
deaths_averted_both <- cum_deaths_noint - cum_deaths_int
cum_deaths_averted_both <- apply(deaths_averted_both, 2, cumsum)
m_cda_both <- apply(cum_deaths_averted_both, 1, median)
low_95_cda_both <- apply(cum_deaths_averted_both, 1, function(x) quantile(x, 0.025))
up_95_cda_both <- apply(cum_deaths_averted_both, 1, function(x) quantile(x, 0.975))

# save all these summaries 
# repeat this for pyuthan to get the summary

# extract and save year-wise results to be later used for cost-effectiveness analysis
# shown only for chitwan, repeat same process for pyuthan

ncolx <- ncol(cases_averted)

yearwise <- function(x) {
    result <- data.frame(matrix(NA, nrow = 20, ncol = ncolx))
    for(j in 1:ncolx) {
            for (i in seq(10, 200, 10)) {
            result[i/10,j] <- sum(x[(i-9):i,j])
    }
    }
    return(result)
}

ch_yw_ca <- yw_ca <- yearwise(cases_averted) # cases_averted comparing no_intervention and intervention (3HP no ACF)
ch_yw_da <- yw_da <- yearwise(deaths_averted)

ch_yw_ca_both <- yw_ca_both <- yearwise(cases_averted_both) # cases_averted comparing no_intervention and intervention (3HP with ACF)
ch_yw_da_both <- yw_da_both <- yearwise(deaths_averted_both)

m_yw_ca <- apply(yw_ca, 1, median)
lower_yw_ca <- apply(yw_ca, 1, quantile, probs = 0.025)
upper_yw_ca <- apply(yw_ca, 1, quantile, probs = 0.975)

m_yw_da <- apply(yw_da, 1, median)
lower_yw_da <- apply(yw_da, 1, quantile, probs = 0.025)
upper_yw_da <- apply(yw_da, 1, quantile, probs = 0.975)

m_yw_ca_both <- apply(yw_ca_both, 1, median)
lower_yw_ca_both <- apply(yw_ca_both, 1, quantile, probs = 0.025)
upper_yw_ca_both <- apply(yw_ca_both, 1, quantile, probs = 0.975)

m_yw_da_both <- apply(yw_da_both, 1, median)
lower_yw_da_both <- apply(yw_da_both, 1, quantile, probs = 0.025)
upper_yw_da_both <- apply(yw_da_both, 1, quantile, probs = 0.975)

# save all the results for cost-effectiveness analysis later

# plotting the cases and deaths averted

par(mar = c(5,5,5,5))
par(mfcol = c(2,2), font.lab = 2, xpd = T)

# Chitwan
# cases averted plots

plot(range(1,201), range(0,150), type = "n", ylab = "Total Cases averted", xlab = "Year", axes = F, font = 2)

polygon(c(1:200, 200:1), c(ch_up_95_cca, rev(ch_low_95_cca)), col = colt2, border = NA)
polygon(c(1:200, 200:1), c(ch_up_95_cca_both, rev(ch_low_95_cca_both)), col = colt4, border = NA)
lines(ch_m_cca, col = col2, lwd = 2, lty = 1)
lines(ch_m_cca_both, col = col4, lwd = 2, lty = 1)
legend("bottomright", legend = c("Impact of TB Treatment and PT","Impact PT only"), fill = c(col4, col2), lty = 1, lwd = 2, bty = "n")
axis(side = 1, at = c(seq(0,160, by = 40), 201), labels = seq(2022, 2042, by = 4))
axis(side = 2, at = c(0,50,100, 150))
axis(side = 4, at = c(0,50,100,150))

mtext(side = 3, "A", line = 1, adj = -0.05, font = 2)
mtext(side =3, "Chitwan", line = 3, adj = 0.5, font = 2)

# deaths averted plots

plot(range(1, 201), range(0,30), type = "n", ylab = "Total Deaths averted", xlab = "Year", axes = F, font = 2)

polygon(c(1:200, 200:1), c(ch_up_95_cda, rev(ch_low_95_cda)), col = colt1, border = NA)
polygon(c(1:200, 200:1), c(ch_up_95_cda_both, rev(ch_low_95_cda_both)), col = colt5, border = NA)
lines(ch_m_cda, col = col1, lwd = 2, lty = 1)
lines(ch_m_cda_both, col = col5, lwd = 2, lty = 1)
legend("bottomright", legend = c("Impact of TB Treatment and PT","Impact PT only"), fill = c(col5, col1), lty = 1, lwd = 2, bty = "n")
axis(side = 1, at = c(seq(0,160, by = 40), 201), labels = seq(2022, 2042, by = 4))
axis(side = 2, at = c(0,5,10,15,20,25,30))
axis(side = 4, at = c(0,5,10,15,20,25,30))

mtext(side = 3, "C", line = 1, adj = -0.05, font = 2)

# Pyuthan
# cases averted plots

plot(range(1,201), range(0,100), type = "n", ylab = "Total Cases averted", xlab = "Year", axes = F, font = 2)

polygon(c(1:200, 200:1), c(py_up_95_cca, rev(py_low_95_cca)), col = colt2, border = NA)
polygon(c(1:200, 200:1), c(py_up_95_cca_both, rev(py_low_95_cca_both)), col = colt4, border = NA)
lines(py_m_cca, col = col2, lwd = 2, lty = 1)
lines(py_m_cca_both, col = col4, lwd = 2, lty = 1)
legend("bottomright", legend = c("Impact of TB Treatment and PT","Impact PT only"), fill = c(col4, col2), lty = 1, lwd = 2, bty = "n")
axis(side = 1, at = c(seq(0,160, by = 40), 201), labels = seq(2022, 2042, by = 4))
axis(side = 2, at = c(seq(0,100, by = 25)))
axis(side = 4, at = c(seq(0,100, by = 25)))

mtext(side = 3, "B", line = 1, adj = -0.05, font = 2)
mtext(side =3, "Pyuthan", line = 3, adj = 0.5, font = 2)

# deaths averted plots

plot(range(1, 201), range(0,15), type = "n", ylab = "Total Deaths averted", xlab = "Year", axes = F, font = 2)

polygon(c(1:200, 200:1), c(py_up_95_cda, rev(py_low_95_cda)), col = colt1, border = NA)
polygon(c(1:200, 200:1), c(py_up_95_cda_both, rev(py_low_95_cda_both)), col = colt5, border = NA)
lines(py_m_cda, col = col1, lwd = 2, lty = 1)
lines(py_m_cda_both, col = col5, lwd = 2, lty = 1)
legend("bottomright", legend = c("Impact of TB Treatment and PT","Impact PT only"), fill = c(col5, col1), lty = 1, lwd = 2, bty = "n")
axis(side = 1, at = c(seq(0,160, by = 40), 201), labels = seq(2022, 2042, by = 4))
axis(side = 2, at = c(0,5,10,15))
axis(side = 4, at = c(0,5,10,15))

mtext(side = 3, "D", line = 1, adj = -0.05, font = 2)

# end
