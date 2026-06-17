# import yearwise results for both districts
# import the intervention results

# extract the parameters (for chitwan) from intervention results
ch_parms <- do.call(rbind, lapply(int_both_results, function(x) as.data.frame(t(x$pars))))

# import costing parameters
he_params <- read.csv("data/costing_parms.csv")

# import intervention costs
int_cost <- read.csv("data/intervention_costs.csv")

# import costing parameters for chitwan to generate distribution
cea_parms <- read_csv("data/costing_parms_chitwan.csv") # py_cea_parms for Pyuthan

    # generating sample using triangle distribution
        n_samples <- 1067 # number of simulations within calibration range for Chitwan # 884 for Pyuthan
        ea_parms_distb <- matrix(NA, n_samples, nrow(cea_params))

        for (i in 1:nrow(cea_params)) {
            cea_parms_distb[,i] <- rtriangle(n_samples, a = cea_params$low[i], b = cea_params$high[i], c = cea_params$mode[i])
        }

        colnames(cea_parms_distb) <- c("int_cost_per_case", "tx_cost","life_exp","disability_weight","duration_disease")

# to save results later

ch_results_cea <- list()

# For Chitwan

for (i in 1:length(ch_yw_ca_both)) {
    
    ch_results <- ch_yw_ca_both[,i]
   
    # intervention cost

    ch_int_cost <- 39845.6 + he_params$xpert[1] * 650 + he_params$ctb[1] * ch_trans_both$AtoR[i] \

    # undiscounted TB treatment cost (saved)
    ch_undis_tbtxcosts <- ch_yw_ca_both[,i] * he_params$ctb[1]

    # undiscounted dalys
    undis_dalys <- function (cases_av, deaths_av) {
        cases_av * he_params$disweight[1] * he_params$duration[1] + deaths_av * he_params$yll[1]
    }

    ch_undis_dalys <- undis_dalys(ch_yw_ca_both[,i], ch_yw_da_both[,i])


    # undiscounted to discounted
    undis_to_dis <- function(undis_x) {
        undis_x * he_params$discount_factor[1] ^ (1:20)
    }

    # discounted cases and deaths
    ch_ca_both_dis <- undis_to_dis(ch_yw_ca_both[,i])
    ch_da_both_dis <- undis_to_dis(ch_yw_da_both[,i])

    # Discounted TB costs and DALYs
    ch_dis_tbtxcosts <- undis_to_dis(ch_undis_tbtxcosts)
    ch_dis_dalys <- undis_to_dis(ch_undis_dalys)

    # Total costs
    ch_undis_tbtxcosts_total <- sum(ch_undis_tbtxcosts)
    ch_dis_tbtxcosts_total <- sum(ch_dis_tbtxcosts)

    # Total DALYs
    ch_undis_dalys_total <- sum(ch_undis_dalys)
    ch_dis_dalys_total <- sum(ch_dis_dalys)


    # Half-cycle correction
    hc_correction <- function(x) {
        x[1]* 0.5 + sum(x[2:19]) + x[20] * 0.5
    }

    # Total costs with half-cycle correction
    ch_undis_tbtxcosts_total_hc <- hc_correction(ch_undis_tbtxcosts)
    ch_dis_tbtxcosts_total_hc <- hc_correction(ch_dis_tbtxcosts)


    # Total DALYs with half-cycle correction
    ch_undis_dalys_total_hc <- hc_correction(ch_undis_dalys)
    ch_dis_dalys_total_hc <- hc_correction(ch_dis_dalys)


    # Total cases and deaths averted with half-cycle correction
    ch_ca_both_dis_hc <- hc_correction(ch_ca_both_dis) 
    ch_da_both_dis_hc <- hc_correction(ch_da_both_dis)

    # ICER Calculation
    ch_added_cost <- ch_int_cost - ch_dis_tbtxcosts_total_hc

    # CEA results
    cost_per_case_prevented <- ch_added_cost / ch_ca_both_dis_hc
    cost_per_death_prevented <- ch_added_cost / ch_da_both_dis_hc
    cost_per_daly_prevented <- ch_added_cost / ch_dis_dalys_total_hc

        ch_results_cea[[i]] <- list(int_cost = ch_int_cost,
                                    future_cost_saving = ch_dis_tbtxcosts_total_hc,
                                    added_cost = ch_added_cost,
                                    dalys = ch_dis_dalys_total_hc,
                                cost_per_case_prevented = cost_per_case_prevented,
                                cost_per_death_prevented = cost_per_death_prevented,
                                cost_per_daly_prevented = cost_per_daly_prevented
    )
}

# repeat the same for pyuthan (requires updating the intervention cost and parameters available in data folder)

# save ch_results_cea

# generating a df with results corresponding to parameter sets

ch_results_cea_flat <- lapply(ch_results_cea, function(x) {
    cea_results <- x$cea_results
    cost_per_case_prevented <- cea_results["cost_per_case_prevented"]
    cost_per_death_prevented <- cea_results["cost_per_death_prevented"]
    cost_per_daly_prevented <- cea_results["cost_per_daly_prevented"]

    cea_parms <- x$cea_parms
    int_cost_per_case <- as.numeric(cea_parms["int_cost_per_case"])
    tx_cost <- as.numeric(cea_parms["tx_cost"])
    life_exp <- as.numeric(cea_parms["life_exp"])
    disability_weight <- as.numeric(cea_parms["disability_weight"])
    duration_disease <- as.numeric(cea_parms["duration_disease"])

    ch_results <- x$ch_results
    avg_cases_averted <- mean(ch_results$cases_averted)
    avg_deaths_averted <- mean(ch_results$deaths_averted)

    epi_parms <- x$epi_parms
    beta <- epi_parms$beta
    beta_dec <- epi_parms$beta_dec
    omega <- epi_parms$omega
    sigma1 <- epi_parms$sigma1
    sigma2 <- epi_parms$sigma2
    gamma <- epi_parms$gamma
    rho1 <- epi_parms$rho1
    phi <- epi_parms$phi
    r1 <- epi_parms$r1
    LE <- epi_parms$LE
    b <- epi_parms$b
    mu <- epi_parms$mu
    mu.atb <- epi_parms$mu.atb
    mu.stb <- epi_parms$mu.stb
    rho2 <- epi_parms$rho2
    w1 <- epi_parms$w1
    w2 <- epi_parms$w2
    propEL <- epi_parms$propEL
    propATB <- epi_parms$propATB
    yield_hh <- epi_parms$yield_hh

    #saving
    data.frame(
    beta,
    beta_dec,
    omega,
    sigma1,
    sigma2,
    gamma,
    rho1,
    phi,
    r1,
    LE,
    b,
    mu,
    mu.atb,
    mu.stb,
    rho2,
    w1,
    w2,
    propEL,
    propATB,
    yield_hh,
    int_cost_per_case,
    tx_cost,
    life_exp,
    disability_weight,
    duration_disease,
    avg_cases_averted,
    avg_deaths_averted,
    cost_per_case_prevented,
    cost_per_death_prevented,
    cost_per_daly_prevented
  )
})

ch_results_cea_final <- bind_rows(ch_results_cea_flat)

rownames(ch_results_cea_final) <- NULL

# save ch_results_cea_final
# repeat the same for pyuthan

# end
