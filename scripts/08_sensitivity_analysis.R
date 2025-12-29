# here sesitivity analyses described in the manuscript are performed

# import the flattened results from "06_cost_effectiveness.R" as ch_combined
ch_combined <- 

parms <- ch_combined[1:25]

# creating empty lists for all the parametes

upper_10_all <- list()
lower_10_all <- list()

for(i in 1:ncol(parms)) {
    sorted <- sort(parms[[i]], decreasing = TRUE)

    n <- nrow(parms)
    cut_off <- ceiling(0.1 * n)
    upper_10 <- sorted[cut_off]
    lower_10 <- sorted[(n - cut_off + 1)]

    upper_10_all[[paste0("upper10_", names(parms)[i])]] <- upper_10
    lower_10_all[[paste0("lower10_", names(parms)[i])]] <- lower_10
}

list2env(upper_10_all, envir = .GlobalEnv)
list2env(lower_10_all, envir = .GlobalEnv)

# creating empty lists to store results
cost_daly_lower_all <- list()
cost_daly_upper_all <- list()
cost_daly_diff_all <- list()

for (j in 1:ncol(parms)) {
    probs = c(0.025, 0.25, 0.5, 0.75, 0.975)

    # cost per daly
    cost_daly_lower_all[[paste0("cost_daly_lower_", names(parms[j]))]] <- quantile(ch_combined$cost_per_daly[which(ch_combined[j] < lower_10_all[j])], probs = probs)
    cost_daly_upper_all[[paste0("cost_daly_upper_", names(parms[j]))]] <- quantile(ch_combined$cost_per_daly[which(ch_combined[j] > upper_10_all[j])], probs = probs)
    cost_daly_diff_all[[paste0("cost_daly_diff_", names(parms[j]))]] <- abs(cost_daly_upper_all[[j]][3] - cost_daly_lower_all[[j]][3])
}

list2env(cost_daly_lower_all, envir = .GlobalEnv)
list2env(cost_daly_upper_all, envir = .GlobalEnv)
list2env(cost_daly_diff_all, envir = .GlobalEnv)

cost_daly_diff_all <- cbind(cost_daly_diff_beta,
                cost_daly_diff_beta_dec, 
                cost_daly_diff_omega,
                cost_daly_diff_sigma1,
                cost_daly_diff_sigma2,
                cost_daly_diff_gamma,
                cost_daly_diff_rho1,
                cost_daly_diff_phi,
                cost_daly_diff_r1,
                cost_daly_diff_LE,
                cost_daly_diff_b,
                cost_daly_diff_mu,
                cost_daly_diff_mu.atb,
                cost_daly_diff_mu.stb,
                cost_daly_diff_rho2,
                cost_daly_diff_w1,
                cost_daly_diff_w2,
                cost_daly_diff_propEL,
                cost_daly_diff_propATB,
                cost_daly_diff_yield_hh,
                cost_daly_diff_int_cost_per_case,
                cost_daly_diff_tx_cost,
                cost_daly_diff_disability_weight,
                cost_daly_diff_duration_disease,
                cost_daly_diff_life_exp)

colnames(cost_daly_diff_all) <- c("beta", "beta_dec", "omega", "sigma1", "sigma2", "gamma", "rho1", "phi", "r1", "LE", "b", "mu", "mu.atb", "mu.stb", "rho2", "w1", "w2", "propEL", "propATB", "yield_hh", "int_cost_per_case", "tx_cost", "disability_weight", "duration_disease", "life_exp")

# ordering for plot

ch_order <- order(cost_daly_diff_all, decreasing = T, na.last = NA)

# repeat the same for pyuthan

# plotting results

# renaming all to add chitwan prefix

vars <- ls()

selected_vars <- grep("^(cost|upper|lower)", vars, value = TRUE)

for (var in selected_vars) {
    assign(paste0("ch_", var), get(var))
    rm(list = var)
}

# do the same for pyuthan after obtaining results for pyuthan separately and importing it here
vars <- ls()
selected_vars <- grep("^(cost|upper|lower)", vars, value = TRUE)

for(var in selected_vars) {
    assign(paste0("py_", var), get(var))
    rm(list = var)
}

# plots

h <- 350
xx <- -100
yy <- 55
cex.a <- 1.1
l.adj <- -.5

makeTransparent<-function(someColor, alpha=100)
{
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}

col1 <- 'steelblue4' #upper Chitwan
col2 <- 'plum4' #lower Chitwan
col3 <- col1 #upper Pyuthan
col4 <- col2 #lower Pyuthan


colt1 <- makeTransparent(col1,alpha=20)
colt2 <- makeTransparent(col2,alpha=20)
colt3 <- makeTransparent(col3,alpha=20)
colt4 <- makeTransparent(col4,alpha=20)

plot(range(1, 1000), range(1, 1000), type = "n", axes = FALSE, xlab = "", ylab = "")
par(fig = c(0, 0.62, 0, 1), new = TRUE) 
par(mar = c(6,0,6,2))

# Plot A

plot(range(xx, 700), range(0.25,55), type = 'n', axes = F, xlab = "", ylab = "")

axis(side = 1, at = seq(30, 350, by = 80), labels = seq(30, 350, by = 80), cex.axis = cex.a, mgp = c(2,1.5,1))
axis(side = 1, at = seq(40, 320, by = 80) + h, labels = seq(40, 320, by = 80), cex.axis = cex.a, mgp = c(2,1.5,1))
mtext(side = 1, at = 350, "Incremental cost per DALY averted (USD)", line = 3.5, font = 2)

axis(side = 3, at = seq(30, 350, by = 80), labels = seq(30, 350, by = 80), cex.axis = cex.a, mgp = c(2,1.5,1))
axis(side = 3, at = seq(40, 320, by = 80) + h, labels = seq(40, 320, by = 80), cex.axis = cex.a, mgp = c(2,1.5,1))
mtext(side = 3, at = 350, "Incremental cost per DALY averted (USD)", line = 3.5, font = 2)
mtext(side =3, at = 100, "A. Incremental Cost per DALY averted (USD)", line = 4.5, cex = 1.3, font = 2)
mtext(side = 3, at = 160, "Chitwan", line = 2.5, cex = 1, font = 2)
mtext(side = 3, at = 170 + h, "Pyuthan", line = 2.5, cex = 1, font = 2)

par(xpd = T)
legend(x = xx-20, y = yy+5, c("top decile", "bottom decile"), fill = c(col1, col2), bty = "n", cex = cex.a, horiz = F)

# plotting segments

segments(ch_cost_daly_lower_yield_hh[1], yy + 0.5, ch_cost_daly_lower_yield_hh[5], yy + 0.5)
segments(ch_cost_daly_upper_yield_hh[1], yy + 1.5, ch_cost_daly_upper_yield_hh[5], yy + 1.5)
rect(ch_cost_daly_lower_yield_hh[2], yy + 0.25, ch_cost_daly_lower_yield_hh[4], yy + 0.75, col = col2, border = 1)
rect(ch_cost_daly_upper_yield_hh[2], yy + 1.25, ch_cost_daly_upper_yield_hh[4], yy + 1.75, col = col1, border = 1)
segments(ch_cost_daly_lower_yield_hh[3], yy + 0.25, ch_cost_daly_lower_yield_hh[3], yy + 0.75)
segments(ch_cost_daly_upper_yield_hh[3], yy + 1.25, ch_cost_daly_upper_yield_hh[3], yy + 1.75)

segments(py_cost_daly_lower_yield_hh[1] + h, yy + 0.5, py_cost_daly_lower_yield_hh[5] + h, yy + 0.5)
segments(py_cost_daly_upper_yield_hh[1] + h, yy + 1.5, py_cost_daly_upper_yield_hh[5] + h, yy + 1.5)
rect(py_cost_daly_lower_yield_hh[2] + h, yy + 0.25, py_cost_daly_lower_yield_hh[4] + h, yy + 0.75, col = col4, border = 1)
rect(py_cost_daly_upper_yield_hh[2] + h, yy + 1.25, py_cost_daly_upper_yield_hh[4] + h, yy + 1.75, col = col3, border = 1)
segments(py_cost_daly_lower_yield_hh[3] + h, yy + 0.25, py_cost_daly_lower_yield_hh[3] + h, yy + 0.75)
segments(py_cost_daly_upper_yield_hh[3] + h, yy + 1.25, py_cost_daly_upper_yield_hh[3] + h, yy + 1.75)

text(xx - 20, yy + 1.5, 'Proportion HH', cex = cex.a, pos = 4)
text(xx - 20, yy + 0.75, "contacts positive", cex = cex.a, pos = 4)

yy <- yy - 4

segments(ch_cost_daly_lower_int_cost_per_case[1], yy + 0.5, ch_cost_daly_lower_int_cost_per_case[5], yy + 0.5)
segments(ch_cost_daly_upper_int_cost_per_case[1], yy + 1.5, ch_cost_daly_upper_int_cost_per_case[5], yy + 1.5)
rect(ch_cost_daly_lower_int_cost_per_case[2], yy + 0.25, ch_cost_daly_lower_int_cost_per_case[4], yy + 0.75, col = col2, border = 1)
rect(ch_cost_daly_upper_int_cost_per_case[2], yy + 1.25, ch_cost_daly_upper_int_cost_per_case[4], yy + 1.75, col = col1, border = 1)
segments(ch_cost_daly_lower_int_cost_per_case[3], yy + 0.25, ch_cost_daly_lower_int_cost_per_case[3], yy + 0.75)
segments(ch_cost_daly_upper_int_cost_per_case[3], yy + 1.25, ch_cost_daly_upper_int_cost_per_case[3], yy + 1.75)

segments(py_cost_daly_lower_int_cost_per_case[1] + h, yy + 0.5, py_cost_daly_lower_int_cost_per_case[5] + h, yy + 0.5)
segments(py_cost_daly_upper_int_cost_per_case[1] + h, yy + 1.5, py_cost_daly_upper_int_cost_per_case[5] + h, yy + 1.5)
rect(py_cost_daly_lower_int_cost_per_case[2] + h, yy + 0.25, py_cost_daly_lower_int_cost_per_case[4] + h, yy + 0.75, col = col4, border = 1)
rect(py_cost_daly_upper_int_cost_per_case[2] + h, yy + 1.25, py_cost_daly_upper_int_cost_per_case[4] + h, yy + 1.75, col = col3, border = 1)
segments(py_cost_daly_lower_int_cost_per_case[3] + h, yy + 0.25, py_cost_daly_lower_int_cost_per_case[3] + h, yy + 0.75)
segments(py_cost_daly_upper_int_cost_per_case[3] + h, yy + 1.25, py_cost_daly_upper_int_cost_per_case[3] + h, yy + 1.75)

text(xx - 20, yy + 1.5, 'Intervention cost', cex = cex.a, pos = 4)
text(xx - 20, yy + 0.75, "per case", cex = cex.a, pos = 4)

yy <- yy - 4

segments(ch_cost_daly_lower_sigma1[1], yy + 0.5, ch_cost_daly_lower_sigma1[5], yy + 0.5)
segments(ch_cost_daly_upper_sigma1[1], yy + 1.5, ch_cost_daly_upper_sigma1[5], yy + 1.5)
rect(ch_cost_daly_lower_sigma1[2], yy + 0.25, ch_cost_daly_lower_sigma1[4], yy + 0.75, col = col2, border = 1)
rect(ch_cost_daly_upper_sigma1[2], yy + 1.25, ch_cost_daly_upper_sigma1[4], yy + 1.75, col = col1, border = 1)
segments(ch_cost_daly_lower_sigma1[3], yy + 0.25, ch_cost_daly_lower_sigma1[3], yy + 0.75)
segments(ch_cost_daly_upper_sigma1[3], yy + 1.25, ch_cost_daly_upper_sigma1[3], yy + 1.75)

segments(py_cost_daly_lower_sigma1[1] + h, yy + 0.5, py_cost_daly_lower_sigma1[5] + h, yy + 0.5)
segments(py_cost_daly_upper_sigma1[1] + h, yy + 1.5, py_cost_daly_upper_sigma1[5] + h, yy + 1.5)
rect(py_cost_daly_lower_sigma1[2] + h, yy + 0.25, py_cost_daly_lower_sigma1[4] + h, yy + 0.75, col = col4, border = 1)
rect(py_cost_daly_upper_sigma1[2] + h, yy + 1.25, py_cost_daly_upper_sigma1[4] + h, yy + 1.75, col = col3, border = 1)
segments(py_cost_daly_lower_sigma1[3] + h, yy + 0.25, py_cost_daly_lower_sigma1[3] + h, yy + 0.75)
segments(py_cost_daly_upper_sigma1[3] + h, yy + 1.25, py_cost_daly_upper_sigma1[3] + h, yy + 1.75)

text(xx - 20, yy + 1.5, 'Rate of early', cex = cex.a, pos = 4)
text(xx - 20, yy + 0.75, "progression", cex = cex.a, pos = 4)

yy <- yy - 4

segments(ch_cost_daly_lower_phi[1], yy + 0.5, ch_cost_daly_lower_phi[5], yy + 0.5)
segments(ch_cost_daly_upper_phi[1], yy + 1.5, ch_cost_daly_upper_phi[5], yy + 1.5)
rect(ch_cost_daly_lower_phi[2], yy + 0.25, ch_cost_daly_lower_phi[4], yy + 0.75, col = col2, border = 1)
rect(ch_cost_daly_upper_phi[2], yy + 1.25, ch_cost_daly_upper_phi[4], yy + 1.75, col = col1, border = 1)
segments(ch_cost_daly_lower_phi[3], yy + 0.25, ch_cost_daly_lower_phi[3], yy + 0.75)
segments(ch_cost_daly_upper_phi[3], yy + 1.25, ch_cost_daly_upper_phi[3], yy + 1.75)

segments(py_cost_daly_lower_phi[1] + h, yy + 0.5, py_cost_daly_lower_phi[5] + h, yy + 0.5)
segments(py_cost_daly_upper_phi[1] + h, yy + 1.5, py_cost_daly_upper_phi[5] + h, yy + 1.5)
rect(py_cost_daly_lower_phi[2] + h, yy + 0.25, py_cost_daly_lower_phi[4] + h, yy + 0.75, col = col4, border = 1)
rect(py_cost_daly_upper_phi[2] + h, yy + 1.25, py_cost_daly_upper_phi[4] + h, yy + 1.75, col = col3, border = 1)
segments(py_cost_daly_lower_phi[3] + h, yy + 0.25, py_cost_daly_lower_phi[3] + h, yy + 0.75)
segments(py_cost_daly_upper_phi[3] + h, yy + 1.25, py_cost_daly_upper_phi[3] + h, yy + 1.75)

text(xx - 20, yy + 1.5, 'Rate of reinfection', cex = cex.a, pos = 4)

yy <- yy - 4

segments(ch_cost_daly_lower_w2[1], yy + 0.5, ch_cost_daly_lower_w2[5], yy + 0.5)
segments(ch_cost_daly_upper_w2[1], yy + 1.5, ch_cost_daly_upper_w2[5], yy + 1.5)
rect(ch_cost_daly_lower_w2[2], yy + 0.25, ch_cost_daly_lower_w2[4], yy + 0.75, col = col2, border = 1)
rect(ch_cost_daly_upper_w2[2], yy + 1.25, ch_cost_daly_upper_w2[4], yy + 1.75, col = col1, border = 1)
segments(ch_cost_daly_lower_w2[3], yy + 0.25, ch_cost_daly_lower_w2[3], yy + 0.75)
segments(ch_cost_daly_upper_w2[3], yy + 1.25, ch_cost_daly_upper_w2[3], yy + 1.75)

segments(py_cost_daly_lower_w2[1] + h, yy + 0.5, py_cost_daly_lower_w2[5] + h, yy + 0.5)
segments(py_cost_daly_upper_w2[1] + h, yy + 1.5, py_cost_daly_upper_w2[5] + h, yy + 1.5)
rect(py_cost_daly_lower_w2[2] + h, yy + 0.25, py_cost_daly_lower_w2[4] + h, yy + 0.75, col = col4, border = 1)
rect(py_cost_daly_upper_w2[2] + h, yy + 1.25, py_cost_daly_upper_w2[4] + h, yy + 1.75, col = col3, border = 1)
segments(py_cost_daly_lower_w2[3] + h, yy + 0.25, py_cost_daly_lower_w2[3] + h, yy + 0.75)
segments(py_cost_daly_upper_w2[3] + h, yy + 1.25, py_cost_daly_upper_w2[3] + h, yy + 1.75)

text(xx - 20, yy + 1.5, 'Self clearance', cex = cex.a, pos = 4)
text(xx - 20, yy + 0.75, 'from late latent', cex = cex.a, pos = 4)

yy <- yy - 4

segments(ch_cost_daly_lower_tx_cost[1], yy + 0.5, ch_cost_daly_lower_tx_cost[5], yy + 0.5)
segments(ch_cost_daly_upper_tx_cost[1], yy + 1.5, ch_cost_daly_upper_tx_cost[5], yy + 1.5)
rect(ch_cost_daly_lower_tx_cost[2], yy + 0.25, ch_cost_daly_lower_tx_cost[4], yy + 0.75, col = col2, border = 1)
rect(ch_cost_daly_upper_tx_cost[2], yy + 1.25, ch_cost_daly_upper_tx_cost[4], yy + 1.75, col = col1, border = 1)
segments(ch_cost_daly_lower_tx_cost[3], yy + 0.25, ch_cost_daly_lower_tx_cost[3], yy + 0.75)
segments(ch_cost_daly_upper_tx_cost[3], yy + 1.25, ch_cost_daly_upper_tx_cost[3], yy + 1.75)

segments(py_cost_daly_lower_tx_cost[1] + h, yy + 0.5, py_cost_daly_lower_tx_cost[5] + h, yy + 0.5)
segments(py_cost_daly_upper_tx_cost[1] + h, yy + 1.5, py_cost_daly_upper_tx_cost[5] + h, yy + 1.5)
rect(py_cost_daly_lower_tx_cost[2] + h, yy + 0.25, py_cost_daly_lower_tx_cost[4] + h, yy + 0.75, col = col4, border = 1)
rect(py_cost_daly_upper_tx_cost[2] + h, yy + 1.25, py_cost_daly_upper_tx_cost[4] + h, yy + 1.75, col = col3, border = 1)
segments(py_cost_daly_lower_tx_cost[3] + h, yy + 0.25, py_cost_daly_lower_tx_cost[3] + h, yy + 0.75)
segments(py_cost_daly_upper_tx_cost[3] + h, yy + 1.25, py_cost_daly_upper_tx_cost[3] + h, yy + 1.75)

text(xx - 20, yy + 1.5, 'Treatment cost', cex = cex.a, pos = 4)

yy <- yy - 4

segments(ch_cost_daly_lower_beta[1], yy + 0.5, ch_cost_daly_lower_beta[5], yy + 0.5)
segments(ch_cost_daly_upper_beta[1], yy + 1.5, ch_cost_daly_upper_beta[5], yy + 1.5)
rect(ch_cost_daly_lower_beta[2], yy + 0.25, ch_cost_daly_lower_beta[4], yy + 0.75, col = col2, border = 1)
rect(ch_cost_daly_upper_beta[2], yy + 1.25, ch_cost_daly_upper_beta[4], yy + 1.75, col = col1, border = 1)
segments(ch_cost_daly_lower_beta[3], yy + 0.25, ch_cost_daly_lower_beta[3], yy + 0.75)
segments(ch_cost_daly_upper_beta[3], yy + 1.25, ch_cost_daly_upper_beta[3], yy + 1.75)

segments(py_cost_daly_lower_beta[1] + h, yy + 0.5, py_cost_daly_lower_beta[5] + h, yy + 0.5)
segments(py_cost_daly_upper_beta[1] +h, yy + 1.5, py_cost_daly_upper_beta[5] + h, yy + 1.5)
rect(py_cost_daly_lower_beta[2] + h, yy + 0.25, py_cost_daly_lower_beta[4] + h, yy + 0.75, col = col4, border = 1)
rect(py_cost_daly_upper_beta[2] + h, yy + 1.25, py_cost_daly_upper_beta[4] + h, yy + 1.75, col = col3, border = 1)
segments(py_cost_daly_lower_beta[3] + h, yy + 0.25, py_cost_daly_lower_beta[3] + h, yy + 0.75)
segments(py_cost_daly_upper_beta[3] + h, yy + 1.25, py_cost_daly_upper_beta[3] + h, yy + 1.75)

text(xx - 20, yy + 1.5, 'Transmission Rate', cex = cex.a, pos = 4)

yy <- yy - 4

segments(ch_cost_daly_lower_beta_dec[1], yy + 0.5, ch_cost_daly_lower_beta_dec[5], yy + 0.5)
segments(ch_cost_daly_upper_beta_dec[1], yy + 1.5, ch_cost_daly_upper_beta_dec[5], yy + 1.5)
rect(ch_cost_daly_lower_beta_dec[2], yy + 0.25, ch_cost_daly_lower_beta_dec[4], yy + 0.75, col = col2, border = 1)
rect(ch_cost_daly_upper_beta_dec[2], yy + 1.25, ch_cost_daly_upper_beta_dec[4], yy + 1.75, col = col1, border = 1)
segments(ch_cost_daly_lower_beta_dec[3], yy + 0.25, ch_cost_daly_lower_beta_dec[3], yy + 0.75)
segments(ch_cost_daly_upper_beta_dec[3], yy + 1.25, ch_cost_daly_upper_beta_dec[3], yy + 1.75)

segments(py_cost_daly_lower_beta_dec[1] + h, yy + 0.5, py_cost_daly_lower_beta_dec[5] + h, yy + 0.5)
segments(py_cost_daly_upper_beta_dec[1] + h, yy + 1.5, py_cost_daly_upper_beta_dec[5] + h, yy + 1.5)
rect(py_cost_daly_lower_beta_dec[2] + h, yy + 0.25, py_cost_daly_lower_beta_dec[4] + h, yy + 0.75, col = col4, border = 1)
rect(py_cost_daly_upper_beta_dec[2] + h, yy + 1.25, py_cost_daly_upper_beta_dec[4] + h, yy + 1.75, col = col3, border = 1)
segments(py_cost_daly_lower_beta_dec[3] + h, yy + 0.25, py_cost_daly_lower_beta_dec[3] + h, yy + 0.75)
segments(py_cost_daly_upper_beta_dec[3] + h, yy + 1.25, py_cost_daly_upper_beta_dec[3] + h, yy + 1.75)

text(xx - 20, yy + 1.5, 'Rate of transmission', cex = cex.a, pos = 4)
text(xx - 20, yy + 0.75, 'decline', cex = cex.a, pos = 4)

yy <- yy - 4

segments(ch_cost_daly_lower_sigma2[1], yy + 0.5, ch_cost_daly_lower_sigma2[5], yy + 0.5)
segments(ch_cost_daly_upper_sigma2[1], yy + 1.5, ch_cost_daly_upper_sigma2[5], yy + 1.5)
rect(ch_cost_daly_lower_sigma2[2], yy + 0.25, ch_cost_daly_lower_sigma2[4], yy + 0.75, col = col2, border = 1)
rect(ch_cost_daly_upper_sigma2[2], yy + 1.25, ch_cost_daly_upper_sigma2[4], yy + 1.75, col = col1, border = 1)
segments(ch_cost_daly_lower_sigma2[3], yy + 0.25, ch_cost_daly_lower_sigma2[3], yy + 0.75)
segments(ch_cost_daly_upper_sigma2[3], yy + 1.25, ch_cost_daly_upper_sigma2[3], yy + 1.75)

segments(py_cost_daly_lower_sigma2[1] + h, yy + 0.5, py_cost_daly_lower_sigma2[5] + h, yy + 0.5)
segments(py_cost_daly_upper_sigma2[1] + h, yy + 1.5, py_cost_daly_upper_sigma2[5] + h, yy + 1.5)
rect(py_cost_daly_lower_sigma2[2] + h, yy + 0.25, py_cost_daly_lower_sigma2[4] + h, yy + 0.75, col = col4, border = 1)
rect(py_cost_daly_upper_sigma2[2] + h, yy + 1.25, py_cost_daly_upper_sigma2[4] + h, yy + 1.75, col = col3, border = 1)
segments(py_cost_daly_lower_sigma2[3] + h, yy + 0.25, py_cost_daly_lower_sigma2[3] + h, yy + 0.75)
segments(py_cost_daly_upper_sigma2[3] + h, yy + 1.25, py_cost_daly_upper_sigma2[3] + h, yy + 1.75)

text(xx - 20, yy + 1.5, 'Rate of late', cex = cex.a, pos = 4)
text(xx - 20, yy + 0.75, 'progression', cex = cex.a, pos = 4)

yy <- yy - 4

segments(ch_cost_daly_lower_mu.atb[1], yy + 0.5, ch_cost_daly_lower_mu.atb[5], yy + 0.5)
segments(ch_cost_daly_upper_mu.atb[1], yy + 1.5, ch_cost_daly_upper_mu.atb[5], yy + 1.5)
rect(ch_cost_daly_lower_mu.atb[2], yy + 0.25, ch_cost_daly_lower_mu.atb[4], yy + 0.75, col = col2, border = 1)
rect(ch_cost_daly_upper_mu.atb[2], yy + 1.25, ch_cost_daly_upper_mu.atb[4], yy + 1.75, col = col1, border = 1)
segments(ch_cost_daly_lower_mu.atb[3], yy + 0.25, ch_cost_daly_lower_mu.atb[3], yy + 0.75)
segments(ch_cost_daly_upper_mu.atb[3], yy + 1.25, ch_cost_daly_upper_mu.atb[3], yy + 1.75)

segments(py_cost_daly_lower_mu.atb[1] + h, yy + 0.5, py_cost_daly_lower_mu.atb[5] + h, yy + 0.5)
segments(py_cost_daly_upper_mu.atb[1] + h, yy + 1.5, py_cost_daly_upper_mu.atb[5] + h, yy + 1.5)
rect(py_cost_daly_lower_mu.atb[2] + h, yy + 0.25, py_cost_daly_lower_mu.atb[4] + h, yy + 0.75, col = col4, border = 1)
rect(py_cost_daly_upper_mu.atb[2] + h, yy + 1.25, py_cost_daly_upper_mu.atb[4] + h, yy + 1.75, col = col3, border = 1)
segments(py_cost_daly_lower_mu.atb[3] + h, yy + 0.25, py_cost_daly_lower_mu.atb[3] + h, yy + 0.75)
segments(py_cost_daly_upper_mu.atb[3] + h, yy + 1.25, py_cost_daly_upper_mu.atb[3] + h, yy + 1.75)

text(xx - 20, yy + 1.5, 'Mortality rate,', cex = cex.a, pos = 4)
text(xx - 20, yy + 0.75, 'asymptomatic', cex = cex.a, pos = 4)

yy <- yy - 4

segments(ch_cost_daly_lower_mu.stb[1], yy + 0.5, ch_cost_daly_lower_mu.stb[5], yy + 0.5)
segments(ch_cost_daly_upper_mu.stb[1], yy + 1.5, ch_cost_daly_upper_mu.stb[5], yy + 1.5)
rect(ch_cost_daly_lower_mu.stb[2], yy + 0.25, ch_cost_daly_lower_mu.stb[4], yy + 0.75, col = col2, border = 1)
rect(ch_cost_daly_upper_mu.stb[2], yy + 1.25, ch_cost_daly_upper_mu.stb[4], yy + 1.75, col = col1, border = 1)
segments(ch_cost_daly_lower_mu.stb[3], yy + 0.25, ch_cost_daly_lower_mu.stb[3], yy + 0.75)
segments(ch_cost_daly_upper_mu.stb[3], yy + 1.25, ch_cost_daly_upper_mu.stb[3], yy + 1.75)

segments(py_cost_daly_lower_mu.stb[1] + h, yy + 0.5, py_cost_daly_lower_mu.stb[5] + h, yy + 0.5)
segments(py_cost_daly_upper_mu.stb[1] + h, yy + 1.5, py_cost_daly_upper_mu.stb[5] + h, yy + 1.5)
rect(py_cost_daly_lower_mu.stb[2] + h, yy + 0.25, py_cost_daly_lower_mu.stb[4] + h, yy + 0.75, col = col4, border = 1)
rect(py_cost_daly_upper_mu.stb[2] + h, yy + 1.25, py_cost_daly_upper_mu.stb[4] + h, yy + 1.75, col = col3, border = 1)
segments(py_cost_daly_lower_mu.stb[3] + h, yy + 0.25, py_cost_daly_lower_mu.stb[3] + h, yy + 0.75)
segments(py_cost_daly_upper_mu.stb[3] + h, yy + 1.25, py_cost_daly_upper_mu.stb[3] + h, yy + 1.75)

text(xx - 20, yy + 1.5, 'Mortality rate,', cex = cex.a, pos = 4)
text(xx - 20, yy + 0.75, 'symptomatic', cex = cex.a, pos = 4)

yy <- yy - 4

segments(ch_cost_daly_lower_gamma[1], yy + 0.5, ch_cost_daly_lower_gamma[5], yy + 0.5)
segments(ch_cost_daly_upper_gamma[1], yy + 1.5, ch_cost_daly_upper_gamma[5], yy + 1.5)
rect(ch_cost_daly_lower_gamma[2], yy + 0.25, ch_cost_daly_lower_gamma[4], yy + 0.75, col = col2, border = 1)
rect(ch_cost_daly_upper_gamma[2], yy + 1.25, ch_cost_daly_upper_gamma[4], yy + 1.75, col = col1, border = 1)
segments(ch_cost_daly_lower_gamma[3], yy + 0.25, ch_cost_daly_lower_gamma[3], yy + 0.75)
segments(ch_cost_daly_upper_gamma[3], yy + 1.25, ch_cost_daly_upper_gamma[3], yy + 1.75)

segments(py_cost_daly_lower_gamma[1] + h, yy + 0.5, py_cost_daly_lower_gamma[5] + h, yy + 0.5)
segments(py_cost_daly_upper_gamma[1] + h, yy + 1.5, py_cost_daly_upper_gamma[5] + h, yy + 1.5)
rect(py_cost_daly_lower_gamma[2] + h, yy + 0.25, py_cost_daly_lower_gamma[4] + h, yy + 0.75, col = col4, border = 1)
rect(py_cost_daly_upper_gamma[2] + h, yy + 1.25, py_cost_daly_upper_gamma[4] + h, yy + 1.75, col = col3, border = 1)
segments(py_cost_daly_lower_gamma[3] + h, yy + 0.25, py_cost_daly_lower_gamma[3] + h, yy + 0.75)
segments(py_cost_daly_upper_gamma[3] + h, yy + 1.25, py_cost_daly_upper_gamma[3] + h, yy + 1.75)

text(xx - 20, yy + 1.5, 'Rate of progression', cex = cex.a, pos = 4)

yy <- yy - 4

segments(ch_cost_daly_lower_rho1[1], yy + 0.5, ch_cost_daly_lower_rho1[5], yy + 0.5)
segments(ch_cost_daly_upper_rho1[1], yy + 1.5, ch_cost_daly_upper_rho1[5], yy + 1.5)
rect(ch_cost_daly_lower_rho1[2], yy + 0.25, ch_cost_daly_lower_rho1[4], yy + 0.75, col = col2, border = 1)
rect(ch_cost_daly_upper_rho1[2], yy + 1.25, ch_cost_daly_upper_rho1[4], yy + 1.75, col = col1, border = 1)
segments(ch_cost_daly_lower_rho1[3], yy + 0.25, ch_cost_daly_lower_rho1[3], yy + 0.75)
segments(ch_cost_daly_upper_rho1[3], yy + 1.25, ch_cost_daly_upper_rho1[3], yy + 1.75)

segments(py_cost_daly_lower_rho1[1] + h, yy + 0.5, py_cost_daly_lower_rho1[5] + h, yy + 0.5)
segments(py_cost_daly_upper_rho1[1] + h, yy + 1.5, py_cost_daly_upper_rho1[5] + h, yy + 1.5)
rect(py_cost_daly_lower_rho1[2] + h, yy + 0.25, py_cost_daly_lower_rho1[4] + h, yy + 0.75, col = col4, border = 1)
rect(py_cost_daly_upper_rho1[2] + h, yy + 1.25, py_cost_daly_upper_rho1[4] + h, yy + 1.75, col = col3, border = 1)
segments(py_cost_daly_lower_rho1[3] + h, yy + 0.25, py_cost_daly_lower_rho1[3] + h, yy + 0.75)
segments(py_cost_daly_upper_rho1[3] + h, yy + 1.25, py_cost_daly_upper_rho1[3] + h, yy + 1.75)

text(xx - 20, yy + 1.5, 'Recovery rate', cex = cex.a, pos = 4)

yy <- yy - 4

segments(ch_cost_daly_lower_propEL[1], yy + 0.5, ch_cost_daly_lower_propEL[5], yy + 0.5)
segments(ch_cost_daly_upper_propEL[1], yy + 1.5, ch_cost_daly_upper_propEL[5], yy + 1.5)
rect(ch_cost_daly_lower_propEL[2], yy + 0.25, ch_cost_daly_lower_propEL[4], yy + 0.75, col = col2, border = 1)
rect(ch_cost_daly_upper_propEL[2], yy + 1.25, ch_cost_daly_upper_propEL[4], yy + 1.75, col = col1, border = 1)
segments(ch_cost_daly_lower_propEL[3], yy + 0.25, ch_cost_daly_lower_propEL[3], yy + 0.75)
segments(ch_cost_daly_upper_propEL[3], yy + 1.25, ch_cost_daly_upper_propEL[3], yy + 1.75)

segments(py_cost_daly_lower_propEL[1] + h, yy + 0.5, py_cost_daly_lower_propEL[5] + h, yy + 0.5)
segments(py_cost_daly_upper_propEL[1] + h, yy + 1.5, py_cost_daly_upper_propEL[5] + h, yy + 1.5)
rect(py_cost_daly_lower_propEL[2] + h, yy + 0.25, py_cost_daly_lower_propEL[4] + h, yy + 0.75, col = col4, border = 1)
rect(py_cost_daly_upper_propEL[2] + h, yy + 1.25, py_cost_daly_upper_propEL[4] + h, yy + 1.75, col = col3, border = 1)
segments(py_cost_daly_lower_propEL[3] + h, yy + 0.25, py_cost_daly_lower_propEL[3] + h, yy + 0.75)
segments(py_cost_daly_upper_propEL[3] + h, yy + 1.25, py_cost_daly_upper_propEL[3] + h, yy + 1.75)

text(xx - 20, yy + 1.5, 'Proportion early latent', cex = cex.a, pos = 4)

yy <- yy - 4

segments(ch_cost_daly_lower_propATB[1], yy + 0.5, ch_cost_daly_lower_propATB[5], yy + 0.5)
segments(ch_cost_daly_upper_propATB[1], yy + 1.5, ch_cost_daly_upper_propATB[5], yy + 1.5)
rect(ch_cost_daly_lower_propATB[2], yy + 0.25, ch_cost_daly_lower_propATB[4], yy + 0.75, col = col2, border = 1)
rect(ch_cost_daly_upper_propATB[2], yy + 1.25, ch_cost_daly_upper_propATB[4], yy + 1.75, col = col1, border = 1)
segments(ch_cost_daly_lower_propATB[3], yy + 0.25, ch_cost_daly_lower_propATB[3], yy + 0.75)
segments(ch_cost_daly_upper_propATB[3], yy + 1.25, ch_cost_daly_upper_propATB[3], yy + 1.75)

segments(py_cost_daly_lower_propATB[1] + h, yy + 0.5, py_cost_daly_lower_propATB[5] + h, yy + 0.5)
segments(py_cost_daly_upper_propATB[1] + h, yy + 1.5, py_cost_daly_upper_propATB[5] + h, yy + 1.5)
rect(py_cost_daly_lower_propATB[2] + h, yy + 0.25, py_cost_daly_lower_propATB[4] + h, yy + 0.75, col = col4, border = 1)
rect(py_cost_daly_upper_propATB[2] + h, yy + 1.25, py_cost_daly_upper_propATB[4] + h, yy + 1.75, col = col3, border = 1)
segments(py_cost_daly_lower_propATB[3] + h, yy + 0.25, py_cost_daly_lower_propATB[3] + h, yy + 0.75)
segments(py_cost_daly_upper_propATB[3] + h, yy + 1.25, py_cost_daly_upper_propATB[3] + h, yy + 1.75)

text(xx - 20, yy + 1.5, 'Proportion', cex = cex.a, pos = 4)
text(xx - 20, yy + 0.75, 'asymptomatic TB', cex = cex.a, pos = 4)

segments(median(ch_combined$cost_per_daly_prevented), yy, median(ch_combined$cost_per_daly_prevented), 57, lty = 2)
segments(median(py_combined$cost_per_daly_prevented) + h, yy, median(py_combined$cost_per_daly_prevented) + h, 57, lty = 2)

# Plot B

ch_results <- # import ch_results_cea_final from 06_cost_effectiveness
py_results <- # run 06_cost_effectivness for pyuthan save results as py_results_cea_final and import here

par(fig = c(0.60, 0.99, 0.67, 0.99), new = TRUE)

par(mar = c(4,2,2,2))
wtp_thres <- seq(0,300, by = 1)

ch_prop_ce <- numeric(length(wtp_thres))
py_prop_ce <- numeric(length(wtp_thres))

for (i in 1:length(wtp_thres)) {
  wtp_thres1 <- wtp_thres[i]
  
  ch_ce <- ch_results$total_added_cost <= (wtp_thres1 * ch_results$dalys)
  py_ce <- py_results$total_added_cost <= (wtp_thres1 * py_results$dalys)
  
  ch_prop_ce[i] <- sum(ch_ce)/length(ch_ce)
  py_prop_ce[i] <- sum(py_ce)/length(py_ce)
  
}

col6 <- 'paleturquoise4'
col7 <- 'rosybrown4'

colt6 <- makeTransparent(col6, alpha = 20)
colt7 <- makeTransparent(col7, alpha = 20)

plot(range(0,300), range(0,1), type = "n", axes = F, xlab = "", ylab = "", cex.main = 1.5, font.lab = 2, cex.axis = 1.2)
lines(wtp_thres, ch_prop_ce, col = col6, type = "l", lwd = 2)
lines(wtp_thres, py_prop_ce, col = col7, type = "l", lwd = 2)
axis(side = 1, at = seq(0,300, by = 50), labels = seq(0, 300, by = 50))
axis(side = 2, at = seq(0,1, by = 0.2), labels = seq(0,1,by = 0.2) * 100)

segments(-5, 0.5, wtp_thres[which(round(py_prop_ce,1) == 0.5)[3]] + 0.1, 0.5, lty = 2, col = "red")
segments(wtp_thres[which(round(ch_prop_ce,1) == 0.5)[3]], 0, wtp_thres[which(round(ch_prop_ce,1) == 0.5)[3]] + 1, 0.5, lty = 2, col = col6)
segments(wtp_thres[which(round(py_prop_ce,1)== 0.5)[3]], 0, wtp_thres[which(round(py_prop_ce,1) == 0.5)[3]] + 1, 0.5, lty = 2, col = col7)
text(wtp_thres[which(round(ch_prop_ce,1) == 0.5)[3]] - 40, 0.52, labels = paste0(wtp_thres[which(round(ch_prop_ce,1) == 0.5)[3]], " USD"), pos = 4, col = col6)
text(wtp_thres[which(round(py_prop_ce,1)== 0.5)[3]], 0.47, labels = paste0(wtp_thres[which(round(py_prop_ce,1)== 0.5)[3]], " USD"), pos = 4, col = col7)

axis(side = 2, at = 0.5, labels = 0.5 * 100)

mtext(side = 1, "Willingness to Pay (USD)", font = 2, line = 2.5)
mtext(side = 2, "Proportion Cost-Effective (%)", font = 2, line = 2.5)
mtext(side = 3, adj = 0.01, "B. Proportion Cost-Effective", font = 2, line = 1, cex = 1.3)

legend(x = 180, y = 0.1, c("Chitwan", "Pyuthan"), fill = c(col6, col7), cex = 0.8, horiz = T, border = NA)

# Plot C and D

# Load results from 07_scale_up and extract the results with and without intervention for chitwan

ch_z_int <- lapply(scale_up_int, function(x) x$y)
ch_z_noint <- lapply(scale_up_noint, function(x) x$y)

pop <- sapply(ch_z_int, function(x) x$t_pop[221])

# repeat the process for pyuthan and extract the results

py_z_int <- lapply(scale_up_int, function(x) x$y)
py_z_noint <- lapply(scale_up_noint, function(x) x$y)

# calculating cumulative cases

ch_cum_cases_int <- sapply(ch_z_int, function(x) ((x$new_cases[-(1:221)] / x$t_pop[-(1:221)]) * 719859))
ch_cum_cases_noint <- sapply(ch_z_noint, function(x) ((x$new_cases[-(1:221)] / x$t_pop[-(1:221)]) * 719859))

py_cum_cases_int <- sapply(py_z_int, function(x) ((x$new_cases[-(1:221)] / x$t_pop[-(1:221)]) * 232019))
py_cum_cases_noint <- sapply(py_z_noint, function(x) ((x$new_cases[-(1:221)] / x$t_pop[-(1:221)]) * 232019))

# calculating cumulative deaths

ch_cum_deaths_int <- sapply(ch_z_int, function(x) ((x$t_deaths[-(1:221)] / x$t_pop[-(1:221)]) * 719859))
ch_cum_deaths_noint <- sapply(ch_z_noint, function(x) ((x$t_deaths[-(1:221)] / x$t_pop[-(1:221)]) * 719859))

py_cum_deaths_int <- sapply(py_z_int, function(x) ((x$t_deaths[-(1:221)] / x$t_pop[-(1:221)]) * 232019))
py_cum_deaths_noint <- sapply(py_z_noint, function(x) ((x$t_deaths[-(1:221)] / x$t_pop[-(1:221)]) * 232019))

# calculating cases and deaths averted

ch_cases_averted <- ch_cum_cases_noint - ch_cum_cases_int
ch_deaths_averted <- ch_cum_deaths_noint - ch_cum_deaths_int

py_cases_averted <- py_cum_cases_noint - py_cum_cases_int
py_deaths_averted <- py_cum_deaths_noint - py_cum_deaths_int

ch_cum_cases_averted <- apply(ch_cases_averted, 2, cumsum)
ch_cum_deaths_averted <- apply(ch_deaths_averted, 2, cumsum)

py_cum_cases_averted <- apply(py_cases_averted, 2, cumsum)
py_cum_deaths_averted <- apply(py_deaths_averted, 2, cumsum)

# Prevalence
ch_prev_int <- sapply(ch_z_int, function(x) x$prev)
ch_prev_noint <- sapply(ch_z_noint, function(x) x$prev)

py_prev_int <- sapply(py_z_int, function(x) x$prev)
py_prev_noint <- sapply(py_z_noint, function(x) x$prev)

# Incidence

ch_incidence_int <- sapply(ch_z_int, function(x) x$incidence)
ch_incidence_noint <- sapply(ch_z_noint, function(x) x$incidence)

py_incidence_int <- sapply(py_z_int, function(x) x$incidence)
py_incidence_noint <- sapply(py_z_noint, function(x) x$incidence)

# Mortality

ch_mortality_int <- sapply(ch_z_int, function(x) x$mortality)
ch_mortality_noint <- sapply(ch_z_noint, function(x) x$mortality)

py_mortality_int <- sapply(py_z_int, function(x) x$mortality)
py_mortality_noint <- sapply(py_z_noint, function(x) x$mortality)

# LTBI prevalence
ch_ltbi_int <- sapply(ch_z_int, function(x) x$ltbi_prev)
ch_ltbi_noint <- sapply(ch_z_noint, function(x) x$ltbi_prev)

py_ltbi_int <- sapply(py_z_int, function(x) x$ltbi_prev)
py_ltbi_noint <- sapply(py_z_noint, function(x) x$ltbi_prev)

# Annual decline in incidence

ch_inc_red <- (ch_incidence_noint - ch_incidence_int)/ch_incidence_noint * 100
py_inc_red <- (py_incidence_noint - py_incidence_int)/py_incidence_noint * 100

# Annual decline in mortality
ch_mort_red <- (ch_mortality_noint - ch_mortality_int)/ch_mortality_noint * 100
py_mort_red <- (py_mortality_noint - py_mortality_int)/py_mortality_noint * 100

# Plot C

years <- seq(2022, 2042, by = 4)
positions <- seq(1, 200, length.out = length(years))

select <- 221:421

par(fig = c(0.60, 0.99, 0.32, 0.67), new = TRUE)
par(mar = c(5,2,3,2))

# cases averted

plot(range(1,200), range(0,1600), type = "n", axes = F, xlab = "", ylab = "")

matlines(ch_cum_cases_averted, lty = 1, col = colt6)
lines(apply(ch_cum_cases_averted,1,median), lty = 1, col = "black", lwd = 2)
lines(apply(ch_cum_cases_averted,1,median), lty = 2, col = "white", lwd = 2)

matlines(py_cum_cases_averted, lty =1, col = colt7)
lines(apply(py_cum_cases_averted,1,median), lty = 1, col = "black", lwd = 2)
lines(apply(py_cum_cases_averted,1,median), lty = 2, col = "white", lwd = 2)

axis(1, at = positions, labels = years)
axis(2, at = seq(0,1600, by = 200), labels = seq(0,1600, by = 200))
axis(4, at = seq(0,1600, by = 200), labels = seq(0,1600, by = 200))

mtext(side = 1, line = 3, "Year", font = 2)
mtext(side = c(2,4), line = 3, "Total Cases Averted", font = 2)
mtext(side = 3, adj = 0.01, "C. Cases Averted - Scale Up", font = 2, line = 1, cex = 1.3)

polygon(c(0,0,50,50), c(0,1600,1600,0), col = makeTransparent("gray", alpha = 70), border = NA)
text(25, 800, "Intervention Period", cex = 1, srt = 90)

legend(120, 1500, legend = c("Chitwan", "Pyuthan"), fill = c(col6, col7), cex = 0.8, horiz = T, border = NA)

# Plot D
# deaths averted

par(fig = c(0.60, 0.99, 0.05, 0.32), new = TRUE)
par(mar = c(1,2,2,2))

plot(range(1, 200), range(0,300), type = "n", axes = F, xlab = "", ylab = "")
matlines(ch_cum_deaths_averted, lty = 1, col = colt6)
lines(apply(ch_cum_deaths_averted,1,median), lty = 1, col = "black", lwd = 2)
lines(apply(ch_cum_deaths_averted,1,median), lty = 2, col = "white", lwd = 2)

matlines(py_cum_deaths_averted, lty = 1, col = colt7)
lines(apply(py_cum_deaths_averted,1,median), lty = 1, col = "black", lwd = 2)
lines(apply(py_cum_deaths_averted,1,median), lty = 2, col = "white", lwd = 2)

axis(1, at = positions, labels = years)
axis(2, at = seq(0,300, by = 50), labels = seq(0,300, by = 50))
axis(4, at = seq(0,300, by = 50), labels = seq(0,300, by = 50))

mtext(side = 1, line = 3, "Year", font = 2)
mtext(side = c(2,4), line = 3, "Total Deaths Averted", font = 2)
mtext(side = 3, adj = 0.01, "D. Deaths Averted - Scale Up", font = 2, line = 1, cex = 1.3)

legend(120, 280, legend = c("Chitwan", "Pyuthan"), fill = c(col6, col7), cex = 0.8, horiz = T)

polygon(c(0,0,50,50), c(0,300,300,0), col = makeTransparent("gray", alpha = 70), border = NA)
text(25, 150, "Intervention Period", cex = 1, srt = 90)

# end