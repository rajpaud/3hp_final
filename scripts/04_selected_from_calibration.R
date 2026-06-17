# import calibration targets for chitwan
targets <- read.csv("data/cal_targets_chitwan.csv")
targets$annual_decline <- 0.03

# results from calibration setup
res <- # import "final_cal" from 03_calibration_setup.R

# setting threshold for selection
th <- 0.25

select <- which(res$prev > targets$prev * (1 - th)
                & res$prev < targets$prev * (1 + th)
                & res$incidence > targets$incidence * (1 - th)
                & res$incidence < targets$incidence * (1 + th)
                & res$mortality > targets$mortality * (1 - th)
                & res$mortality < targets$mortality * (1 + th)
                & res$StoAratio > targets$StoARatio * (1 - th)
                & res$StoAratio < targets$StoARatio * (1 + th)
                & res$cal_r > targets$annual_decline * (1 - th)
                & res$cal_r < targets$annual_decline * (1 + th)
                )

# save the selected indices as "select" and selected parameters as "res[select,]" 

# import calibration targets for pyuthan

targets <- read.csv("data/cal_targets_pyuthan.csv")
targets$annual_decline <- 0.03

#setting threshold for selection
th <- 0.25

select2 <- which(res$prev > targets$prev * (1 - th)
                & res$prev < targets$prev * (1 + th)
                & res$incidence > targets$incidence * (1 - th)
                & res$incidence < targets$incidence * (1 + th)
                & res$mortality > targets$mortality * (1 - th)
                & res$mortality < targets$mortality * (1 + th)
                & res$StoAratio > targets$StoARatio * (1 - th)
                & res$StoAratio < targets$StoARatio * (1 + th)
                & res$cal_r > targets$annual_decline * (1 - th)
                & res$cal_r < targets$annual_decline * (1 + th)
                )

# save the selected incides as "select2" and selected parameters as "res[select2,]"

# results from calibration falling within selected parameter range for chitwan and pyuthan
ch_selected <- lapply(results[select], function(x) x$y)
py_selected <- lapply(results[select2], function(x) x$y)

# save "ch_selected" and "py_selected"

# importing plotting data
# import calibration targets for both districts as ch_targ and py_targ

ch_targ_high <- ch_targ + 0.25 * ch_targ
ch_targ_low <- ch_targ - 0.25 * ch_targ

ch_target <- t(data.frame(round(ch_targ_low), ch_targ, round(ch_targ_high)))

nt <- 10
h <- 1/nt
tmax <- 22

select <- (nt + 1):(tmax*nt + 1)

ch_pop <- ch_prev <- ch_ltbi_prev <- ch_inc <- ch_mort <- ch_ASratio <- ch_prop_recent <- matrix(NA, nrow = length(ch_select_result), ncol = length(select))

for (jr in 1:length(ch_select_result)) {
  
  z <- ch_select_result[[jr]][select,]
  y <- ch_select_result[[jr]]
  
  ch_pop[jr,] <- z$t_pop
  ch_prev[jr,] <- z$prev
  ch_ltbi_prev[jr,] <- z$ltbi_prev
  ch_inc[jr,] <- z$incidence
  ch_mort[jr,] <- z$mortality
  ch_ASratio[jr,] <- z$ATB/z$STB
  ch_prop_recent[jr,] <- z$prop_recent
}

py_targ_high <- py_targ + 0.25 * py_targ
py_targ_low <- py_targ - 0.25 * py_targ

py_target <- t(data.frame(round(py_targ_low), py_targ, round(py_targ_high)))

py_pop <- py_prev <- py_ltbi_prev <- py_inc <- py_mort <- py_ASratio <- py_prop_recent <- matrix(NA, nrow = length(py_select_result), ncol = length(select))

for (jr in 1:length(py_select_result)) {
  
  z <- py_select_result[[jr]][select,]
  y <- py_select_result[[jr]]
  
  py_pop[jr,] <- z$t_pop
  py_prev[jr,] <- z$prev
  py_ltbi_prev[jr,] <- z$ltbi_prev
  py_inc[jr,] <- z$incidence
  py_mort[jr,] <- z$mortality
  py_ASratio[jr,] <- z$ATB/z$STB
  py_prop_recent[jr,] <- z$prop_recent
}

# colors

makeTransparent<-function(someColor, alpha=100)
{
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}

col1 <- 'hotpink4'
col2 <- 'skyblue3'
col3 <- 'grey20'
col4 <- 'orange3'
col5 <- "darkred"

colt1 <- makeTransparent(col1, alpha =10)
colt2 <- makeTransparent(col2,alpha=10)
colt3 <- makeTransparent(col3,alpha=10)
colt4 <- makeTransparent(col4,alpha=10)

colh1 <- makeTransparent(col1,alpha=100)
colh2 <- makeTransparent(col2,alpha=100)
colh3 <- makeTransparent(col3,alpha=100)
colh4 <- makeTransparent(col4,alpha=40)

cex.leg2 <- 0.6
cex.lab <- 0.7
cex.a <- 0.9

par(oma = c(0, 2, 2, 0))

par(mar = c(4, 4, 2, 1)) 

par(mfcol = c(4,2))

#############################################################

# TB Prevalence

plot(range(1,211+92), range(0,800), type = 'n', ylab = '', xlab = '', axes = F)
matlines(t(ch_prev), lty = 1, lwd = .5, col = colt1)
lines(apply(ch_prev, 2, median), lwd = 2.5, lty = 1, col = col3)
lines(apply(ch_prev, 2, median), lwd = 2, lty = 3, col = 'white')

axis(side = 1, at = seq(1, 211, by = 70), labels = c(2000, 2007, 2014, 2022), cex.axis = cex.a)
axis(side = 2, at = seq(0, 800, by = 100), las = 2, cex.axis = cex.a)
axis(side = 4, at = seq(0,800, by = 100)[-c(4:6)], las = 2, mgp = c(0, -6,-7), cex.axis = cex.a, labels = F)
axis(side = 4, at = c(ch_target$prev[1], ch_target$prev[2], ch_target$prev[3]), col.axis = col5, col = col5, las = 2, cex.axis = cex.a, mgp = c(0,-6,-7), cex.axis = 0.9)

mtext('Year', side = 1, line = 2.5, font = 2, adj = 0.35, cex = cex.lab)
mtext('Prevalence (per 100 000)', side = 2, line = 3, font = 2, cex = cex.lab)

breaks <- seq(0, 900, by = 5)
py1 <- hist(ch_prev[,211], breaks = breaks, plot = FALSE)
 
 for(j in 1:length(py1$counts)){
   rect(220, py1$breaks[j], 220 + py1$counts[j]*.3, py1$breaks[j+1], col=colh1, border=NA)
}
 
ch_prev.qq <- quantile(ch_prev[,211], probs = c(0.025, 0.5, 0.975))
 
segments(220, ch_prev.qq[2], 220+10, ch_prev.qq[2], lwd=1, col=col1)
segments(220, ch_prev.qq[1], 220+10, ch_prev.qq[1], lwd=1, col=col1, lty=1)
segments(220, ch_prev.qq[3], 220+10, ch_prev.qq[3], lwd=1, col=col1, lty=1)

text(220+15,ch_prev.qq[2],signif(ch_prev.qq[2],2), pos=3, cex=cex.leg2, offset=.1)
text(220+15,ch_prev.qq[1],signif(ch_prev.qq[1],2), pos=3, cex=cex.leg2, offset=.1)
text(220+15,ch_prev.qq[3],signif(ch_prev.qq[3],2), pos=3, cex=cex.leg2, offset=.1)

#######################################################

# TB Incidence

plot(range(1,211+90), range(0,700), type = 'n', ylab = '', xlab = '', axes = F)
matlines(t(ch_inc), lty = 1, lwd = .5, col = colt2)
lines(apply(ch_inc, 2, median), lwd = 2.5, lty = 1, col = col3)
lines(apply(ch_inc, 2, median), lwd = 2, lty = 3, col = 'white')

axis(side = 1, at = seq(1, 211, by = 70), labels = c(2000, 2007, 2014, 2022), cex.axis = cex.a)
axis(side = 2, at = seq(0, 700, by = 100), las = 2, cex.axis = cex.a)
axis(side = 4, at = seq(0,700, by = 100)[-c(3:5)], las = 2, mgp = c(0,-6, -7), cex.axis = cex.a, labels = F)
axis(side = 4, at = c(ch_target$incidence[1], ch_target$incidence[2], ch_target$incidence[3]), las = 2, col.axis = col5, col = col5, cex.axis = cex.a, mgp = c(0,-6,-7))

mtext('Year', side = 1, line = 2.5, adj = 0.35, cex = cex.lab, font = 2)
mtext('TB Incidence', side = 2, line = 4, font = 2, cex = cex.lab)
mtext('per 100 000) per year', side = 2, line = 3, font = 2, cex = cex.lab)

breaks <- seq(0, 700, by = 5)
py1 <- hist(ch_inc[,211], breaks = breaks, plot = FALSE)

for(j in 1:length(py1$counts)){
  rect(220, py1$breaks[j], 220 + py1$counts[j]*.3, py1$breaks[j+1], col=colh2, border=NA)
}

ch_inc.qq <- quantile(ch_inc[,211], probs = c(0.025, 0.5, 0.975))

segments(220, ch_inc.qq[2], 220+10, ch_inc.qq[2], lwd=1, col=col2)
segments(220, ch_inc.qq[1], 220+10, ch_inc.qq[1], lwd=1, col=col2, lty=1)
segments(220, ch_inc.qq[3], 220+10, ch_inc.qq[3], lwd=1, col=col2, lty=1)

text(220+15,ch_inc.qq[2],signif(ch_inc.qq[2],2), pos=3, cex=cex.leg2, offset=.1)
text(220+15,ch_inc.qq[1],signif(ch_inc.qq[1],2), pos=3, cex=cex.leg2, offset=.1)
text(220+15,ch_inc.qq[3],signif(ch_inc.qq[3],2), pos=3, cex=cex.leg2, offset=.1)

#############################################################

# TB Morality

plot(range(1,211+90), range(0,80), type = 'n', ylab = '', xlab = '', axes = F)
matlines(t(ch_mort), lty = 1, lwd = .5, col = colt3)
lines(apply(ch_mort, 2, median), lwd = 2.5, lty = 1, col = col3)
lines(apply(ch_mort, 2, median), lwd = 2, lty = 3, col = 'white')

axis(side = 1, at = seq(1, 211, by = 70), labels = c(2000, 2007, 2014, 2022), cex.axis = cex.a)
axis(side = 2, at = seq(0, 80, by = 10), las = 2, cex.axis = cex.a)
axis(side = 4, at = seq(0, 80, by = 10)[-c(4,5)], las = 2, mgp = c(0,-6,-7), cex.axis = cex.a, labels = F)
axis(side = 4, at = c(ch_target$mortality[1], ch_target$mortality[2], ch_target$mortality[3]), las = 2, col.axis = col5, col = col5, cex.axis = cex.a, mgp = c(0,-6,-7))

mtext('Year', side = 1, line = 2.5, adj = 0.35, cex = cex.lab, font = 2)
mtext('TB Mortality', side = 2, line = 4, font = 2, cex = cex.lab)
mtext('per 100 000) per year', side = 2, line = 3, font = 2, cex = cex.lab)

breaks <- seq(0, 80, by = 1)
py1 <- hist(ch_mort[,211], breaks = breaks, plot = FALSE)

for(j in 1:length(py1$counts)){
  rect(220, py1$breaks[j], 220 + py1$counts[j]*.3, py1$breaks[j+1], col=colh3, border=NA)
}

ch_mort.qq <- quantile(ch_mort[,211], probs = c(0.025, 0.5, 0.975))

segments(220, ch_mort.qq[2], 220+10, ch_mort.qq[2], lwd=1, col=col3)
segments(220, ch_mort.qq[1], 220+10, ch_mort.qq[1], lwd=1, col=col3, lty=1)
segments(220, ch_mort.qq[3], 220+10, ch_mort.qq[3], lwd=1, col=col3, lty=1)

text(220+15,ch_mort.qq[2],signif(ch_mort.qq[2],2), pos=3, cex=cex.leg2, offset=.1)
text(220+15,ch_mort.qq[1],signif(ch_mort.qq[1],2), pos=3, cex=cex.leg2, offset=.1)
text(220+15,ch_mort.qq[3],signif(ch_mort.qq[3],2), pos=3, cex=cex.leg2, offset=.1)

#############################################################

# LTBI Prevalence

plot(range(1,211+90), range(0,70), type = 'n', ylab = '', xlab = '', axes = F)
matlines(t(ch_ltbi_prev), lty = 1, lwd = .5, col = colt4)
lines(apply(ch_ltbi_prev, 2, median), lwd = 2.5, lty = 1, col = col3)
lines(apply(ch_ltbi_prev, 2, median), lwd = 2, lty = 3, col = 'white')

axis(side = 1, at = seq(1, 211, by = 70), labels = c(2000, 2007, 2014, 2022), cex.axis = cex.a)
axis(side = 2, at = seq(0, 70, by = 10), las = 2, cex.axis = cex.a)
axis(side = 4, at = seq(0, 70, by = 10), las = 2, mgp = c(0,-6, -7), cex.axis = cex.a)

mtext('Year', side = 1, line = 2.5, adj = 0.35, cex = cex.lab, font = 2)
mtext('LTBI Prevalence (%)', side = 2, line = 3, font = 2, cex = cex.lab)

breaks <- seq(0, 70, by = 1)
py1 <- hist(ch_ltbi_prev[,211], breaks = breaks, plot = FALSE)

for(j in 1:length(py1$counts)){
  rect(220, py1$breaks[j], 220 + py1$counts[j]*.3, py1$breaks[j+1], col=colh4, border=NA)
}

ch_ltbi_prev.qq <- quantile(ch_ltbi_prev[,211], probs = c(0.025, 0.5, 0.975))

segments(220, ch_ltbi_prev.qq[2], 220+10, ch_ltbi_prev.qq[2], lwd=1, col=col4)
segments(220, ch_ltbi_prev.qq[1], 220+10, ch_ltbi_prev.qq[1], lwd=1, col=col4, lty=1)
segments(220, ch_ltbi_prev.qq[3], 220+10, ch_ltbi_prev.qq[3], lwd=1, col=col4, lty=1)

text(220+15,ch_ltbi_prev.qq[2],signif(ch_ltbi_prev.qq[2],2), pos=3, cex=cex.leg2, offset=.1)
text(220+15,ch_ltbi_prev.qq[1],signif(ch_ltbi_prev.qq[1],2), pos=3, cex=cex.leg2, offset=.1)
text(220+15,ch_ltbi_prev.qq[3],signif(ch_ltbi_prev.qq[3],2), pos=3, cex=cex.leg2, offset=.1)


#############################################################
# Pyuthan

# TB Prevalence

plot(range(1,221+105), range(0,800), type = 'n', ylab = '', xlab = '', axes = F)

matlines(t(py_prev), lty = 1, lwd = .5, col = colt1)
lines(apply(py_prev, 2, median), lwd = 2.5, lty = 1, col = col3)
lines(apply(py_prev, 2, median), lwd = 2, lty = 3, col = 'white')

axis(side = 1, at = seq(1, 211, by = 70), labels = c(2000, 2007, 2014, 2022), cex.axis = cex.a)
axis(side = 2, at = seq(0, 800, by = 100), las = 2, cex.axis = cex.a)
axis(side = 4, at = seq(0,800, by = 100)[-(4:6)], las = 2, mgp = c(0, -6,-7), cex.axis = cex.a, labels = F)
axis(side = 4, at = c(py_target$prev[1], py_target$prev[2], py_target$prev[3]), col.axis = col5, col = col5, las = 2, cex.axis = cex.a, mgp = c(0,-6,-7))

mtext('Year', side = 1, line = 2.5, adj = 0.35, font = 2, cex = cex.lab)
mtext('TB Prevalence (per 100 000)', side = 2, line = 3, font = 2, cex = cex.lab)

breaks <- seq(0, 800, by = 5)
py1 <- hist(py_prev[,211], breaks = breaks, plot = FALSE)
 
 for(j in 1:length(py1$counts)){
   rect(220, py1$breaks[j], 220 + py1$counts[j]*.3, py1$breaks[j+1], col=colh1, border=NA)
}
 
py_prev.qq <- quantile(py_prev[,211], probs = c(0.025, 0.5, 0.975))
 
segments(220, py_prev.qq[2], 220+10, py_prev.qq[2], lwd=1, col=col1)
segments(220, py_prev.qq[1], 220+10, py_prev.qq[1], lwd=1, col=col1, lty=1)
segments(220, py_prev.qq[3], 220+10, py_prev.qq[3], lwd=1, col=col1, lty=1)

text(220+15,py_prev.qq[2],signif(py_prev.qq[2],2), pos=3, cex=cex.leg2, offset=.1)
text(220+15,py_prev.qq[1],signif(py_prev.qq[1],2), pos=3, cex=cex.leg2, offset=.1)
text(220+15,py_prev.qq[3],signif(py_prev.qq[3],2), pos=3, cex=cex.leg2, offset=.1)

#######################################################

# TB Incidence
plot(range(1,221+105), range(0,700), type = 'n', ylab = '', xlab = '', axes = F)
matlines(t(py_inc), lty = 1, lwd = .5, col = colt2)
lines(apply(py_inc, 2, median), lwd = 2.5, lty = 1, col = col3)
lines(apply(py_inc, 2, median), lwd = 2, lty = 3, col = 'white')

axis(side = 1, at = seq(1, 211, by = 70), labels = c(2000, 2007, 2014, 2022), cex.axis = cex.a)
axis(side = 2, at = seq(0, 700, by = 100), las = 2, cex.axis = cex.a)
axis(side = 4, at = seq(0,700, by = 100)[-c(3:5)], las = 2, mgp = c(0,-6, -7), cex.axis = cex.a, labels = F)
axis(side = 4, at = c(py_target$incidence[1], py_target$incidence[2], py_target$incidence[3]), las = 2, col.axis = col5, col = col5, cex.axis = cex.a, mgp = c(0,-6,-7))

mtext('Year', side = 1, line = 2.5, adj = 0.35, font = 2, cex = cex.lab)
mtext('TB Incidence', side = 2, line = 4, font = 2, cex = cex.lab)
mtext('per 100 000) per year', side = 2, line = 3, font = 2, cex = cex.lab)

breaks <- seq(0, 700, by = 5)
py1 <- hist(py_inc[,211], breaks = breaks, plot = FALSE)

for(j in 1:length(py1$counts)){
  rect(220, py1$breaks[j], 220 + py1$counts[j]*.3, py1$breaks[j+1], col=colh2, border=NA)
}

py_inc.qq <- quantile(py_inc[,211], probs = c(0.025, 0.5, 0.975))

segments(220, py_inc.qq[2], 220+10, py_inc.qq[2], lwd=1, col=col2)
segments(220, py_inc.qq[1], 220+10, py_inc.qq[1], lwd=1, col=col2, lty=1)
segments(220, py_inc.qq[3], 220+10, py_inc.qq[3], lwd=1, col=col2, lty=1)

text(220+15,py_inc.qq[2],signif(py_inc.qq[2],2), pos=3, cex=cex.leg2, offset=.1)
text(220+15,py_inc.qq[1],signif(py_inc.qq[1],2), pos=3, cex=cex.leg2, offset=.1)
text(220+15,py_inc.qq[3],signif(py_inc.qq[3],2), pos=3, cex=cex.leg2, offset=.1)

###########################################################

# TB Morality

plot(range(1,221+105), range(0,80), type = 'n', ylab = '', xlab = '', axes = F)
matlines(t(py_mort), lty = 1, lwd = .5, col = colt3)
lines(apply(py_mort, 2, median), lwd = 2.5, lty = 1, col = col3)
lines(apply(py_mort, 2, median), lwd = 2, lty = 3, col = 'white')

axis(side = 1, at = seq(1, 211, by = 70), labels = c(2000, 2007, 2014, 2022), cex.axis = cex.a)
axis(side = 2, at = seq(0, 80, by = 10), las = 2, cex.axis = cex.a)
axis(side = 4, at = seq(0, 80, by = 10)[-c(4,5)], las = 2, mgp = c(0,-6,-7), cex.axis = cex.a, labels = F)
axis(side = 4, at = c(py_target$mortality[1], py_target$mortality[2], py_target$mortality[3]), las = 2, col.axis = col5, col = col5, cex.axis = cex.a, mgp = c(0,-6,-7))

mtext('Year', side = 1, line = 2.5, adj = 0.35, font = 2, cex = cex.lab)
mtext('TB Mortality', side = 2, line = 4, font = 2, cex = cex.lab)
mtext('per 100 000) per year', side = 2, line = 3, font = 2, cex = cex.lab)

breaks <- seq(0, 80, by = 1)
py1 <- hist(py_mort[,211], breaks = breaks, plot = FALSE)

for(j in 1:length(py1$counts)){
  rect(220, py1$breaks[j], 220 + py1$counts[j]*.3, py1$breaks[j+1], col=colh3, border=NA)
}

py_mort.qq <- quantile(py_mort[,211], probs = c(0.025, 0.5, 0.975))

segments(220, py_mort.qq[2], 220+10, py_mort.qq[2], lwd=1, col=col3)
segments(220, py_mort.qq[1], 220+10, py_mort.qq[1], lwd=1, col=col3, lty=1)
segments(220, py_mort.qq[3], 220+10, py_mort.qq[3], lwd=1, col=col3, lty=1)

text(220+15,py_mort.qq[2],signif(py_mort.qq[2],2), pos=3, cex=cex.leg2, offset=.1)
text(220+15,py_mort.qq[1],signif(py_mort.qq[1],2), pos=3, cex=cex.leg2, offset=.1)
text(220+15,py_mort.qq[3],signif(py_mort.qq[3],2), pos=3, cex=cex.leg2, offset=.1)

#############################################################

# LTBI Prevalence

plot(range(1,221+105), range(0,70), type = 'n', ylab = '', xlab = '', axes = F)
matlines(t(py_ltbi_prev), lty = 1, lwd = .5, col = colt4)
lines(apply(py_ltbi_prev, 2, median), lwd = 2.5, lty = 1, col = col3)
lines(apply(py_ltbi_prev, 2, median), lwd = 2, lty = 3, col = 'white')

axis(side = 1, at = seq(1, 211, by = 70), labels = c(2000, 2007, 2014, 2022), cex.axis = cex.a)
axis(side = 2, at = seq(0, 70, by = 10), las = 2, cex.axis = cex.a)
axis(side = 4, at = seq(0, 70, by = 10), las = 2, mgp = c(0,-6,-7), cex.axis = cex.a)

mtext('Year', side = 1, line = 2.5, adj = 0.35, font = 2, cex = cex.lab)
mtext('LTBI Prevalence (%)', side = 2, line = 3, font = 2, cex = cex.lab)

breaks <- seq(0, 70, by = 1)
py1 <- hist(py_ltbi_prev[,211], breaks = breaks, plot = FALSE)

for(j in 1:length(py1$counts)){
  rect(220, py1$breaks[j], 220 + py1$counts[j]*.3, py1$breaks[j+1], col=colh4, border=NA)
}

py_ltbi_prev.qq <- quantile(py_ltbi_prev[,211], probs = c(0.025, 0.5, 0.975))

segments(220, py_ltbi_prev.qq[2], 220+10, py_ltbi_prev.qq[2], lwd=1, col=col4)
segments(220, py_ltbi_prev.qq[1], 220+10, py_ltbi_prev.qq[1], lwd=1, col=col4, lty=1)
segments(220, py_ltbi_prev.qq[3], 220+10, py_ltbi_prev.qq[3], lwd=1, col=col4, lty=1)

text(220+15,py_ltbi_prev.qq[2],signif(py_ltbi_prev.qq[2],2), pos=3, cex=cex.leg2, offset=.1)
text(220+15,py_ltbi_prev.qq[1],signif(py_ltbi_prev.qq[1],2), pos=3, cex=cex.leg2, offset=.1)
text(220+15,py_ltbi_prev.qq[3],signif(py_ltbi_prev.qq[3],2), pos=3, cex=cex.leg2, offset=.1)

mtext(side = 3, "Chitwan", line = -1, outer = T, adj = 0.20, font = 2)
mtext(side = 3, 'Pyuthan', line = -1, outer = T, adj = 0.75, font = 2)
mtext(side = 3, "A", line = -1, outer = T, adj = 0.03, font = 2)
mtext(side = 3, "B", line = -1, outer = T, adj = 0.54, font = 2)

mtext(side = 3, "C", line = -20, outer = T, adj = 0.03, font = 2)
mtext(side = 3, "D", line = -20, outer = T, adj = 0.54, font = 2)

mtext(side = 3, "E", line = -38, outer = T, adj = 0.03, font = 2)
mtext(side = 3, "F", line = -38, outer = T, adj = 0.54, font = 2)

mtext(side = 3, "G", line = -57, outer = T, adj = 0.03, font = 2)
mtext(side = 3, "H", line = -57, outer = T, adj = 0.54, font = 2)

# end
