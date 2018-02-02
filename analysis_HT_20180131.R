#################
#               #
#  HT INSIGHT   #
#  31 JAN 2018  # 
#               #
#################



rm(list = ls())

library(lme4)
library(MASS)


dat <- read.csv("/Users/nsircar/Dropbox/Papers/HTanalysis/PRS_ADR_0914_v2.csv", stringsAsFactors =  F)

attend <- as.numeric(strsplit(dat$Attendance, "%")) ## Attendance
debates <-  as.numeric(dat$Debates) ## Participation in Debates
questions <- as.numeric(dat$Questions) ## Questions Asked
attend90 <- as.numeric(attend > 90) ## Attendance at least 90% of the time -- Regular Attendance
pg <- as.numeric(dat$Educational.qualifications %in% c("Post Graduate", "Doctorate")) # Postgraduate Degree
crim <- as.numeric(dat$Serious_CrimCase == 1) ## Indicator of Major Criminal Cases
state <- as.numeric(as.factor(dat$State)) ## State
logmoveable <- log10(dat$Moveable) ## Log (base 10) of moveable assets
keepval <- which(logmoveable > 0 & !is.na(logmoveable)) ## Subset of data to analyze


## REGRESSIONS

fit <- lmer(attend[keepval]~logmoveable[keepval] + crim[keepval] + pg[keepval] + (1|state[keepval])) ## Basic Regression w/ Attendance as Dep. Var (Multilevel)

fit.simple <- glm(attend90[keepval]~logmoveable[keepval] + crim[keepval] + pg[keepval], family = binomial(link = "logit")) ## Non-Multilevel Regression for Regular Attendance

fit2 <- glmer(attend90[keepval]~logmoveable[keepval] + crim[keepval] + pg[keepval] + (1|state[keepval]), family = binomial(link = "logit")) ## State Multilevel Regression -- Regular Attendance

fit3 <- glmer(attend90[keepval]~logmoveable[keepval] + crim[keepval] + pg[keepval] + (1|state[keepval]) + (1|dat$Political.party[keepval]), family = binomial(link = "logit")) ## State + Party Multilevel Regression -- Regular Attendance


set.seed(123456)

coef.sims <- mvrnorm(1000, summary(fit3)$coef[,1], summary(fit3)$vcov)  ## Simulation from Last Model



invlogit <- function(x) 1/(1 + exp(-x))  # Inverse of Logit Function

vec <- seq(5, 8, .01) ## Values over which Simulated (Log) Moveable Assets Displayed

assetval <- cbind(rep(1, length(vec)), vec, rep(0, length(vec)), rep(0, length(vec))) ## X values for Asset Simulation -- No PG degree, no serious case
crimval <- cbind(rep(1,2), rep(mean(logmoveable[keepval]), 2), c(0,1), rep(0, 2)) ## X values for Crim Case Simulation -- Mean moveable assets, no PG degree
pgval <- cbind(rep(1,2), rep(mean(logmoveable[keepval]), 2), rep(0, 2),  c(0,1)) ## X values for PG degree Simulation -- Mean moveable assets, no serious case



## GENERATE FIGURE 1


vals <- c(apply(invlogit((pgval %*% t(coef.sims))), 1, mean), apply(invlogit((crimval %*% t(coef.sims))), 1, mean)) ## Predicted Values from Simulations
vals.ci <- cbind(apply(invlogit((pgval %*% t(coef.sims))), 1, quantile, c(.05, .95)), apply(invlogit((crimval %*% t(coef.sims))), 1, quantile, c(.05, .95))) ## 90% intervals from simulations
 
pdf("/Users/nsircar/Dropbox/Papers/HTanalysis/attend90_fig1.pdf", width=8, height=6)

par(mar = rep(0,4), mai = rep(0,4))
plot(0,0, xlim = c(-.7,7), ylim = c(-.05, .4), ann = F, axes = F, type = "n")
x.coords <- barplot(vals, col = c(rgb(1,0,0,.5), rgb(1,0,0,.5), rgb(0,0,1,.5), rgb(0,0,1,.5)), space = c(.5,.5,1,.5), axes = F, add = T )
axis(2, at=seq(0,.4,.1), labels = seq(0,.4,.1), pos = 0, col = gray(.5), col.axis = gray(.5))

cats <- c("Graduate\nor Less", "Post\nGraduate", "Not Major\nCriminal", "Major\nCriminal")

cols <- c(rgb(.8,0,0, .5), rgb(.8,0,0, .5), rgb(0,0,.8, .5), rgb(0,0,.8, .5))
for (i in 1:4){
  
  text(x.coords[i],-.03, cats[i], cex = 1.2)
  text(x.coords[i], vals[i], round(vals[i], 2), pos = 3, cex = 1.8)
  
  arrows(x.coords[i], vals.ci[1,i], x.coords[i], vals.ci[2,i], col = cols[i], code = 3, angle = 90, length = .07, lwd = 1)
  
  
}

text(-.68, .2, "Estimated Probability of 90%+ Attendance", srt=90, cex = 1.2, col = gray(.5))

dev.off()

## GENERATE FIGURE 2

vals <- apply(invlogit((assetval %*% t(coef.sims))), 1, mean) ## Predicted Value from Simulations
vals.ci <- apply(invlogit((assetval %*% t(coef.sims))), 1, quantile, c(.05, .95)) ## 90% Intervals from Simulations

colsline <- rgb(50/255,100/255,0,.2)
colsfill <- rgb(50/255,100/255,0,.8)


pdf("/Users/nsircar/Dropbox/Papers/HTanalysis/attend90_fig2.pdf", width=8, height=6)

par(mar = rep(0,4), mai = rep(0,4))
plot(0,0, xlim = c(min(vec) - .5, max(vec)+.1), ylim = c(-.1, .5), ann =F, axes = F, type = "n")
axis(1, at = seq(5, 8, 1), labels = c("1 Lakh", "10 Lakh", "1 Crore", "10 Crore"),  pos=0)
axis(2, at = seq(0,.5,.1), pos = min(vec))

polygon(c(vec, rev(vec)), c(vals.ci[1,], rev(vals.ci[2,])), col = colsline,border = NA   )  ## Colored Interval Band
points(vec, vals, lwd = 2, type = "l", col = colsfill)  ## Predicted Value Curve

text(6.5, -.09, "Reported Candidate Asset Wealth (Moveable)", cex = 1.2)
text(4.55, .25, "Estimated Probability of 90%+ Attendance", cex = 1.2, srt = 90)
rug(logmoveable[logmoveable >= 5 & logmoveable <= 8], pos = 0)  ## Rug Plot denoting (log) asset location of each observation

dev.off()



## Analysis of Predicted Probability by Wealth Levels

## Predicted Probability of MPs with 10-50 Lakh (no PG degree and no serious criminal cases)

xval <- logmoveable[keepval][log10(1000000) <= logmoveable[keepval] & log10(5000000) >= logmoveable[keepval]]
xpred <- cbind(rep(1, length(xval)), xval, rep(0, length(xval)), rep(0, length(xval)))

mean(colMeans(invlogit((xpred %*% t(coef.sims)))))

## Predicted Probability of Crorepati MPs (no PG degree and no serious criminal cases)

xval <- logmoveable[keepval][log10(10000000) <= logmoveable[keepval] ]
xpred <- cbind(rep(1, length(xval)), xval, rep(0, length(xval)), rep(0, length(xval)))

mean(colMeans(invlogit((xpred %*% t(coef.sims)))))



## Parties that are "big" analysis

bigparties <- c("Communist Party of India (Marxist)", "Janata Dal (United)", "Bahujan Samaj Party", "Samajwadi Party", "Bharatiya Janata Party", "Indian National Congress")

mean(dat$Political.party[keepval] %in% bigparties) ## Share of MPs in big parties

mean(attend[keepval][dat$Political.party[keepval] %in% bigparties]) ## Attendance from big party
mean(attend[keepval][!(dat$Political.party[keepval] %in% bigparties)]) ## Attendance from small party
