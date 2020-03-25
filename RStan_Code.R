## Code to accompany dissertation paper entitled "Bayesian Regression via RStan". 
## The data used can be requested at http://www.share-project.org/data-access/user-registration.html

## Load required packages ####

library("rstanarm")
library("bayesplot")
library("ggplot2")
library("broom")
library("tidyverse")

## Load data ####

load("data/easySHARE_rel7_0_0.rda")

## Create cognitive score ####

cogheathvars <- c("recall_1","recall_2","orienti","numeracy_1","numeracy_2")
coghealth = easySHARE_rel7_0_0[cogheathvars]
# issues with numeracy measures (in latter waves only one score recorded - take avg if both availble)
num_combined = rep(NA,dim(coghealth)[1])
num_combined[coghealth$numeracy_1 >= 0 & coghealth$numeracy_2 >= 0] = (coghealth$numeracy_1[coghealth$numeracy_1 >= 0 & coghealth$numeracy_2 >= 0] + coghealth$numeracy_2[coghealth$numeracy_1 >= 0 & coghealth$numeracy_2 >= 0] )/2
num_combined[coghealth$numeracy_1 >= 0 & coghealth$numeracy_2  < 0] = coghealth$numeracy_1[coghealth$numeracy_1 >= 0 & coghealth$numeracy_2 < 0]
num_combined[coghealth$numeracy_1 < 0 & coghealth$numeracy_2 >= 0] = coghealth$numeracy_2[coghealth$numeracy_1 < 0 & coghealth$numeracy_2 >= 0]

cogscore = rep(NA,dim(coghealth)[1])
ind = coghealth$recall_1 >= 0 & coghealth$recall_2 >= 0 & coghealth$orienti >= 0 & !is.na(num_combined)
cogscore[ind] = coghealth$recall_1[ind] + coghealth$recall_2[ind] + (4 - coghealth$orienti[ind]) + num_combined[ind]

## Restrict data ####
datavars <- c("mergeid","wave","country","female","age","eduyears_mod","eurod","bmi","smoking","br015_","ac002dno")
data <- easySHARE_rel7_0_0[datavars]
data$cogscore = cogscore

filter.allcountries.allwaves <- data$age!="-15"&
  data$eduyears_mod!="-12"&data$eduyears_mod!="-3"&  data$eduyears_mod!="-15"&
  data$eurod!="-15"&
  data$bmi!="-15"&data$bmi!="-12"&data$bmi!="-3"&
  data$smoking!="-15"&data$smoking!="-12"&
  data$br015_!="-15"&data$br015_!="-12"&
  data$ac002dno!="-15"&data$ac002dno!="-13"&
  !is.na(data$cogscore)

data.allcountries.allwaves = data.frame(data[filter.allcountries.allwaves,])
names(data.allcountries.allwaves)=names(data)
table(data.allcountries.allwaves$country,data.allcountries.allwaves$wave)

# Filter for each country
filter.Italy <- data.allcountries.allwaves$country=="16"
data.Italy = data.frame(data.allcountries.allwaves[filter.Italy,])
filter.Austria <- data.allcountries.allwaves$country=="11"
data.Austria = data.frame(data.allcountries.allwaves[filter.Austria,])
filter.Germany <- data.allcountries.allwaves$country=="12"
data.Germany = data.frame(data.allcountries.allwaves[filter.Germany,])
filter.Sweden <- data.allcountries.allwaves$country=="13"
data.Sweden = data.frame(data.allcountries.allwaves[filter.Sweden,])
filter.Netherlands <- data.allcountries.allwaves$country=="14"
data.Netherlands = data.frame(data.allcountries.allwaves[filter.Netherlands,])
filter.Spain <- data.allcountries.allwaves$country=="15"
data.Spain = data.frame(data.allcountries.allwaves[filter.Spain,])
filter.France <- data.allcountries.allwaves$country=="17"
data.France = data.frame(data.allcountries.allwaves[filter.France,])
filter.Denmark <- data.allcountries.allwaves$country=="18"
data.Denmark = data.frame(data.allcountries.allwaves[filter.Denmark,])
filter.Greece <- data.allcountries.allwaves$country=="19"
data.Greece = data.frame(data.allcountries.allwaves[filter.Greece,])
filter.Switzerland <- data.allcountries.allwaves$country=="20"
data.Switzerland = data.frame(data.allcountries.allwaves[filter.Switzerland,])
filter.Belgium <- data.allcountries.allwaves$country=="23"
data.Belgium = data.frame(data.allcountries.allwaves[filter.Belgium,])
filter.Israel <- data.allcountries.allwaves$country=="25"
data.Israel = data.frame(data.allcountries.allwaves[filter.Israel,])

# Histograms to compare populations
hist(data.allcountries.allwaves$cogscore,xlab = "Cognitive Score", main = "Whole population")
hist(data.Austria$cogscore,xlab = "Cognitive Score")
hist(data.Belgium$cogscore,xlab = "Cognitive Score",main="Belgium")
hist(data.Denmark$cogscore,xlab = "Cognitive Score")
hist(data.France$cogscore,xlab = "Cognitive Score")
hist(data.Germany$cogscore,xlab = "Cognitive Score")
hist(data.Greece$cogscore,xlab = "Cognitive Score")
hist(data.Israel$cogscore,xlab = "Cognitive Score")
hist(data.Italy$cogscore,xlab = "Cognitive Score", main = "Italy")
hist(data.Netherlands$cogscore,xlab = "Cognitive Score")
hist(data.Spain$cogscore,xlab = "Cognitive Score")
hist(data.Sweden$cogscore,xlab = "Cognitive Score")
hist(data.Switzerland$cogscore,xlab = "Cognitive Score")

#-------------------------#
##-------## SLM ##-----####
## Italy | All variables ##
#-------------------------#

# Specify the model
SingleLevelModel.ita <- stan_glm(cogscore ~ eduyears_mod + age + bmi + smoking + eurod + ac002dno + br015_ + female, data = data.Italy)

prior_summary(SingleLevelModel.ita)
# See help('prior_summary.stanreg') for more details

# Check sampling quality
# (a) numerical
summarySingleLevelModel.ita <- summary(SingleLevelModel.ita)
print(summarySingleLevelModel.ita[, c("mcse","Rhat","n_eff")])

# Some effective sample sizes too small so increase iterations
SingleLevelModel.ita <- stan_glm(cogscore ~ eduyears_mod + age + bmi + smoking + eurod + ac002dno + br015_ + female, data = data.Italy, iter = 3000)

prior_summary(SingleLevelModel.ita)
# See help('prior_summary.stanreg') for more details

# Check sampling quality
# (a) numerical
summarySingleLevelModel.ita <- summary(SingleLevelModel.ita)
print(summarySingleLevelModel.ita[, c("mcse","Rhat","n_eff")])

# (b) visual
#     (i) trace plots 
trace.ita <- plot(SingleLevelModel.ita, "trace",     pars = c("eduyears_mod","age","bmi","smoking","eurod","ac002dno","br015_","female"))

plot(trace.ita)

#     (ii) shinystan
launch_shinystan(SingleLevelModel.ita)

# Posterior predictive check
pp_dist1.ita <- pp_check(SingleLevelModel.ita, nreps = 500)  + xlab("Cognitive Health Score")
plot(pp_dist1.ita)

# Summarize and interpret results
# Confidence intervals
plot(SingleLevelModel.ita,"hist",pars = c("eduyears_mod","age","bmi","smoking","eurod","ac002dno","br015_","female"))
#All look symmetrical so central intervals are acceptable
posterior_interval(SingleLevelModel.ita, prob = 0.95,type = "central", pars = c("eduyears_mod","age","bmi","smoking","eurod","ac002dno","br015_","female"))

# Generate slope/regression charts based on confidence intervals
# Extract the (post-warmup) posterior draws
posterior1.ita <- as.matrix(SingleLevelModel.ita)
colnames(posterior1.ita)
means1.ita <- colMeans(posterior1.ita[,-10]) #-10 removes sigma as we are not interested in this variable

# Take random 100 posterior draws of intercept and slope
# 100 isn't special, but enough to show uncertainty without making the graph unreadable
betas.ita <- posterior1.ita[sample(nrow(posterior1.ita), 100), 1:9]

xmean.ita = c(1,mean(data.italy$eduyears_mod),mean(data.italy$age),mean(data.italy$bmi),mean(data.italy$smoking),mean(data.italy$eurod),mean(data.italy$ac002dno),mean(data.italy$br015_),mean(data.italy$female))
xmean.ita = matrix(xmean.ita,length(xmean.ita),1)

# Plot regression lines implied by the betas
blues <- color_scheme_get("brightblue")

index = 2 # Changing the index enables us to easily change the variable being considered in the plots
mod1p1.ita.eduyears_mod <- ggplot(data.italy, aes(x = eduyears_mod, y = cogscore)) +
  geom_point(color = "gray30") +
  geom_abline(
    intercept = betas.ita[, -index]%*%xmean.ita[-index], 
    slope = betas.ita[, index],
    color = blues[[2]], 
    size = 0.15, 
    alpha = 0.5
  ) +
  geom_abline(
    intercept = sum(means1.ita[-index]*xmean.ita[-index]), 
    slope = means1.ita[index],
    size = 1.25, 
    color = blues[[6]]
  ) +
  ylim(0, 25)
index = 3
mod1p1.ita.age <- ggplot(data.italy, aes(x = age, y = cogscore)) +
  geom_point(color = "gray30") +
  geom_abline(
    intercept = betas.ita[, -index]%*%xmean.ita[-index], 
    slope = betas.ita[, index],
    color = blues[[2]], 
    size = 0.15, 
    alpha = 0.5
  ) +
  geom_abline(
    intercept = sum(means1.ita[-index]*xmean.ita[-index]), 
    slope = means1.ita[index],
    size = 1.25, 
    color = blues[[6]]
  ) +
  ylim(0, 25)
index = 4
mod1p1.ita.bmi <- ggplot(data.italy, aes(x = bmi, y = cogscore)) +
  geom_point(color = "gray30") +
  geom_abline(
    intercept = betas.ita[, -index]%*%xmean.ita[-index], 
    slope = betas.ita[, index],
    color = blues[[2]], 
    size = 0.15, 
    alpha = 0.5
  ) +
  geom_abline(
    intercept = sum(means1.ita[-index]*xmean.ita[-index]), 
    slope = means1.ita[index],
    size = 1.25, 
    color = blues[[6]]
  ) +
  ylim(0, 25)
index = 5
mod1p1.ita.smoking <- ggplot(data.italy, aes(x = smoking, y = cogscore)) +
  geom_point(color = "gray30") +
  geom_abline(
    intercept = betas.ita[, -index]%*%xmean.ita[-index], 
    slope = betas.ita[, index],
    color = blues[[2]], 
    size = 0.15, 
    alpha = 0.5
  ) +
  geom_abline(
    intercept = sum(means1.ita[-index]*xmean.ita[-index]), 
    slope = means1.ita[index],
    size = 1.25, 
    color = blues[[6]]
  ) +
  ylim(0, 25)
index = 6
mod1p1.ita.eurod <- ggplot(data.italy, aes(x = eurod, y = cogscore)) +
  geom_point(color = "gray30") +
  geom_abline(
    intercept = betas.ita[, -index]%*%xmean.ita[-index], 
    slope = betas.ita[, index],
    color = blues[[2]], 
    size = 0.15, 
    alpha = 0.5
  ) +
  geom_abline(
    intercept = sum(means1.ita[-index]*xmean.ita[-index]), 
    slope = means1.ita[index],
    size = 1.25, 
    color = blues[[6]]
  ) +
  ylim(0, 25)
index = 7
mod1p1.ita.ac002dno <- ggplot(data.italy, aes(x = ac002dno, y = cogscore)) +
  geom_point(color = "gray30") +
  geom_abline(
    intercept = betas.ita[, -index]%*%xmean.ita[-index], 
    slope = betas.ita[, index],
    color = blues[[2]], 
    size = 0.15, 
    alpha = 0.5
  ) +
  geom_abline(
    intercept = sum(means1.ita[-index]*xmean.ita[-index]), 
    slope = means1.ita[index],
    size = 1.25, 
    color = blues[[6]]
  ) +
  ylim(0, 25)
index = 8
mod1p1.ita.br015_ <- ggplot(data.italy, aes(x = br015_, y = cogscore)) +
  geom_point(color = "gray30") +
  geom_abline(
    intercept = betas.ita[, -index]%*%xmean.ita[-index], 
    slope = betas.ita[, index],
    color = blues[[2]], 
    size = 0.15, 
    alpha = 0.5
  ) +
  geom_abline(
    intercept = sum(means1.ita[-index]*xmean.ita[-index]), 
    slope = means1.ita[index],
    size = 1.25, 
    color = blues[[6]]
  ) +
  ylim(0, 25)
index = 9
mod1p1.ita.female <- ggplot(data.italy, aes(x = female, y = cogscore)) +
  geom_point(color = "gray30") +
  geom_abline(
    intercept = betas.ita[, -index]%*%xmean.ita[-index], 
    slope = betas.ita[, index],
    color = blues[[2]], 
    size = 0.15, 
    alpha = 0.5
  ) +
  geom_abline(
    intercept = sum(means1.ita[-index]*xmean.ita[-index]), 
    slope = means1.ita[index],
    size = 1.25, 
    color = blues[[6]]
  ) +
  ylim(0, 25)

# Plot regression charts described above
regression.ita <- bayesplot_grid(
  mod1p1.ita.eduyears_mod,mod1p1.ita.age,mod1p1.ita.bmi,mod1p1.ita.smoking,mod1p1.ita.eurod,mod1p1.ita.ac002dno,mod1p1.ita.br015_,mod1p1.ita.female,
  #xlim = c(-30,60), ylim = c(0,0.125),
  legends = FALSE,
  subtitles = c("Years in education","Age","BMI","Current smoker","Score on depression scale","ac002dno","br015_","Gender"),
  grid_args = list(ncol = 2))

plot(regression.ita)

#---------------------------#
##--------## SLM ##------####
## Belgium | All variables ##
#---------------------------#

# Specify the model
SingleLevelModel.bel <- stan_glm(cogscore ~ eduyears_mod + age + bmi + smoking + eurod + ac002dno + br015_ + female, data = data.Belgium)

prior_summarySingleLevelModel.bel 
# See help('prior_summary.stanreg') for more details

# Check sampling quality
# (a) numerical
summarySingleLevelModel.bel <- summary(SingleLevelModel.bel )
print(summarySingleLevelModel.bel[, c("mcse","Rhat","n_eff")])

# Some effective sample sizes too small so increase iterations
SingleLevelModel.bel <- stan_glm(cogscore ~ eduyears_mod + age + bmi + smoking + eurod + ac002dno + br015_ + female, data = data.Belgium, iter = 3000)

prior_summary(SingleLevelModel.bel)
# See help('prior_summary.stanreg') for more details

# Check sampling quality
# (a) numerical
summarySingleLevelModel.bel <- summary(SingleLevelModel.bel)
print(summarySingleLevelModel.bel[, c("mcse","Rhat","n_eff")])

# (b) visual
#     (i) trace plots 
trace.bel <- plot(SingleLevelModel.bel, "trace",     pars = c("eduyears_mod","age","bmi","smoking","eurod","ac002dno","br015_","female"))

plot(trace.bel)

#     (ii) shinystan
launch_shinystan(SingleLevelModel.bel)

# Posterior predictive check
pp_dist1.bel <- pp_check(SingleLevelModel.bel, nreps = 500)  + xlab("Cognitive Health Score")
plot(pp_dist1.bel)

# Summarize and interpret results
# Confidence intervals
plot(SingleLevelModel.bel,"hist",pars = c("eduyears_mod","age","bmi","smoking","eurod","ac002dno","br015_","female"))
#All look symmetrical so central intervals are acceptable
posterior_interval(SingleLevelModel.bel, prob = 0.95,type = "central", pars = c("eduyears_mod","age","bmi","smoking","eurod","ac002dno","br015_","female"))

# Generate slope/regression charts based on confidence intervals
# Extract the (post-warmup) posterior draws
posterior1.bel <- as.matrix(SingleLevelModel.bel)
colnames(posterior1.bel)
means1.bel <- colMeans(posterior1.bel[,-10]) #-10 removes sigma as we are not interested in this variable

# Take random 100 posterior draws of intercept and slope
# 100 isn't special, but enough to show uncertainty without making the graph unreadable
betas.bel <- posterior1.bel[sample(nrow(posterior1.bel), 100), 1:9]

xmean.bel = c(1,mean(data.Belgium$eduyears_mod),mean(data.Belgium$age),mean(data.Belgium$bmi),mean(data.Belgium$smoking),mean(data.Belgium$eurod),mean(data.Belgium$ac002dno),mean(data.Belgium$br015_),mean(data.Belgium$female))
xmean.bel = matrix(xmean.bel,length(xmean.bel),1)

# Plot regression lines implied by the betas
blues.bel <- color_scheme_get("brightblue")

index = 2 # Changing the index enables us to easily change the variable being considered in the plots
mod1p1.bel.eduyears_mod <- ggplot(data.Belgium, aes(x = eduyears_mod, y = cogscore)) +
  geom_point(color = "gray30") +
  geom_abline(
    intercept = betas.bel[, -index]%*%xmean.bel[-index], 
    slope = betas.bel[, index],
    color = blues.bel[[2]], 
    size = 0.15, 
    alpha = 0.5
  ) +
  geom_abline(
    intercept = sum(means1.bel[-index]*xmean.bel[-index]), 
    slope = means1.bel[index],
    size = 1.25, 
    color = blues[[6]]
  ) +
  ylim(0, 25)
index = 3
mod1p1.bel.age <- ggplot(data.Belgium, aes(x = age, y = cogscore)) +
  geom_point(color = "gray30") +
  geom_abline(
    intercept = betas.bel[, -index]%*%xmean.bel[-index], 
    slope = betas.bel[, index],
    color = blues.bel[[2]], 
    size = 0.15, 
    alpha = 0.5
  ) +
  geom_abline(
    intercept = sum(means1.bel[-index]*xmean.bel[-index]), 
    slope = means1.bel[index],
    size = 1.25, 
    color = blues[[6]]
  ) +
  ylim(0, 25)
index = 4
mod1p1.bel.bmi <- ggplot(data.Belgium, aes(x = bmi, y = cogscore)) +
  geom_point(color = "gray30") +
  geom_abline(
    intercept = betas.bel[, -index]%*%xmean.bel[-index], 
    slope = betas.bel[, index],
    color = blues.bel[[2]], 
    size = 0.15, 
    alpha = 0.5
  ) +
  geom_abline(
    intercept = sum(means1.bel[-index]*xmean.bel[-index]), 
    slope = means1.bel[index],
    size = 1.25, 
    color = blues[[6]]
  ) +
  ylim(0, 25)
index = 5
mod1p1.bel.smoking <- ggplot(data.Belgium, aes(x = smoking, y = cogscore)) +
  geom_point(color = "gray30") +
  geom_abline(
    intercept = betas.bel[, -index]%*%xmean.bel[-index], 
    slope = betas.bel[, index],
    color = blues.bel[[2]], 
    size = 0.15, 
    alpha = 0.5
  ) +
  geom_abline(
    intercept = sum(means1.bel[-index]*xmean.bel[-index]), 
    slope = means1.bel[index],
    size = 1.25, 
    color = blues[[6]]
  ) +
  ylim(0, 25)
index = 6
mod1p1.bel.eurod <- ggplot(data.Belgium, aes(x = eurod, y = cogscore)) +
  geom_point(color = "gray30") +
  geom_abline(
    intercept = betas.bel[, -index]%*%xmean.bel[-index], 
    slope = betas.bel[, index],
    color = blues.bel[[2]], 
    size = 0.15, 
    alpha = 0.5
  ) +
  geom_abline(
    intercept = sum(means1.bel[-index]*xmean.bel[-index]), 
    slope = means1.bel[index],
    size = 1.25, 
    color = blues[[6]]
  ) +
  ylim(0, 25)
index = 7
mod1p1.bel.ac002dno <- ggplot(data.Belgium, aes(x = ac002dno, y = cogscore)) +
  geom_point(color = "gray30") +
  geom_abline(
    intercept = betas.bel[, -index]%*%xmean.bel[-index], 
    slope = betas.bel[, index],
    color = blues.bel[[2]], 
    size = 0.15, 
    alpha = 0.5
  ) +
  geom_abline(
    intercept = sum(means1.bel[-index]*xmean.bel[-index]), 
    slope = means1.bel[index],
    size = 1.25, 
    color = blues[[6]]
  ) +
  ylim(0, 25)
index = 8
mod1p1.bel.br015_ <- ggplot(data.Belgium, aes(x = br015_, y = cogscore)) +
  geom_point(color = "gray30") +
  geom_abline(
    intercept = betas.bel[, -index]%*%xmean.bel[-index], 
    slope = betas.bel[, index],
    color = blues.bel[[2]], 
    size = 0.15, 
    alpha = 0.5
  ) +
  geom_abline(
    intercept = sum(means1.bel[-index]*xmean.bel[-index]), 
    slope = means1.bel[index],
    size = 1.25, 
    color = blues[[6]]
  ) +
  ylim(0, 25)
index = 9
mod1p1.bel.female <- ggplot(data.Belgium, aes(x = female, y = cogscore)) +
  geom_point(color = "gray30") +
  geom_abline(
    intercept = betas.bel[, -index]%*%xmean.bel[-index], 
    slope = betas.bel[, index],
    color = blues.bel[[2]], 
    size = 0.15, 
    alpha = 0.5
  ) +
  geom_abline(
    intercept = sum(means1.bel[-index]*xmean.bel[-index]), 
    slope = means1.bel[index],
    size = 1.25, 
    color = blues[[6]]
  ) +
  ylim(0, 25)

# Plot regression charts described above
regression.bel <- bayesplot_grid(
  mod1p1.bel.eduyears_mod,mod1p1.bel.age,mod1p1.bel.bmi,mod1p1.bel.smoking,mod1p1.bel.eurod,mod1p1.bel.ac002dno,mod1p1.bel.br015_,mod1p1.bel.female,
  #xlim = c(-30,60), ylim = c(0,0.125),
  legends = FALSE,
  subtitles = c("Years in education","Age","BMI","Current smoker","Score on depression scale","ac002dno","br015_","Gender"),
  grid_args = list(ncol = 2))

plot(regression.bel)

#---------------------#
##----## HLM ##----####
##  Italy | Age only ##
#---------------------#

HLM.ita.age <- stan_lmer(cogscore ~ eduyears_mod + age + bmi + smoking + eurod + ac002dno + br015_ + female + (1 + age | mergeid), data=data.italy, iter = 3000, adapt_delta = 0.999)

prior_summary(HLM.ita.age)

# Check sampling quality and results

# (a) numerical checks
summaryHLM.ita.age <- summary(HLM.ita.age)
print(summaryHLM.ita.age[, c("mcse","Rhat","n_eff")])

max(summaryHLM.ita.age[,"Rhat"])
max(summaryHLM.ita.age[,"mcse"])
max(summaryHLM.ita.age[,"n_eff"])

min(summaryHLM.ita.age[,"Rhat"])
min(summaryHLM.ita.age[,"mcse"])
min(summaryHLM.ita.age[,"n_eff"])

mean(summaryHLM.ita.age[,"Rhat"])
mean(summaryHLM.ita.age[,"mcse"])
mean(summaryHLM.ita.age[,"n_eff"])

# (b) Visual checks
#     (i) trace plots
plot(HLM.ita.age,"trace", regex_pars = "age")

#     (iii) with shinystan
launch_shinystan(HLM.ita.age)

# Posterior predictive checks
color_scheme_set("viridis")
pp_dist2.HLM.ita.age<-pp_check(HLM.ita.age,nreps=500)+xlab("Cognitive Health Score")
plot(pp_dist2.HLM.ita.age)

# Compare with single level model
comparison2 <- bayesplot_grid(
  pp_dist1, pp_dist2.HLM.ita.age,
  xlim = c(-10,30), ylim = c(0,0.125),
  legends = FALSE,
  subtitles = c("Single-level Model","Hierarchical Model - age only"),
  grid_args = list(ncol = 3))

plot(comparison2)

# Check fit and interpret results

# Summary tables for HLM parameters
#  Population-level estimates
summaryHLM.ita.age.Pop <- tidy(HLM.ita.age, intervals = TRUE, prob=.95, parameters = "non-varying")
print(summaryHLM.ita.age.Pop,digits = 2)

#  Variance estimates
summaryTwoLevelModel.Var <- tidy(HLM.ita.age, intervals = TRUE, prob=.95, parameters = "hierarchical")
print(summaryTwoLevelModel.Var,digits = 2)

#  Person-specific estimates
summaryHLM.ita.age.Per <- tidy(HLM.ita.age, intervals = TRUE, prob=.95, parameters = "varying")
print(summaryHLM.ita.age.Per,digits = 2)

#-------------------------#
##---## HLM ##---------####
## Italy | All variables ##
#-------------------------#

HLM.ita <- stan_lmer(cogscore ~ eduyears_mod + age + bmi + smoking + eurod + ac002dno + br015_ + female + (1 + age + bmi + smoking + eurod + ac002dno + br015_ | mergeid), data=data.italy, iter = 3000, adapt_delta = 0.999)

prior_summary(HLM.ita)
# Keep as default priors.

# Check sampling quality and results

# (a) numerical checks
summaryHLM.ita <- summary(HLM.ita)
print(summaryHLM.ita[, c("mcse","Rhat","n_eff")])

max(summaryHLM.ita[,"mcse"])
max(summaryHLM.ita[,"Rhat"])
max(summaryHLM.ita[,"n_eff"])

min(summaryHLM.ita[,"mcse"])
min(summaryHLM.ita[,"Rhat"])
min(summaryHLM.ita[,"n_eff"])

mean(summaryHLM.ita[,"mcse"])
mean(summaryHLM.ita[,"Rhat"])
mean(summaryHLM.ita[,"n_eff"])

# (b) Visual checks
#     (i) trace plots
plot(HLM.ita,"trace", regex_pars = "age")

#     (iii) with shinystan
launch_shinystan(HLM.ita)

# Posterior predictive checks
color_scheme_set("purple")
pp_dist2.HLM.ita<-pp_check(HLM.ita,nreps=500)+xlab("Cognitive Health Score")
plot(pp_dist2.HLM.ita)

# Compare with single level model
comparison3 <- bayesplot_grid(
  pp_dist1, pp_dist2.HLM.ita.age, pp_dist2.HLM.ita,
  xlim = c(-10,30), ylim = c(0,0.125),
  legends = FALSE,
  subtitles = c("Single-level Model","Hierarchical Model - age only","Hierarchical Model - all"),
  grid_args = list(ncol = 3))

plot(comparison3)

# Check fit and interpret results

#tidy summary tables for HLM parameters
#  Population-level estimates
summaryHLM.ita.Pop <- tidy(HLM.ita, intervals = TRUE, prob=.95, parameters = "non-varying")
print(summaryHLM.ita.Pop,digits = 2)

#  Variance estimates
summaryHLM.ita.Var <- tidy(HLM.ita, intervals = TRUE, prob=.95, parameters = "hierarchical")
print(summaryHLM.ita.Var,digits = 2)

#  Person-specific estimates
summaryHLM.ita.Per <- tidy(HLM.ita, intervals = TRUE, prob=.95, parameters = "varying")
print(summaryHLM.ita.Per,digits = 2)

