rm(list=ls())
library(ggplot2)
library(bayesplot)
theme_set(bayesplot::theme_default(base_family = "sans"))
library(posterior)
library(tidyr)
library(latex2exp)
library(loo)
library("rstan")
library(gridExtra)
#setwd()
set.seed(565923)
penguins <- readRDS ("penguins.RDS")
head(penguins)

boxplot(penguins$bill_depth~penguins$species)
boxplot(penguins$bill_length~penguins$species)

boxplot(penguins$bill_depth~penguins$sex)
boxplot(penguins$bill_length~penguins$sex)
#ha senso
table(penguins$species, penguins$sex)
#uguali

hist(penguins$bill_length)


library(ggplot2)
library(GGally)
ggpairs(penguins)

penguins2 <- penguins
penguins2$sex<-as.factor(as.numeric(penguins$sex)-1)
penguins2$species<-as.numeric(penguins$species)

head(penguins)
penguins$sex<-as.numeric(penguins$sex)-1
penguins$species<-as.numeric(penguins$species)



model.lm <- lm(bill_length~., penguins2[penguins2$species==1,])
summary(model.lm) #Coefficients: (1 not defined because of singularities)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  35.2589     3.2429  10.873  < 2e-16 ***
 # species           NA         NA      NA       NA    
#sex1          2.9683     0.4459   6.657 5.55e-10 ***
 # bill_depth    0.1134     0.1835   0.618    0.537  
model.lm <- lm(bill_length~., penguins2[penguins2$species==2,])
summary(model.lm)#Coefficients: (1 not defined because of singularities)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  29.5139     6.6436   4.442 3.54e-05 ***
#  species           NA         NA      NA       NA    
#sex1          2.9059     0.8498   3.419  0.00109 ** 
 # bill_depth    0.9699     0.3770   2.573  0.01239 * 
model.lm <- lm(bill_length~., penguins2[penguins2$species==3,])
summary(model.lm)#Coefficients: (1 not defined because of singularities)
#Estimate Std. Error t value Pr(>|t|)    
#Intercept)  27.1016     4.6103   5.879 4.06e-08 ***
 # species           NA         NA      NA       NA    
#sex1          1.9907     0.6347   3.136 0.002167 ** 
#  bill_depth    1.2967     0.3231   4.013 0.000107 ***

#mean(c(35.2589,29.5139,27.1016)) == 30.6248
#var(penguins$bill_length) = 29.90633

#mean(c(0.1134,0.9699,1.2967))
#[1] 0.7933333
######→ MODELS #######

##### 0.YES Pooled ####
stan_data <- list(N = nrow(penguins),
                  N_new=nrow(penguins),
                  C = (ncol(penguins)-1),
                  X = penguins[,-4],
                  y = penguins[,4]
) #has to have the same names of the RStan file

model_pooled <- stan_model('PoolModel.stan')

sample <- sampling(model_pooled, data=stan_data, iter=1000, chains=4)

print(sample)
summary(sample)
plot(sample)



class(sample)

list_of_draws_pool <- rstan::extract(sample)
print(names(list_of_draws_pool))
y_pred_pool <- list_of_draws_pool$ynew
dim(y_pred_pool)

#' Histogram of y + 8 yrep histograms
ppc_hist(penguins$bill_length, y_pred_pool[1:8,])

#' Kernel density estimate of y + 100 yrep kernel density estimates
ppc_dens_overlay(penguins$bill_length, y_pred_pool[1:500,])

#' ECDF of y + 500 yrep ECDFs
ppc_ecdf_overlay(penguins$bill_length, y_pred_pool[1:500,])

#' Scatterplot of yrep vs y
ppc_scatter(penguins$bill_length, y_pred_pool[191:199,])+geom_abline()

color_scheme_set("brewer-Paired")
ppc_stat_2d(penguins$bill_length, y_pred_pool, stat=c("min","max"))
color_scheme_set()

##### 0.2NO Pooled Uninforative####
stan_data <- list(N = nrow(penguins),
                  N_new=nrow(penguins),
                  C = (ncol(penguins)-1),
                  X = penguins[,-4],
                  y = penguins[,4]
) #has to have the same names of the RStan file

model_pooled1 <- stan_model('PoolModel_uninformative.stan')

sample1 <- sampling(model_pooled1, data=stan_data, iter=1000, chains=4)

print(sample1)
summary(sample1)
plot(sample1)



class(sample1)

list_of_draws_pool <- rstan::extract(sample1)
print(names(list_of_draws_pool))
y_pred_pool <- list_of_draws_pool$ynew
dim(y_pred_pool)

#' Histogram of y + 8 yrep histograms
ppc_hist(penguins$bill_length, y_pred_pool[1:8,])

#' Kernel density estimate of y + 100 yrep kernel density estimates
ppc_dens_overlay(penguins$bill_length, y_pred_pool[1:500,])

#' ECDF of y + 500 yrep ECDFs
ppc_ecdf_overlay(penguins$bill_length, y_pred_pool[1:500,])

#' Scatterplot of yrep vs y
ppc_scatter(penguins$bill_length, y_pred_pool[191:199,])+geom_abline()

color_scheme_set("brewer-Paired")
ppc_stat_2d(penguins$bill_length, y_pred_pool, stat=c("min","max"))
color_scheme_set()


##### 1.NO Separate1 ######

##Warnings: Tree Depth, ESS, Rhat high indicating chain has not mixed

stan_data <- list(
  N = nrow(penguins),
  N_new=c(146,119,68),
  bill_length = penguins$bill_length,
  bill_width = penguins$bill_depth,
  sex= penguins$sex,
  N_species = length(unique(penguins$species)),
  species = as.integer(penguins$species)
)
model1 = stan_model("Separate1.stan")

fit1 <- sampling(model1,data=stan_data,iter=2000,chains=4) 

plot(fit1)
summary(fit1)
a <- summary(fit1)$summary

rstan::traceplot(fit1,pars=c("gamma[1]","gamma[2]","gamma[3]","beta_width_2[1]","beta_width_2[2]","beta_width_2[3]","beta_sex_2[1]","beta_sex_2[2]","beta_sex_2[3]","lp__"), inc_warmup=TRUE, nrow=3)


class(fit1)
list_of_draws1 <- rstan::extract(fit1)
print(names(list_of_draws1))
y_pred1 <- list_of_draws1$ynew


#' Histogram of y + 8 yrep histograms
ppc_hist(penguins$bill_length, y_pred1[211:218,])

y_pred1

#' Kernel density estimate of y + 100 yrep kernel density estimates
ppc_dens_overlay(penguins$bill_length, y_pred1)

#' ECDF of y + 500 yrep ECDFs
ppc_ecdf_overlay(penguins$bill_length, y_pred1[1:500,])

#' Scatterplot of yrep vs y
ppc_scatter(penguins$bill_length, y_pred1[191:199,])+geom_abline()

color_scheme_set("brewer-Paired")
ppc_stat_2d(penguins$bill_length, y_pred1, stat=c("min","max"))
color_scheme_set()


#####2.YES Hierarchical1 ######
set.seed(565923)
stan_data <- list(
  N = nrow(penguins),
  N_new=c(146,119,68),
  bill_length = penguins$bill_length,
  bill_width = penguins$bill_depth,
  sex= penguins$sex,
  N_species = length(unique(penguins$species)),
  species = as.integer(penguins$species)
)
model2 = stan_model("Hierarchical1.stan")

fit2 <- sampling(model2,data=stan_data,iter=2000,warmup=500,chains=4,refresh=1200) 

plot(fit2)
summary(fit2)

rstan::traceplot(fit2,pars=c("gamma[1]","gamma[2]","gamma[3]","beta_width_2[1]","beta_width_2[2]","beta_width_2[3]","beta_sex_2[1]","beta_sex_2[2]","beta_sex_2[3]","lp__"), inc_warmup=TRUE, nrow=3)


class(fit2)
list_of_draws2 <- rstan::extract(fit2)
print(names(list_of_draws2))
y_pred2 <- list_of_draws2$ynew


#' Histogram of y + 8 yrep histograms
ppc_hist(penguins$bill_length, y_pred2[211:218,])

y_pred2

#' Kernel density estimate of y + 100 yrep kernel density estimates
ppc_dens_overlay(penguins$bill_length, y_pred2)

#' ECDF of y + 500 yrep ECDFs
ppc_ecdf_overlay(penguins$bill_length, y_pred2[1:500,])

#' Scatterplot of yrep vs y
ppc_scatter(penguins$bill_length, y_pred2[191:199,])+geom_abline()

color_scheme_set("brewer-Paired")
ppc_stat_2d(penguins$bill_length, y_pred2, stat=c("min","max"))
color_scheme_set()

ppc_stat(penguins$bill_length, y_pred2)

#plot hamilonian montecalo convergence with 1 chain (no warm up)

sample_plot_hier <- sampling(model2, data=stan_data, iter=2000, chains=1)
params_cp_hier <- as.data.frame(extract(sample_plot_hier , permuted=FALSE))
names(params_cp_hier) <- gsub("chain:1.", "", names(sample_plot_hier), fixed = TRUE)
params_cp_hier$iter <- 1:length(params_cp_hier$"beta_width_2[1]")

p1_hier<-ggplot(params_cp_hier) +
  geom_point(aes(x = iter, y = `beta_width_2[1]`), color = "darkorange", size = 2, alpha=0.4) +
  labs(x = "Iteration", y = "beta_depth") +
  ylim(-2, 4) +
  theme_minimal()+
  geom_point(aes(x = iter, y = `beta_width_2[2]`), color ="seagreen3", size = 2, alpha=0.4)+
  geom_point(aes(x = iter, y = `beta_width_2[3]`), color = "yellow2", size = 2, alpha=0.4)

p2_hier<-ggplot(params_cp_hier) +
  geom_point(aes(x = iter, y = `gamma[1]`), color = "darkorange", size = 2, alpha=0.4) +
  labs(x = "Iteration", y = "intercept") +
  ylim(0, 55) +
  theme_minimal()+
  geom_point(aes(x = iter, y = `gamma[2]`), color ="seagreen3", size = 2, alpha=0.4)+
  geom_point(aes(x = iter, y = `gamma[3]`), color = "yellow2", size = 2, alpha=0.4)

p3_hier<-ggplot(params_cp_hier) +
  geom_point(aes(x = iter, y = `beta_sex_2[1]`), color = "darkorange", size = 2, alpha=0.4) +
  labs(x = "Iteration", y = "beta_sex") +
  ylim(-1, 7) +
  theme_minimal()+
  geom_point(aes(x = iter, y = `beta_sex_2[2]`), color ="seagreen3", size = 2, alpha=0.4)+
  geom_point(aes(x = iter, y = `beta_sex_2[3]`), color = "yellow2", size = 2, alpha=0.4)

p4_hier<-ggplot(params_cp_hier) +
  geom_point(aes(x = iter, y = `sigma`), color = "blue", size = 2, alpha=0.4) +
  labs(x = "Iteration", y = "sigma") +
  ylim(3.5, 9) +
  theme_minimal()
grid.arrange(p1_hier, p2_hier, p3_hier, p4_hier, ncol = 2)
p5_hier<-ggplot(params_cp_hier) +
  geom_point(aes(x = iter, y = `tau_depth`), color = "purple", size = 2, alpha=0.4) +
  labs(x = "Iteration", y = "tau_depth") +
  ylim(-2, 5) +
  theme_minimal()
p6_hier<-ggplot(params_cp_hier) +
  geom_point(aes(x = iter, y = `tau_sex`), color = "brown", size = 2, alpha=0.4) +
  labs(x = "Iteration", y = "tau_sex") +
  ylim(-1, 6) +
  theme_minimal()
grid.arrange(p5_hier,p6_hier,p1_hier, p2_hier, p3_hier, p4_hier, ncol = 2)

#mean conergences

running_means_betaw1_hier <- sapply(params_cp_hier$iter, function(n) mean(params_cp_hier$"beta_width_2[1]"[1:n]))
running_means_betaw2_hier <- sapply(params_cp_hier$iter, function(n) mean(params_cp_hier$"beta_width_2[2]"[1:n]))
running_means_betaw3_hier <- sapply(params_cp_hier$iter, function(n) mean(params_cp_hier$"beta_width_2[3]"[1:n]))
p1mu_hier<-ggplot(params_cp_hier) +
  geom_point(aes(x = iter, y = running_means_betaw1_hier), color = "darkorange", size = 1, shape = 16) +
  labs(x = "Iteration", y = "beta_depth") +
  ylim(-0.5, 1.8) +
  theme_minimal()+
  geom_point(aes(x = iter, y = running_means_betaw2_hier), color = "seagreen", size = 1, shape = 16) +
  geom_point(aes(x = iter, y = running_means_betaw3_hier), color = "yellow2", size = 1, shape = 16) 

running_means_beta_gamma1_hier <- sapply(params_cp_hier$iter, function(n) mean(params_cp_hier$"gamma[1]"[1:n]))
running_means_beta_gamma2_hier <- sapply(params_cp_hier$iter, function(n) mean(params_cp_hier$"gamma[2]"[1:n]))
running_means_beta_gamma3_hier <- sapply(params_cp_hier$iter, function(n) mean(params_cp_hier$"gamma[3]"[1:n]))
p2mu_hier<-ggplot(params_cp_hier) +
  geom_point(aes(x = iter, y = running_means_beta_gamma1_hier), color = "darkorange", size = 1, shape = 16) +
  labs(x = "Iteration", y = "intercepts") +
  ylim(19, 45) +
  theme_minimal()+
  geom_point(aes(x = iter, y = running_means_beta_gamma2_hier), color = "seagreen", size = 1, shape = 16) +
  geom_point(aes(x = iter, y = running_means_beta_gamma3_hier), color = "yellow2", size = 1, shape = 16) 

running_means_beta_sex1_hier <- sapply(params_cp_hier$iter, function(n) mean(params_cp_hier$"beta_sex_2[1]"[1:n]))
running_means_beta_sex2_hier <- sapply(params_cp_hier$iter, function(n) mean(params_cp_hier$"beta_sex_2[2]"[1:n]))
running_means_beta_sex3_hier <- sapply(params_cp_hier$iter, function(n) mean(params_cp_hier$"beta_sex_2[3]"[1:n]))
p3mu_hier<-ggplot(params_cp_hier) +
  geom_point(aes(x = iter, y = running_means_beta_sex1_hier), color = "darkorange", size = 1, shape = 16) +
  labs(x = "Iteration", y = "beta_sex") +
  ylim(1, 4.25) +
  theme_minimal()+
  geom_point(aes(x = iter, y = running_means_beta_sex2_hier), color = "seagreen", size = 1, shape = 16) +
  geom_point(aes(x = iter, y = running_means_beta_sex3_hier), color = "yellow2", size = 1, shape = 16) 

running_means_sigma_hier <- sapply(params_cp_hier$iter, function(n) mean(params_cp_hier$"sigma"[1:n]))
p4mu_hier<-ggplot(params_cp_hier) +
  geom_point(aes(x = iter, y = running_means_sigma_hier), color = "blue", size = 1, shape = 16) +
  labs(x = "Iteration", y = "sigma") +
  ylim(5.5, 6.75) +
  theme_minimal()
grid.arrange(p1mu_hier, p2mu_hier, p3mu_hier, p4mu_hier, ncol = 2)

running_means_tausex_hier <- sapply(params_cp_hier$iter, function(n) mean(params_cp_hier$"tau_sex"[1:n]))
p6mu_hier<-ggplot(params_cp_hier) +
  geom_point(aes(x = iter, y = running_means_tausex_hier), color = "brown", size = 1, shape = 16) +
  labs(x = "Iteration", y = "tau_sex") +
  ylim(1, 3.5) +
  theme_minimal()

running_means_taudepth_hier <- sapply(params_cp_hier$iter, function(n) mean(params_cp_hier$"tau_depth"[1:n]))
p5mu_hier<-ggplot(params_cp_hier) +
  geom_point(aes(x = iter, y = running_means_taudepth_hier), color = "purple", size = 1, shape = 16) +
  labs(x = "Iteration", y = "tau_depth") +
  ylim(0, 2) +
  theme_minimal()
grid.arrange(p5mu_hier,p6mu_hier,p1mu_hier, p2mu_hier, p3mu_hier, p4mu_hier, ncol = 2)

#plot divergence 2 dim 2 coef

divergent_hier <- get_sampler_params(sample_plot_hier, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent_hier) #zero
sum(divergent_hier) / 10000
#zero divergences
params_cp_hier$divergent_hier <- divergent_hier

div_params_cp_hier <- params_cp_hier[params_cp_hier$divergent_hier == 1,]
nondiv_params_cp_hier <- params_cp_hier[params_cp_hier$divergent_hier == 0,]

ggplot(nondiv_params_cp_hier) +
  geom_point(aes(x = `beta_sex_2[1]`, y = `beta_width_2[1]`), color = "darkorange", size = 2, alpha=0.4) +
  labs(x = "beta_sex", y = "beta_depth") +
  ylim(-1, 2.75) +
  xlim(-1,6)+
  theme_minimal()+
  geom_point(aes(x = `beta_sex_2[2]`, y = `beta_width_2[2]`), color ="seagreen3", size = 2, alpha=0.4)+
  geom_point(aes(x = `beta_sex_2[3]`, y = `beta_width_2[3]`), color = "yellow2", size = 2, alpha=0.4)+
  #adding_divergences point here 0 points
  geom_point(data=div_params_cp_hier, aes(x = `beta_sex_2[1]`, y = `beta_width_2[1]`), color = "darkorange", size = 2)+
  geom_point(data=div_params_cp_hier, aes(x = `beta_sex_2[2]`, y = `beta_width_2[2]`), color ="seagreen3", size = 2)+
  geom_point(data=div_params_cp_hier, aes(x = `beta_sex_2[3]`, y = `beta_width_2[3]`), color = "yellow2", size = 2)

#plot regression lines

gamma_values_hier <- as.numeric(apply(list_of_draws2$gamma, 2, mean))
beta_width_values_hier <- as.numeric(apply(list_of_draws2$beta_width_2, 2, mean))
beta_sex_values_hier <- as.numeric(apply(list_of_draws2$beta_sex_2, 2, mean))

# Create a data frame for plotting
plot_data <- data.frame(
  bill_depth = penguins$bill_depth,
  bill_length = penguins$bill_length,
  sex=penguins$sex,
  species = as.factor(penguins$species)
)

# Predicted values using the linear models
plot_data$predicted <- with(plot_data, gamma_values_hier[species] + beta_width_values_hier[species] * bill_depth + beta_sex_values_hier[species]*as.numeric(sex))
plotdata1___0_hier<-plot_data[plot_data$species==1 & plot_data$sex==0,]
plotdata1___1_hier<-plot_data[plot_data$species==1 & plot_data$sex==1,]
plotdata3___0_hier<-plot_data[plot_data$species==3 & plot_data$sex==0,]
plotdata3___1_hier<-plot_data[plot_data$species==3 & plot_data$sex==1,]
plotdata2___0_hier<-plot_data[plot_data$species==2 & plot_data$sex==0,]
plotdata2___1_hier<-plot_data[plot_data$species==2 & plot_data$sex==1,]

# Create a scatterplot with lines for each species
plot_hier <- ggplot(plot_data) +
  geom_point(aes(x = bill_depth, y = bill_length, color = species), size = 1.8, alpha=0.5 ) +
  geom_line(data= plotdata1___0_hier, aes(x=bill_depth,y = predicted), linewidth = 2, col='darkorange')+
  geom_line(data= plotdata1___1_hier, aes(x=bill_depth,y = predicted), linewidth = 2, col='darkorange')+
  geom_line(data= plotdata2___0_hier, aes(x=bill_depth,y = predicted), linewidth = 2, col='seagreen3')+
  geom_line(data= plotdata2___1_hier, aes(x=bill_depth,y = predicted), linewidth = 2, col='seagreen3')+
  geom_line(data= plotdata3___0_hier, aes(x=bill_depth,y = predicted), linewidth = 2, col='yellow2')+
  geom_line(data= plotdata3___1_hier, aes(x=bill_depth,y = predicted), linewidth = 2, col='yellow2')+
  scale_color_manual(values = c('darkorange', 'seagreen3', 'yellow2'),
                     labels = c("Adelie", "Chinstrap", "Gentoo")) +
  labs(title = "Scatterplot of Hierarchical Bayesian Linear Models for Each Species") +
  theme_minimal()
plot_hier


## 3.YES Separate2 - more informative priors ####
set.seed(565923)
stan_data <- list(
  N = nrow(penguins),
  N_new=c(146, 119, 68),
  bill_length = penguins$bill_length,
  bill_depth = penguins$bill_depth,
  sex= penguins$sex,
  N_species = length(unique(penguins$species)),
  species = as.integer(penguins$species),
  N_ger = 2
)

model3 = stan_model("Separate2.stan")

fit3 <- sampling(model3,data=stan_data,iter=2000,chains=4) 

plot(fit3)
summary(fit3)



class(fit3)
list_of_draws3 <- rstan::extract(fit3)
print(names(list_of_draws3))
y_pred3 <- list_of_draws3$ynew

#' Histogram of y + 8 yrep histograms
ppc_hist(penguins$bill_length, y_pred3[211:218,])

y_pred3

#' Kernel density estimate of y + 100 yrep kernel density estimates
ppc_dens_overlay(penguins$bill_length, y_pred3)

#' ECDF of y + 500 yrep ECDFs
ppc_ecdf_overlay(penguins$bill_length, y_pred3[1:500,])

#' Scatterplot of yrep vs y
ppc_scatter(penguins$bill_length, y_pred3[191:199,])+geom_abline()

color_scheme_set("brewer-Paired")
ppc_stat_2d(penguins$bill_length, y_pred3, stat=c("min","max"))
color_scheme_set()

ppc_stat(penguins$bill_length, y_pred3)

#plot hamilonian montecalo convergence with 1 chain (no wam up)

sample_plot_separate2 <- sampling(model3, data=stan_data, iter=2000, chains=1)
params_cp_sep2 <- as.data.frame(extract(sample_plot_separate2 , permuted=FALSE))
names(params_cp_sep2) <- gsub("chain:1.", "", names(sample_plot_separate2), fixed = TRUE)
 
length(params_cp_sep2$"beta_width_2[1]")
params_cp_sep2$iter <- 1:1000

p1_sep2<-ggplot(params_cp_sep2) +
  geom_point(aes(x = iter, y = `beta_width_2[1]`), color = "darkorange", size = 2, alpha=0.4) +
  labs(x = "Iteration", y = "beta_depth") +
  ylim(-2, 4) +
  theme_minimal()+
  geom_point(aes(x = iter, y = `beta_width_2[2]`), color ="seagreen3", size = 2, alpha=0.4)+
  geom_point(aes(x = iter, y = `beta_width_2[3]`), color = "yellow2", size = 2, alpha=0.4)

p2_sep2<-ggplot(params_cp_sep2) +
  geom_point(aes(x = iter, y = `gamma[1]`), color = "darkorange", size = 2, alpha=0.4) +
  labs(x = "Iteration", y = "intercept") +
  ylim(5, 55) +
  theme_minimal()+
  geom_point(aes(x = iter, y = `gamma[2]`), color ="seagreen3", size = 2, alpha=0.4)+
  geom_point(aes(x = iter, y = `gamma[3]`), color = "yellow2", size = 2, alpha=0.4)

p3_sep2<-ggplot(params_cp_sep2) +
  geom_point(aes(x = iter, y = `beta_sex_2[1]`), color = "darkorange", size = 2, alpha=0.4) +
  labs(x = "Iteration", y = "beta_sex") +
  ylim(-1, 8) +
  theme_minimal()+
  geom_point(aes(x = iter, y = `beta_sex_2[2]`), color ="seagreen3", size = 2, alpha=0.4)+
  geom_point(aes(x = iter, y = `beta_sex_2[3]`), color = "yellow2", size = 2, alpha=0.4)

p4_sep2<-ggplot(params_cp_sep2) +
  geom_point(aes(x = iter, y = `sigma`), color = "blue", size = 2, alpha=0.4) +
  labs(x = "Iteration", y = "sigma") +
  ylim(3.5, 8) +
  theme_minimal()
grid.arrange(p1_sep2, p2_sep2, p3_sep2, p4_sep2, ncol = 2)

#mean conergences
running_means_betaw1 <- sapply(params_cp_sep2$iter, function(n) mean(params_cp_sep2$"beta_width_2[1]"[1:n]))
running_means_betaw2 <- sapply(params_cp_sep2$iter, function(n) mean(params_cp_sep2$"beta_width_2[2]"[1:n]))
running_means_betaw3 <- sapply(params_cp_sep2$iter, function(n) mean(params_cp_sep2$"beta_width_2[3]"[1:n]))
p1mu_sep2<-ggplot(params_cp_sep2) +
  geom_point(aes(x = iter, y = running_means_betaw1), color = "darkorange", size = 1, shape = 16) +
  labs(x = "Iteration", y = "beta_depth") +
  ylim(-0.5, 1.8) +
  theme_minimal()+
  geom_point(aes(x = iter, y = running_means_betaw2), color = "seagreen", size = 1, shape = 16) +
  geom_point(aes(x = iter, y = running_means_betaw3), color = "yellow2", size = 1, shape = 16) 

running_means_beta_gamma1 <- sapply(params_cp_sep2$iter, function(n) mean(params_cp_sep2$"gamma[1]"[1:n]))
running_means_beta_gamma2 <- sapply(params_cp_sep2$iter, function(n) mean(params_cp_sep2$"gamma[2]"[1:n]))
running_means_beta_gamma3 <- sapply(params_cp_sep2$iter, function(n) mean(params_cp_sep2$"gamma[3]"[1:n]))
p2mu_sep2<-ggplot(params_cp_sep2) +
  geom_point(aes(x = iter, y = running_means_beta_gamma1), color = "darkorange", size = 1, shape = 16) +
  labs(x = "Iteration", y = "intercepts") +
  ylim(19, 45) +
  theme_minimal()+
  geom_point(aes(x = iter, y = running_means_beta_gamma2), color = "seagreen", size = 1, shape = 16) +
  geom_point(aes(x = iter, y = running_means_beta_gamma3), color = "yellow2", size = 1, shape = 16) 

running_means_beta_sex1 <- sapply(params_cp_sep2$iter, function(n) mean(params_cp_sep2$"beta_sex_2[1]"[1:n]))
running_means_beta_sex2 <- sapply(params_cp_sep2$iter, function(n) mean(params_cp_sep2$"beta_sex_2[2]"[1:n]))
running_means_beta_sex3 <- sapply(params_cp_sep2$iter, function(n) mean(params_cp_sep2$"beta_sex_2[3]"[1:n]))
p3mu_sep2<-ggplot(params_cp_sep2) +
  geom_point(aes(x = iter, y = running_means_beta_sex1), color = "darkorange", size = 1, shape = 16) +
  labs(x = "Iteration", y = "beta_sex") +
  ylim(1, 4.25) +
  theme_minimal()+
  geom_point(aes(x = iter, y = running_means_beta_sex2), color = "seagreen", size = 1, shape = 16) +
  geom_point(aes(x = iter, y = running_means_beta_sex3), color = "yellow2", size = 1, shape = 16) 

running_means_sigma <- sapply(params_cp_sep2$iter, function(n) mean(params_cp_sep2$"sigma"[1:n]))
p4mu_sep2<-ggplot(params_cp_sep2) +
  geom_point(aes(x = iter, y = running_means_sigma), color = "blue", size = 1, shape = 16) +
  labs(x = "Iteration", y = "sigma") +
  ylim(5.5, 6.75) +
  theme_minimal()
grid.arrange(p1mu_sep2, p2mu_sep2, p3mu_sep2, p4mu_sep2, ncol = 2)

#plot divergence 2 dim 2 coef

divergent <- get_sampler_params(sample_plot_separate2, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent) #zero
sum(divergent) / 10000
#zero divergences
params_cp_sep2$divergent <- divergent

div_params_cp_sep2 <- params_cp_sep2[params_cp_sep2$divergent == 1,]
nondiv_params_cp_sep2 <- params_cp_sep2[params_cp_sep2$divergent == 0,]

ggplot(nondiv_params_cp_sep2) +
  geom_point(aes(x = `beta_sex_2[1]`, y = `beta_width_2[1]`), color = "darkorange", size = 2, alpha=0.4) +
  labs(x = "beta_sex", y = "beta_depth") +
  ylim(-1, 2.75) +
  xlim(-1,6)+
  theme_minimal()+
  geom_point(aes(x = `beta_sex_2[2]`, y = `beta_width_2[2]`), color ="seagreen3", size = 2, alpha=0.4)+
  geom_point(aes(x = `beta_sex_2[3]`, y = `beta_width_2[3]`), color = "yellow2", size = 2, alpha=0.4)+
  #adding_divergences point here 0 points
  geom_point(data=div_params_cp_sep2, aes(x = `beta_sex_2[1]`, y = `beta_width_2[1]`), color = "darkorange", size = 2)+
  geom_point(data=div_params_cp_sep2, aes(x = `beta_sex_2[2]`, y = `beta_width_2[2]`), color ="seagreen3", size = 2)+
  geom_point(data=div_params_cp_sep2, aes(x = `beta_sex_2[3]`, y = `beta_width_2[3]`), color = "yellow2", size = 2)

#plot regression lines

gamma_values_sep2 <- as.numeric(apply(list_of_draws3$gamma, 2, mean))
beta_width_values_sep2 <- as.numeric(apply(list_of_draws3$beta_width_2, 2, mean))
beta_sex_values_sep2 <- as.numeric(apply(list_of_draws3$beta_sex_2, 2, mean))

# Create a data frame for plotting
plot_data <- data.frame(
  bill_depth = penguins$bill_depth,
  bill_length = penguins$bill_length,
  sex=penguins$sex,
  species = as.factor(penguins$species)
)

# Predicted values using the linear models
plot_data$predicted <- with(plot_data, gamma_values_sep2[species] + beta_width_values_sep2[species] * bill_depth + beta_sex_values_sep2[species]*sex)
plotdata1___0_sep2<-plot_data[plot_data$species==1 & plot_data$sex==0,]
plotdata1___1_sep2<-plot_data[plot_data$species==1 & plot_data$sex==1,]
plotdata3___0_sep2<-plot_data[plot_data$species==3 & plot_data$sex==0,]
plotdata3___1_sep2<-plot_data[plot_data$species==3 & plot_data$sex==1,]
plotdata2___0_sep2<-plot_data[plot_data$species==2 & plot_data$sex==0,]
plotdata2___1_sep2<-plot_data[plot_data$species==2 & plot_data$sex==1,]

# Create a scatterplot with lines for each species
plot_sep2 <- ggplot(plot_data) +
  geom_point(aes(x = bill_depth, y = bill_length, color = species), size = 1.8, alpha=0.5 ) +
  geom_line(data= plotdata1___0_sep2, aes(x=bill_depth,y = predicted), linewidth = 2, col='darkorange')+
  geom_line(data= plotdata1___1_sep2, aes(x=bill_depth,y = predicted), linewidth = 2, col='darkorange')+
  geom_line(data= plotdata2___0_sep2, aes(x=bill_depth,y = predicted), linewidth = 2, col='seagreen3')+
  geom_line(data= plotdata2___1_sep2, aes(x=bill_depth,y = predicted), linewidth = 2, col='seagreen3')+
  geom_line(data= plotdata3___0_sep2, aes(x=bill_depth,y = predicted), linewidth = 2, col='yellow2')+
  geom_line(data= plotdata3___1_sep2, aes(x=bill_depth,y = predicted), linewidth = 2, col='yellow2')+
  scale_color_manual(values = c('darkorange', 'seagreen3', 'yellow2'),
                     labels = c("Adelie", "Chinstrap", "Gentoo")) +
  labs(title = "Scatterplot with Separate Bayesian Linear Models for Each Species") +
  theme_minimal()
plot_sep2


####4.YES Separate_1 + Hierarchical_23 ####### 


model.lm <- lm(bill_length~., penguins2[penguins2$species==1,])
summary(model.lm)
#'Coefficients: (1 not defined because of singularities)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  35.2589     3.2429  10.873  < 2e-16 ***
#  species           NA         NA      NA       NA    
#sex1          2.9683     0.4459   6.657 5.55e-10 ***
#  bill_depth    0.1134     0.1835   0.618    0.537    


model.lm <- lm(bill_length~., penguins2[penguins2$species==2,])
summary(model.lm)
#Coefficients: (1 not defined because of singularities)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  29.5139     6.6436   4.442 3.54e-05 ***
# species           NA         NA      NA       NA    
#sex1          2.9059     0.8498   3.419  0.00109 ** 
# bill_depth    0.9699     0.3770   2.573  0.01239 *  

# intercept mean 32.3864
# sex mean 2.9371
# bill_depth 0.54165

stan_data <- list(
  N = nrow(penguins),
  N_new=c(146, 119, 68),
  bill_length = penguins$bill_length,
  bill_width = penguins$bill_depth,
  sex= penguins$sex,
  N_species = length(unique(penguins$species)),
  species = as.integer(penguins$species),
  N_ger = 2
)
model4 = stan_model("Separate_Hierarchical.stan")

fit4 <- sampling(model4,data=stan_data,iter=2000,chains=4) 

plot(fit4)
summary(fit4)

rstan::traceplot(fit4,pars=c("gamma[1]","gamma[2]","gamma[3]","beta_width_2[1]","beta_width_2[2]","beta_width_2[3]","beta_sex_2[1]","beta_sex_2[2]","beta_sex_2[3]","lp__"), inc_warmup=TRUE, nrow=3)


class(fit4)
list_of_draws4 <- rstan::extract(fit4)
print(names(list_of_draws4))
y_pred4 <- list_of_draws4$ynew


#' Histogram of y + 8 yrep histograms
ppc_hist(penguins$bill_length, y_pred4[211:218,])

y_pred4

#' Kernel density estimate of y + 100 yrep kernel density estimates
ppc_dens_overlay(penguins$bill_length, y_pred4)

#' ECDF of y + 500 yrep ECDFs
ppc_ecdf_overlay(penguins$bill_length, y_pred4[1:500,])

#' Scatterplot of yrep vs y
ppc_scatter(penguins$bill_length, y_pred4[191:199,])+geom_abline()

color_scheme_set("brewer-Paired")
ppc_stat_2d(penguins$bill_length, y_pred4, stat=c("min","max"))
color_scheme_set()

ppc_stat(penguins$bill_length, y_pred4)



#####5.  NO Spec and sex separate #####

stan_data <- list(
  N = nrow(penguins),
  N_new=c(146, 119, 68),
  bill_length = penguins$bill_length,
  bill_width = penguins$bill_depth,
  sex= penguins$sex,
  N_species = length(unique(penguins$species)),
  species = as.integer(penguins$species),
  N_sex = length(unique(penguins$sex)),
  sex_id = as.integer(penguins$sex)+1
)
model5 = stan_model("AllSeparate.stan")

fit5 <- sampling(model5,data=stan_data,iter=2000,chains=4,refresh=1200) 

plot(fit5)
summary(fit5)

rstan::traceplot(fit5,pars=c("gamma_species[1]","gamma_species[2]","gamma_species[3]","beta_width_species[1]","beta_width_species[2]","beta_width_species[3]","lp__"), inc_warmup=TRUE, nrow=3)

class(fit5)
list_of_draws5 <- rstan::extract(fit5)
print(names(list_of_draws5))
y_pred5 <- list_of_draws5$ynew


#' Histogram of y + 8 yrep histograms
ppc_hist(penguins$bill_length, y_pred5[211:218,])

y_pred5

#' Kernel density estimate of y + 100 yrep kernel density estimates
ppc_dens_overlay(penguins$bill_length, y_pred5)

#' ECDF of y + 500 yrep ECDFs
ppc_ecdf_overlay(penguins$bill_length, y_pred5[1:500,])

#' Scatterplot of yrep vs y
ppc_scatter(penguins$bill_length, y_pred5[191:199,])+geom_abline()

color_scheme_set("brewer-Paired")
ppc_stat_2d(penguins$bill_length, y_pred5, stat=c("min","max"))
color_scheme_set()

ppc_stat(penguins$bill_length, y_pred5)

####7. NO INTERACTION ####

stan_data <- list(
  N = nrow(penguins),
  N_new=c(146, 119, 68),
  bill_length = penguins$bill_length,
  bill_width = penguins$bill_depth,
  sex= penguins$sex,
  N_species = length(unique(penguins$species)),
  species = as.integer(penguins$species),
  N_sex = length(unique(penguins$sex)),
  sex_id = as.integer(penguins$sex)+1
)
model7 = stan_model("AllSeparate_nointeraction.stan")

fit7 <- sampling(model7,data=stan_data,iter=2000,chains=4,refresh=1200) 

plot(fit7)
summary(fit7)

rstan::traceplot(fit7,pars=c("gamma_species[1]","gamma_species[2]","gamma_species[3]","beta_width_species[1]","beta_width_species[2]","beta_width_species[3]","lp__"), inc_warmup=TRUE, nrow=3)


class(fit7)
list_of_draws7 <- rstan::extract(fit7)
print(names(list_of_draws7))
y_pred7 <- list_of_draws7$ynew


#' Histogram of y + 8 yrep histograms
ppc_hist(penguins$bill_length, y_pred7[211:218,])

y_pred7

#' Kernel density estimate of y + 100 yrep kernel density estimates
ppc_dens_overlay(penguins$bill_length, y_pred7)

#' ECDF of y + 500 yrep ECDFs
ppc_ecdf_overlay(penguins$bill_length, y_pred7[1:500,])

#' Scatterplot of yrep vs y
ppc_scatter(penguins$bill_length, y_pred7[191:199,])+geom_abline()

color_scheme_set("brewer-Paired")
ppc_stat_2d(penguins$bill_length, y_pred7, stat=c("min","max"))
color_scheme_set()

ppc_stat(penguins$bill_length, y_pred7)


####8.YES NO INT E NO SEXWIDTH ####
set.seed(565923)
stan_data <- list(
  N = nrow(penguins),
  N_new=c(146, 119, 68),
  bill_length = penguins$bill_length,
  bill_width = penguins$bill_depth,
  sex= penguins$sex,
  N_species = length(unique(penguins$species)),
  species = as.integer(penguins$species),
  N_sex = length(unique(penguins$sex)),
  sex_id = as.integer(penguins$sex)+1
)
model8 = stan_model("AllSeparate_nointer_nosexwidth.stan")

fit8 <- sampling(model8,data=stan_data,iter=2000,chains=4,refresh=1200) 

plot(fit8)
s8 <-summary(fit8)$summary

rstan::traceplot(fit8,pars=c("gamma_species[1]","gamma_species[2]","gamma_species[3]","beta_width_species[1]","beta_width_species[2]","beta_width_species[3]","lp__"), inc_warmup=TRUE, nrow=3)


class(fit8)
list_of_draws8 <- rstan::extract(fit8)
print(names(list_of_draws8))
y_pred8 <- list_of_draws8$ynew


#' Histogram of y + 8 yrep histograms
ppc_hist(penguins$bill_length, y_pred8[211:218,])

y_pred8

#' Kernel density estimate of y + 100 yrep kernel density estimates
ppc_dens_overlay(penguins$bill_length, y_pred8)

#' ECDF of y + 500 yrep ECDFs
ppc_ecdf_overlay(penguins$bill_length, y_pred8[1:500,])

#' Scatterplot of yrep vs y
ppc_scatter(penguins$bill_length, y_pred8[191:199,])+geom_abline()

color_scheme_set("brewer-Paired")
ppc_stat_2d(penguins$bill_length, y_pred8, stat=c("min","max"))
color_scheme_set()

ppc_stat(penguins$bill_length, y_pred8)


#plot hamilonian montecalo convergence with 1 chain (no warm up)

#sample_plot_sep_selection <- sampling(model8, data=stan_data, iter=2000, chains=1)
sample_plot_sep_selection <- sampling(model8, data=stan_data, iter=2000, chains=1,control=list(max_treedepth=15))
params_cp_sep_selection <- as.data.frame(extract(sample_plot_sep_selection , permuted=FALSE))
names(params_cp_sep_selection) <- gsub("chain:1.", "", names(sample_plot_sep_selection), fixed = TRUE)
params_cp_sep_selection$iter <- 1:length(params_cp_sep_selection$"beta_width_species[1]")



p1_sep_selection<-ggplot(params_cp_sep_selection) +
  geom_point(aes(x = iter, y = `gamma_species[1]`), color = "darkorange", size = 2, alpha=0.4) +
  labs(x = "Iteration", y = "intercept_species") +
  ylim(-120, 170) +
  theme_minimal()+
  geom_point(aes(x = iter, y = `gamma_species[2]`), color ="seagreen3", size = 2, alpha=0.4)+
  geom_point(aes(x = iter, y = `gamma_species[3]`), color = "yellow2", size = 2, alpha=0.4)

p2_sep_selection<-ggplot(params_cp_sep_selection) +
  geom_point(aes(x = iter, y = `gamma_sex[1]`), color = "pink1", size = 2, alpha=0.8) +
  labs(x = "Iteration", y = "intercept_sex") +
  ylim(-150, 155) +
  theme_minimal()+
  geom_point(aes(x = iter, y = `gamma_sex[2]`), color ="lightblue", size = 2, alpha=0.2)

p3_sep_selection<-ggplot(params_cp_sep_selection) +
  geom_point(aes(x = iter, y = `beta_width_species[1]`), color = "darkorange", size = 2, alpha=0.4) +
  labs(x = "Iteration", y = "beta_depth_species") +
  ylim(-1, 2.5) +
  theme_minimal()+
  geom_point(aes(x = iter, y = `beta_width_species[2]`), color ="seagreen3", size = 2, alpha=0.4)+
  geom_point(aes(x = iter, y = `beta_width_species[3]`), color = "yellow2", size = 2, alpha=0.4)

p4_sep_selection<-ggplot(params_cp_sep_selection) +
  geom_point(aes(x = iter, y = `sigma`), color = "blue", size = 2, alpha=0.4) +
  labs(x = "Iteration", y = "sigma") +
  ylim(3.5, 9) +
  theme_minimal()
grid.arrange(p1_sep_selection, p2_sep_selection, p3_sep_selection, p4_sep_selection, ncol = 2)


#mean conergences

running_means_gammaspe1_sep_selection <- sapply(params_cp_sep_selection$iter, function(n) mean(params_cp_sep_selection$"gamma_species[1]"[1:n]))
running_means_gammaspe2_sep_selection <- sapply(params_cp_sep_selection$iter, function(n) mean(params_cp_sep_selection$"gamma_species[2]"[1:n]))
running_means_gammaspe3_sep_selection <- sapply(params_cp_sep_selection$iter, function(n) mean(params_cp_sep_selection$"gamma_species[3]"[1:n]))

p1mu_sep_selection<-ggplot(params_cp_sep_selection) +
  geom_point(aes(x = iter, y = running_means_gammaspe1_sep_selection), color = "darkorange", size = 1, shape = 16) +
  labs(x = "Iteration", y = "intercept_species") +
  ylim(-50, 50) +
  theme_minimal()+
  geom_point(aes(x = iter, y = running_means_gammaspe2_sep_selection), color = "seagreen", size = 1, shape = 16) +
  geom_point(aes(x = iter, y = running_means_gammaspe3_sep_selection), color = "yellow2", size = 1, shape = 16) 

running_means_beta_gamma1_sep_selection <- sapply(params_cp_sep_selection$iter, function(n) mean(params_cp_sep_selection$"gamma_sex[1]"[1:n]))
running_means_beta_gamma2_sep_selection <- sapply(params_cp_sep_selection$iter, function(n) mean(params_cp_sep_selection$"gamma_sex[2]"[1:n]))
p2mu_sep_selection<-ggplot(params_cp_sep_selection) +
  geom_point(aes(x = iter, y = running_means_beta_gamma1_sep_selection), color = "blue", size = 1, shape = 16) +
  labs(x = "Iteration", y = "intercepts_sex") +
  ylim(0, 70) +
  theme_minimal()+
  geom_point(aes(x = iter, y = running_means_beta_gamma2_sep_selection), color = "pink", size = 1, shape = 16)

running_means_beta_depth1_sep_selection <- sapply(params_cp_sep_selection$iter, function(n) mean(params_cp_sep_selection$"beta_width_species[1]"[1:n]))
running_means_beta_depth2_sep_selection <- sapply(params_cp_sep_selection$iter, function(n) mean(params_cp_sep_selection$"beta_width_species[2]"[1:n]))
running_means_beta_depth3_sep_selection <- sapply(params_cp_sep_selection$iter, function(n) mean(params_cp_sep_selection$"beta_width_species[3]"[1:n]))
p3mu_sep_selection<-ggplot(params_cp_sep_selection) +
  geom_point(aes(x = iter, y = running_means_beta_depth1_sep_selection), color = "darkorange", size = 1, shape = 16) +
  labs(x = "Iteration", y = "beta_species_depth") +
  ylim(0, 2) +
  theme_minimal()+
  geom_point(aes(x = iter, y = running_means_beta_depth2_sep_selection), color = "seagreen", size = 1, shape = 16) +
  geom_point(aes(x = iter, y = running_means_beta_depth3_sep_selection), color = "yellow2", size = 1, shape = 16, alpha=0.2) 

running_means_sigma_sep_selection <- sapply(params_cp_sep_selection$iter, function(n) mean(params_cp_sep_selection$"sigma"[1:n]))
p4mu_sep_selection<-ggplot(params_cp_sep_selection) +
  geom_point(aes(x = iter, y = running_means_sigma_sep_selection), color = "blue", size = 1, shape = 16) +
  labs(x = "Iteration", y = "sigma") +
  ylim(5.5, 6.25) +
  theme_minimal()
grid.arrange(p1mu_sep_selection, p2mu_sep_selection, p3mu_sep_selection, p4mu_sep_selection, ncol = 2)


#plot divergence 2 dim 2 coef

divergent_sep_selection <- get_sampler_params(sample_plot_sep_selection, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent_sep_selection) #zero
sum(divergent_sep_selection) / 10000
#zero divergences
params_cp_sep_selection$divergent_sep_selection <- divergent_sep_selection

div_params_cp_sep_selection <- params_cp_sep_selection[params_cp_sep_selection$divergent_sep_selection == 1,]
nondiv_params_cp_sep_selection <- params_cp_sep_selection[params_cp_sep_selection$divergent_sep_selection == 0,]

ggplot(nondiv_params_cp_sep_selection) +
  geom_point(aes(x = `gamma_species[1]`, y = `beta_width_species[1]`), color = "darkorange", size = 2, alpha=0.4) +
  labs(x = "intercept_species", y = "beta_depth_species") +
  ylim(-1, 2.70) +
  xlim(-100,170)+
  theme_minimal()+
  geom_point(aes(x = `gamma_species[2]`, y = `beta_width_species[2]`), color ="seagreen3", size = 2, alpha=0.4)+
  geom_point(aes(x = `gamma_species[3]`, y = `beta_width_species[3]`), color = "yellow2", size = 2, alpha=0.4)+
  #adding_divergences point here 0 points
  geom_point(data=div_params_cp_sep_selection, aes(x = `gamma_species[1]`, y = `beta_width_species[1]`), color = "darkorange", size = 2)+
  geom_point(data=div_params_cp_sep_selection, aes(x = `gamma_species[2]`, y = `beta_width_species[2]`), color ="seagreen3", size = 2)+
  geom_point(data=div_params_cp_sep_selection, aes(x = `gamma_species[3]`, y = `beta_width_species[3]`), color = "yellow2", size = 2)


#plot regression lines

gamma_values_sep_selection <- as.numeric(apply(list_of_draws8$gamma_species, 2, mean))
gamma_sex_values_sep_selection <- as.numeric(apply(list_of_draws8$gamma_sex, 2, mean))
beta_width_values_sep_selection <- as.numeric(apply(list_of_draws8$beta_width_species, 2, mean))

# Create a data frame for plotting
plot_data <- data.frame(
  bill_depth = penguins$bill_depth,
  bill_length = penguins$bill_length,
  sex=penguins$sex,
  species = as.factor(penguins$species)
)

# Predicted values using the linear models
plot_data$predicted <- with(plot_data, gamma_values_sep_selection[species] + gamma_sex_values_sep_selection[sex+1] + beta_width_values_sep_selection[species]*bill_depth)

plotdata1___0_sep_selection<-plot_data[plot_data$species==1 & plot_data$sex==0,]
plotdata1___1_sep_selection<-plot_data[plot_data$species==1 & plot_data$sex==1,]
plotdata3___0_sep_selection<-plot_data[plot_data$species==3 & plot_data$sex==0,]
plotdata3___1_sep_selection<-plot_data[plot_data$species==3 & plot_data$sex==1,]
plotdata2___0_sep_selection<-plot_data[plot_data$species==2 & plot_data$sex==0,]
plotdata2___1_sep_selection<-plot_data[plot_data$species==2 & plot_data$sex==1,]

# Create a scatterplot with lines for each species
plot_sep_selection <- ggplot(plot_data) +
  geom_point(aes(x = bill_depth, y = bill_length, color = species), size = 1.8, alpha=0.5 ) +
  geom_line(data= plotdata1___0_sep_selection, aes(x=bill_depth,y = predicted), linewidth = 2, col='darkorange')+
  geom_line(data= plotdata1___1_sep_selection, aes(x=bill_depth,y = predicted), linewidth = 2, col='darkorange')+
  geom_line(data= plotdata2___0_sep_selection, aes(x=bill_depth,y = predicted), linewidth = 2, col='seagreen3')+
  geom_line(data= plotdata2___1_sep_selection, aes(x=bill_depth,y = predicted), linewidth = 2, col='seagreen3')+
  geom_line(data= plotdata3___0_sep_selection, aes(x=bill_depth,y = predicted), linewidth = 2, col='yellow2')+
  geom_line(data= plotdata3___1_sep_selection, aes(x=bill_depth,y = predicted), linewidth = 2, col='yellow2')+
  scale_color_manual(values = c('darkorange', 'seagreen3', 'yellow2'),
                     labels = c("Adelie", "Chinstrap", "Gentoo")) +
  labs(title = "Scatterplot of Hierarchical Bayesian Linear Models for Each Species and Sex") +
  theme_minimal()
plot_sep_selection


####9. NO All Separate Hierarchical ####


stan_data <- list(
  N = nrow(penguins),
  N_new=c(146, 119, 68),
  bill_length = penguins$bill_length,
  bill_width = penguins$bill_depth,
  sex= penguins$sex,
  N_species = length(unique(penguins$species)),
  species = as.integer(penguins$species),
  N_sex = length(unique(penguins$sex)),
  sex_id = as.integer(penguins$sex)+1
)
model9 = stan_model("AllSeparate_hierarchical.stan")

fit9 <- sampling(model9,data=stan_data,iter=2000,chains=4,refresh=1200) 

plot(fit9)
s <-summary(fit9)$summary

rstan::traceplot(fit9,pars=c("gamma_species[1]","gamma_species[2]","gamma_species[3]","beta_width_species[1]","beta_width_species[2]","beta_width_species[3]","lp__"), inc_warmup=TRUE, nrow=3)


class(fit9)
list_of_draws9 <- rstan::extract(fit9)
print(names(list_of_draws9))
y_pred9 <- list_of_draws9$ynew


#' Histogram of y + 8 yrep histograms
ppc_hist(penguins$bill_length, y_pred9[211:218,])

y_pred9

#' Kernel density estimate of y + 100 yrep kernel density estimates
ppc_dens_overlay(penguins$bill_length, y_pred9)

#' ECDF of y + 500 yrep ECDFs
ppc_ecdf_overlay(penguins$bill_length, y_pred9[1:500,])

#' Scatterplot of yrep vs y
ppc_scatter(penguins$bill_length, y_pred9[191:199,])+geom_abline()

color_scheme_set("brewer-Paired")
ppc_stat_2d(penguins$bill_length, y_pred9, stat=c("min","max"))
color_scheme_set()

ppc_stat(penguins$bill_length, y_pred9)



####6.YES Just intercepts ####

stan_data <- list(
  N = nrow(penguins),
  bill_length = penguins$bill_length,
  N_species = length(unique(penguins$species)),
  species = as.integer(penguins$species)
)
model6 = stan_model("InterceptSpecies.stan")

fit6 <- sampling(model6,data=stan_data,iter=2000,chains=4) 

plot(fit6)
summary(fit6)
a <- summary(fit6)$summary

rstan::traceplot(fit1,pars=c("gamma[1]","gamma[2]","gamma[3]","beta_width_2[1]","beta_width_2[2]","beta_width_2[3]","beta_sex_2[1]","beta_sex_2[2]","beta_sex_2[3]","lp__"), inc_warmup=TRUE, nrow=3)

class(fit6)
list_of_draws6 <- rstan::extract(fit6)
print(names(list_of_draws6))
y_pred6 <- list_of_draws6$ynew


#' Histogram of y + 8 yrep histograms
ppc_hist(penguins$bill_length, y_pred6[211:218,])

y_pred6

#' Kernel density estimate of y + 100 yrep kernel density estimates
ppc_dens_overlay(penguins$bill_length, y_pred6)

#' ECDF of y + 500 yrep ECDFs
ppc_ecdf_overlay(penguins$bill_length, y_pred1[1:500,])

#' Scatterplot of yrep vs y
ppc_scatter(penguins$bill_length, y_pred1[191:199,])+geom_abline()

color_scheme_set("brewer-Paired")
ppc_stat_2d(penguins$bill_length, y_pred1, stat=c("min","max"))
color_scheme_set()

# Linear regression PLOT
N <- 333
N_species <- 3
gamma <- summary(fit6)$summary[1:3]

posterior_samples <- extract(fit6, "gamma")$gamma
median_intercepts <- apply(posterior_samples, 2, median)

# Create a data frame for plotting
plot_data <- data.frame(
  species = 1:N_species,
  intercept = median_intercepts
)

# Plotting
ggplot(data = penguins, aes(x = bill_depth, y = bill_length, color = factor(species))) +
  geom_point()+
  geom_hline(yintercept = gamma, )


#### 10. YES Only species separate model ####
set.seed(565923)
stan_data <- list(
  N = nrow(penguins),
  N_new=c(146,119,68),
  bill_length = penguins$bill_length,
  bill_width = penguins$bill_depth,
  sex= penguins$sex,
  N_species = length(unique(penguins$species)),
  species = as.integer(penguins$species)
)
model10 = stan_model("Separate_only_species.stan")

fit10 <- sampling(model10,data=stan_data,iter=2000,warmup=500,chains=4,refresh=1200) 

plot(fit10)
summary(fit10)


class(fit10)
list_of_draws10 <- rstan::extract(fit10)
print(names(list_of_draws10))
y_pred10 <- list_of_draws10$ynew


#' Histogram of y + 8 yrep histograms
ppc_hist(penguins$bill_length, y_pred10[211:218,])

y_pred10

#' Kernel density estimate of y + 100 yrep kernel density estimates
ppc_dens_overlay(penguins$bill_length, y_pred10)

#' ECDF of y + 500 yrep ECDFs
ppc_ecdf_overlay(penguins$bill_length, y_pred10[1:500,])

#' Scatterplot of yrep vs y
ppc_scatter(penguins$bill_length, y_pred10[191:199,])+geom_abline()

color_scheme_set("brewer-Paired")
ppc_stat_2d(penguins$bill_length, y_pred10, stat=c("min","max"))
color_scheme_set()

ppc_stat(penguins$bill_length, y_pred10)


#####→ Plots #####
library("bayesplot")
library("rstanarm")
library("ggplot2")

#POSTERIOR fit2
posterior <- as.matrix(fit2)

plot_title <- ggtitle("Posteriors Hierarchical model",
                      "medians and 80% intervals")
mcmc_areas(posterior,
           pars = c("beta_width_2[1]",
                    "beta_width_2[2]", "beta_width_2[3]"),
           prob = 0.8) + plot_title

mcmc_areas(posterior,
           pars = c("beta_sex_2[1]",
                    "beta_sex_2[2]", "beta_sex_2[3]"),
           prob = 0.8) + plot_title

mcmc_areas(posterior,
           pars = c("tau_depth",
                    "tau_sex", "sigma"),
           prob = 0.8) + plot_title

mcmc_areas(posterior,
           pars = c("gamma[1]",
                    "gamma[2]", "gamma[3]"),
           prob = 0.8) + plot_title


#POSTERIOR fit3
posterior <- as.matrix(fit3)

plot_title <- ggtitle("Posteriors Separate model",
                      "medians and 80% intervals")
mcmc_areas(posterior,
           pars = c("beta_width_2[1]",
                    "beta_width_2[2]", "beta_width_2[3]"),
           prob = 0.8) + plot_title

plot_title <- ggtitle("Posteriors Separate model",
                      "medians and 80% intervals")
mcmc_areas(posterior,
           pars = c("beta_sex_2[1]",
                    "beta_sex_2[2]", "beta_sex_2[3]"),
           prob = 0.8) + plot_title

plot_title <- ggtitle("Posteriors Separate model",
                      "medians and 80% intervals")
mcmc_areas(posterior,
           pars = c("gamma[1]",
                    "gamma[2]", "gamma[3]"),
           prob = 0.8) + plot_title

plot_title <- ggtitle("Posteriors Separate model",
                      "medians and 80% intervals")
mcmc_areas(posterior,
           pars = c("sigma"),
           prob = 0.8) + plot_title


intercept <- summary(fit3)$summary[c(1,2,3)]
beta.w <- summary(fit3)$summary[c(4,5,6)]
beta.s <- summary(fit3)$summary[c(7,8,9)]


#POSTERIORS fit8
posterior <- as.matrix(fit8)

plot_title <- ggtitle("Posteriors Model 3",
                      "medians and 80% intervals")
mcmc_areas(posterior,
           pars = c("beta_width_species[1]",
                    "beta_width_species[2]", "beta_width_species[3]"),
           prob = 0.8) + plot_title

mcmc_areas(posterior,
           pars = c("gamma_sex[1]",
                    "gamma_sex[2]"),
           prob = 0.8) + plot_title

mcmc_areas(posterior,
           pars = c("gamma_species[1]",
                    "gamma_species[2]", "gamma_species[3]"),
           prob = 0.8) + plot_title

plot_title <- ggtitle("Posteriors Separate model",
                      "medians and 80% intervals")
mcmc_areas(posterior,
           pars = c("sigma"),
           prob = 0.8) + plot_title

plot_title <- ggtitle("Posteriors Separate model",
                      "medians and 80% intervals")
mcmc_areas(posterior,
           pars = c("sigma"),
           prob = 0.8) + plot_title


######→ Model selection ######

#### Coefficient comparison ####
round(summary(fit3)$summary, 3)
round(summary(fit2)$summary, 3)
round(summary(fit8)$summary, 3)

plot(fit3)
plot(fit2)
plot(fit8)

#### LOO ####
loo0<-loo(sample, moment_match = TRUE)
loo2 <- loo(fit2, moment_match = TRUE) #Hierarchical 1
loo3 <- loo(fit3, moment_match = TRUE) #Separate informative
loo4 <- loo(fit4, moment_match = TRUE) #Separate spec 1 + Hierarch spec 2-3
loo6 <- loo(fit6, moment_match = TRUE) #Just intercept
loo8 <- loo(fit8, moment_match = TRUE) #All separate only Sex intercept
loo10 <- loo(fit10, moment_match = TRUE) #No sex separate
loo_compare(loo0, loo2, loo3, loo4, loo6, loo8,loo10)


#### RMSE e MAE #####


rmse1 <- sqrt(mean((y_pred1 - penguins$bill_length)^2))
mae1 <- mean(abs(y_pred1 - penguins$bill_length))
rmse2 <- sqrt(mean((y_pred2 - penguins$bill_length)^2))
mae2 <- mean(abs(y_pred2 - penguins$bill_length))
rmse3 <- sqrt(mean((y_pred3 - penguins$bill_length)^2))
mae3 <- mean(abs(y_pred3 - penguins$bill_length))
rmse4 <- sqrt(mean((y_pred4 - penguins$bill_length)^2))
mae4 <- mean(abs(y_pred4 - penguins$bill_length))

rmse5 <- sqrt(mean((y_pred5 - penguins$bill_length)^2))
mae5 <- mean(abs(y_pred5 - penguins$bill_length))
rmse6 <- sqrt(mean((y_pred6 - penguins$bill_length)^2))
mae6 <- mean(abs(y_pred6 - penguins$bill_length))
rmse7 <- sqrt(mean((y_pred7 - penguins$bill_length)^2))
mae7 <- mean(abs(y_pred7 - penguins$bill_length))
rmse8 <- sqrt(mean((y_pred8 - penguins$bill_length)^2))
mae8 <- mean(abs(y_pred8 - penguins$bill_length))
rmse9 <- sqrt(mean((y_pred9 - penguins$bill_length)^2))
mae9 <- mean(abs(y_pred9 - penguins$bill_length))


####→Sensitivity analysis ####

#### model3 ####
##uninformativeness####
set.seed(565923)
stan_data <- list(
  N = nrow(penguins),
  N_new=c(146, 119, 68),
  bill_length = penguins$bill_length,
  bill_depth = penguins$bill_depth,
  sex= penguins$sex,
  N_species = length(unique(penguins$species)),
  species = as.integer(penguins$species),
  N_ger = 2
)

sep_unin = stan_model("sens1.stan")

sens1 <- sampling(sep_unin,data=stan_data,iter=2000,warmup=500,chains=4,refresh=1200) 

plot(sens1)
summary(sens1)$summary

rstan::traceplot(sens1,pars=c("gamma[1]","gamma[2]","gamma[3]","beta_width_2[1]","beta_width_2[2]","beta_width_2[3]","beta_sex_2[1]","beta_sex_2[2]","beta_sex_2[3]"), inc_warmup=TRUE, nrow=3)


class(sens1)
list_of_drawssens1 <- rstan::extract(sens1)
print(names(list_of_drawssens1))
y_predsens1 <- list_of_drawssens1$ynew


#' Histogram of y + 8 yrep histograms
ppc_hist(penguins$bill_length, y_predsens1[211:218,])

y_predsens1

#' Kernel density estimate of y + 100 yrep kernel density estimates
ppc_dens_overlay(penguins$bill_length, y_predsens1)

#' ECDF of y + 500 yrep ECDFs
ppc_ecdf_overlay(penguins$bill_length, y_predsens1[1:500,])

#' Scatterplot of yrep vs y
ppc_scatter(penguins$bill_length, y_predsens1[191:199,])+geom_abline()

color_scheme_set("brewer-Paired")
ppc_stat_2d(penguins$bill_length, y_predsens1, stat=c("min","max"))
color_scheme_set()

ppc_stat(penguins$bill_length, y_predsens1)

loosens1 <- loo(sens1, moment_match = TRUE)

#wrong parameters####
set.seed(565923)
stan_data <- list(
  N = nrow(penguins),
  N_new=c(146, 119, 68),
  bill_length = penguins$bill_length,
  bill_depth = penguins$bill_depth,
  sex= penguins$sex,
  N_species = length(unique(penguins$species)),
  species = as.integer(penguins$species),
  N_ger = 2
)

sep_wrong = stan_model("sens2.stan")

sens2 <- sampling(sep_wrong,data=stan_data,iter=2000,warmup=500,chains=4,refresh=1200) 

plot(sens2)
summary(sens2)$summary

rstan::traceplot(sens2,pars=c("gamma[1]","gamma[2]","gamma[3]","beta_width_2[1]","beta_width_2[2]","beta_width_2[3]","beta_sex_2[1]","beta_sex_2[2]","beta_sex_2[3]"), inc_warmup=TRUE, nrow=3)


class(sens2)
list_of_drawssens2 <- rstan::extract(sens2)
print(names(list_of_drawssens2))
y_predsens2 <- list_of_drawssens2$ynew


#' Histogram of y + 8 yrep histograms
ppc_hist(penguins$bill_length, y_predsens2[211:218,])

y_predsens2

#' Kernel density estimate of y + 100 yrep kernel density estimates
ppc_dens_overlay(penguins$bill_length, y_predsens2)

#' ECDF of y + 500 yrep ECDFs
ppc_ecdf_overlay(penguins$bill_length, y_predsens2[1:500,])

#' Scatterplot of yrep vs y
ppc_scatter(penguins$bill_length, y_predsens2[191:199,])+geom_abline()

color_scheme_set("brewer-Paired")
ppc_stat_2d(penguins$bill_length, y_predsens2, stat=c("min","max"))
color_scheme_set()

ppc_stat(penguins$bill_length, y_predsens2)

loosens2 <- loo(sens2, moment_match = TRUE)


#### model2 ####
set.seed(565923)
stan_data <- list(
  N = nrow(penguins),
  N_new=c(146,119,68),
  bill_length = penguins$bill_length,
  bill_width = penguins$bill_depth,
  sex= penguins$sex,
  N_species = length(unique(penguins$species)),
  species = as.integer(penguins$species)
)
# setting the priors very uninformative
summary(fit2)
sep_unin = stan_model("sens1_hier.stan")

sens1_hier <- sampling(sep_unin,data=stan_data,iter=2000,warmup=500,chains=4,refresh=1200) 

plot(sens1_hier)
round(summary(sens1_hier)$summary[1:12,1:2],5)

rstan::traceplot(sens1_hier,pars=c("gamma[1]","gamma[2]","gamma[3]","beta_width_2[1]","beta_width_2[2]","beta_width_2[3]","beta_sex_2[1]","beta_sex_2[2]","beta_sex_2[3]"), inc_warmup=TRUE, nrow=3)


class(sens1_hier)
list_of_drawssens1_hier <- rstan::extract(sens1_hier)
print(names(list_of_drawssens1_hier))
y_predsens1_hier <- list_of_drawssens1_hier$ynew


#' Histogram of y + 8 yrep histograms
ppc_hist(penguins$bill_length, y_predsens1_hier[211:218,])

y_predsens1_hier

#' Kernel density estimate of y + 100 yrep kernel density estimates
ppc_dens_overlay(penguins$bill_length, y_predsens1_hier)

#' ECDF of y + 500 yrep ECDFs
ppc_ecdf_overlay(penguins$bill_length, y_predsens1_hier[1:500,])

#' Scatterplot of yrep vs y
ppc_scatter(penguins$bill_length, y_predsens1_hier[191:199,])+geom_abline()

color_scheme_set("brewer-Paired")
ppc_stat_2d(penguins$bill_length, y_predsens1_hier, stat=c("min","max"))
color_scheme_set()

ppc_stat(penguins$bill_length, y_predsens1_hier)

loosens1_hier <- loo(sens1_hier, moment_match = TRUE)

#wrong prior parameters####
set.seed(565923)
stan_data <- list(
  N = nrow(penguins),
  N_new=c(146, 119, 68),
  bill_length = penguins$bill_length,
  bill_depth = penguins$bill_depth,
  sex= penguins$sex,
  N_species = length(unique(penguins$species)),
  species = as.integer(penguins$species)
)

sep_wrong = stan_model("sens2_hier.stan")

sens2_hier <- sampling(sep_wrong,data=stan_data,iter=2000,warmup=500,chains=4,refresh=1200) 

plot(sens2_hier)
round(summary(sens2_hier)$summary[1:12,1:2],6)

rstan::traceplot(sens2_hier,pars=c("gamma[1]","gamma[2]","gamma[3]","beta_width_2[1]","beta_width_2[2]","beta_width_2[3]","beta_sex_2[1]","beta_sex_2[2]","beta_sex_2[3]"), inc_warmup=TRUE, nrow=3)


class(sens2_hier)
list_of_drawssens2_hier <- rstan::extract(sens2_hier)
print(names(list_of_drawssens2_hier))
y_predsens2_hier <- list_of_drawssens2_hier$ynew


#' Histogram of y + 8 yrep histograms
ppc_hist(penguins$bill_length, y_predsens2_hier[211:218,])

y_predsens2_hier

#' Kernel density estimate of y + 100 yrep kernel density estimates
ppc_dens_overlay(penguins$bill_length, y_predsens2_hier)

#' ECDF of y + 500 yrep ECDFs
ppc_ecdf_overlay(penguins$bill_length, y_predsens2_hier[1:500,])

#' Scatterplot of yrep vs y
ppc_scatter(penguins$bill_length, y_predsens2_hier[191:199,])+geom_abline()

color_scheme_set("brewer-Paired")
ppc_stat_2d(penguins$bill_length, y_predsens2_hier, stat=c("min","max"))
color_scheme_set()

ppc_stat(penguins$bill_length, y_predsens2_hier)

loosens2_hier <- loo(sens2_hier, moment_match = TRUE)

###model 8###
set.seed(565923)
stan_data <- list(
  N = nrow(penguins),
  N_new=c(146, 119, 68),
  bill_length = penguins$bill_length,
  bill_width = penguins$bill_depth,
  sex= penguins$sex,
  N_species = length(unique(penguins$species)),
  species = as.integer(penguins$species),
  N_sex = length(unique(penguins$sex)),
  sex_id = as.integer(penguins$sex+1)
)

summary(fit8)$summary[1:9,1:2]
sep_var_sens1 = stan_model("sep_spe_sex_sens1.stan")

sens1_sep_var <- sampling(sep_var_sens1,data=stan_data,iter=2000,warmup=500,chains=4,refresh=1200) 

plot(sens1_sep_var)
round(summary(sens1_sep_var)$summary[1:9,1:2],5)

rstan::traceplot(sens1_sep_var,pars=c("gamma[1]","gamma[2]","gamma[3]","beta_width_2[1]","beta_width_2[2]","beta_width_2[3]","beta_sex_2[1]","beta_sex_2[2]","beta_sex_2[3]"), inc_warmup=TRUE, nrow=3)


class(sens1_sep_var)
list_of_drawssens1_sep_var <- rstan::extract(sens1_sep_var)
print(names(list_of_drawssens1_sep_var))
y_predsens1_sep_var <- list_of_drawssens1_sep_var$ynew


#' Histogram of y + 8 yrep histograms
ppc_hist(penguins$bill_length, y_predsens1_sep_var[211:218,])

y_predsens1_sep_var

#' Kernel density estimate of y + 100 yrep kernel density estimates
ppc_dens_overlay(penguins$bill_length, y_predsens1_sep_var)

#' ECDF of y + 500 yrep ECDFs
ppc_ecdf_overlay(penguins$bill_length, y_predsens1_sep_var[1:500,])

#' Scatterplot of yrep vs y
ppc_scatter(penguins$bill_length, y_predsens1_sep_var[191:199,])+geom_abline()

color_scheme_set("brewer-Paired")
ppc_stat_2d(penguins$bill_length, y_predsens1_sep_var, stat=c("min","max"))
color_scheme_set()

ppc_stat(penguins$bill_length, y_predsens1_sep_var)

loosens1_sep_var <- loo(sens1_sep_var, moment_match = TRUE)

#wrong parameters####
set.seed(565923)

sep_var_wrong = stan_model("sep_spe_sex_sens2.stan")

sens2_sep_var <- sampling(sep_var_wrong,data=stan_data,iter=2000,warmup=500,chains=4,refresh=1200) 

plot(sens2_sep_var)
round(summary(sens2_sep_var)$summary[1:9,1:2],6)


class(sens2_sep_var)
list_of_drawssens2_sep_var <- rstan::extract(sens2_sep_var)
print(names(list_of_drawssens2_sep_var))
y_predsens2_sep_var <- list_of_drawssens2_sep_var$ynew


#' Histogram of y + 8 yrep histograms
ppc_hist(penguins$bill_length, y_predsens2_sep_var[211:218,])


#' Kernel density estimate of y + 100 yrep kernel density estimates
ppc_dens_overlay(penguins$bill_length, y_predsens2_sep_var)

#' ECDF of y + 500 yrep ECDFs
ppc_ecdf_overlay(penguins$bill_length, y_predsens2_sep_var[1:500,])

#' Scatterplot of yrep vs y
ppc_scatter(penguins$bill_length, y_predsens2_sep_var[191:199,])+geom_abline()

color_scheme_set("brewer-Paired")
ppc_stat_2d(penguins$bill_length, y_predsens2_sep_var, stat=c("min","max"))
color_scheme_set()

ppc_stat(penguins$bill_length, y_predsens2_sep_var)

loosens2_sep_var <- loo(sens2_sep_var, moment_match = TRUE)
