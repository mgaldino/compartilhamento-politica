library(nnet)
library(rstan)
library(ggplot2)
library(tidyverse)
library(bayesplot)
library(loo)
library(rstanarm)
# For execution on a local, multicore CPU with excess RAM
options(mc.cores = parallel::detectCores())

#To avoid recompilation of unchanged Stan programs
rstan_options(auto_write = TRUE)

# function to compute p-values based on z-scores (2-tailed z test)

multi_p_value <- function(fit_object) {
  z <- summary(fit_object)$coefficients/summary(fit_object)$standard.errors
  p <- (1 - pnorm(abs(z), 0, 1)) * 2
  p
}

compartilhamento_3_estudos <- readRDS("Transformados/compartilhamento_3_estudos.rds")

# Calcular scores de narcisismo e identidade nacional
compartilhamento_3_estudos <- compartilhamento_3_estudos %>%
  mutate(score_narcisism = select(., 24:26) %>% apply(1, sum, na.rm=TRUE))
compartilhamento_3_estudos <- compartilhamento_3_estudos %>%
  mutate(score_id_nac = select(., 27:28) %>% apply(1, sum, na.rm=TRUE))

# estudo 1
compartilhamento1 <- compartilhamento_3_estudos %>%
  filter(!is.na(decisao_compartilhamento)) %>%
  filter(Estudo == 1,
         genero != "outros") %>% #exclui outros, pois tem muito poucos casos
  droplevels()

compartilhamento1_b <- compartilhamento1 %>%
  filter(politico_escolhido == "Bolsonaro") 

compartilhamento1_l <- compartilhamento1 %>%
  filter(politico_escolhido == "Lula")

# transformado os bancos de tiblle para df
dat_b <- as.data.frame(compartilhamento1_b)
dat_l <- as.data.frame(compartilhamento1_l)

#modelos a serem estimados

f0 <- as.formula("decisao_compartilhamento ~ nivel_identificacao_recode")
f1 <- as.formula("decisao_compartilhamento ~ nivel_identificacao_recode + liberal_conservador")
f2 <- as.formula("decisao_compartilhamento ~ nivel_identificacao_recode + liberal_conservador + score_narcisism + score_id_nac")

# dados pro Stan para cada modelo

# base Bolsonaro
M0b <- model.matrix(f0, dat_b)
M1b <- model.matrix(f1, dat_b)
M2b <- model.matrix(f2, dat_b)

# base Lula
M0l <- model.matrix(f0, dat_l)
M1l <- model.matrix(f1, dat_l)
M2l <- model.matrix(f2, dat_l)

dat_pred_b <- dat_b %>%
  mutate(intercept = 1) %>%
  dplyr::select(intercept, nivel_identificacao_recode)

dat_pred_l <- dat_l %>%
  mutate(intercept = 1) %>%
  dplyr::select(intercept, nivel_identificacao_recode)

#data for stan - Bolsonaro
datlist0_b <- list(N=nrow(M0),                     #nr of obs
                K=length(unique(dat_b[,51])),     #possible outcomes
                D=ncol(M0),                     #dimension of predictor matrix
                x=M0,                           #predictor matrix
                y=as.numeric(dat_b[,51]),
                N_new = nrow(dat_pred_b), # num obs prediction set
                x_new = dat_pred_b)      # new pred matrix

#data for stan - Lula
datlist0_l <- list(N=nrow(M1),                     #nr of obs
                   K=length(unique(dat_l[,51])),     #possible outcomes
                   D=ncol(M1),                     #dimension of predictor matrix
                   x=M1,                           #predictor matrix
                   y=as.numeric(dat_l[,51]),
                   N_new = nrow(dat_pred_l), # num obs prediction set
                   x_new = dat_pred_l)      # new pred matrix

#estimate model
bayes_out0_b <- stan(file ="stan_model_pred.stan", 
              data = datlist0_b,
              iter = 2000,
              chains = 4,
              seed = 1259,
              control = list(max_treedepth = 15))

res0_b <- summary(bayes_out0_b, par=c("beta", "y_new"), probs=.5)$summary %>% as.data.frame

log_lik_1_b <- extract_log_lik(bayes_out0_b, merge_chains = FALSE)
r_eff_b <- relative_eff(exp(log_lik_1_b)) 
loo_1_b <- loo(log_lik_1_b, r_eff = r_eff_b)
print(loo_1_b)


#estimate model
bayes_out0_l <- stan(file ="stan_model_pred.stan", 
                     data = datlist0_l,
                     iter = 2000,
                     chains = 4,
                     seed = 1259,
                     control = list(max_treedepth = 15))

res0_l <- summary(bayes_out0_l, par=c("beta", "y_new"), probs=.5)$summary %>% as.data.frame

head(res0_b, 10)
head(res0_l, 10)

summary(res0_l[7:nrow(res0_l),4])
summary(res0_b[7:nrow(res0_b),4])

## PPC

y_rep_b <- as.matrix(bayes_out0_b, pars = "y_new")
ppc_dens_overlay(datlist0_b$y, y_rep_b[1:nrow(res0_b), ])
ppc_bars(datlist0_l$y, y_rep_l[1:nrow(res0_l), ], prob = .95, freq = F)
ppc_hist(datlist0_b$y, y_rep_b[1:20, ])

y_rep_l <- as.matrix(bayes_out0_l, pars = "y_new")
ppc_dens_overlay(datlist0_l$y, y_rep_l[1:nrow(res0_l), ])
ppc_bars(datlist0_l$y, y_rep_l[1:nrow(res0_l), ], prob = .95, freq = F)
ppc_hist(datlist0_l$y, y_rep_l[1:20, ])

## to use loo


out1_b <- multinom(f1, data=compartilhamento1_b)
out1_l <- multinom(f1, data=compartilhamento1_l)


dat_b <- as.data.frame(compartilhamento1_b)
dat_l <- as.data.frame(compartilhamento1_l)
M0 <- model.matrix(f1, dat_b)
M1 <- model.matrix(f1, dat_l)


dat_pred_b <- dat_b1 %>%
  mutate(intercept = 1) %>%
  dplyr::select(intercept, nivel_identificacao_recode, liberal_conservador)

dat_pred_l <- dat_l1 %>%
  mutate(intercept = 1) %>%
  dplyr::select(intercept, nivel_identificacao_recode, liberal_conservador)

#data for stan - Bolsonaro
datlist0_b <- list(N=nrow(M0),                     #nr of obs
                   K=length(unique(dat_b[,51])),     #possible outcomes
                   D=ncol(M0),                     #dimension of predictor matrix
                   x=M0,                           #predictor matrix
                   y=as.numeric(dat_b[,51]),
                   N_new = nrow(dat_pred_b), # num obs prediction set
                   x_new = dat_pred_b)      # new pred matrix

#data for stan - Lula
datlist0_l <- list(N=nrow(M1),                     #nr of obs
                   K=length(unique(dat_l[,51])),     #possible outcomes
                   D=ncol(M1),                     #dimension of predictor matrix
                   x=M1,                           #predictor matrix
                   y=as.numeric(dat_l[,51]),
                   N_new = nrow(dat_pred_l), # num obs prediction set
                   x_new = dat_pred_l)      # new pred matrix


#estimate model
bayes_out0_b1 <- stan(file ="stan_model_pred_v2.stan", 
                     data = datlist0_b,
                     iter = 2000,
                     chains = 4,
                     seed = 1259,
                     control = list(max_treedepth = 15))

res0_b1 <- summary(bayes_out0_b1, par=c("beta", "y_new"), probs=.5)$summary %>% as.data.frame

## PPC

y_rep_b <- as.matrix(bayes_out0_b1, pars = "y_new")
yrep_b_alt <- posterior_predict(bayes_out0_b1)

ppc_dens_overlay(datlist0_b$y, y_rep_b[1:nrow(res0_b1), ])
ppc_bars(datlist0_b$y, y_rep_b[1:nrow(res0_b1), ], prob = .95, freq = F)
ppc_hist(datlist0_b$y, y_rep_b[1:20, ])

log_lik_1_b1 <- extract_log_lik(bayes_out0_b1, merge_chains = FALSE)
r_eff_b1 <- relative_eff(exp(log_lik_1_b1)) 
loo_1_b1 <- loo(log_lik_1_b1, r_eff = r_eff_b1, save_psis = TRUE)
psis1 <- loo_1_b1$psis_objec
print(loo_1_b1)

comp <- loo_compare(loo_1_b, loo_1_b1)
print(comp)

# Marginal posterior predictive checks
# see https://arxiv.org/abs/1709.01449
lw_b1 <- weights(psis1)

ppc_loo_pit_overlay(
  y = datlist0_b$y,
  yrep = y_rep_b,
  lw = lw_b1
)

bayes_out0_l1 <- stan(file ="stan_model_pred_v2.stan", 
                      data = datlist0_l,
                      iter = 2000,
                      chains = 4,
                      seed = 1259,
                      control = list(max_treedepth = 15))

res0_l1 <- summary(bayes_out0_l1, par=c("beta", "y_new"), probs=.5)$summary %>% as.data.frame





res0_l <- summary(bayes_out0_l, par=c("beta", "y_new"), probs=.5)$summary %>% as.data.frame

log_lik_1_l <- extract_log_lik(bayes_out0_l, merge_chains = FALSE)
r_eff_l <- relative_eff(exp(log_lik_1_l)) 
loo_1_l <- loo(log_lik_1_l, r_eff = r_eff_l)
print(loo_1_l)

# Compare
comp <- loo_compare(loo_1_b, loo_1_l)

## para fazer diagnósticos, conferir
# https://personalpages.manchester.ac.uk/staff/david.selby/rthritis/2021-03-26-stan/Bayesian-modelling-with-Stan.html#15
# https://study.sagepub.com/sites/default/files/chapter16.pdf
# hierarchical https://rpubs.com/TCEagle/672172 e https://www.occasionaldivergences.com/post/choice-models/


# comparando com frequentista, sanity check

out0_b <- multinom(f0, data=compartilhamento1_b)

summary(out0_b)
multi_p_value(out0_b)

out0_l <- multinom(f1, data=compartilhamento1_l)

summary(out0_l)
multi_p_value(out0_l)