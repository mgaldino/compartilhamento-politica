library(nnet)
library(rstan)
library(ggplot2)
library(tidyverse)
library(bayesplot)
library(loo)
library(rstanarm)
library(posterior)
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
f1 <- as.formula("decisao_compartilhamento ~ nivel_identificacao_recode + genero")
f2 <- as.formula("decisao_compartilhamento ~ nivel_identificacao_recode + liberal_conservador + score_narcisism + ladder + genero")

# dados pro Stan para cada modelo

# base Bolsonaro
M0b <- model.matrix(f0, dat_b)
M1b <- model.matrix(f1, dat_b)
M2b <- model.matrix(f2, dat_b)

# base Lula
M0l <- model.matrix(f0, dat_l)
M1l <- model.matrix(f1, dat_l)
M2l <- model.matrix(f2, dat_l)

# dados para y rep
dat_pred_b0 <- dat_b %>%
  mutate(intercept = 1) %>%
  dplyr::select(intercept, nivel_identificacao_recode)

dat_pred_b1 <- dat_b %>%
  mutate(intercept = 1) %>%
  dplyr::select(intercept, nivel_identificacao_recode, liberal_conservador)

dat_pred_b2 <- dat_b %>%
  mutate(intercept = 1) %>%
  dplyr::select(intercept, nivel_identificacao_recode, liberal_conservador, score_narcisism, ladder, genero)

dat_pred_l0 <- dat_l %>%
  mutate(intercept = 1) %>%
  dplyr::select(intercept, nivel_identificacao_recode)

dat_pred_l1 <- dat_l %>%
  mutate(intercept = 1) %>%
  dplyr::select(intercept, nivel_identificacao_recode, liberal_conservador)

dat_pred_l2 <- dat_l %>%
  mutate(intercept = 1) %>%
  dplyr::select(intercept, nivel_identificacao_recode, liberal_conservador, score_narcisism, score_id_nac)

list_dat_pred <- list(dat_pred_b0, dat_pred_b1, dat_pred_b2)
listab <- list(M0b, M1b, M2b)
lista_fits_b <- list()
lista_results_b <- list()
lista_loo_b <- list()
lista_psis_b <- list()
lista_dados_ppc <- list()

# loop que cria dados e roda cada modelo no stan
for (i in 1:3) {
  # cria dados
  datlist_b <- list(N=nrow(listab[[i]]),                     #nr of obs
                    K=length(unique(dat_b[,51])),     #possible outcomes
                    D=ncol(listab[[i]]),                     #dimension of predictor matrix
                    x=listab[[i]],                           #predictor matrix
                    y=as.numeric(dat_b[,51]),
                    N_new = nrow(list_dat_pred[[i]]), # num obs prediction set
                    x_new = list_dat_pred[[i]])      # new pred matrix
  
  lista_dados_ppc[[i]] <- datlist_b
  # estima model
  
  fit_b <- stan(file ="stan_model_pred_v2.stan", 
                       data = datlist_b,
                       iter = 2000,
                       chains = 4,
                       seed = 1259,
                       control = list(max_treedepth = 15))
  
  lista_fits_b[[i]] <- fit_b # armazena os resultados
  lista_results_b[[i]] <- summary(lista_fits_b[[i]], par=c("beta", "y_new"), probs=.5)$summary %>% as.data.frame
  
  # loo
  log_lik_b <- extract_log_lik(lista_fits_b[[i]], merge_chains = FALSE)
  r_eff_b <- relative_eff(exp(log_lik_b)) 
  loo_1_b <- loo(log_lik_b, r_eff = r_eff_b, save_psis = TRUE)
  psis_b <- loo_1_b$psis_objec
  lista_loo_b[[i]] <- loo_1_b
  lista_psis_b[[i]] <- psis_b
 
  print(i)
}
rm(fit_b) # remove modelo, já que tá tudo na lista


## Lula
list_dat_pred <- list(dat_pred_l0, dat_pred_l1, dat_pred_l2)
listal <- list(M0l, M1l, M2l)
lista_fits_l <- list()
lista_results_l <- list()
lista_loo_l <- list()
lista_psis_l <- list()
lista_dados_ppc_l <- list()
# loop que cria dados e roda cada modelo no stan pro Bolsonaro
for (i in 1:3) {
  # cria dados
  datlist_l <- list(N=nrow(listal[[i]]),                     #nr of obs
                    K=length(unique(dat_b[,51])),     #possible outcomes
                    D=ncol(listal[[i]]),                     #dimension of predictor matrix
                    x=listal[[i]],                           #predictor matrix
                    y=as.numeric(dat_b[,51]),
                    N_new = nrow(dat_pred_b), # num obs prediction set
                    x_new = dat_pred_b)      # new pred matrix
  
  lista_dados_ppc_l[[i]] <- datlist_b
  # estima model
  
  fit_l <- stan(file ="stan_model_pred_v2.stan", 
                data = datlist_l,
                iter = 2000,
                chains = 4,
                seed = 1259,
                control = list(max_treedepth = 15))
  
  lista_fits_l[[i]] <- fit_l # armazena os resultados
  lista_results_l[[i]] <- summary(lista_fits_l[[i]], par=c("beta", "y_new"), probs=.5)$summary %>% as.data.frame
  
  # loo
  log_lik_l <- extract_log_lik(lista_fits_l[[i]], merge_chains = FALSE)
  r_eff_l <- relative_eff(exp(log_lik_l)) 
  loo_1_l <- loo(log_lik_l, r_eff = r_eff_l, save_psis = TRUE)
  psis_l <- loo_1_l$psis_objec
  lista_loo_l[[i]] <- loo_1_l
  lista_psis_l[[i]] <- psis_l
  
  print(i)
}

rm(fit_l) # remove modelo, já que tá tudo na lista


## PPC

# bolsonaro
array_of_draws <- as.array(lista_fits_b[[1]], pars = "beta")

summarise_draws(array_of_draws)

array_of_draws <- as.array(lista_fits_b[[3]], pars = "beta")
summarise_draws(array_of_draws)
# modelo 1
y_rep_b1 <- as.matrix(lista_fits_b[[1]], pars = "y_new")
ppc_dens_overlay(lista_dados_ppc[[1]]$y, y_rep_b1[1:nrow(lista_results_b[[1]]), ])
ppc_bars(lista_dados_ppc[[1]]$y, y_rep_b1[1:nrow(lista_results_b[[1]]), ], prob = .95, freq = F)
ppc_hist(lista_dados_ppc[[1]]$y, y_rep_b1[1:20, ])

# modelo 2
y_rep_b2 <- as.matrix(lista_fits_b[[2]], pars = "y_new")
ppc_dens_overlay(lista_dados_ppc[[2]]$y, y_rep_b2[1:nrow(lista_results_b[[2]]), ])
ppc_bars(lista_dados_ppc[[2]]$y, y_rep_b2[1:nrow(lista_results_b[[2]]), ], prob = .95, freq = F)
ppc_hist(lista_dados_ppc[[2]]$y, y_rep_b2[1:20, ])

# modelo 3
y_rep_b3 <- as.matrix(lista_fits_b[[3]], pars = "y_new")
ppc_dens_overlay(lista_dados_ppc[[3]]$y, y_rep_b3[1:nrow(lista_results_b[[3]]), ])
ppc_bars(lista_dados_ppc[[3]]$y, y_rep_b3[1:nrow(lista_results_b[[3]]), ], prob = .95, freq = F)
ppc_hist(lista_dados_ppc[[3]]$y, y_rep_b3[1:20, ])


# loo

print(lista_loo_b[[1]])
print(lista_loo_b[[2]])
print(lista_loo_b[[3]])

comp <- loo_compare(lista_loo_b[[1]], lista_loo_b[[2]], lista_loo_b[[3]])
print(comp)

## nvestigando ppc ruim

compartilhamento1_b %>%
  group_by(genero, decisao_compartilhamento) %>%
  summarise(amostra_total = n(), .groups="drop") 

compartilhamento1_b %>%
  group_by(decisao_compartilhamento) %>%
  summarise(amostra_total = n(), media_ladder = mean(ladder)) 

# Lula
# modelo 1
y_rep_11 <- as.matrix(lista_fits_l[[1]], pars = "y_new")
ppc_dens_overlay(lista_dados_ppc_l[[1]]$y, y_rep_11[1:nrow(lista_results_l[[1]]), ])
ppc_bars(lista_dados_ppc_l[[1]]$y, y_rep_11[1:nrow(lista_results_b[[1]]), ], prob = .95, freq = F)
ppc_hist(lista_dados_ppc_l[[1]]$y, y_rep_11[1:20, ])

# modelo 2
y_rep_l2 <- as.matrix(lista_fits_l[[2]], pars = "y_new")
ppc_dens_overlay(lista_dados_ppc_l[[2]]$y, y_rep_l2[1:nrow(lista_results_l[[2]]), ])
ppc_bars(lista_dados_ppc_l[[2]]$y, y_rep_l2[1:nrow(lista_results_l[[2]]), ], prob = .95, freq = F)
ppc_hist(lista_dados_ppc_l[[2]]$y, y_rep_l2[1:20, ])

# modelo 3
y_rep_l3 <- as.matrix(lista_fits_l[[3]], pars = "y_new")
ppc_dens_overlay(lista_dados_ppc_l[[3]]$y, y_rep_l3[1:nrow(lista_results_l[[3]]), ])
ppc_bars(lista_dados_ppc_l[[3]]$y, y_rep_l3[1:nrow(lista_results_l[[3]]), ], prob = .95, freq = F)
ppc_hist(lista_dados_ppc_l[[3]]$y, y_rep_l3[1:20, ])

lista_results_b[[1]][1:5,]


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