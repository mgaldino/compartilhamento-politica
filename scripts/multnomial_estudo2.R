# Modelo Bayesiano para estudo 2

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

compartilhamento_3_estudos <- readRDS("Transformados/compartilhamento_3_estudos.rds")

# Calcular scores de narcisismo e identidade nacional
compartilhamento_3_estudos <- compartilhamento_3_estudos %>%
  mutate(score_narcisism = select(., 24:26) %>% apply(1, sum, na.rm=TRUE))
compartilhamento_3_estudos <- compartilhamento_3_estudos %>%
  mutate(score_id_nac = select(., 27:28) %>% apply(1, sum, na.rm=TRUE))

# estudo 2
compartilhamento2 <- compartilhamento_3_estudos %>%
  filter(!is.na(decisao_compartilhamento)) %>%
  filter(Estudo == 2,
         genero != "outros") %>% #exclui outros, pois tem muito poucos casos
  droplevels() %>%
  mutate(nivel_identificacao_recode = 
           if_else(as.character(politico_escolhido) == "Lula", nivel_identificacao_recode*-1, nivel_identificacao_recode)) # multiplica score dos lulitas por -1

# transformado os bancos de tiblle para df
dat <- as.data.frame(compartilhamento2)


#modelos a serem estimados

f0 <- as.formula("decisao_compartilhamento ~ nivel_identificacao_recode")
f1 <- as.formula("decisao_compartilhamento ~ nivel_identificacao_recode + genero")
f2 <- as.formula("decisao_compartilhamento ~ nivel_identificacao_recode + liberal_conservador + score_narcisism + ladder + genero")

# dados pro Stan para cada modelo

# base estudo 2
M0 <- model.matrix(f0, dat)
M1 <- model.matrix(f1, dat)
M2 <- model.matrix(f2, dat)

# dados para y rep
dat_pred_1 <- dat %>%
  mutate(intercept = 1) %>%
  dplyr::select(intercept, nivel_identificacao_recode)

dat_pred_2 <- dat %>%
  mutate(intercept = 1) %>%
  dplyr::select(intercept, nivel_identificacao_recode, liberal_conservador)

dat_pred_3 <- dat %>%
  mutate(intercept = 1) %>%
  dplyr::select(intercept, nivel_identificacao_recode, liberal_conservador, score_narcisism, ladder, genero)

list_dat_pred <- list(dat_pred_1, dat_pred_2, dat_pred_3)
lista <- list(M0, M1, M2)
lista_fits <- list()
lista_results <- list()
lista_loo <- list()
lista_psis <- list()
lista_dados_ppc <- list()

# loop que cria dados e roda cada modelo no stan
for (i in 1:3) {
  # cria dados
  datlist <- list(N=nrow(lista[[i]]),                     #nr of obs
                  K=length(unique(dat[,51])),     #possible outcomes
                  D=ncol(lista[[i]]),                     #dimension of predictor matrix
                  x=lista[[i]],                           #predictor matrix
                  y=as.numeric(dat[,51]),
                  N_new = nrow(list_dat_pred[[i]]), # num obs prediction set
                  x_new = list_dat_pred[[i]])      # new pred matrix
  
  lista_dados_ppc[[i]] <- datlist
  # estima model
  
  fit <- stan(file ="stan_model_pred_v2.stan", 
              data = datlist,
              iter = 2000,
              chains = 4,
              seed = 1259,
              control = list(max_treedepth = 15))
  
  lista_fits[[i]] <- fit # armazena os resultados
  lista_results[[i]] <- summary(lista_fits[[i]], par=c("beta", "y_new"), probs=.5)$summary %>% as.data.frame
  
  # loo
  log_lik <- extract_log_lik(lista_fits[[i]], merge_chains = FALSE)
  r_eff <- relative_eff(exp(log_lik)) 
  loo_1 <- loo(log_lik, r_eff = r_eff, save_psis = TRUE)
  psis <- loo_1$psis_objec
  lista_loo[[i]] <- loo_1
  lista_psis[[i]] <- psis
  
  print(i)
}
rm(fit) # remove modelo, já que tá tudo na lista



## PPC modelo 1

array_of_draws <- as.array(lista_fits[[1]], pars = "beta")

summarise_draws(array_of_draws)

array_of_draws <- as.array(lista_fits[[3]], pars = "beta")
summarise_draws(array_of_draws)

# modelo 1
y_rep1 <- as.matrix(lista_fits[[1]], pars = "y_new")
ppc_dens_overlay(lista_dados_ppc[[1]]$y, y_rep1[1:nrow(lista_results[[1]]), ])
ppc_bars(lista_dados_ppc[[1]]$y, y_rep1[1:nrow(lista_results[[1]]), ], prob = .95, freq = F)
ppc_hist(lista_dados_ppc[[1]]$y, y_rep1[1:20, ])

# modelo 2
y_rep2 <- as.matrix(lista_fits[[2]], pars = "y_new")
ppc_dens_overlay(lista_dados_ppc[[2]]$y, y_rep2[1:nrow(lista_results[[2]]), ])
ppc_bars(lista_dados_ppc[[2]]$y, y_rep2[1:nrow(lista_results[[2]]), ], prob = .95, freq = F)
ppc_hist(lista_dados_ppc[[2]]$y, y_rep2[1:20, ])

# modelo 3
y_rep3 <- as.matrix(lista_fits[[3]], pars = "y_new")
ppc_dens_overlay(lista_dados_ppc[[3]]$y, y_rep3[1:nrow(lista_results[[3]]), ])
ppc_bars(lista_dados_ppc[[3]]$y, y_rep3[1:nrow(lista_results[[3]]), ], prob = .95, freq = F)
ppc_hist(lista_dados_ppc[[3]]$y, y_rep3[1:20, ])


# loo

print(lista_loo[[1]])
print(lista_loo[[2]])
print(lista_loo[[3]])

comp <- loo_compare(lista_loo[[1]], lista_loo[[2]], lista_loo[[3]])
print(comp)

## nvestigando ppc ruim

compartilhamento1 %>%
  group_by(genero, decisao_compartilhamento) %>%
  summarise(amostra_total = n(), .groups="drop") 

compartilhamento1 %>%
  group_by(decisao_compartilhamento) %>%
  summarise(amostra_total = n(), media_ladder = mean(ladder)) 


## para fazer diagnósticos, conferir
# https://personalpages.manchester.ac.uk/staff/david.selby/rthritis/2021-03-26-stan/Bayesian-modelling-with-Stan.html#15
# https://study.sagepub.com/sites/default/files/chapter16.pdf
# hierarchical https://rpubs.com/TCEagle/672172 e https://www.occasionaldivergences.com/post/choice-models/



