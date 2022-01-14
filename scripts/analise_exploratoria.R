## Análise exploratória

library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(MNP)
library(nnet)
library(arm)


here()

# importando dados processados
compartilhamento_3_estudos <- readRDS("Transformados/compartilhamento_3_estudos.rds")

glimpse(compartilhamento_3_estudos)

# modelo multinomial
compartilhamento1 <- compartilhamento_3_estudos %>%
  filter(!is.na(decisao_compartilhamento)) %>%
  filter(estudo == "estudo 1",
         genero != "outros") %>% #exclui outros, pois tem muito poucos casos
  droplevels() %>%
  mutate(atitude_lula_bolsonaro_recode = if_else(politico == "Bolsonaro", atitude_lula_bolsonaro_recode, -atitude_lula_bolsonaro_recode))
  
glimpse(compartilhamento1) # n 146

# function to compute p-values based on z-scores (2-tailed z test)

multi_p_value <- function(fit_object) {
  z <- summary(fit_object)$coefficients/summary(fit_object)$standard.errors
  p <- (1 - pnorm(abs(z), 0, 1)) * 2
  p
}

fit0 <- multinom(decisao_compartilhamento ~ politico,
                 data = compartilhamento1)
summary(fit0)
multi_p_value(fit0)

fit1 <- multinom(decisao_compartilhamento ~ politico*genero,
                 data = compartilhamento1)

summary(fit1)
multi_p_value(fit1)

fit2 <- multinom(decisao_compartilhamento ~ politico*genero + idade,
                 data = compartilhamento1)

summary(fit2)
multi_p_value(fit2)

# 
# Não rodar - ainda em teste se funciona
# m1 <- mnp(decisao_compartilhamento ~ atitude_lula_bolsonaro_recode + score_narcissism,
#           data = compartilhamento1, n.draws = 4000,
#            burnin = 2000, thin = 3, verbose = TRUE)
# summary(m1)

