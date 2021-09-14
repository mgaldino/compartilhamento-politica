## Análise exploratória

library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(MNP)


here()

# importando dados processados
compartilhamento_3_estudos <- readRDS("Transformados/compartilhamento_3_estudos.rds")

glimpse(compartilhamento_3_estudos)

# modelo multinomial
compartilhamento1 <- compartilhamento_3_estudos %>%
  filter(!is.na(decisao_compartilhamento)) %>%
  filter(estudo == "estudo 1") %>%
  mutate(atitude_lula_bolsonaro_recode = if_else(politico == "Bolsonaro", atitude_lula_bolsonaro_recode, -atitude_lula_bolsonaro_recode))
  
glimpse(compartilhamento1)
m1 <- mnp(decisao_compartilhamento ~ atitude_lula_bolsonaro_recode + score_narcissism,
          data = compartilhamento1, n.draws = 4000,
           burnin = 2000, thin = 3, verbose = TRUE)
summary(m1)

