
library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(janitor)


here()

# importando dados processados
compartilhamento_3_estudos <- readRDS("Transformados/compartilhamento_3_estudos.rds")

# análises descritivas do banco


# amostra por estudo
compartilhamento_3_estudos %>%
  group_by(estudo) %>%
  summarise(amostra_total = n())

# amostra por estudo e político
compartilhamento_3_estudos %>%
  group_by(estudo, politico) %>%
  summarise(amostra_total = n(), .groups="drop_last") %>%
  mutate(subtotal_estudo = sum(amostra_total),
         perc_politico = round(amostra_total/subtotal_estudo, 2))

# demográficos por estudo e político

# gênero (percentual por estudo)
compartilhamento_3_estudos %>%
  group_by(genero, estudo) %>%
  summarise(amostra_total = n(), .groups="drop") %>%
  mutate(subtotal_estudo = sum(amostra_total),
         perc_genero = round(amostra_total/subtotal_estudo, 3)) %>%
  arrange(estudo)

# idade média
compartilhamento_3_estudos %>%
  group_by(estudo, politico) %>%
  summarise(idade_mediana = median(idade), .groups="drop")

# 
# escolaridade (percentual por estudo)
escol <- compartilhamento_3_estudos %>%
  group_by(grau_de_escolaridade, estudo) %>%
  summarise(amostra_total = n(), .groups="drop") %>%
  mutate(subtotal_estudo = sum(amostra_total),
         perc_escolaridade = round(amostra_total/subtotal_estudo, 3)) %>%
  arrange(estudo)

# gráfico de barra de escolaridade

escol %>%
  ggplot(aes(y=perc_escolaridade, x=grau_de_escolaridade)) + geom_col() +
  coord_flip() + scale_y_continuous(labels = scales::percent) +
  facet_wrap(~estudo)

# cruzamento entre liberal-conservador e bolsonarismo ou lulismo

# primeiro, coloco Lula como número negativo, ou seja, 80 lula é -80 etc.
ideologia <- compartilhamento_3_estudos %>%
  mutate(liberal_conservador = as.numeric(liberal_conservador),
    atitude_lula_bolsonaro_recode = if_else(politico == "Bolsonaro", atitude_lula_bolsonaro_recode, -atitude_lula_bolsonaro_recode))

ideologia %>%
  filter(!is.na(atitude_lula_bolsonaro_recode)) %>%
  summarise(amostra = n(),
            cor(liberal_conservador, atitude_lula_bolsonaro_recode))
  # 68% de correlação.


# decisao versus politico de identificação
compartilhamento_3_estudos %>%
  filter(!is.na(decisao_compartilhamento)) %>%
  group_by(estudo, politico, decisao_compartilhamento) %>%
  summarise(total = n()) %>%
  pivot_wider(names_from = decisao_compartilhamento, 
              values_from = total,
              values_fill = 0) %>%
  clean_names() %>%
  ungroup() %>%
  group_by(estudo) %>%
  mutate(total = nao_publicar_nenhuma_noticia + opcao_1 + opcao_2,
           prop_op1 = opcao_1 /total,
         prop_op2 = opcao_2/total,
         nenhuma = nao_publicar_nenhuma_noticia/total)

# Gênero influencia decisão de compartilhar?
# estudo 1
compartilhamento_3_estudos %>%
  filter(estudo == "estudo 1",
         !is.na(decisao_compartilhamento),
         genero != "outros") %>%
  droplevels() %>%
  ggplot(aes(decisao_compartilhamento)) + geom_bar() +
  facet_grid(genero ~ politico)

# estudo 2
compartilhamento_3_estudos %>%
  filter(estudo == "estudo 2",
         !is.na(decisao_compartilhamento),
         genero != "outros") %>%
  droplevels() %>%
  ggplot(aes(decisao_compartilhamento)) + geom_bar() +
  facet_grid(genero ~ politico)


# estudo 3
compartilhamento_3_estudos %>%
  filter(estudo == "estudo 3",
         !is.na(decisao_compartilhamento),
         genero != "outros") %>%
  droplevels() %>%
  ggplot(aes(decisao_compartilhamento)) + geom_bar() +
  facet_grid(genero ~ politico)

# idade e compartilhamento

# estudo 1
compartilhamento_3_estudos %>%
  filter(estudo == "estudo 1",
         !is.na(decisao_compartilhamento),
         genero != "outros") %>%
  droplevels() %>%
  ggplot(aes(col=decisao_compartilhamento, x=idade)) + geom_density()

# opção 2 é bem maior entre mais jovens

# estudo 2
compartilhamento_3_estudos %>%
  filter(estudo == "estudo 2",
         !is.na(decisao_compartilhamento),
         genero != "outros") %>%
  droplevels() %>%
  ggplot(aes(col=decisao_compartilhamento, x=idade)) + geom_density()
# opção 2 é bem maior entre mais jovens

# estudo 3
compartilhamento_3_estudos %>%
  filter(estudo == "estudo 3",
         !is.na(decisao_compartilhamento),
         genero != "outros") %>%
  droplevels() %>%
  ggplot(aes(col=decisao_compartilhamento, x=idade)) + geom_density()
# único caso em que opção 2 não é maior entre mais jovens


## scores 

# estudo 1
compartilhamento_3_estudos %>%
  filter(estudo == "estudo 1",
         !is.na(decisao_compartilhamento),
         genero != "outros") %>%
  droplevels() %>%
  ggplot(aes(x=score_narcissism, y=score_id_nac, color=politico)) + geom_point() +
  ggtitle("Estudo 1") +
  facet_wrap(vars(politico), nrow = 2)

# estudo 2
compartilhamento_3_estudos %>%
  filter(estudo == "estudo 2",
         !is.na(decisao_compartilhamento),
         genero != "outros") %>%
  droplevels() %>%
  ggplot(aes(x=score_narcissism, y=score_id_nac, color=politico)) + geom_point() +
  ggtitle("Estudo 2") +
  facet_wrap(vars(politico), nrow = 2)

# estudo 3
compartilhamento_3_estudos %>%
  filter(estudo == "estudo 3",
         !is.na(decisao_compartilhamento),
         genero != "outros") %>%
  droplevels() %>%
ggplot(aes(x=score_narcissism, y=score_id_nac, color=politico)) + geom_point() +
  ggtitle("Estudo 3") +
  facet_wrap(vars(politico), nrow = 2)




## scores e decisão

# estudo 1
compartilhamento_3_estudos %>%
  filter(estudo == "estudo 1",
         !is.na(decisao_compartilhamento),
         genero != "outros") %>%
  droplevels() %>%
  ggplot(aes(x=score_narcissism, y=score_id_nac, color=politico)) + geom_point() +
  ggtitle("Estudo 1") +
  facet_wrap(vars(decisao_compartilhamento, politico), nrow = 3)

# estudo 2
compartilhamento_3_estudos %>%
  filter(estudo == "estudo 2",
         !is.na(decisao_compartilhamento),
         genero != "outros") %>%
  droplevels() %>%
  ggplot(aes(x=score_narcissism, y=score_id_nac, color=politico)) + geom_point() +
  ggtitle("Estudo 2") +
  facet_wrap(vars(decisao_compartilhamento, politico), nrow = 3)

# estudo 3
compartilhamento_3_estudos %>%
  filter(estudo == "estudo 3",
         !is.na(decisao_compartilhamento),
         genero != "outros") %>%
  droplevels() %>%
  ggplot(aes(x=score_narcissism, y=score_id_nac, color=politico)) + geom_point() +
  ggtitle("Estudo 3") +
  facet_wrap(vars(decisao_compartilhamento, politico), nrow = 3)