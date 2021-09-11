
library(here)
library(dplyr)
library(tidyr)
library(ggplot2)


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

x11() # meu r está com bug e o ggplot trava o R. Para não travar preciso rodar esse comando antes.
# se seu R está funcionando, pode ignorar essa linha
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
  filter(estudo == "estudo 1") %>%
  filter(!is.na(decisao_compartilhamento)) %>%
  mutate(decisao_compartilhamento1 = if_else(grepl("Não", decisao_compartilhamento), decisao_compartilhamento, 
                                             str_sub(decisao_compartilhamento, 45, end = 60)),
         decisao_compartilhamento1 = gsub("stas ", "", decisao_compartilhamento1)) %>%
  group_by(politico, decisao_compartilhamento1) %>%
  summarise(total = n()) %>%
  pivot_wider(names_from = decisao_compartilhamento1, 
              values_from = total,
              values_fill = 0) %>%
  clean_names() %>%
  mutate(total = nao_publicar_nenhuma_noticia + opcao_1 + opcao_2,
           prop_op1 = opcao_1 /total,
         prop_op2 = opcao_2/total,
         nenhuma = nao_publicar_nenhuma_noticia/total)

compartilhamento_1_analise <- compartilhamento_3_estudos %>%
  mutate(decisao_compartilhamento = factor(decisao_compartilhamento)) %>%
  filter(estudo == "estudo 1") %>%
  filter(!is.na(decisao_compartilhamento))
  


