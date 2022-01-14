

library(tidyverse)
library(readr)
library(here)
library(data.table)
library(janitor)
library(stringr)
library(readxl)

# este comando, da biblioteca here, facilita que o direttório onde estão os dados não precisam ser escritos no script
## como criei um projeto com repositório, ao rodar o comando here(), o r transforma o diretório default como o diretório do projeto
## como sempre crio uma pasta Dados, onde armazendo os dados brusos, basta indicar essa pasta e o nome arquivo baixado do drive
# para importar no R, usando a biblioteca readxl


# importa os dados dos três estudos

# link 1 (estudo 2)
TCC_Estudo2 <- read_excel("Dados/TCC_Dados_Brutos_v2.xlsx", 
                               sheet = "Link 1")

# link 2 (estudo 1)
TCC_Estudo1 <- read_excel("Dados/TCC_Dados_Brutos_v2.xlsx", 
                                 sheet = "Link 2")
# link 3 (estudo 3)
TCC_Estudo3 <- read_excel("Dados/TCC_Dados_Brutos_v2.xlsx", 
                                 sheet = "Link 3")

# essa função renomeia as variáveis do banco e coloca em um formato bonito
muda_nomes_variaveis2 <- function(df) {
  if(!require(janitor)) {install.packages("janitor")}
  df <- df %>%
    janitor::clean_names() %>%
    dplyr::slice(-1) %>% # remove primeira linha
    rename(genero_especifico = x15,
           partido_especifico = x20,
           psicotropicos = voce_faz_uso_de_algum_medicamento_que_atue_no_seu_sistema_nervoso_central_isso_inclui_antidepressivos_ansioliticos_anticonvulsivantes_etc,
           doencas_psiquias = voce_tem_historico_de_doencas_neurologicas_psiquiatricas_e_psicologicas_severas,
           uso_drogas = voce_tem_historico_de_dependencia_de_alcool_ou_outras_drogas,
           filiacao_partidaria = voce_e_filiado_a_algum_partido_politico,
           iniciais = digite_abaixo_as_iniciais_de_seus_nomes_e_sobrenomes_e_sua_idade_para_que_possamos_identificar_seus_dados_mantendo_seu_anonimato_ex_lynm23,
           ladder = em_que_degrau_voce_se_posiciona,
           liberal_conservador = de_maneira_geral_voce_se_considera_liberal_ou_conservador_em_uma_perspectiva_social_igualdade_de_casamento_aborto,
           brasileiro_especial = brasileiros_merecem_tratamento_especial,
           brasileiro_importancia = poucas_pessoas_parecem_compreender_completamente_a_importancia_dos_brasileiros,
           brasileiro_reconhecimento = eu_nunca_ficarei_satisfeito_ate_que_os_brasileiros_tenham_o_reconhecimento_que_merecem,
           nacionalismo_identidade_brasileiro = eu_me_identifico_como_brasileiro,
           nacionalismo_brasileiro_quem_sou = ser_um_brasileiro_e_um_importante_aspecto_de_quem_eu_sou,
           ajuda_imigrantes = politicas_de_ajuda_a_imigrantes,
           fronteira_imigrantes = fechamento_das_fronteiras_para_imigrantes,
           atitude_lula_bolsonaro = o_quao_favoravel_voce_e_as_ideias_defendidas_pelos_partidos_associados_as_figuras_politicas_abaixo,
           img_alinhamento_bolsonarismo = escolha_a_alternativa_que_melhor_descreve_sua_relacao_com_o_grupo_de_partidos_alinhados_com_as_ideias_propostas_por_jair_bolsonaro,
           img_alinhamento_lulismo = escolha_a_alternativa_que_melhor_descreve_sua_relacao_com_o_grupo_de_partidos_alinhados_com_as_ideias_propostas_por_luis_inacio_lula_da_silva_lula,
           decisao_compartilhamento_bolsonaro = qual_a_sua_decisao_51,
           decisao_compartilhamento_lula = qual_a_sua_decisao_53,
           participacao_estudo = essa_e_a_primeira_vez_que_voce_participa_desse_estudo,
           nivel_identificacao =  o_quao_favoravel_voce_e_as_ideias_defendidas_pelos_partidos_associados_as_figuras_politicas_abaixo) 
  return(df)
}


# função que junta os dados ramificados, de bolsonaristas, lulistas e neutros
empilha_Dados_ramificados <- function(df) {
  
  df_Bolsonaro <- df %>%
    dplyr::filter(politico == "Bolsonaro") %>%
    dplyr::select(-img_alinhamento_lulismo) %>%
    dplyr::select(-decisao_compartilhamento_lula) %>%
    rename(img_alinhamento = img_alinhamento_bolsonarismo,
           decisao_compartilhamento = decisao_compartilhamento_bolsonaro)
  
  
  df_lula <- df %>%
    dplyr::filter(politico == "Lula") %>%
    dplyr::select(-img_alinhamento_bolsonarismo) %>%
    dplyr::select(-decisao_compartilhamento_bolsonaro) %>%
    rename(img_alinhamento = img_alinhamento_lulismo,
           decisao_compartilhamento = decisao_compartilhamento_lula)
  
  df_neutro <- df %>%
    dplyr::select(!starts_with("img_alinhamento")) %>%
    dplyr::select(-c("decisao_compartilhamento_lula", "decisao_compartilhamento_bolsonaro")) %>%
    dplyr::filter(politico == "neutro") %>%
    mutate(img_alinhamento = NA)
  
  
  df_junto <- bind_rows(df_Bolsonaro, df_lula, df_neutro) %>%
    mutate(atitude_lula_bolsonaro_recode = as.numeric(str_extract(atitude_lula_bolsonaro, "[0-9]+")))
  
  return(df_junto)
}

# roda a função para cada estudo

TCC_Estudo2 <- muda_nomes_variaveis2(TCC_Estudo2)
TCC_Estudo1 <- muda_nomes_variaveis2(TCC_Estudo1)
TCC_Estudo3 <- muda_nomes_variaveis2(TCC_Estudo3)



adiciona_legenda_respostas <- function(df) {
  df <- df %>%
    mutate(idade = as.numeric(idade),
           estado_civil = factor(estado_civil, levels = c("Solteiro(a)","Casado(a)","Separado(a)","Viúvo(a)"), labels = c("solteiro", "casado", "separado", "viuvo")),
           genero = factor(genero, levels = c("Homem", "Mulher", "Outro (especifique)"), labels = c("homem", "mulher", "outros")),
           grau_de_escolaridade = factor(grau_de_escolaridade, levels=c("Ensino Superior Completo", "Ensino Superior Incompleto", "Ensino Médio Completo", "Ensino Médio Incompleto", "Ensino Fundamental Completo")),
           filiacao_partidaria = factor(filiacao_partidaria, levels = unique(filiacao_partidaria)),
           nivel_identificacao = factor(nivel_identificacao, levels = unique(nivel_identificacao)))
  return(df)
} #em R, o último comando rodado é o que vai ser retornado na função. return(df) funciona para devolver o resultado final do que for rodado na função, e não os outputs intermediários. Se escrevermos unique sem () temos o código da função#


TCC_Estudo1 <- adiciona_legenda_respostas(TCC_Estudo1)
TCC_Estudo2 <- adiciona_legenda_respostas(TCC_Estudo2)
TCC_Estudo3 <- adiciona_legenda_respostas(TCC_Estudo3)

#Remover participantes que não concluíram a pesquisa#
TCC_Estudo1 <- TCC_Estudo1 %>% drop_na(nivel_identificacao)
TCC_Estudo2 <- TCC_Estudo2 %>% drop_na(nivel_identificacao)
TCC_Estudo3 <- TCC_Estudo3 %>% drop_na(nivel_identificacao)

#Remover participantes que podem ter participado duas vezes do estudo#
TCC_Estudo1 <- TCC_Estudo1 %>% 
  filter(participacao_estudo != "Não, já fiz um estudo muito parecido antes") # No script da Letícia, ela atribuiu a outro Df, testes.

#Fazer mutate: usar case when pra criar a coluna de político. 1. se o valor em imagem bolsonaro for True, cria códico 1 em político. Criar caso em que os dois forem NA (is.NA). Usar função AND.

TCC_Estudo1 <- TCC_Estudo1 %>%
  mutate(politico_escolhido = case_when(
    !is.na(img_alinhamento_bolsonarismo) ~ '1',
    !is.na(img_alinhamento_lulismo) ~ '2',
    TRUE ~ '0'))

TCC_Estudo2 <- TCC_Estudo2%>%
  mutate(politico_escolhido = case_when(
    !is.na(img_alinhamento_bolsonarismo) ~ '1',
    !is.na(img_alinhamento_lulismo) ~ '2',
    TRUE ~ '0'))

TCC_Estudo3 <- TCC_Estudo3%>%
  mutate(politico_escolhido = case_when(
    !is.na(img_alinhamento_bolsonarismo) ~ '1',
    !is.na(img_alinhamento_lulismo) ~ '2',
    TRUE ~ '0'))

#Transformando a variável de grupo em fator#
TCC_Estudo1 <- TCC_Estudo1 %>% 
  mutate(politico_escolhido = factor(politico_escolhido,
                                     levels = c("0","1","2"),
                                     labels = c("Nenhum", "Bolsonaro", "Lula")))
TCC_Estudo2 <- TCC_Estudo2 %>% 
  mutate(politico_escolhido = factor(politico_escolhido,
                                     levels = c("0","1","2"),
                                     labels = c("Nenhum", "Bolsonaro", "Lula")))
TCC_Estudo3 <- TCC_Estudo3 %>% 
  mutate(politico_escolhido = factor(politico_escolhido,
                                     levels = c("0","1","2"),
                                     labels = c("Nenhum", "Bolsonaro", "Lula")))

#Empilha os dados ramificados#
empilha_Dados_ramificados <- function(df) {
  
  df_Bolsonaro <- df %>%
    dplyr::filter(politico_escolhido == "Bolsonaro") %>%
    dplyr::select(-img_alinhamento_lulismo) %>%
    dplyr::select(-decisao_compartilhamento_lula) %>%
    rename(img_alinhamento = img_alinhamento_bolsonarismo,
           decisao_compartilhamento = decisao_compartilhamento_bolsonaro)
  
  
  df_lula <- df %>%
    dplyr::filter(politico_escolhido == "Lula") %>%
    dplyr::select(-img_alinhamento_bolsonarismo) %>%
    dplyr::select(-decisao_compartilhamento_bolsonaro) %>%
    rename(img_alinhamento = img_alinhamento_lulismo,
           decisao_compartilhamento = decisao_compartilhamento_lula)
  
  df_neutro <- df %>%
    dplyr::select(!starts_with("img_alinhamento")) %>%
    dplyr::select(-c("decisao_compartilhamento_lula", "decisao_compartilhamento_bolsonaro")) %>%
    dplyr::filter(politico_escolhido == "Nenhum") %>%
    mutate(img_alinhamento = NA)
  
  # pro TCC1, não houve neutro. Então, if que faz o bind com ou sem neutro ,dependendo se tiver neutro na base.
  if (nrow(df_neutro) > 0) {
    df_junto <- bind_rows(df_Bolsonaro, df_lula, df_neutro) %>%
      mutate(nivel_identificacao_recode = as.numeric(str_extract(nivel_identificacao, "[0-9]+")))
  } else {
    df_junto <- bind_rows(df_Bolsonaro, df_lula) %>%
      mutate(nivel_identificacao_recode = as.numeric(str_extract(nivel_identificacao, "[0-9]+")))
  }

  
  return(df_junto)
}

TCC_Estudo1 <- empilha_Dados_ramificados(TCC_Estudo1)
TCC_Estudo2 <- empilha_Dados_ramificados(TCC_Estudo2)
TCC_Estudo3 <- empilha_Dados_ramificados(TCC_Estudo3)


#Criando coluna de identificação do estudo
TCC_Estudo1 <- TCC_Estudo1 %>%
  mutate(Estudo = 1)
TCC_Estudo2 <- TCC_Estudo2 %>%
  mutate(Estudo = 2)
TCC_Estudo3 <- TCC_Estudo3 %>%
  mutate(Estudo = 3)

#Criando banco final com os três estudos#
compartilhamento_3_estudos <- bind_rows(TCC_Estudo1, TCC_Estudo2, TCC_Estudo3)
glimpse(compartilhamento_3_estudos)


#Preenchendo com 'outro' o campo gênero para pessoas não-binárias#

compartilhamento_3_estudos <- compartilhamento_3_estudos %>%
  mutate(genero = as.character(genero),
         genero = if_else(grepl("binári", genero_especifico), "outros", genero),
         genero = as.factor(genero))


#Mudando o nome das opções da variável 'decisão_compartilhamento'
compartilhamento_3_estudos <- compartilhamento_3_estudos %>%
  mutate(decisao_compartilhamento = if_else(grepl("Não", decisao_compartilhamento), decisao_compartilhamento, 
                                            str_sub(decisao_compartilhamento, 45, end = 60)),
         decisao_compartilhamento = gsub("stas ", "", decisao_compartilhamento))

#Convertendo números que estavam como string para numérico e strings que seriam fatores para fatores#
compartilhamento_3_estudos <- compartilhamento_3_estudos %>%
  mutate(ladder = str_extract(ladder, "[0-9]+")) %>% # extra número (de 1 abaixo de todos e 10 acima de todos)
  mutate(across(ladder:prostituicao, as.numeric),
         across(psicotropicos:filiacao_partidaria, as.factor),
         across(c(img_alinhamento, decisao_compartilhamento, participacao_estudo, Estudo), as.factor))

# coloca Bolsonaro como negativo
compartilhamento_3_estudos <- compartilhamento_3_estudos %>%
  mutate(nivel_identificacao_recode_alt = case_when(politico_escolhido == "Bolsonaro" ~ nivel_identificacao_recode*-1,
                                                TRUE ~ nivel_identificacao_recode))

# salva o banco em formato RDS
saveRDS(compartilhamento_3_estudos, file= "Transformados/compartilhamento_3_estudos.rds")

# para ler, basta rodar (apontando para o diretório onde os dados estão. Dados processados eu salvo na pasta Transformados)
compartilhamento_3_estudos <- readRDS("Transformados/compartilhamento_3_estudos.rds")


