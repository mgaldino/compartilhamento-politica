

library(dplyr)
library(readr)
library(here)
library(data.table)
library(janitor)
library(stringr)
here()

library(readxl)
TCC_Dados_Brutos_1 <- read_excel("Dados/TCC Dados Brutos.xlsx", 
                               sheet = "Link 1")

TCC_Dados_Brutos_2 <- read_excel("Dados/TCC Dados Brutos.xlsx", 
                                 sheet = "Link 2")

TCC_Dados_Brutos_3 <- read_excel("Dados/TCC Dados Brutos.xlsx", 
                                 sheet = "Link 3")

muda_nomes_variaveis1 <- function(df) {
  df <- df %>%
    janitor::clean_names() %>%
    dplyr::slice(-1) %>% # remove primeira linha
    rename(genero_especifico = x6,
           partido_especifico = x11,
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
           decisao_compartilhamento_bolsonaro = qual_a_sua_decisao_45,
           decisao_compartilhamento_lula = qual_a_sua_decisao_47,
           participacao_estudo = essa_e_a_primeira_vez_que_voce_participa_desse_estudo,
           politico = x43) 
  return(df)
}

# link 2 e 3 tem mais variáveis, então a ordem muda. Aí criei outra função para mudar os nomes
muda_nomes_variaveis2 <- function(df) {
  df <- df %>%
    janitor::clean_names() %>%
    dplyr::slice(-1) %>% # remove primeira linha
    rename(genero_especifico = x14,
           partido_especifico = x19,
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
           decisao_compartilhamento_bolsonaro = qual_a_sua_decisao_53,
           decisao_compartilhamento_lula = qual_a_sua_decisao_55,
           participacao_estudo = essa_e_a_primeira_vez_que_voce_participa_desse_estudo,
           politico = x50) 
  return(df)
}

adicina_legenda_respostas <- function(df) {
  df <- df %>%
    mutate(idade = as.numeric(idade),
           estado_civil = factor(estado_civil, levels = 1:4, labels = c("solteiro", "casado", "viuvo", "divorciado")),
           genero = factor(genero, levels = 1:3, labels = c("homem", "mulher", "outros")),
           grau_de_escolaridade = factor(grau_de_escolaridade, levels=1:6, labels = c("Fundamental Incompleto", "Fundamental Completo",
                                                                                      "Médio Incompleto", "Médio Completo", "Superio Incompleto",
                                                                                      "Superior Completo")),
           filiacao_partidaria = factor(filiacao_partidaria, levels = 1:2, labels = c("sim", "não")),
           politico = factor(politico, levels = 0:2, labels = c("neutro", "Bolsonaro", "Lula")))
  return(df)
}

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

link1 <- muda_nomes_variaveis(TCC_Dados_Brutos_1)
link1 <- adicina_legenda_respostas(link1)
link1 <- empilha_Dados_ramificados(link1)

link2 <- muda_nomes_variaveis2(TCC_Dados_Brutos_2)
link2 <- adicina_legenda_respostas(link2)
link2 <-  empilha_Dados_ramificados(link2)

link3 <- muda_nomes_variaveis2(TCC_Dados_Brutos_3)
link3 <- adicina_legenda_respostas(link3)
link3 <-  empilha_Dados_ramificados(link3)


link1 <- link1 %>%
  mutate(date_created = NA,
         date_modified = NA,
         ip_address = NA,
         email_address = NA,
         first_name = NA,
         last_name = NA,
         custom_1 = NA,
         termo_de_consentimento = NA) %>%
  relocate(any_of(c("date_created", "date_modified", "ip_address", "email_address",
                  "first_name", "last_name", "custom_1", "termo_de_consentimento")), .after=link)

link1 <- link1 %>%
  rename(score_narcissism = score_nascissism) # corrige erro de digitação

compartilhamento_3_estudos <- bind_rows(link1, link2, link3) %>%
  mutate(estudo = if_else(link == 1, "estudo 2",
                          if_else(link == 2, "estudo 1", "estudo 3")))
glimpse(compartilhamento_3_estudos)


