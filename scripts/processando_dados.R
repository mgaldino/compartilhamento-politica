

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



TCC_Dados_Brutos_1 <- TCC_Dados_Brutos_1 %>%
  clean_names() %>%
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

dados_compartilhamento1 <- TCC_Dados_Brutos_1 %>%
  mutate(idade = as.numeric(idade),
         estado_civil = factor(estado_civil, levels = 1:4, labels = c("solteiro", "casado", "viuvo", "divorciado")),
         genero = factor(genero, levels = 1:3, labels = c("homem", "mulher", "outros")),
         grau_de_escolaridade = factor(grau_de_escolaridade, levels=1:6, labels = c("Fundamental Incompleto", "Fundamental Completo",
                                                                        "Médio Incompleto", "Médio Completo", "Superio Incompleto",
                                                                        "Superior Completo")),
         filiacao_partidaria = factor(filiacao_partidaria, levels = 1:2, labels = c("sim", "não")),
         politico = factor(politico, levels = 0:2, labels = c("neutro", "Bolsonaro", "Lula")))

glimpse(dados_compartilhamento1)
# como o survey ramifica, vou criar três bancos (um p/ Bolsonaro, outro Lula e outro Neutro) E depois juntar, empilhando

link1_bolsonaro <- dados_compartilhamento1 %>%
  dplyr::filter(politico == "Bolsonaro") %>%
  dplyr::select(-img_alinhamento_lulismo) %>%
  dplyr::select(-decisao_compartilhamento_lula) %>%
  rename(img_alinhamento = img_alinhamento_bolsonarismo,
         decisao_compartilhamento = decisao_compartilhamento_bolsonaro)
  

link1_lula <- dados_compartilhamento1 %>%
  dplyr::filter(politico == "Lula") %>%
  dplyr::select(-img_alinhamento_bolsonarismo) %>%
  dplyr::select(-decisao_compartilhamento_bolsonaro) %>%
  rename(img_alinhamento = img_alinhamento_lulismo,
         decisao_compartilhamento = decisao_compartilhamento_lula)

link1_neutro <- dados_compartilhamento1 %>%
  dplyr::select(!starts_with("img_alinhamento")) %>%
  dplyr::select(-c("decisao_compartilhamento_lula", "decisao_compartilhamento_bolsonaro")) %>%
  dplyr::filter(politico == "neutro") %>%
  mutate(img_alinhamento = NA)

link1 <- bind_rows(link1_bolsonaro, link1_lula, link1_neutro) %>%
  mutate(atitude_lula_bolsonaro_recode = as.numeric(str_extract(atitude_lula_bolsonaro, "[0-9]+")))



glimpse(TCC_Dados_Brutos)


df <- fread("Dados/dados tcc jamovi - dados tcc jamovi.csv")
glimpse(df)
