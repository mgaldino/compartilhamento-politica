

library(dplyr)
library(readr)
library(here)
library(data.table)
library(janitor)
here()

library(readxl)
TCC_Dados_Brutos_1 <- read_excel("Dados/TCC Dados Brutos.xlsx", 
                               sheet = "Link 1") %>%
  clean_names() %>%
  rename(genero_especifico = x6,
         partido_especifico = x11)
TCC_Dados_Brutos_2 <- read_excel("Dados/TCC Dados Brutos.xlsx", 
                                 sheet = "Link 2")
TCC_Dados_Brutos_3 <- read_excel("Dados/TCC Dados Brutos.xlsx", 
                                 sheet = "Link 3")

glimpse(TCC_Dados_Brutos)


df <- fread("Dados/dados tcc jamovi - dados tcc jamovi.csv")
glimpse(df)
