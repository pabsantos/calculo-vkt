library(tidyverse)
library(readxl)
library(janitor)

source("R/frota.R")

load("data/frota_combustivel.rda")
load("data/frota_tipo.rda")

frota_combustivel_sintese <- arrange_frota_combustivel(frota_combustivel)
frota_tipo_sintese <- frota_tipo |> fix_tipo_uf()

fator_correcao_frota <- calc_fator_correcao_frota(
  frota_tipo_sintese,
  frota_combustivel_sintese
)

frota_tipo_corrigido <- calc_tipo_correcao(
  frota_tipo_sintese,
  fator_correcao_frota
)

frota_dict <- create_frota_dict()

tabela_prop_eletrico <- create_prop_eletrico(frota_tipo_corrigido)

frota_tipo_real <- remove_eletrico(
  frota_tipo_corrigido,
  frota_combustivel_sintese,
  tabela_prop_eletrico
)

