arrange_frota_combustivel <- function(tabela_combustivel) {
  class_alcool <- c("ALCOOL", "ALCOOL/GAS NATURAL COMBUSTIVEL")
  class_gnv <- c(
    "ALCOOL/GAS NATURAL VEICULAR",
    "DIESEL/GAS NATURAL VEICULAR", 
    "GAS NATURAL VEICULAR", 
    "GASOLINA/ALCOOL/GAS NATURAL",
    "GASOLINA/GAS NATURAL VEICULAR"
  )
  class_flex <- "ALCOOL/GASOLINA"
  class_diesel <- c("DIESEL", "DIESEL/GAS NATURAL COMBUSTIVEL")
  class_eletrico <- c(
    "ELETRICO/FONTE EXTERNA", 
    "ELETRICO/FONTE INTERNA", 
    "GASOLINA/ALCOOL/ELETRICO", 
    "GASOLINA/ELETRICO"
  )
  class_gasolina <- c("GASOL/GAS NATURAL COMBUSTIVEL", "GASOLINA")

  frota_agrupado <- tabela_combustivel |>
    mutate(combustivel = case_match(
      combustivel,
      class_alcool ~ "Álcool",
      class_gnv ~ "GNV",
      class_flex ~ "Flex",
      class_diesel ~ "Diesel",
      class_eletrico ~ "Elétrico",
      class_gasolina ~ "Gasolina",
      .default = NA
    )) |>
    filter(!uf %in% c(
      "NÃ£o Identificado", "NÃ£o se Aplica", "Sem Informação",
      "Sem InformaÃ§Ã£o", "Não Identificado", "Não se Aplica"
    )) |> 
    group_by(ano, uf, combustivel) |>
    summarise(quantidade = sum(quantidade)) |>
    ungroup() |>
    drop_na()

  return(frota_agrupado)
}

fix_tipo_uf <- function(tabela_tipo) {
  tabela_tipo |> 
    filter(!uf %in% c(
      "NÃ£o Identificado", "NÃ£o se Aplica", "Sem Informação",
      "Sem InformaÃ§Ã£o", "Não Identificado", "Não se Aplica"
    ))
}

calc_fator_correcao_frota <- function(tabela_tipo, tabela_combustivel) {
  table_qtde_comb <- tabela_combustivel |> 
    group_by(ano) |> 
    summarise(qtde_comb = sum(quantidade))
  
  table_qtde_frota <- tabela_tipo |> 
    group_by(ano) |> 
    summarise(qtde_tipo = sum(qtde))
  
  table_fator_correcao <- table_qtde_comb |> 
    left_join(table_qtde_frota, by = "ano") |> 
    mutate(fator_correcao_frota = qtde_comb / qtde_tipo) |> 
    select(ano, fator_correcao_frota)
  
  return(table_fator_correcao)
}

calc_tipo_correcao <- function(table_tipo, table_fator) {
  table_tipo |> 
    left_join(table_fator, by = "ano") |> 
    mutate(qtde = round(qtde * fator_correcao_frota)) |> 
    select(-fator_correcao_frota)
}

create_frota_dict <- function() {
  class_tipo <- c(
    rep("AUTOMOVEL", 5), "CAMINHAO", "CAMINHAO TRATOR", rep("CAMINHONETE", 3),
    rep("CAMIONETA", 3), "MICROONIBUS", "MOTOCICLETA", "MOTONETA", "ONIBUS",
    rep("UTILITARIO", 3)
  )
  
  class_combustivel <- c(
    "GNV", "Flex", "Álcool", "Gasolina", "Elétrico", "Diesel", "Diesel",
    "Gasolina", "Diesel", "Elétrico", "Gasolina", "Diesel", "Elétrico",
    "Diesel", "Gasolina", "Gasolina", "Diesel", "Flex", "Gasolina", "Elétrico"
  )
  
  tibble(tipo = class_tipo, combustivel = class_combustivel)
}

create_prop_eletrico <- function(tabela_tipo) {
  tabela_tipo |> 
    group_by(ano, uf) |> 
    filter(tipo %in% c(
      "AUTOMOVEL",
      "CAMINHONETE",
      "CAMIONETA",
      "UTILITARIO"
    )) |> 
    mutate(soma_auto = sum(qtde), prop_eletrico = qtde / soma_auto) |> 
    select(ano, uf, tipo, prop_eletrico)
}

remove_eletrico <- function(tabela_tipo, tabela_comb, tab_prop_eletrico) {
  
  tab_eletrico <- tabela_comb |> 
    filter(combustivel == "Elétrico") |> 
    select(ano, uf, auto_eletrico = quantidade)
  
  tabela_tipo |> 
    left_join(tab_prop_eletrico, by = c("ano", "uf", "tipo")) |> 
    left_join(tab_eletrico, by = c("ano", "uf")) |> 
    replace_na(list(auto_eletrico = 0)) |> 
    mutate(
      quantidade_final = if_else(
        is.na(prop_eletrico),
        qtde,
        round(qtde - (prop_eletrico * auto_eletrico))
      )
    ) |> 
    select(ano, uf, tipo, quantidade = quantidade_final)
}

calc_frota_utilitario <- function(tabela_tipo_real, tabela_comb, tab_tipo) {
  
  prop_utilitario <- tabela_tipo_real |> 
    filter(tipo %in% c("AUTOMOVEL", "UTILITARIO")) |> 
    mutate(prop = quantidade / sum(quantidade))
  
  tab_flex <- tabela_comb |> 
    filter(combustivel == "Flex") |> 
    select(ano, uf, qtde_flex = quantidade)
  
  tab_elec <- tabela_tipo_real |> 
    left_join(tab_tipo, by = c("ano", "uf", "tipo")) |> 
    filter(tipo == "UTILITARIO") |> 
    mutate(utilitario_eletrico = qtde - quantidade) |> 
    select(ano, uf, utilitario_eletrico)
  
  prop_utilitario |> 
    left_join(tab_flex, by = c("ano", "uf")) |> 
    filter(tipo == "UTILITARIO") |> 
    mutate(
      utilitario_flex = if_else(
        (prop * qtde_flex) > quantidade, 
        quantidade,
        round(prop * qtde_flex)
      ),
      utilitario_gasolina = quantidade - utilitario_flex
    ) |> 
    select(ano, uf, utilitario_flex, utilitario_gasolina) |> 
    left_join(tab_elec, by = c("ano", "uf"))
  
}  

calc_frota_auto <- function(tabela_combustivel, tabela_frota_real, tab_frota) {
  
  tab_auto_alcool <- tabela_combustivel |> 
    filter(combustivel == "Álcool") |> 
    select(ano, uf, automovel_alcool = quantidade)
  
  tab_auto_gnv <- tabela_combustivel |> 
    filter(combustivel == "GNV") |> 
    select(ano, uf, automovel_gnv = quantidade)
  
  
  tab_comb_auto <- tabela_combustivel |> 
    filter(!combustivel %in% c("Diesel", "Elétrico")) |> 
    rename(qtde_comb = quantidade)
  
  tab_auto_gas_flex <- tabela_frota_real |> 
    filter(tipo %in% c("AUTOMOVEL", "UTILITARIO")) |> 
    mutate(prop_auto = quantidade / sum(quantidade)) |> 
    filter(tipo == "AUTOMOVEL") |>
    select(ano, uf, qtde_auto = quantidade, prop_auto) |>
    left_join(tab_comb_auto, by = c("ano", "uf")) |> 
    pivot_wider(names_from = combustivel, values_from = qtde_comb) |> 
    mutate(
      automovel_flex = if_else(
        (prop_auto * Flex > qtde_auto) | (Álcool + GNV + (prop_auto * Flex) > qtde_auto),
        qtde_auto - Álcool - GNV,
        round(prop_auto * Flex)
      ),
      automovel_gasolina = if_else(
        qtde_auto - Álcool - automovel_flex - GNV < 0,
        0,
        qtde_auto - Álcool - automovel_flex - GNV
      )
    ) |> 
    select(ano, uf, automovel_flex, automovel_gasolina)
  
  tab_auto_elet <- tabela_frota_real |> 
    rename(qtde_real = quantidade) |> 
    left_join(tab_frota, by = c("ano", "uf", "tipo")) |> 
    filter(tipo == "AUTOMOVEL") |> 
    mutate(automovel_eletrico = qtde - qtde_real) |> 
    select(ano, uf, automovel_eletrico)
  
  tab_auto_full <- tab_auto_gas_flex |> 
    left_join(tab_auto_alcool, by = c("ano", "uf")) |> 
    left_join(tab_auto_gnv, by = c("ano", "uf")) |>  
    left_join(tab_auto_elet, by = c("ano", "uf"))
  
  return(tab_auto_full)
}

calc_frota_cam <- function(tab_combustivel, tab_frota_real, tab_frota) {
  
  tab_comb_diesel <- tab_combustivel |> 
    filter(combustivel == "Diesel") |> 
    select(ano, uf, qtde_diesel = quantidade)
  
  tab_tipo_diesel <- tab_frota_real |> 
    filter(tipo %in% c("CAMINHAO", "CAMINHAO TRATOR", "MICROONIBUS", "ONIBUS")) |> 
    pivot_wider(names_from = tipo, values_from = quantidade)
  
  tab_cam_diesel_gas <- tab_frota_real |> 
    filter(tipo %in% c("CAMINHONETE", "CAMIONETA")) |> 
    mutate(prop = quantidade / sum(quantidade)) |> 
    pivot_wider(names_from = tipo, values_from = c(quantidade, prop)) |> 
    left_join(tab_comb_diesel, by = c("ano", "uf")) |> 
    left_join(tab_tipo_diesel, by = c("ano", "uf")) |> 
    mutate(
      diesel_disponivel = 
        qtde_diesel - CAMINHAO - `CAMINHAO TRATOR` - MICROONIBUS - ONIBUS,
      caminhonete_diesel = round(diesel_disponivel * prop_CAMINHONETE),
      camioneta_diesel = round(diesel_disponivel * prop_CAMIONETA),
      caminhonete_gasolina = quantidade_CAMINHONETE - caminhonete_diesel,
      camioneta_gasolina = quantidade_CAMIONETA - camioneta_diesel
    ) |> 
    select(ano, uf, starts_with(c("caminhonete", "camioneta")))
  
  tab_cam_elet <- tab_frota_real |> 
    rename(qtde_real = quantidade) |> 
    left_join(tab_frota, by = c("ano", "uf", "tipo")) |> 
    filter(tipo %in% c("CAMINHONETE", "CAMIONETA")) |> 
    mutate(eletrico = qtde - qtde_real) |> 
    pivot_wider(names_from = tipo, values_from = c(qtde_real, qtde, eletrico)) |> 
    select(
      ano,
      uf,
      caminhonete_eletrico = eletrico_CAMINHONETE,
      camioneta_eletrico = eletrico_CAMIONETA
    )
  
  tab_cam_diesel_gas |> 
    left_join(tab_cam_elet, by = c("ano", "uf"))
  
}

calc_caminhao_onibus_moto <- function(tab_frota) {
  tab_frota |> 
    filter(tipo %in% c(
      "CAMINHAO",
      "CAMINHAO TRATOR",
      "ONIBUS",
      "MICROONIBUS",
      "MOTOCICLETA",
      "MOTONETA"
    )) |> 
    pivot_wider(names_from = tipo, values_from = qtde) |> 
    rename(
      caminhao_diesel = CAMINHAO,
      caminhaotrator_diesel = `CAMINHAO TRATOR`,
      microonibus_diesel = MICROONIBUS,
      motocicleta_gasolina = MOTOCICLETA,
      motoneta_gasolina = MOTONETA,
      onibus_diesel = ONIBUS
    )
}

calc_frota_total <- function(
    tab_auto,
    tab_util,
    tab_caminhonete,
    tab_caminhao
) {
  tab_auto |> 
    left_join(tab_util, by = c("ano", "uf")) |> 
    left_join(tab_caminhonete, by = c("ano", "uf")) |> 
    left_join(tab_caminhao, by = c("ano", "uf")) |>
    pivot_longer(
      automovel_flex:onibus_diesel,
      names_to = "tipo_combustivel",
      values_to = "qtde"
    ) |> 
    separate(tipo_combustivel, into = c("tipo", "combustivel"), sep = "_") |> 
    group_by(ano, uf, tipo, combustivel) |> 
    summarise(qtde = sum(qtde))
}
