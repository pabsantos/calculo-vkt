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
    mutate(
      quantidade_final = if_else(
        is.na(prop_eletrico),
        qtde,
        round(qtde - (prop_eletrico * auto_eletrico))
      )
    ) |> 
    select(ano, uf, tipo, quantidade = quantidade_final)
}




