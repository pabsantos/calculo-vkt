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
    group_by(ano, uf, combustivel) |>
    summarise(quantidade = sum(quantidade)) |>
    ungroup() |>
    drop_na()

  return(frota_agrupado)
}

