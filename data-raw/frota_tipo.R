library(tidyverse)
library(readxl)

tipo_files <- list.files("data-raw/frota-tipo/")

tipo_path <- here::here("data-raw", "frota-tipo", tipo_files)

frota_tipo_lista <- map(tipo_path, read_excel)

frota_tipo_anos <- str_extract(tipo_path, "(\\d{4})")

add_frota_years <- function(table, years) {
  mutate(table, ano = years)
}

frota_tipo_lista <- map2(frota_tipo_lista, frota_tipo_anos, add_frota_years)

clean_tables <- function(tables) {
  tables |> 
    janitor::clean_names() |> 
    select(uf, qtde = starts_with("qtd"), tipo = starts_with("tipo"), ano) |> 
    mutate(qtde = as.numeric(qtde))
}

frota_tipo <- map(frota_tipo_lista, clean_tables) |> 
  reduce(bind_rows)

frota_tipo <- frota_tipo |> 
  group_by(ano, uf, tipo) |> 
  summarise(qtde = sum(qtde)) |> 
  filter(tipo %in% c(
    "AUTOMOVEL", "CAMINHAO", "CAMINHAO TRATOR", "CAMINHONETE", "CAMIONETA",
    "MICROONIBUS", "MOTOCICLETA", "MOTONETA", "ONIBUS", "UTILITARIO"
  ))

write_csv(frota_tipo, "data/frota_tipo.csv")
save(frota_tipo, file = "data/frota_tipo.rda")
