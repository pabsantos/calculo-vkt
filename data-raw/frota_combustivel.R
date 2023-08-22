library(tidyverse)
library(readxl)

add_frota_years <- function(table, years) {
  mutate(table, ano = years)
}

read_frota_comb <- function() {
  frota_comb_path <- paste0(
    "data-raw/frota-combustivel/",
    list.files("data-raw/frota-combustivel/")
  )

  frota_list <- map(
    frota_comb_path,
    ~read_excel(
      .x,
      col_names = c("uf", "municipio", "combustivel", "quantidade"),
      skip = 1,
      col_types = c("text", "text", "text", "numeric")
    )
  )

  frota_anos <- str_extract(frota_comb_path, "(\\d{4})")

  list <- map2(frota_list, frota_anos, add_frota_years)

  tabela_final <- list |>
    reduce(bind_rows) |>
    arrange(ano)

  return(tabela_final)
}

frota_combustivel <- read_frota_comb()

save(frota_combustivel, file = "data/frota_combustivel.rda")
write_csv(frota_combustivel, "data/frota_combustivel.csv")
