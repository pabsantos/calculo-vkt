library(tidyverse)
library(readxl)
library(janitor)

load("data/frota_combustivel.rda")

frota_combustivel_sintese <- arrange_frota_combustivel(frota_combustivel)
