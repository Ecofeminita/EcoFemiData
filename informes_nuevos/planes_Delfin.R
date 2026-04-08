# descargo bases####
library (eph)
library (tidyverse)
library (dplyr)
library(httr)
library(stringr)
library(ggthemes)
library(scales)
library(knitr)
library(ggalt)
library(kableExtra)
library(formattable)
library(openxlsx)
library(gridExtra)
library(ggridges)
library(magrittr)
library(DT)
library(tidyr)
library(readr)
library(lubridate)
library(ggplot2)
library(forcats)


# armo loop####
# Crear un nuevo workbook####
#wb <- createWorkbook()
#R_sumnueva <- data.frame()
# for (tri in 1:4) {
  
hogares <- get_microdata(year = 2023, trimester = 4, type = "hogar")#tri
##cuadros tasas####
  sumnueva <- hogares %>% 
   filter( T_VI > 0 ) %>% 
   select(CAT_OCUP, PP07H, PP07I, PONDERA, V21_M) %>%  # Agregado V21_M
   filter(T_VI != 0) %>%  # Corregido `T_VI` en vez de `T_Vi`
   mutate (Vlibrary(dplyr)) %>% 
     rename(
        monto_jubilacion = V2,

        monto_subsidio = V5,
             ) %>% #dicotomicas 
    mutate(across(starts_with("monto_"), ~ case_when(
      . == 1 ~ "perciben",
      . == 2 ~ "no perciben",
      TRUE ~ NA_character_ # Dejar NA para luego filtrar
  )) # Cambiar según corresponda
    ) %>%
    # Filtrar los valores 9
    filter(if_all(starts_with("monto_"), ~ !is.na(.))) %>%
  group_by(monto_jubilacion, monto_subsidio) %>% 
  summarise(count = sum(PONDERA, na.rm = TRUE)) %>% 
        mutate(count = format(count, big.mark = ".", decimal.mark = ","))
  
  ##pobreza###
  
  canastas_regionales <- get_poverty_lines(regional = TRUE)
  bases <- get_microdata(
    year = 2016:2022,
    trimester = 1:4,
    type = "individual",
    vars = c("ANO4", "TRIMESTRE", "REGION", "CODUSU", "NRO_HOGAR", "CH04", "CH06", "ITF", "PONDIH", "PP07H", "PP04D_COD")
    # ,destfile = 'bases_eph.rds'
  )
    # bases <- bases %>% unnest(cols = c(microdata))
  bases_pobreza <- calculate_poverty(bases, canastas_regionales, print_summary = TRUE)
  bases_pobreza
  pobreza_oficial <- read_csv("https://raw.githubusercontent.com/holatam/data/master/eph/canasta/pobreza_oficial.csv")
  pobreza_oficial <- pobreza_oficial %>%
    mutate(periodo = parse_date_time(paste0(ANO4, "-", SEMESTRE * 2), "Y.q")) %>%
    select(periodo, pobreza_oficial = tasa_pobreza, indigencia_oficial = tasa_indigencia)
  pobreza_oficial
  
  