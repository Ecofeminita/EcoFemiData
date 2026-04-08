# descargo bases####
library (eph)
library (tidyverse)
library (dplyr)
library(httr)
library(stringr)
library(ggthemes)
library(scales)
library(knitr)
#library(ggalt)
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
  
individual <- get_microdata(year = 2024, trimester =4 , type = "individual")#tri

##cuadros tasas####
cuadro_transferencias_dicotomico <- individual %>% 
  mutate(T_VI = V2_01_M + V2_02_M + V2_03_M + V3_M + V4_M + V5_01_M + V5_02_M + 
              V5_03_M + V8_M + V9_M + V10_M + 
               +#V11_01 + V11_02# ver que paso con estas variables(becas x estudios)
              V12_M +
              V18_M + V19_AM + V21_01_M + V21_02_M + V21_03_M +
              V22_01_M + V22_02_M + V22_03_M , 
          transferencia_status = case_when(
               T_VI > 0  ~ "con transferencia",
               T_VI == 0 ~ "sin transferencia",
               is.na(T_VI)  ~ "sin aclaracion",
               TRUE      ~ NA_character_ 
               ))       
                  #%>% 
     # group_by(transferencia_status) %>% 
       #summarise(count = sum(PONDERA, na.rm = TRUE)) %>% 
        #  mutate(count = format(count, big.mark = ".", decimal.mark = ","))



# POBREZA -----------------------------------------------------------------
canastas_regionales <- get_poverty_lines(regional = TRUE)
  bases <- get_microdata(
    year = 2024,
    trimester = 4,
    type = "individual",
    vars = c("ANO4", "TRIMESTRE", "REGION", "CODUSU","COMPONENTE", 
             "NRO_HOGAR", "CH04", "CH06", "ITF", "PONDIH", "PP07H", "PP04D_COD")
    # ,destfile = 'bases_eph.rds'
  )
     #bases <- bases %>% unnest(cols = c(microdata))

      bases_pobreza <- calculate_poverty(bases, canastas_regionales, print_summary = TRUE)
  bases_pobreza


  #JUNTAR LOS CUADROS
  base_pobreza_transferencias <- left_join( bases_pobreza,     cuadro_transferencias_dicotomico, 
                            by = c("ANO4", "TRIMESTRE", "REGION", "CODUSU","COMPONENTE", 
                                   "NRO_HOGAR", "CH04", "CH06", "ITF", "PONDIH", "PP07H", "PP04D_COD")  ,
                            relationship = "many-to-many" )
  
  
  base_pobreza_transferencia_on <- base_pobreza_transferencias %>% 
    group_by(transferencia_status,situacion) %>% 
    summarise(count = sum(PONDERA, na.rm = TRUE)) %>% 
     mutate(count = format(count, big.mark = ".", decimal.mark = ","))
  
#PUEDE SER UTIL PERO NO TIENE 2023 NI 2024  
  pobreza_oficial <- read_csv("https://raw.githubusercontent.com/holatam/data/master/eph/canasta/pobreza_oficial.csv")
  pobreza_oficial <- pobreza_oficial %>%
    mutate(periodo = parse_date_time(paste0(ANO4, "-", SEMESTRE * 2), "Y.q")) %>%
    select(periodo, pobreza_oficial = tasa_pobreza, indigencia_oficial = tasa_indigencia)
  pobreza_oficial
  
  # POBREZA SIN TRANSFERENCIAS EN GENERAL ----------------------------------------------
  #esto se esta trabajando, 
  
    base_pobreza_transferencias_off <- base_pobreza_transferencias %>% 
    mutate(T_VI = V2_01_M + V2_02_M + V2_03_M + V3_M + V4_M + V5_01_M + V5_02_M + 
             V5_03_M + V8_M + V9_M + V10_M + 
             +#V11_01 + V11_02# ver que paso con estas variables(becas x estudios)
             V12_M +
             V18_M + V19_AM + V21_01_M + V21_02_M + V21_03_M +
             V22_01_M + V22_02_M + V22_03_M, 
           ITF = ITF- T_VI) 
  #  %>% 
   # group_by(transferencia_status,situacion) %>% 
    #summarise(count = sum(PONDERA, na.rm = TRUE)) %>% 
      #mutate(count = format(count, big.mark = ".", decimal.mark = ","))
  
    #ver COMO CALCULAR POBREZA CON NUEVA VARIABLE DE INGRESOS.
  bases_pobreza_off<- calculate_poverty(base_pobreza_transferencias_off, canastas_regionales, print_summary = TRUE)
  
    #uso resta para el ingreso total familiar, ver si esto afecta ingreso per capita familiar
    

# POBREZA SIN TRANSFERENCIAS PARTICULARES ---------------------------------

   pobreza_AUH_off <- base_pobreza_transferencias %>% 
  
  
  
  
  
  
  
  
  