rm(list = ls())
library(tidyverse)
library(httr)
library(stringr)
library(xlsx)
library(haven)
library(readxl)
library(foreign)
library(spatstat)


# Función que levanta bases, sea que estén en formato txt, sav o excel.
levantar_bases <- function(path){
  
  extension <- substr(path, start = nchar(path)-3, stop =nchar(path))
  
  if (extension == ".txt") {
    individuos <- read.table(path, sep=";", dec=",", header = TRUE, fill = TRUE)
  }
  
  if (extension == ".sav") {
    individuos <- read_spss(path)
  }
  
  if (extension == "xlsx") {
    individuos <- read_xlsx(path)
  }
  
  if (extension == ".xls") {
    individuos <- read_excel(path)
  }
  
  if (extension == ".DBF") {
    individuos <- read.dbf(path)
  }
  
  return(individuos)
}

# Función de limpieza de la base individual
limpieza_individuos <- function(base){
  
  individuos <- base %>% 
    mutate(Sexo = as.character(CH04),
           Sexo = case_when(Sexo=="1" ~ "Varones",
                            Sexo=="2" ~ "Mujeres"),
           PP04D_COD = as.character(PP04D_COD),
           PP04D_COD = case_when(nchar(PP04D_COD) == 5 ~ PP04D_COD,
                                 nchar(PP04D_COD) == 4 ~ paste0("0", PP04D_COD),
                                 nchar(PP04D_COD) == 3 ~ paste0("00", PP04D_COD),
                                 nchar(PP04D_COD) == 2 ~ paste0("000", PP04D_COD),
                                 nchar(PP04D_COD) == 1 ~ paste0("0000", PP04D_COD)),
           CALIFICACION = substr(PP04D_COD, 5, 5),
           CALIFICACION = case_when(CALIFICACION=="1" ~ "Profesionales",
                                    CALIFICACION=="2" ~ "Técnicos",
                                    CALIFICACION=="3" ~ "Operativos",
                                    CALIFICACION=="4" ~ "No Calificados",
                                    TRUE ~ "0"),
           CALIFICACION = factor(CALIFICACION, c("Profesionales", "Técnicos", "Operativos", "No Calificados")),
           JERARQUIA = substr(PP04D_COD, 3, 3),
           JERARQUIA = case_when(JERARQUIA=="0" ~ "Dirección",
                                 JERARQUIA=="1" ~ "Cuentapropia",
                                 JERARQUIA=="2" ~ "Jefes",
                                 JERARQUIA=="3" ~ "Trabajadores Asalariados",
                                 TRUE ~ "0"),
           JERARQUIA = factor(JERARQUIA, c("Jefes", "Dirección", "Trabajadores Asalariados", "Cuentapropia")),
           NIVEL_EDUCATIVO = case_when(NIVEL_ED==1 ~ "Sin Instrucción",
                                       NIVEL_ED==2 ~ "Primaria",
                                       NIVEL_ED==3 ~ "Primaria",
                                       NIVEL_ED==4 ~ "Secundaria",
                                       NIVEL_ED==5 ~ "Secundaria",
                                       NIVEL_ED==6 ~ "Superior",
                                       NIVEL_ED==7 ~ "Sin Instrucción",
                                       NIVEL_ED==9 ~ "NS/NR"),
           NIVEL_EDUCATIVO = factor(NIVEL_EDUCATIVO, levels = c("Sin Instrucción", "Primaria", "Secundaria", "Superior")),
           GRUPO_EDAD = case_when(CH06 >= 14 & CH06 <= 29 ~ "de 14 a 29 años",
                                  CH06 >= 30 & CH06 <= 64 ~ "de 30 a 64 años"))
  
  return(individuos)  
}

# Función de recambio de nombres (de version 03-06 a 16)
limpieza_nombres <- function(base){
  nombres <- levantar_bases(path = "Fuentes/variables_viejas_a_nuevas.txt")
  
  for (i in c(1:nrow(nombres))) {
    row <- nombres[i,]
    if (row$old %in% names(base)) {
      nro_nombre <- which(names(base)==row$old)
      names(base)[nro_nombre] <- as.character(row$new) 
    }
  }
  return(base)
}

# Función de tasas por sexo (14 años y más)
tasas_por_sexo <- function(base){
  
  tabla <- base %>% 
    filter(CH06 >= 14) %>% 
    group_by(ANO4, TRIMESTRE, Sexo) %>% 
    summarise(Poblacion         = sum(PONDERA),
              Ocupados          = sum(PONDERA[ESTADO == 1]),
              Desocupados       = sum(PONDERA[ESTADO == 2]),
              PEA               = Ocupados + Desocupados,
              Ocupados_demand   = sum(PONDERA[ESTADO == 1 & PP03J ==1]),
              Suboc_demandante  = sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J==1]),
              Suboc_no_demand   = sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J %in% c(2,9)]),
              Subocupados       = Suboc_demandante + Suboc_no_demand,
              'Tasa Actividad'                  = round(PEA/Poblacion*100, 1),
              'Tasa Empleo'                     = round(Ocupados/Poblacion*100, 1),
              'Tasa Desocupación'               = round(Desocupados/PEA*100, 1),
              'Tasa Ocupados Demandantes'       = round(Ocupados_demand/PEA*100, 1),
              'Tasa Subocupación'               = round(Subocupados/PEA*100, 1),
              'Tasa Subocupación demandante'    = round(Suboc_demandante/PEA*100, 1),
              'Tasa Subocupación no demandante' = round(Suboc_no_demand/PEA*100, 1)) %>% 
    gather(indicador, valor, 4:ncol(.))
  
  return(tabla)
  
}

# Función de tasas por sexo y grupos de edad (14 años y más)
tasas_por_sexo_edad <- function(base){
  
  tabla <- base %>% 
    filter(CH06 >= 14) %>% 
    group_by(ANO4, TRIMESTRE, Sexo, GRUPO_EDAD) %>% 
    summarise(Poblacion         = sum(PONDERA),
              Ocupados          = sum(PONDERA[ESTADO == 1]),
              Desocupados       = sum(PONDERA[ESTADO == 2]),
              PEA               = Ocupados + Desocupados,
              Ocupados_demand   = sum(PONDERA[ESTADO == 1 & PP03J ==1]),
              Suboc_demandante  = sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J==1]),
              Suboc_no_demand   = sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J %in% c(2,9)]),
              Subocupados       = Suboc_demandante + Suboc_no_demand,
              'Tasa Actividad'                  = round(PEA/Poblacion*100, 1),
              'Tasa Empleo'                     = round(Ocupados/Poblacion*100, 1),
              'Tasa Desocupación'               = round(Desocupados/PEA*100, 1),
              'Tasa Subocupación'               = round(Subocupados/PEA*100, 1)) %>% 
    gather(indicador, valor, 5:ncol(.))
  
  return(tabla)
  
}

# Función de tasa de no registro (ocupades asalariades)
tasas_no_registro <- function(base){
  
  tabla <- base %>% 
    filter(ESTADO==1,       # Ocupades
           CAT_OCUP==3) %>% # Asalariades
    group_by(ANO4, TRIMESTRE, Sexo) %>% 
    summarise("Proporción de no Registrados" = round(sum(PONDERA[PP07H==2])/sum(PONDERA)*100, 1))
  
  return(tabla)
  
}

# Función de jerarquías en cada sexo (ocupades con jerarquía válida)
sexo_segun_jerarquias <- function(base){
  
  tabla <- base %>% 
    filter(JERARQUIA != "0", # Jerarquia valida
           ESTADO == 1) %>%  # Ocupades
    group_by(Sexo) %>% 
    mutate(Frecuencia = sum(PONDERA)) %>% 
    group_by(ANO4, TRIMESTRE, Sexo, JERARQUIA) %>% 
    summarise(tasa = round(sum(PONDERA)/unique(Frecuencia)*100, 1))
  
  return(tabla)
  
}

# Función de sexo en cada jerarquía (ocupades con jerarquía válida)
jerarquias_segun_sexo <- function(base){
  
  tabla <- base %>% 
    filter(JERARQUIA != "0", # Jerarquia valida
           ESTADO == 1) %>%  # Ocupades
    group_by(JERARQUIA) %>% 
    mutate(Frecuencia = sum(PONDERA)) %>% 
    group_by(ANO4, TRIMESTRE, Sexo, JERARQUIA) %>% 
    summarise(tasa = round(sum(PONDERA)/unique(Frecuencia)*100, 1))
  
  return(tabla)
  
}

# Función de brecha del ingreso total individual (perceptores)
brecha_ITI <- function(base){
  
  tabla <- base %>% 
    filter(P47T > 0) %>% 
    group_by(ANO4, TRIMESTRE, Sexo) %>% 
    summarise(Media.ITI = round(weighted.mean(P47T, PONDII), 2)) %>% ##############
  spread(., Sexo, Media.ITI) %>% 
    mutate(brecha.ITI = round(((Varones-Mujeres)/Varones)*100, 1)) %>% 
    select(ANO4, TRIMESTRE, media.mujeres = Mujeres, media.varones = Varones, brecha.ITI)
  
  return(tabla)
  
}

# Función de brecha del ingreso de la ocupación principal (ocupades)
brecha_IOP <- function(base){
  
  tabla <- base %>% 
    filter(ESTADO == 1) %>% 
    group_by(ANO4, TRIMESTRE, Sexo) %>% 
    summarise(Media.IOP = round(weighted.mean(P21, PONDIIO), 2)) %>%
    spread(., Sexo, Media.IOP) %>% 
    mutate(brecha.IOP = round(((Varones-Mujeres)/Varones)*100, 1)) %>% 
    select(ANO4, TRIMESTRE, media.mujeres = Mujeres, media.varones = Varones, brecha.IOP)
  
  return(tabla)  
  
}

# Función de brecha del ingreso de la ocupación principal (asalariades no registrades)
brecha_IOP_no_reg <- function(base){
  
  tabla <- base %>% 
    filter(ESTADO == 1 &      # Ocupades
             CAT_OCUP == 3 &  # Asalariades
             PP07H == 2) %>%  # No registrades
    group_by(ANO4, TRIMESTRE, Sexo) %>% 
    summarise(Media.IOP.nr = round(weighted.mean(P21, PONDIIO), 2)) %>%
    spread(., Sexo, Media.IOP.nr) %>% 
    mutate(brecha.IOP.nr = round(((Varones-Mujeres)/Varones)*100, 1)) %>% 
    select(ANO4, TRIMESTRE, media.mujeres = Mujeres, media.varones = Varones, brecha.IOP.nr)
  
  return(tabla)  
  
}

# Función de calificación en cada sexo (ocupades con calificación válida)
sexo_segun_calif <- function(base){
  
  tabla <- base %>% 
    filter(ESTADO == 1,
           CALIFICACION!="0") %>% 
    group_by(Sexo) %>% 
    mutate(Frecuencia = sum(PONDERA)) %>% 
    group_by(ANO4, TRIMESTRE, Sexo, CALIFICACION) %>% 
    summarise(tasa = round(sum(PONDERA)/unique(Frecuencia)*100, 1))
  
  return(tabla)
  
}

# Función de sexo en cada calificación (ocupades con calificación válida)
calif_segun_sexo <- function(base){
  
  tabla <- base %>% 
    filter(ESTADO == 1,
           CALIFICACION!="0") %>% 
    group_by(CALIFICACION) %>% 
    mutate(Frecuencia = sum(PONDERA)) %>% 
    group_by(ANO4, TRIMESTRE, Sexo, CALIFICACION) %>% 
    summarise(tasa = round(sum(PONDERA)/unique(Frecuencia)*100, 1))
  
  return(tabla)
  
}

# Función de brecha del ingreso de la ocupación principal por calificación (ocupades con calificación válida)
brecha_IOP_calif <- function(base){
  
  tabla <- base %>% 
    filter(CALIFICACION!="0", # Calificacion valida
           ESTADO == 1) %>%   # Ocupades
    group_by(ANO4, TRIMESTRE, Sexo, CALIFICACION) %>% 
    summarise(Media.IOP.calif  = round(weighted.mean(P21, PONDIIO), 2)) %>%
    spread(., Sexo, Media.IOP.calif) %>% 
    mutate(brecha.IOP.calif = round(((Varones-Mujeres)/Varones)*100, 1)) %>% 
    select(ANO4, TRIMESTRE, CALIFICACION, media.mujeres = Mujeres, media.varones = Varones, brecha.IOP.calif)
  
  return(tabla)  
  
}

# Función de nivel educativo en cada sexo (ocupades con nivel educativo válido)
sexo_segun_nivel_educ <- function(base){
  
  tabla <- base %>% 
    filter(ESTADO == 1,                 # Ocupades
           !is.na(NIVEL_EDUCATIVO)) %>% # Nivel educativo valido
    group_by(Sexo) %>% 
    mutate(Frecuencia = sum(PONDERA)) %>% 
    group_by(ANO4, TRIMESTRE, Sexo, NIVEL_EDUCATIVO) %>% 
    summarise(tasa = round(sum(PONDERA)/unique(Frecuencia)*100, 1))
  
  return(tabla)
  
}

# Función de sexo en cada nivel educativo (ocupades con nivel educativo válido)
nivel_educ_segun_sexo <- function(base){
  
  tabla <- base %>% 
    filter(ESTADO == 1,                 # Ocupades
           !is.na(NIVEL_EDUCATIVO)) %>% # Nivel educativo valido
    group_by(NIVEL_EDUCATIVO) %>% 
    mutate(Frecuencia = sum(PONDERA)) %>% 
    group_by(ANO4, TRIMESTRE, Sexo, NIVEL_EDUCATIVO) %>% 
    summarise(tasa = round(sum(PONDERA)/unique(Frecuencia)*100, 1))
  
  return(tabla)
  
}

# Función del brecha del ingreso de la ocupación principal por nivel educativo (ocupades con nivel educativo válido)
brecha_IOP_nivel_educ <- function(base){
  
  tabla <- base %>%
    filter(ESTADO == 1,                 # Ocupades
           !is.na(NIVEL_EDUCATIVO)) %>% # Nivel educativo valido
    group_by(ANO4, TRIMESTRE, Sexo, NIVEL_EDUCATIVO) %>%
    summarise(Media.IOP.nivel.educ  = round(weighted.mean(P21, PONDIIO), 2)) %>%
    spread(., Sexo, Media.IOP.nivel.educ) %>% 
    mutate(brecha.IOP.nivel.educ = round(((Varones-Mujeres)/Varones)*100, 1)) %>% 
    select(ANO4, TRIMESTRE, NIVEL_EDUCATIVO, media.mujeres = Mujeres, media.varones = Varones, brecha.IOP.nivel.educ)
  
  return(tabla)  
  
}

# Función de horas (ocupades con horas positivas)
horas_semanales <- function(base){
  
  tabla <- base %>%
    filter(ESTADO == 1,
           PP3E_TOT > 0,
           PP3E_TOT != 999) %>%                      # Filtro los NAs en las horas de ocupacion principal
    mutate(PP3F_TOT = as.double(PP3F_TOT),           
           PP3F_TOT = case_when(PP3F_TOT == 999 ~ 0, # Cambio los NAs en las horas de ocup sec para que sumen 0
                                TRUE ~ PP3F_TOT),
           hs.total.ocup = PP3E_TOT + PP3F_TOT) %>%  # Sumo las horas de la ocup princ y de ocup sec
    group_by(ANO4, TRIMESTRE, Sexo) %>% 
    summarise(Media.hs.ocup.princ = round(weighted.mean(PP3E_TOT, PONDERA), 2),
              Media.hs.total.ocup = round(weighted.mean(hs.total.ocup, PONDERA), 2))
  
  return(tabla)
  
}

# Función de brecha del INGHORA de la ocupación principal (ocupades con horas positivas)
brecha_IOP_hr <- function(base){
  
  tabla <- base %>% 
    filter(ESTADO == 1,
           PP3E_TOT > 0,
           PP3E_TOT != 999) %>% 
    mutate(IOP_hr = round(P21/(PP3E_TOT * 30 / 7), 2)) %>% 
    group_by(ANO4, TRIMESTRE, Sexo) %>% 
    summarise(Media.IOP.hr = round(weighted.mean(IOP_hr, PONDIIO), 2)) %>%
    spread(., Sexo, Media.IOP.hr) %>% 
    mutate(brecha.IOP.hr = round(((Varones-Mujeres)/Varones)*100, 1)) %>% 
    select(ANO4, TRIMESTRE, media.mujeres = Mujeres, media.varones = Varones, brecha.IOP.hr)
  
  return(tabla)  
  
}

# Función de brecha del INGHORA de la ocupación principal por calificación (ocupades con calificación válida 
# y horas positivas)
brecha_IOP_hr_calif <- function(base){
  
  tabla <- base %>% 
    filter(ESTADO == 1,
           CALIFICACION!="0",
           PP3E_TOT > 0,
           PP3E_TOT != 999) %>% 
    mutate(IOP_hr = round(P21/(PP3E_TOT * 30 / 7), 2)) %>%
    group_by(ANO4, TRIMESTRE, Sexo, CALIFICACION) %>% 
    summarise(Media.IOP.hr.calif  = round(weighted.mean(IOP_hr, PONDIIO), 2)) %>%
    spread(., Sexo, Media.IOP.hr.calif) %>% 
    mutate(brecha.IOP.hr.calif = round(((Varones-Mujeres)/Varones)*100, 1)) %>% 
    select(ANO4, TRIMESTRE, CALIFICACION, media.mujeres = Mujeres, media.varones = Varones, brecha.IOP.hr.calif)
  
  return(tabla)
  
}

# Función de brecha del INGHORA de la ocupación principal por nivel educativo (ocupades con nivel educativo válido
# y horas positivas)
brecha_IOP_hr_nivel_educ <- function(base){
  
  tabla <- base %>% 
    filter(ESTADO == 1,
           !is.na(NIVEL_EDUCATIVO),
           PP3E_TOT > 0,
           PP3E_TOT != 999) %>% 
    mutate(IOP_hr = round(P21/(PP3E_TOT * 30 / 7), 2)) %>% 
    group_by(ANO4, TRIMESTRE, Sexo, NIVEL_EDUCATIVO) %>% 
    summarise(Media.IOP.hr.nivel.educ = round(weighted.mean(IOP_hr, PONDIIO), 2)) %>% 
    spread(., Sexo, Media.IOP.hr.nivel.educ) %>% 
    mutate(brecha.IOP.hr.nivel.educ = round(((Varones-Mujeres)/Varones)*100, 1)) %>% 
    select(ANO4, TRIMESTRE, NIVEL_EDUCATIVO, media.mujeres = Mujeres, media.varones = Varones, brecha.IOP.hr.nivel.educ)
  
  return(tabla)
  
}

# Función de proporción de cada sexo entre las personas que realizan las tareas domésticas del hogar
tareas_domesticas_sexo <- function(base, base_hogar){
  
  tabla <- base %>% 
    left_join(., base_hogar %>% select(CODUSU, NRO_HOGAR, VII1_1, VII1_2), by = c("CODUSU", "NRO_HOGAR")) %>% 
    mutate(proporcion = case_when(VII1_1 == COMPONENTE | VII1_2 == COMPONENTE ~ 1,
                                  TRUE ~ 0)) %>% 
    select(ANO4, TRIMESTRE, Sexo, proporcion, PONDERA) %>% 
    group_by(ANO4, TRIMESTRE, Sexo) %>% 
    summarise(proporcion = sum(proporcion*PONDERA)) %>% 
    mutate(proporcion = round(proporcion/sum(proporcion)*100, 0))
  
  return(tabla)
  
}

# Función de proporción de cada sexo entre quienes se dedican al servicio doméstico
servicio_domestico_sexo <- function(base){
  
  tabla <- base %>%
    filter(PP04B1 == 1) %>% 
    mutate(Total = sum(PONDERA)) %>% 
    group_by(ANO4, TRIMESTRE, Sexo) %>% 
    summarise(Proporcion = round(sum(PONDERA)/unique(Total)*100, 1))
  
  return(tabla)
  
}

# Función de proporción de mujeres que se dedican al servicio doméstico entre el total de ocupadas
servicio_domestico_ocupadas <- function(base){
  
  tabla <- base %>% 
    filter(ESTADO == 1,
           Sexo == "Mujeres") %>% 
    mutate(servicio.domestico = case_when(PP04B1 == 1 ~ "Sí",
                                          PP04B1 != 1 ~ "No")) %>% 
    group_by(ANO4, TRIMESTRE, servicio.domestico) %>% 
    summarise(frecuencia = sum(PONDERA)) %>% 
    mutate(proporcion = round(frecuencia/sum(frecuencia)*100, 0))
  
  return(tabla)
  
}

# Función de la composición según sexo de los deciles de ingresos totales individuales (perceptores)
deciles_ITI_sexo <- function(base){
  
  tabla <- base %>% 
    select(ANO4, TRIMESTRE, DECINDR, P47T, PONDII, Sexo) %>% 
    filter(DECINDR %in% c(1:10)) %>% 
    group_by(DECINDR) %>% 
    mutate(Pob = sum(PONDII)) %>% 
    group_by(ANO4, TRIMESTRE, DECINDR, Sexo) %>%
    summarise(Prop = round(sum(PONDII)/unique(Pob)*100, 1))
  
  return(tabla)
  
}

# Función de la composición según sexo de los deciles del ingreso per cápita familiar (total de la población)
deciles_IPCF_sexo <- function(base){
  
  tabla <- base %>% 
    select(ANO4, TRIMESTRE, DECCFR, IPCF, PONDIH, Sexo) %>% 
    filter(DECCFR %in% c(1:10)) %>% 
    group_by(DECCFR) %>% 
    mutate(Pob = sum(PONDIH)) %>% 
    group_by(ANO4, TRIMESTRE, DECCFR, Sexo) %>%
    summarise(Prop = round(sum(PONDIH)/unique(Pob)*100, 1))
  
  return(tabla)
  
}

# Función que crea las tablas
creador_tablas <- function(base, base_hogar,tareas_domesticas=TRUE){
  
  tasas_por_sexo_df              <- tasas_por_sexo(base)
  tasas_por_sexo_edad_df         <- tasas_por_sexo_edad(base)
  tasas_no_registro_df           <- tasas_no_registro(base)
  sexo_segun_jerarquias_df       <- sexo_segun_jerarquias(base)
  jerarquias_segun_sexo_df       <- jerarquias_segun_sexo(base)
  brecha_ITI_df                  <- brecha_ITI(base)
  brecha_IOP_df                  <- brecha_IOP(base)
  brecha_IOP_no_reg_df           <- brecha_IOP_no_reg(base)
  sexo_segun_calif_df            <- sexo_segun_calif(base)
  calif_segun_sexo_df            <- calif_segun_sexo(base)
  brecha_IOP_calif_df            <- brecha_IOP_calif(base)
  sexo_segun_nivel_educ_df       <- sexo_segun_nivel_educ(base)
  nivel_educ_segun_sexo_df       <- nivel_educ_segun_sexo(base)
  brecha_IOP_nivel_educ_df       <- brecha_IOP_nivel_educ(base)
  horas_semanales_df             <- horas_semanales(base)
  brecha_IOP_hr_df               <- brecha_IOP_hr(base)
  brecha_IOP_hr_calif_df         <- brecha_IOP_hr_calif(base)
  brecha_IOP_hr_nivel_educ_df    <- brecha_IOP_hr_nivel_educ(base)
  if (tareas_domesticas) {
    tareas_domesticas_sexo_df  <- tareas_domesticas_sexo(base, base_hogar)
  }else{
    tareas_domesticas_sexo_df  <- tibble()
  }
  servicio_domestico_sexo_df     <- servicio_domestico_sexo(base)
  servicio_domestico_ocupadas_df <- servicio_domestico_ocupadas(base)
  deciles_ITI_sexo_df            <- deciles_ITI_sexo(base)
  deciles_IPCF_sexo_df           <- deciles_IPCF_sexo(base)
  
  lista <- list("tasas_por_sexo_df" = tasas_por_sexo_df,
                "tasas_por_sexo_edad_df" = tasas_por_sexo_edad_df,
                "tasas_no_registro_df" = tasas_no_registro_df,
                "sexo_segun_jerarquias_df" = sexo_segun_jerarquias_df,
                "jerarquias_segun_sexo_df" = jerarquias_segun_sexo_df,
                "brecha_ITI_df" = brecha_ITI_df,
                "brecha_IOP_df" = brecha_IOP_df,
                "brecha_IOP_no_reg_df" = brecha_IOP_no_reg_df,
                "sexo_segun_calif_df" = sexo_segun_calif_df,
                "calif_segun_sexo_df" = calif_segun_sexo_df,
                "brecha_IOP_calif_df" = brecha_IOP_calif_df,
                "sexo_segun_nivel_educ_df" = sexo_segun_nivel_educ_df,
                "nivel_educ_segun_sexo_df" = nivel_educ_segun_sexo_df,
                "brecha_IOP_nivel_educ_df" = brecha_IOP_nivel_educ_df,
                "horas_semanales_df" = horas_semanales_df,
                "brecha_IOP_hr_df" = brecha_IOP_hr_df,
                "brecha_IOP_hr_calif_df" = brecha_IOP_hr_calif_df,
                "brecha_IOP_hr_nivel_educ_df" = brecha_IOP_hr_nivel_educ_df,
                "tareas_domesticas_sexo_df" = tareas_domesticas_sexo_df,
                "servicio_domestico_sexo_df" = servicio_domestico_sexo_df,
                "servicio_domestico_ocupadas_df" = servicio_domestico_ocupadas_df,
                "deciles_ITI_sexo_df" = deciles_ITI_sexo_df,
                "deciles_IPCF_sexo_df" = deciles_IPCF_sexo_df)
  
  return(lista)
  
}

# Función que ejecuta el creador de tablas para cada trimestre y junta los resultados
multiples_cuatris <- function(vector_directorios){
  
  tablas_por_trim <- list("tasas_por_sexo_df" = tibble(),
                          "tasas_por_sexo_edad_df" = tibble(),
                          "tasas_no_registro_df" = tibble(),
                          "sexo_segun_jerarquias_df" = tibble(),
                          "jerarquias_segun_sexo_df" = tibble(),
                          "brecha_ITI_df" = tibble(),
                          "brecha_IOP_df" = tibble(),
                          "brecha_IOP_no_reg_df" = tibble(),
                          "sexo_segun_calif_df" = tibble(),
                          "calif_segun_sexo_df" = tibble(),
                          "brecha_IOP_calif_df" = tibble(),
                          "sexo_segun_nivel_educ_df" = tibble(),
                          "nivel_educ_segun_sexo_df" = tibble(),
                          "brecha_IOP_nivel_educ_df" = tibble(),
                          "horas_semanales_df" = tibble(),
                          "brecha_IOP_hr_df" = tibble(),
                          "brecha_IOP_hr_calif_df" = tibble(),
                          "brecha_IOP_hr_nivel_educ_df" = tibble(),
                          "tareas_domesticas_sexo_df" = tibble(),
                          "servicio_domestico_sexo_df" = tibble(),
                          "servicio_domestico_ocupadas_df" = tibble(),
                          "deciles_ITI_sexo_df" = tibble(),
                          "deciles_IPCF_sexo_df" = tibble())
  
  for (directorio in vector_directorios) {
    
    individual <- levantar_bases(directorio) %>% 
      limpieza_individuos(.)
    
    individual <- limpieza_nombres(individual)
    
    dir_hogar <- gsub("individual", "hogar", directorio)
    
    hogar <- levantar_bases(dir_hogar)
    
    hogar <- limpieza_nombres(hogar)
    
    #agregamos la condicion, porque las bases viejas no tienen las variables de tareas domesticas
    td <- "VII1_1" %in% names(hogar) & "VII1_2" %in% names(hogar)
    
    tablas_list <- creador_tablas(base = individual, base_hogar = hogar, tareas_domesticas = td)
    
    for (x in names(tablas_por_trim)) {
      
      tablas_por_trim[[x]] <- bind_rows(tablas_por_trim[[x]], tablas_list[[x]])
      
    }
    
  }  
  
  return(tablas_por_trim)
}

##### Ejecuto #####

vec_dirs <- function(path = "Fuentes/2016-2019/"){
  
  vector_directorios <- paste0(path, list.files(path))
  vector_directorios <- vector_directorios[grepl("individual", vector_directorios)]
  df_dir <- vector_directorios %>% as.data.frame(.) %>% 
    mutate(ano =  substr(vector_directorios,(nchar(vector_directorios)-5),(nchar(vector_directorios)-4)),
           trim = substr(vector_directorios,(nchar(vector_directorios)-6),(nchar(vector_directorios)-6))) %>% 
    arrange(ano, trim)
  
  vector_directorios <- as_vector(df_dir$.)
  return(vector_directorios)
  
}

tablas_16_19 <- multiples_cuatris(vector_directorios = vec_dirs("Fuentes/2016-2019/"))


resultados <- list("tablas_16_19"=tablas_16_19)

saveRDS(resultados,"Resultados/tablas_resultados.RDS")

openxlsx::write.xlsx(tablas_16_19, "Resultados/tablas_resultados_16_19.xlsx")