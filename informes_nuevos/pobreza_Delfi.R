# ========================================================
# Análisis de pobreza / indigencia y planes sociales con EPH
# ========================================================

library(eph) 
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(tidyverse)

# --------------------------------------------------------
# 1. Tabla de umbrales CBA / CBT por semestre (llenar con datos oficiales)
# --------------------------------------------------------
# IMPORTANTE: reemplaza estos valores ejemplares con los reales del INDEC
umbrales_semestre <- tibble(
  semestre = c("1S 2023", "2S 2023", "1S 2024", "2S 2024", "1S 2025"),
  CBA = c(33730, 77889, 127287, 145408, 163756),   # valores de CBA
  CBT = c(75219, 160452, 282578, 331532, 365177)   # valores de CBT
)

# --------------------------------------------------------
# 2. Función para procesar cada semestre
# --------------------------------------------------------
procesar_semestre <- function(year, trimester) {
  # Descarga base individual
  base <- get_microdata(year = year, trimester = trimester, type = "individual")
  # Estandarizar CH05 como numérico (1 = varón, 2 = mujer)
  base <- base %>%
    mutate(
      CH05 = as.integer(CH05)
    )
  # Determinar semestre correcto (solo usamos trimestres 2 y 4)
  sem <- ifelse(trimester == 2, "1S", "2S")
  semestre <- paste0(sem, " ", year)
  
  # Unir con tabla de umbrales
  base2 <- base %>%
    mutate(semestre = semestre) %>%
    left_join(umbrales_semestre, by = "semestre")
  
  # Crear variables de interés
  base3 <- base2 %>%
    mutate(
      indigente = ifelse(IPCF < CBA, 1, 0),
      pobre     = ifelse(IPCF < CBT, 1, 0),
      plan      = ifelse(!is.na(PP07H) & PP07H > 0, 1, 0)
    )
  
  return(base3)
}

# --------------------------------------------------------
# 3. Generar lista de semestres válidos (por ahora hasta 1S 2025)
# --------------------------------------------------------
semestres <- expand.grid(
  year = 2023:2025,
  trimester = c(2, 4)   # trimestre 2 = 1S, trimestre 4 = 2S
) %>%
  arrange(year, trimester) %>%
  filter(!(year == 2025 & trimester > 1))  # solo 1t 2025 por ahora

# --------------------------------------------------------
# 4. Ejecutar el mapeo y producir la base combinada
# --------------------------------------------------------
datos <- map2_df(semestres$year, semestres$trimester, procesar_semestre)

# (Opcional) Verificar que se asignaron correctamente las canastas
# datos %>% count(semestre, !is.na(CBA))

# --------------------------------------------------------
# 5. Tabla de pobreza / indigencia vs recepción de planes
# --------------------------------------------------------
tabla_resumen <- datos %>%
  group_by(semestre, pobre, indigente, plan) %>%
  summarise(personas = sum(PONDERA, na.rm = TRUE), .groups = "drop") %>%
  group_by(semestre) %>%
  mutate(pct = round(personas / sum(personas) * 100, 2)) %>%
  ungroup()

# Pivotear para mejor legibilidad
tabla_pivot <- tabla_resumen %>%
  mutate(
    estado = case_when(
      pobre == 1 & indigente == 1 ~ "indigente",
      pobre == 1 & indigente == 0 ~ "pobre_no_indigente",
      pobre == 0 ~ "no_pobre"
    )
  ) %>%
  select(semestre, estado, plan, personas, pct) %>%
  pivot_wider(
    names_from = plan,
    values_from = c(personas, pct),
    names_prefix = "plan_"
  )

# --------------------------------------------------------
# 6. Evolución de AUH real vs CBA / CBT — ejemplo
# --------------------------------------------------------
# Debés obtener los montos nominales reales de la AUH por semestre
auh_semestre <- tibble(
  semestre = c("1S 2023","2S 2023","1S 2024","2S 2024","1S 2025"),
  monto_nominal = c(13834, 20661, 74354, 93281, 106505)
)

evol_auh <- auh_semestre %>%
  left_join(umbrales_semestre, by = "semestre") %>%
  mutate(
    pct_cba = round((monto_nominal / CBA) * 100, 2),
    pct_cbt = round((monto_nominal / CBT) * 100, 2)
  )

# --------------------------------------------------------
# 7. Gráficos de la evolución
# --------------------------------------------------------
g1 <- ggplot(evol_auh, aes(x = semestre, y = pct_cba, group = 1)) +
  geom_line(color = "blue") + geom_point(color = "red") +
  labs(
    x = "Semestre", y = "% de CBA cubierta por AUH",
    title = "Evolución de la AUH respecto a la CBA"
  ) +
  theme_minimal()

g2 <- ggplot(evol_auh, aes(x = semestre, y = pct_cbt, group = 1)) +
  geom_line(color = "green") + geom_point(color = "darkgreen") +
  labs(
    x = "Semestre", y = "% de CBT cubierta por AUH",
    title = "Evolución de la AUH respecto a la CBT"
  ) +
  theme_minimal()

# Gráfico combinado
g_comb <- ggplot(evol_auh, aes(x = semestre)) +
  geom_line(aes(y = pct_cba, color = "AUH / CBA")) +
  geom_point(aes(y = pct_cba, color = "AUH / CBA")) +
  geom_line(aes(y = pct_cbt, color = "AUH / CBT")) +
  geom_point(aes(y = pct_cbt, color = "AUH / CBT")) +
  labs(
    x = "Semestre", y = "% cubierta",
    title = "AUH como porcentaje de la CBA y la CBT",
    color = "Relación"
  ) +
  theme_minimal()

# --------------------------------------------------------
# 8. Salidas importantes
# --------------------------------------------------------
print(tabla_pivot)
print(evol_auh)
print(g1)
print(g2)
print(g_comb)



#---------------- Gráfico Clari --------------------------
library(ggplot2)
library(dplyr)

# -------------------------
# Datos
# -------------------------
evol_auh <- tibble(
  semestre = c("1S 2023","2S 2023","1S 2024","2S 2024","1S 2025"),
  CBA_real = c(100, 111.734, 101.572, 95.7887, 93.7258),
  CBT_real = c(100, 103.215, 101.115, 97.9356, 93.7247),
  AUH_real = c(100, 72.2653, 144.664, 149.826, 148.628),
  AUH_CBA  = c(41.0, 26.5, 58.4, 64.2, 65.0),
  AUH_CBT  = c(18.4, 12.9, 26.3, 28.1, 29.2)
)

# Ordenar y acortar etiquetas de semestres
evol_auh$semestre <- factor(
  evol_auh$semestre,
  levels = c("1S 2023","2S 2023","1S 2024","2S 2024","1S 2025"),
  labels = c("1°23","2°23","1°24","2°24","1°25")
)

# -------------------------
# Escalado para usar dos ejes
# -------------------------
factor <- max(evol_auh$AUH_CBA, evol_auh$AUH_CBT) / max(evol_auh$AUH_real)

# -------------------------
# Gráfico
# -------------------------
g <- ggplot(evol_auh, aes(x = semestre)) +
  
  # % cobertura (eje izquierdo)
  geom_line(aes(y = AUH_CBA, color = "AUH/CBA", group = 1), 
            size = 1.2, linetype = "dashed") +
  geom_point(aes(y = AUH_CBA, color = "AUH/CBA"), size = 2) +
  
  geom_line(aes(y = AUH_CBT, color = "AUH/CBT", group = 1), 
            size = 1.2, linetype = "dashed") +
  geom_point(aes(y = AUH_CBT, color = "AUH/CBT"), size = 2) +
  
  # Índices reales (escalados al eje izquierdo, con eje secundario)
  geom_line(aes(y = AUH_real * factor, color = "AUH real", group = 1), size = 1.2) +
  geom_point(aes(y = AUH_real * factor, color = "AUH real"), size = 2) +
  
  geom_line(aes(y = CBA_real * factor, color = "CBA real", group = 1), size = 1.2) +
  geom_point(aes(y = CBA_real * factor, color = "CBA real"), size = 2) +
  
  geom_line(aes(y = CBT_real * factor, color = "CBT real", group = 1), size = 1.2) +
  geom_point(aes(y = CBT_real * factor, color = "CBT real"), size = 2) +
  
  # Ejes
  scale_y_continuous(
    name = "% de cobertura AUH",
    sec.axis = sec_axis(~./factor, 
                        name = "Índice real (1°23 = 100)",
                        breaks = c(50, 100, 150))   # mostrar 100 y 150
  ) +
  
  # Colores estilo Ecofeminita
  scale_color_manual(
    values = c(
      "AUH/CBA" = "#E94F64",   # rosa
      "AUH/CBT" = "#2EC4B6",   # turquesa
      "CBA real" = "#E94F64",  # rosa
      "CBT real" = "#2EC4B6",  # turquesa
      "AUH real" = "#999999"   # gris
    ),
    breaks = c("AUH/CBA","AUH/CBT","CBA real","CBT real","AUH real") # orden de leyenda
  ) +
  
  labs(
    title = "Participación de la AUH sobre CBA y CBT (eje izq). Evolución real CBA, CBT y AUH (eje der)",
    x = "Semestre"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.title = element_blank(),   # saca "Serie"
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )

# Mostrar en pantalla
print(g)

# Exportar en PNG con fondo blanco
ggsave("evolucion_AUH.png", g, width = 8, height = 6, dpi = 300, bg = "white")
#----

############################### ECOFEM CODE ####################################
# =========================================================
# Análisis de pobreza e indigencia (EPH - INDEC)
# 4to trimestre 2024
# =========================================================

# --- Paquetes necesarios ---
library(eph)         # Para bajar microdatos y canastas
library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)
library(kableExtra)

# --- 1. Descargamos microdatos EPH (individuales) ---
base_individual <- eph::get_microdata(year = 2024, trimester = 4, type = "individual")

# --- 2. Descargamos canastas (CBA y CBT) ---
canastas <- eph::get_poverty_lines(regional = TRUE)

# --- 3. Identificador de período (ej: "2024.4") ---
date_trimestre <- paste(base_individual$ANO4[1], base_individual$TRIMESTRE[1], sep = ".")
show.pobreza <- nrow(canastas %>% filter(periodo == date_trimestre)) > 0

# --- 4. Cálculo de pobreza e indigencia ---
if(show.pobreza){
  
  tabla_pobreza <- base_individual %>% 
    eph::calculate_poverty(basket = canastas, print_summary = FALSE) %>% 
    filter(ESTADO == 1,          # Solo ocupados
           PP3E_TOT > 0,         # Horas trabajadas positivas
           PP3E_TOT != 999,
           P21 > 0)              # Ingreso laboral positivo
  
  # ---- Nacional ----
  pobreza_nacional <- tabla_pobreza %>% 
    summarise(
      Pobreza = round(sum(PONDIH[situacion %in% c("pobre","indigente")], na.rm = TRUE) / sum(PONDIH, na.rm = TRUE) * 100, 2),
      Indigencia = round(sum(PONDIH[situacion == "indigente"], na.rm = TRUE) / sum(PONDIH, na.rm = TRUE) * 100, 2)
    )
  
  cat("\n>>> Tasas nacionales (4to trimestre 2024):\n")
  print(pobreza_nacional)
  
  # ---- Por sexo ----
  pobreza_sexo <- tabla_pobreza %>% 
    mutate(Sexo = ifelse(CH04 == 1, "Varones", "Mujeres")) %>% 
    group_by(Sexo) %>% 
    summarise(
      Pobreza = round(sum(PONDIH[situacion %in% c("pobre","indigente")], na.rm = TRUE) / sum(PONDIH, na.rm = TRUE) * 100, 2),
      Indigencia = round(sum(PONDIH[situacion == "indigente"], na.rm = TRUE) / sum(PONDIH, na.rm = TRUE) * 100, 2)
    )
  
  cat("\n>>> Tasas por sexo (4to trimestre 2024):\n")
  print(pobreza_sexo)
  
  # ---- Por región ----
  pobreza_region <- tabla_pobreza %>% 
    group_by(Region) %>% 
    summarise(
      Pobreza = round(sum(PONDIH[situacion %in% c("pobre","indigente")], na.rm = TRUE)/sum(PONDIH, na.rm = TRUE)*100, 2),
      Indigencia = round(sum(PONDIH[situacion == "indigente"], na.rm = TRUE)/sum(PONDIH, na.rm = TRUE)*100, 2)
    )
  
  cat("\n>>> Tasas por región (4to trimestre 2024):\n")
  print(pobreza_region)
  
} else {
  cat("\nTodavía no están disponibles las canastas para el 4to trimestre 2024.\n")
}

###################################### NEW ##########################################

if(show.pobreza){
  
  # Creamos variable: cobra o no transferencias sociales
  tabla_pobreza <- tabla_pobreza %>% 
    mutate(cobra_transferencia = ifelse(P47T > 0, "Cobra", "No cobra"))
  
  # Proporción de pobres que cobran/no cobran
  pobres_transfer <- tabla_pobreza %>% 
    filter(situacion %in% c("pobre","indigente")) %>% 
    group_by(situacion, cobra_transferencia) %>% 
    summarise(
      Proporcion = round(sum(PONDIH, na.rm = TRUE) / 
                           sum(PONDIH[situacion %in% c("pobre","indigente")], na.rm = TRUE) * 100, 1)
    )
  
  cat("\n>>> Proporción de pobres e indigentes según reciben transferencias sociales (4T 2024):\n")
  print(pobres_transfer)
}

if(show.pobreza){
  
  # Canasta equivalente individual (ya está en tu tabla si usaste calculate_poverty con join)
  tabla_pobreza <- tabla_pobreza %>% 
    left_join(canastas, by = c("REGION" = "codigo", "periodo")) %>% 
    left_join(eph::adulto_equivalente, by = c("CH04", "CH06")) %>% 
    mutate(
      CBA_indiv = CBA * adequi,
      CBT_indiv = CBT * adequi
    )
  
  # Relación de transferencias sobre canasta
  rel_transfer <- tabla_pobreza %>% 
    filter(situacion %in% c("pobre","indigente"), P47T > 0) %>% 
    mutate(
      ratio_CBT = ifelse(situacion=="pobre", P47T / CBT_indiv, NA),
      ratio_CBA = ifelse(situacion=="indigente", P47T / CBA_indiv, NA)
    ) %>% 
    group_by(situacion) %>% 
    summarise(
      Promedio_transferencias = mean(P47T, na.rm = TRUE),
      Promedio_ratio_CBT = mean(ratio_CBT, na.rm = TRUE, na.rm = TRUE),
      Promedio_ratio_CBA = mean(ratio_CBA, na.rm = TRUE, na.rm = TRUE)
    )
  
  cat("\n>>> Relación entre monto transferido y canasta correspondiente (4T 2024):\n")
  print(rel_transfer)
}

# -------------------------
# Paquetes
# -------------------------
library(eph)
library(dplyr)
library(ggplot2)

# -------------------------
# 1. Descarga microdatos EPH 4T 2024
# -------------------------
base <- get_microdata(year = 2024, trimester = 4, type = "individual")

# -------------------------
# 2. Calcular pobreza nacional
# -------------------------
#Cargar las canastas básicas
canastas <- get_poverty_lines(regional = TRUE)

#Calcular pobreza nacional con CBT (total)
pobreza <- calculate_poverty(base, basket = canastas)

#Calcular tasas nacionales
tasas_nac <- pobreza %>% 
  summarise(
    Pobreza   = round(sum(PONDIH[situacion %in% c("pobre","indigente")], na.rm = TRUE) / sum(PONDIH, na.rm = TRUE) * 100, 1),
    Indigencia = round(sum(PONDIH[situacion == "indigente"], na.rm = TRUE) / sum(PONDIH, na.rm = TRUE) * 100, 1)
  )

cat("\n>>> Tasas nacionales (4T 2024):\n")
print(tasas_nac)


# -------------------------
# 3. Transferencias sociales (usamos T_VI)
# -------------------------
# T_VI: tipo de ingreso, hay categorías específicas de transferencias sociales
# ejemplo: jubilaciones, AUH, otros programas, etc.
# Creamos un indicador: "cobra_transferencia" si declara ingreso no laboral de ese tipo

base_transf <- base %>% 
  mutate(cobra_transferencia = ifelse(T_VI %in% c(4,5,6,7,8) & ITF > 0, 1, 0)) 
# (ajustar códigos exactos de T_VI según la doc EPH, acá pongo los comunes: jubilaciones, AUH, planes, etc.)

# Unimos con situación de pobreza
tabla_pobreza <- pobreza %>% 
  left_join(base_transf %>% select(CODUSU, NRO_HOGAR, COMPONENTE, cobra_transferencia), 
            by = c("CODUSU","NRO_HOGAR","COMPONENTE"))

# -------------------------
# 4. Proporción pobres e indigentes según cobran/no cobran
# -------------------------
pobres_transfer <- tabla_pobreza %>% 
  filter(situacion %in% c("pobre","indigente")) %>% 
  group_by(situacion, cobra_transferencia) %>% 
  summarise(Proporcion = sum(PONDIH)/sum(PONDIH[situacion %in% c("pobre","indigente")])*100) %>% 
  mutate(cobra_transferencia = ifelse(cobra_transferencia==1,"Cobra","No cobra"))

cat("\n>>> Proporción de pobres e indigentes según cobran transferencias (4T 2024):\n")
print(pobres_transfer)

# -------------------------
# 5. Relación transferencias sociales / CBA y CBT
# -------------------------
# Unimos canastas
tabla_pobreza <- tabla_pobreza %>%
  mutate(periodo = paste(ANO4, TRIMESTRE, sep = ".")) %>%  # 🔑 crear periodo
  left_join(canastas, by = c("REGION" = "codigo", "periodo")) %>% 
  left_join(eph::adulto_equivalente, by = c("CH04", "CH06")) %>% 
  mutate(
    CBA_indiv = CBA * adequi,
    CBT_indiv = CBT * adequi
  )


# Supongamos que ingreso transferencias = monto declarado por T_VI transferencias
# (ajustar si tenés un campo específico en tu base consolidada)
tabla_pobreza <- tabla_pobreza %>% 
  mutate(ingreso_transferencias = ifelse(cobra_transferencia==1, ITF, 0))

rel_transfer <- tabla_pobreza %>% 
  filter(situacion %in% c("pobre","indigente"), cobra_transferencia==1) %>% 
  mutate(
    ratio_CBT = ifelse(situacion=="pobre", ingreso_transferencias / CBT_indiv, NA),
    ratio_CBA = ifelse(situacion=="indigente", ingreso_transferencias / CBA_indiv, NA)
  ) %>% 
  group_by(situacion) %>% 
  summarise(
    Promedio_transferencias = mean(ingreso_transferencias, na.rm = TRUE),
    Promedio_ratio_CBT = mean(ratio_CBT, na.rm = TRUE),
    Promedio_ratio_CBA = mean(ratio_CBA, na.rm = TRUE)
  )

cat("\n>>> Relación entre monto transferido y canasta correspondiente (4T 2024):\n")
print(rel_transfer)

# -------------------------
# 6. Gráficos
# -------------------------

# ---- Gráfico 1: proporción pobres/indigentes cobra vs no cobra ----
g1 <- ggplot(pobres_transfer, aes(x = situacion, y = Proporcion, fill = cobra_transferencia)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(round(Proporcion,1),"%")),
            position = position_stack(vjust = 0.5), color="white", size=4) +
  scale_fill_manual(values = c("Cobra"="#d73027","No cobra"="#4575b4")) +
  labs(title="Distribución de pobres e indigentes según transferencias sociales (4T 2024)",
       x="", y="% del total", fill="Transferencias") +
  theme_minimal(base_size=13) +
  theme(plot.title = element_text(face="bold",hjust=0.5),
        legend.position="bottom")

# ---- Gráfico 2: cobertura promedio transferencias / canasta ----
rel_plot <- rel_transfer %>% 
  mutate(ratio = ifelse(situacion=="pobre", Promedio_ratio_CBT, Promedio_ratio_CBA)*100)

g2 <- ggplot(rel_plot, aes(x=situacion, y=ratio, group=1, color=situacion)) +
  geom_line(linewidth=1.2) +
  geom_point(size=3) +
  geom_text(aes(label=paste0(round(ratio,1),"%")), vjust=-1, size=4) +
  scale_color_manual(values=c("pobre"="#e41a1c","indigente"="#377eb8")) +
  labs(title="Cobertura promedio de transferencias sociales sobre canastas (4T 2024)",
       x="", y="% de la canasta cubierta") +
  theme_minimal(base_size=13) +
  theme(plot.title = element_text(face="bold",hjust=0.5),
        legend.position="none")

# Mostrar gráficos
print(g1)
print(g2)
