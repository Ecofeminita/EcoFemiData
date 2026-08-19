# Insumos para shinyapp (prepro) ----

# reiniciamos completamente RStudio para el preprocesamiento
.rs.restartR() # esperar a que termine y luego correr rm() y gc()
rm(list = ls())
gc()

# Librerías
suppressPackageStartupMessages(library(tidyverse))

## PPG ----
df_raw <- readRDS("MenstruAccion/Calcu-M/insumos/precios-gestion-menstrual-limpio.RDS")

# Como vamos a volar las marcas y tenemos una sobrerrepresentacion muy fuerte de las observaciones en el AMBA, vamos a comparar los valores
# que nos devuelve una mediana y una media truncada (5% - 10%). De esta forma lo que buscamos es evitar sesgo geográfico y la dispersión
# por marca/calidad 

# Limpiamos outliers y marcas extremas

precio_provincia <- df_raw %>%
  group_by(Categoría, Provincia) %>%
  summarise(
    precio_mediana = median(precio_unidad, na.rm = TRUE),
    precio_trim = mean(precio_unidad, trim = 0.05, na.rm = TRUE),
    n = n()
  )


# Corregimos el sesgo del AMBA y las diferencias regionales
menstruan <- openxlsx::read.xlsx("MenstruAccion/Calcu-M/insumos/poblaciones_2022_2040_mujeres_edad.xlsx", sheet = 1)
menstruan <- menstruan %>% 
  mutate(total = sum(Menstruan),
         ponderador = Menstruan/total) %>% 
  select(Provincia, ponderador)

precio_nacional <- precio_provincia %>%
  left_join(menstruan, by = "Provincia") %>%
  group_by(Categoría) %>%
  summarise(
    precio_nacional = weighted.mean(precio_mediana, w = ponderador, na.rm = TRUE)
  )

rm(df_raw, menstruan)

## Medicamentos ----
medicamentos <- read.csv("./MenstruAccion/scrappeoMedicamentos/scrappeoMedicamentos.csv") %>% janitor::clean_names()
medicamentos$fecha <- as.Date(medicamentos$fecha, format = "%d/%m/%Y")
medicamentos <- medicamentos %>% select(nombre, presentacion, droga, forma, via, categoria_busqueda, query, precio)

medicamentos <- medicamentos %>%
  mutate(
    across(
      c(nombre, presentacion, droga, forma, via, categoria_busqueda, query),
      ~ str_to_lower(str_squish(as.character(.x)))
    )
  )

# Drogas relevantes para climaterio / menopausia
drogas_relevantes <- c(
  "estradiol",
  "estriol",
  "promestriene",
  "prasterona",
  "progesterona",
  "tibolona",
  "paroxetina",
  "paroxetina mesilato",
  "venlafaxina",
  "desvenlafaxina",
  "sertralina",
  "melatonina",
  "isoflavonas de soja",
  "isoflavonas de soja+asoc.",
  "testosterona",
  "ibuprofeno",
  "testosterona",
  "oxazepam"
)

# Falsos positivos / productos a excluir
patron_excluir_droga <- paste(c(
  # anticonceptivos combinados o anticoncepción
  "etinilestradiol",
  "desogestrel",
  "gestodeno",
  "levonorgestrel",
  "etonogestrel",
  "norelgestromin",
  "ciproterona",
  "dienogest",
  "nomegestrol",
  "clomifeno",
  
  # analgésicos / otros fármacos no específicos
  "hioscina",
  "homatropina",
  "dexclorfeniramina",
  "letrozol",
  "ibandrónico",
  "fólico",
  "hialurónico",
  "láctico",
  "angélica",
  "centella",
  "ginseng",
  "l-arginina",
  "hipérico",
  
  # suplementos generales que conviene analizar aparte
  "calcio",
  "vit",
  "hierro",
  "magnesio"
), collapse = "|")

patron_excluir_forma <- paste(c(
  "shampoo",
  "jabón",
  "crema de enjuague",
  "emulsiones",
  "leches de limpieza",
  "batidos",
  "alimentos",
  "caramelos",
  "cremas dentales",
  "profilácticos",
  "apósitos",
  "curitas"
), collapse = "|")

patron_excluir_nombre_presentacion <- paste(c(
  "shampoo",
  "jabón",
  "colageno",
  "colágeno",
  "crema de enjuague",
  "alimento",
  "batido",
  "caramelo",
  "toall",
  "pañal"
), collapse = "|")

# Clasificación terapéutica útil
medicamentos_limpio <- medicamentos %>%
  mutate(
    droga_relevante = droga %in% drogas_relevantes |
      str_detect(droga, "estradiol\\+medroxiprogesterona|estradiol\\+noretisterona|drospirenona\\+estradiol|\\+oxazepam"),
    
    falso_positivo = str_detect(droga, patron_excluir_droga) |
      str_detect(forma, patron_excluir_forma) |
      str_detect(nombre, patron_excluir_nombre_presentacion) |
      str_detect(presentacion, patron_excluir_nombre_presentacion)
  ) %>%
  filter(droga_relevante == TRUE) %>%
  filter(falso_positivo == FALSE)

# Revisión de lo que quedó
medicamentos_limpio %>%
  count(droga_relevante, droga, sort = TRUE)

# Revisión de lo excluido por si querés auditar
df_excluidos <- medicamentos %>%
  mutate(
    excluido_por_regla = str_detect(droga, patron_excluir_droga) |
      str_detect(forma, patron_excluir_forma) |
      str_detect(nombre, patron_excluir_nombre_presentacion) |
      str_detect(presentacion, patron_excluir_nombre_presentacion)
  ) %>%
  filter(excluido_por_regla == TRUE)

medicamentos_limpio <- medicamentos_limpio %>%
  mutate(
    droga_original = droga,
    
    droga_costeo = case_when(
      str_detect(droga, "isoflavonas de soja") ~ "isoflavonas",
      str_detect(droga, "paroxetina") ~ "paroxetina",
      str_detect(droga, "estradiol") & str_detect(forma, "crema") ~ "estradiol_gel",
      str_detect(droga, "estradiol") ~ "estradiol",
      str_detect(droga, "progesterona") ~ "progesterona",
      str_detect(droga, "drospirenona\\+estradiol") ~ "fem",
      str_detect(droga, "estradiol\\+noretisterona") ~ "fem",
      str_detect(droga, "estradiol\\+medroxiprogesterona") ~ "fem",
      str_detect(droga, "oxazepam") ~ "oxazepam",
      TRUE ~ droga
    )
  )

# Calculo el costo mensual a partir de consumo estimado

## tabla de referencia de uso de medicamentos según recomendación médica (consultado a GPT)
referencia_formas <- tibble::tribble(
  ~droga_costeo, ~forma_ref, ~unidad_mes_base,
  "estradiol", "comprimidos", 30,

  "estradiol_gel", "crema", 1,

  "fem", "comprimidos", 10,
  
  "progesterona", "comprimidos", 30,
  
  "tibolona", "comprimidos", 30,
  
  "estriol", "crema", 1,
  
  "promestriene", "crema", 1,
  
  "prasterona", "ovulos", 30,
  
  "paroxetina", "comprimidos", 30,
  "venlafaxina", "comprimidos", 30,
  "desvenlafaxina", "comprimidos", 30,
  "sertralina", "comprimidos", 30,
  "melatonina", "comprimidos", 30,
  "oxazepam", "comprimidos", 30,
  "isoflavonas", "comprimidos", 30
)

# Clasifico el formato en el que se presenta la medicación
medicamentos_limpio_ref <- medicamentos_limpio %>%
  mutate(
    forma_ref = case_when(
      
      str_detect(forma, "ovulo") ~ "ovulos",
      str_detect(forma, "comprim|cáps") ~ "comprimidos",
      str_detect(forma, "parche") ~ "parche",
      str_detect(forma, "crema") ~ "crema",
      str_detect(forma, "ampolla|jerin") ~ "ampollas",
      
      TRUE ~ "otra"
    )
  )

# Limpio los que quedan por fuera de las opciones que presentamos en la encuesta de la app
medicamentos_limpio_ref <- medicamentos_limpio_ref %>% left_join(referencia_formas, by = c("droga_costeo", "forma_ref")) %>% 
  filter(!is.na(unidad_mes_base))


# Busco las unidades que trae cada envase
medicamentos_limpio_ref <- medicamentos_limpio_ref %>%
  mutate(
    unidades_envase = case_when(
      forma_ref=="crema" ~ "1", # única unidad
      TRUE ~ str_extract(presentacion, "(?<=x )\\d+|(?<=x)\\d+")),
    unidades_envase = as.numeric(unidades_envase)
  )

medicamentos_limpio_ref <- medicamentos_limpio_ref %>%
  # me quedo con los que tengan precio y unidades por envase
  filter(!is.na(precio), precio > 0, !is.na(unidades_envase), unidades_envase > 0)

medicamentos_limpio_ref <- medicamentos_limpio_ref %>%
  mutate(
    precio_unitario_envase = precio / unidades_envase,
    costo_mensual_estimado = precio_unitario_envase * unidad_mes_base
  )

# Tenemos más observaciones de los antidepresivos, mientras que para los demás medicamentos la muestra es baja.
# Con la intención de responder la pregunta que nos hacemos "¿cuánto cuesta usualmente tratar menopausia/climaterio?", vamos a quedarnos con
# el precio promedio que conseguimos de la web de Precios de Medicamentos del Ministerio de Salud de la Nación.Nno tenemos ninguna
# variable que nos indique que estos precios pertenecen a una Región o aglomerado urbano específico por lo que vamos a considerarlos referencia
# a nivel nacional.
costo_medicamentos <- medicamentos_limpio_ref %>%
  group_by(droga_costeo) %>%
  summarise(
    n_observaciones = n(),
    costo_mensual_mediano = median(costo_mensual_estimado, na.rm = TRUE),
    costo_mensual_promedio = mean(costo_mensual_estimado, na.rm = TRUE), # vamos a usar este para el cálculo de la canasta
    .groups = "drop"
  )


# Exporto los csv con los precios
write.csv(precio_nacional, "MenstruAccion/Calcu-M/insumos/preciosPGM.csv")
write.csv(costo_medicamentos, "MenstruAccion/Calcu-M/insumos/preciosMED.csv")