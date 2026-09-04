# Análisis exploratorio de las respuestas recolectadas con la encuesta de calcu-M (app.R)
# Corre con el working directory ubicado en MenstruAccion/calcu-M (mismo supuesto que usa app.R)

## Librerías ----
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(googlesheets4))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(ggwordcloud))

options(scipen = 100, digits = 4)

if (!dir.exists(".secrets") && dir.exists("MenstruAccion/calcu-M/.secrets")) {
  setwd("MenstruAccion/calcu-M")
}

## Funciones ----
source("funciones/google_sheets_auth.R", local = TRUE)
source("funciones/analisis_helpers.R", local = TRUE)
source("funciones/costo_fila_helpers.R", local = TRUE)

## Conexión a Google Sheets ----
if (file.exists(".secrets/.env")) readRenviron(".secrets/.env")

SHEET_ID <- Sys.getenv("SHEET_ID")
if (identical(SHEET_ID, "")) {
  stop("SHEET_ID no está definida. Configurá .secrets/.env (local) o la variable de entorno en shinyapps.io.")
}

gs4_auth(path = resolve_google_credentials())

respuestas <- read_sheet(SHEET_ID)

## Vocabularios de opciones (idénticos a las constantes definidas en app.R) ----
# Los necesitamos para poder reconstruir qué opciones se marcaron en cada campo de checkboxes:
# guardar_respuesta() en app.R junta las opciones elegidas con ", ", pero varias opciones tienen
# comas *dentro* de su propio texto (por ej. "Laboratorio (glucosa, colesterol, vitamina D)"),
# por lo que separar el string guardado por comas rompe esas opciones en fragmentos falsos.
# La única forma confiable de recuperar la selección original es buscar, para cada opción conocida,
# si su texto completo aparece dentro del campo guardado.
LIFE_STAGE <- c(
  "Menstruando y sin signos de climaterio/menopausia",
  "Experimentando signos de climaterio/menopausia",
  "Atravesando una menopausia \"inducida\" (por tratamiento médico u otra situación)",
  "Post menopausia (ya pasó un año o más desde la última menstruación)"
)

AGE_RANGES <- c("Prefiero no responder", "18-24", "25-34", "35-44", "45-54", "55-64", "65+")

MENSTRUAL_PRODUCTS <- c("Toallas higiénicas", "Protectores diarios", "Tampones", "Copa menstrual", "Ninguno/a")

MEDS <- c(
  "Estradiol (pastillas/comprimidos) — terapia hormonal con estrógenos",
  "Estradiol (gel o crema) — terapia hormonal con estrógenos de uso transdérmico",
  "Estriol — estrógeno de uso vaginal o tópico (gel, óvulos o crema)",
  "Promestriene — crema u óvulos vaginales para sequedad o molestias íntimas",
  "Prasterona (DHEA) — tratamiento vaginal hormonal para sequedad o dolor",
  "Progesterona — cápsulas hormonales, muchas veces combinadas con estrógenos",
  "Tibolona — comprimidos hormonales para síntomas de menopausia",
  "Testosterona — gel hormonal indicado en algunos casos de baja libido o cansancio",
  "Ninguno/a"
)

OTHERS_MEDS <- c(
  "Melatonina — comprimidos o gotas para dormir o mejorar el sueño",
  "Isoflavonas — suplementos “naturales” o de soja para síntomas menopáusicos",
  "Oxazepam (por ejemplo, Pausafren T) — ansiolítico o medicación para nervios/ansiedad",
  "Antidepresivos (por ejemplo, sertralina, venlafaxina o paroxetina) — usados para depresión, ansiedad o sofocos",
  "Analgésicos o antiinflamatorios (por ejemplo, línea FEM) — para dolores, migrañas o malestar corporal",
  "Ninguno/a"
)

PRACTICES <- c(
  "Mamografía",
  "Ecografía mamaria",
  "PAP y Colposcopía",
  "Ecografía transvaginal",
  "Densitometría ósea",
  "Laboratorio (glucosa, colesterol, vitamina D)",
  "Ninguno/a"
)

SELFCARE_PRODUCTS <- c(
  "Geles lubricantes o hidratantes vaginales",
  "Crema corporal/facial",
  "Toallas para incontinencia leve",
  "Protectores de incontinencias",
  "Consultorio de Ginecología",
  "Consultorio de Psicología",
  "Consultorio de Sexología",
  "Consultorio de Psiquiatría",
  "Consultorio de Nutrición",
  "Terapias alternativas",
  "Hierbas medicinales u otros productos no farmacológicos",
  "Ninguno/a"
)

SUPPLEMENTS <- c("Nueces", "Almendras", "Jugo de arándanos", "Vitaminas", "Calcio", "Magnesio", "Hierro", "Ninguno/a")

## Filtro de filas de prueba ----
# test_sheets_connection.R escribe una fila con "TEST" en todos los campos para validar la conexión;
# la sacamos exigiendo que life_stage sea una de las opciones reales de la encuesta.
n_total_crudo <- nrow(respuestas)
respuestas <- respuestas %>% filter(life_stage %in% LIFE_STAGE)
cat("Filas descartadas por no ser respuestas válidas (pruebas de conexión, etc.):", n_total_crudo - nrow(respuestas), "\n")
cat("Respuestas válidas analizadas:", nrow(respuestas), "\n")

## Insumos de precios (los mismos que usa la app para estimar la canasta) ----
preciosPGM <- read.csv("./insumos/preciosPGM.csv", encoding = "UTF-8")
preciosMED <- read.csv("./insumos/preciosMED.csv", encoding = "UTF-8")


# 1. Perfil sociodemográfico ----
cat("\n== Ciclo vital (life_stage) ==\n")
print(tabla_freq(respuestas, life_stage))

cat("\n== Género ==\n")
print(tabla_freq(respuestas, gender))

cat("\n== Rango etario ==\n")
print(tabla_freq(respuestas, age_range))

cat("\n== Ciclo vital autopercibido cruzado con rango etario ==\n")
respuestas %>%
  count(life_stage, age_range) %>%
  arrange(life_stage, factor(age_range, levels = AGE_RANGES)) %>%
  print(n = 50)

cat("\n== Provincia ==\n")
print(tabla_freq(respuestas, province))

cat("\n== Cobertura de salud ==\n")
print(tabla_freq(respuestas, coverage))

cat("\n== Regularidad del período ==\n")
print(tabla_freq(respuestas, regular_period))

# 2. Uso de productos de gestión menstrual ----
cat("\n== Productos de gestión menstrual utilizados ==\n")
print(tabla_freq_multi(respuestas$menstrual_products, MENSTRUAL_PRODUCTS, nrow(respuestas), excluir_ninguno = FALSE))

# Promedio de unidades mensuales, solo entre quienes efectivamente usan cada producto (cantidad > 0)
promedios_cantidades <- respuestas %>%
  summarise(
    prom_toallas     = mean(qty_toallas[qty_toallas > 0], na.rm = TRUE),
    prom_protectores = mean(qty_protectores[qty_protectores > 0], na.rm = TRUE),
    prom_tampones    = mean(qty_tampones[qty_tampones > 0], na.rm = TRUE)
  )

cat("\n== Promedio de unidades mensuales usadas (entre quienes usan cada producto) ==\n")
print(promedios_cantidades)

# 3. Medicamentos y prácticas médicas ----
cat("\n== Medicamentos / tratamientos utilizados ==\n")
print(tabla_freq_multi(respuestas$meds_used, MEDS, nrow(respuestas)))

cat("\n== Otros tratamientos o suplementos utilizados ==\n")
print(tabla_freq_multi(respuestas$othersmeds_used, OTHERS_MEDS, nrow(respuestas)))

cat("\n== Prácticas médicas realizadas ==\n")
print(tabla_freq_multi(respuestas$practices_used, PRACTICES, nrow(respuestas)))

# 4. Autocuidado, suplementos y ejercicio (solo responden quienes están en el grupo menopausia) ----
menopausicas <- respuestas %>% filter(!is.na(selfcare_used) & selfcare_used != "")

if (nrow(menopausicas) > 0) {
  cat("\n== Autocuidado iniciado/incrementado en menopausia (n =", nrow(menopausicas), ") ==\n")
  print(tabla_freq_multi(menopausicas$selfcare_used, SELFCARE_PRODUCTS, nrow(menopausicas)))

  cat("\n== Suplementos/alimentos recomendados consumidos ==\n")
  print(tabla_freq_multi(menopausicas$supplements_used, SUPPLEMENTS, nrow(menopausicas)))

  cat("\n== Ejercicio físico semanal ==\n")
  print(tabla_freq(menopausicas, exercise_weekly))
}

# 5. Costo mensual estimado por respuesta (misma lógica que cost_breakdown() en app.R) ----
p_toallas     <- preciosPGM$precio_nacional[preciosPGM$Categoría == "toallitas"]
p_protectores <- preciosPGM$precio_nacional[preciosPGM$Categoría == "protectores diarios"]
p_tampones    <- preciosPGM$precio_nacional[preciosPGM$Categoría == "tampones"]
p_incontinencia <- preciosPGM$precio_nacional[preciosPGM$Categoría == "protectores para incontinencia"]

# Cantidad mensual asumida para "Protectores de incontinencias" (no se le pide
# cantidad a la persona, a diferencia de toallas/protectores diarios/tampones):
# Estudios de salud sugieren que una persona con incontinencia moderada utiliza unos 120 apósitos al mes (un promedio de 4 al día)
QTY_INCONTINENCIA_MES <- 120


respuestas <- respuestas %>%
  rowwise() %>%
  mutate(
    costo_menstrual = sum(
      qty_toallas * p_toallas,
      qty_protectores * p_protectores,
      qty_tampones * p_tampones,
      na.rm = TRUE
    ),
    costo_medicamentos = costo_medicamentos_fila(meds_used, othersmeds_used),
    costo_incontinencia = costo_incontinencia_fila(selfcare_used),
    costo_total_mensual = sum(costo_menstrual, costo_medicamentos, costo_incontinencia, na.rm = TRUE)
  ) %>%
  ungroup()

cat("\n== Costo mensual estimado (canasta) ==\n")
respuestas %>%
  summarise(
    promedio = mean(costo_total_mensual, na.rm = TRUE),
    mediana  = median(costo_total_mensual, na.rm = TRUE),
    minimo   = min(costo_total_mensual, na.rm = TRUE),
    maximo   = max(costo_total_mensual, na.rm = TRUE)
  ) %>%
  print()

cat("\n== Costo mensual estimado por etapa de ciclo vital ==\n")
respuestas %>%
  group_by(life_stage) %>%
  summarise(
    n = n(),
    promedio = mean(costo_total_mensual, na.rm = TRUE),
    mediana  = median(costo_total_mensual, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print()

cat("\n== Costo mensual estimado por rango etario ==\n")
respuestas %>%
  group_by(age_range) %>%
  summarise(
    n = n(),
    promedio = mean(costo_total_mensual, na.rm = TRUE),
    mediana  = median(costo_total_mensual, na.rm = TRUE),
    minimo   = min(costo_total_mensual, na.rm = TRUE),
    maximo   = max(costo_total_mensual, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(age_range) %>%
  print()

# 6. Gráficos ----

# Grilla de referencia (gris tenue) y quiebres de eje x más finos, reutilizadas en los gráficos de costo
tema_grilla_tenue <- theme(
  panel.grid.major = element_line(color = "grey88"),
  panel.grid.minor = element_line(color = "grey94")
)
breaks_costo <- scales::breaks_width(25000)
grafico_provincias <- respuestas %>%
  count(province, sort = TRUE) %>%
  ggplot(aes(x = reorder(province, n), y = n)) +
  geom_col(fill = "violetred") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Respuestas por provincia",
    x = "", y = "Cantidad de respuestas",
    caption = "Fuente: Encuesta #EcoFemiData - calcu.M"
  )

grafico_ciclo_edad <- respuestas %>%
  count(life_stage, age_range) %>%
  complete(life_stage, age_range = AGE_RANGES, fill = list(n = 0)) %>%
  mutate(age_range = factor(age_range, levels = AGE_RANGES)) %>%
  ggplot(aes(x = age_range, y = str_wrap(life_stage, 30), fill = n)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = ifelse(n == 0, "", n), color = n > max(n) * 0.5), size = 3.5, show.legend = FALSE) +
  scale_fill_gradient(low = "#fce4ef", high = "#93120f") +
  scale_color_manual(values = c("TRUE" = "white", "FALSE" = "grey20")) +
  theme_minimal() +
  theme(panel.grid = element_blank(), legend.position = "none") +
  labs(
    title = "Etapa del ciclo vital autopercibida según rango etario",
    subtitle = "Cantidad de respuestas por combinación, más allá del diagnóstico clínico",
    x = "Rango etario", y = "",
    caption = "Fuente: Encuesta #EcoFemiData - calcu.M"
  )

grafico_costo <- respuestas %>%
  ggplot(aes(x = costo_total_mensual)) +
  geom_histogram(bins = 20, fill = "orange", color = "white") +
  scale_x_continuous(breaks = breaks_costo, labels = scales::comma) +
  theme_minimal() +
  tema_grilla_tenue +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Distribución del costo mensual estimado",
    x = "Costo mensual ($)", y = "Frecuencia",
    caption = "Fuente: Encuesta #EcoFemiData - calcu.M"
  )

grafico_costo_edad <- respuestas %>%
  ggplot(aes(x = costo_total_mensual)) +
  geom_histogram(bins = 15, fill = "orange", color = "white") +
  scale_x_continuous(breaks = breaks_costo, labels = scales::comma) +
  facet_wrap(~ age_range, scales = "free_y") +
  theme_minimal() +
  tema_grilla_tenue +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7)) +
  labs(
    title = "Distribución del costo mensual estimado por rango etario",
    x = "Costo mensual ($)", y = "Frecuencia",
    caption = "Fuente: Encuesta #EcoFemiData - calcu.M"
  )

print(grafico_provincias)
print(grafico_ciclo_edad)
print(grafico_costo)
print(grafico_costo_edad)

## Exporto los gráficos a PNG ----
dir.create("analisis_respuestas/graficos_respuestas", showWarnings = FALSE)

ggsave("analisis_respuestas/graficos_respuestas/respuestas_por_provincia.png", grafico_provincias, width = 8, height = 6, dpi = 150)
ggsave("analisis_respuestas/graficos_respuestas/ciclo_vital_por_rango_etario.png", grafico_ciclo_edad, width = 9, height = 6, dpi = 150)
ggsave("analisis_respuestas/graficos_respuestas/distribucion_costo_mensual.png", grafico_costo, width = 8, height = 6, dpi = 150)
ggsave("analisis_respuestas/graficos_respuestas/distribucion_costo_por_edad.png", grafico_costo_edad, width = 10, height = 7, dpi = 150)

# 7. Nube de palabras de gastos adicionales mencionados libremente (other_costs) ----
# Buscamos patrones de consumo que no estén cubiertos por las preguntas cerradas del resto de la encuesta.
stopwords_es <- c(
  "de", "la", "el", "en", "y", "o", "u", "e", "que", "los", "las", "con", "por", "no", "si", "sí", "a",
  "del", "al", "este", "esta", "estos", "estas", "ese", "esa", "esos", "esas", "se", "su", "sus",
  "es", "son", "un", "una", "unos", "unas", "lo", "le", "les", "mi", "mis", "tengo", "tiene", "tienen",
  "muy", "mas", "más", "como", "porque", "para", "desde", "hasta", "sobre", "entre", "también", "tambien",
  "ya", "me", "te", "nos", "fue", "ser", "estar", "hay", "he", "ha", "han", "puedo", "puede", "pueden",
  "soy", "eres", "somos", "sido", "así", "asi", "pero", "sino", "ni", "sin", "solo", "sólo",
  "toda", "todo", "todos", "todas", "otra", "otro", "otros", "otras", "cada", "cual", "cuales",
  "donde", "cuando", "quien", "quienes", "eso", "aquí", "aqui", "allí", "alli", "poco", "mucho", "muchos",
  "muchas", "tan", "tanto", "bien", "mal", "aun", "aún", "incluso", "última", "ultimo", "último", "ultima",
  "etc", "vez", "caso", "cosas", "cosa", "hace", "hacer", "haciendo", "ir", "voy", "mi", "in", "por", "lo",
  "les", "las", "los", "eh", "ah",
  # Muletillas / comentarios sobre la encuesta en sí, sin contenido de consumo
  "aclaro", "comentarles", "puse", "gracias", "saludos", "iniciativa", "deberian", "poner", "opcion",
  "pensar", "suerte", "realidad", "lado", "encuesta", "responder", "todavia", "totalidad", "tenia",
  "viene", "vuelto", "tuve", "intento", "trato", "gastod", "ano", "pro"
)

# Variantes de singular/plural o género que conviene unificar para no subestimar su frecuencia
normalizar_variantes <- c(
  "consultas" = "consulta", "controles" = "control", "cambios" = "cambio", "costosa" = "costoso"
)

respuestas_texto <- respuestas %>%
  filter(!is.na(other_costs), !str_to_lower(str_trim(other_costs)) %in% c("", "no", "si", "sí")) %>%
  pull(other_costs)

cat("\n== Respuestas de texto libre analizadas para la nube de palabras (other_costs) ==\n")
cat(length(respuestas_texto), "de", nrow(respuestas), "respuestas totales tenían texto útil (se descartaron vacías y 'si'/'no')\n")

palabras <- respuestas_texto %>%
  str_to_lower() %>%
  iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>% # saco tildes para agrupar variantes (médico/medico)
  str_remove_all("[[:punct:][:digit:]]") %>%
  str_split("\\s+") %>%
  unlist()

palabras <- palabras[nchar(palabras) > 2 & !palabras %in% stopwords_es]
palabras <- recode(palabras, !!!normalizar_variantes)

frecuencia_palabras <- tibble(palabra = palabras) %>%
  count(palabra, sort = TRUE)

cat("\n== Top 20 palabras más frecuentes en 'other_costs' ==\n")
print(frecuencia_palabras, n = 20)

# Con tan pocas respuestas de texto libre, casi todas las palabras son únicas (aparecen 1 vez); igual las
# incluimos todas (sin filtrar por frecuencia mínima) para que la nube muestre la variedad real de temas
# mencionados, aunque el tamaño de letra ya deja en claro cuáles se repiten más entre las personas encuestadas.
grafico_nube <- frecuencia_palabras %>%
  ggplot(aes(label = palabra, size = n, color = n)) +
  geom_text_wordcloud(area_corr = TRUE, rm_outside = TRUE, seed = 42) +
  scale_size_area(max_size = 26) +
  scale_color_gradient(low = "#fd81cd", high = "#93120f") +
  theme_minimal() +
  labs(
    title = "Gastos adicionales mencionados libremente (other_costs)",
    caption = "Fuente: Encuesta #EcoFemiData - calcu.M"
  )

print(grafico_nube)
ggsave("analisis_respuestas/graficos_respuestas/nube_other_costs.png", grafico_nube, width = 11, height = 9, dpi = 150)

# 8. Costo estimado según cobertura de salud, y señales de barrera de acceso ----
# Un costo más bajo en "No tengo cobertura" puede reflejar un gasto real menor, o puede reflejar que
# esas personas directamente no acceden a prácticas/medicación que si tuvieran cobertura sí realizarían.
# Para distinguir ambos casos, además del costo comparamos qué proporción de cada grupo no reporta
# ninguna práctica médica ni ninguna medicación.
cat("\n== Costo mensual estimado por cobertura de salud ==\n")
respuestas %>%
  group_by(coverage) %>%
  summarise(
    n = n(),
    promedio = mean(costo_total_mensual, na.rm = TRUE),
    mediana  = median(costo_total_mensual, na.rm = TRUE),
    minimo   = min(costo_total_mensual, na.rm = TRUE),
    maximo   = max(costo_total_mensual, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(promedio)) %>%
  print()

tasa_sin_practicas <- respuestas %>%
  mutate(
    sin_practicas = map_lgl(practices_used, ~ "Ninguno/a" %in% detectar_opciones(.x, PRACTICES)),
    sin_medicacion = map2_lgl(meds_used, othersmeds_used, function(m, om) {
      opciones <- c(detectar_opciones(m, MEDS), detectar_opciones(om, OTHERS_MEDS))
      opciones_reales <- opciones[opciones != "Ninguno/a"]
      length(opciones_reales) == 0
    })
  ) %>%
  group_by(coverage) %>%
  summarise(
    n = n(),
    pct_sin_practicas = mean(sin_practicas),
    pct_sin_medicacion = mean(sin_medicacion),
    .groups = "drop"
  )

cat("\n== Proporción sin ninguna práctica médica / sin ninguna medicación, por cobertura ==\n")
tasa_sin_practicas %>%
  mutate(
    pct_sin_practicas = percent(pct_sin_practicas),
    pct_sin_medicacion = percent(pct_sin_medicacion)
  ) %>%
  print()

grafico_costo_cobertura <- respuestas %>%
  ggplot(aes(x = coverage, y = costo_total_mensual, fill = coverage)) +
  geom_boxplot(outlier.alpha = 0.4) +
  scale_y_continuous(labels = scales::comma, breaks = breaks_costo) +
  scale_fill_manual(values = c(
    "Obra social" = "#fd81cd", "Prepaga" = "#93120f", "No tengo cobertura" = "#fed7ce"
  )) +
  theme_minimal() +
  tema_grilla_tenue +
  theme(legend.position = "none") +
  labs(
    title = "Costo mensual estimado según cobertura de salud",
    x = "", y = "Costo mensual ($)",
    caption = "Fuente: Encuesta #EcoFemiData - calcu.M"
  )

grafico_barrera_acceso <- tasa_sin_practicas %>%
  select(coverage, "Sin ninguna práctica médica" = pct_sin_practicas, "Sin ninguna medicación" = pct_sin_medicacion) %>%
  pivot_longer(-coverage, names_to = "indicador", values_to = "valor") %>%
  ggplot(aes(x = coverage, y = valor, fill = indicador)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = c("Sin ninguna práctica médica" = "#93120f", "Sin ninguna medicación" = "#fd81cd")) +
  theme_minimal() +
  tema_grilla_tenue +
  labs(
    title = "Proporción sin prácticas médicas / sin medicación, por cobertura",
    x = "", y = "% de respuestas", fill = "",
    caption = "Fuente: Encuesta #EcoFemiData - calcu.M"
  )

print(grafico_costo_cobertura)
print(grafico_barrera_acceso)
ggsave("analisis_respuestas/graficos_respuestas/costo_por_cobertura.png", grafico_costo_cobertura, width = 8, height = 6, dpi = 150)
ggsave("analisis_respuestas/graficos_respuestas/barrera_acceso_por_cobertura.png", grafico_barrera_acceso, width = 8, height = 6, dpi = 150)

# 9. Sensibilidad geográfica de precios: nacional (usado por la app) vs. provincial ----
# app.R usa un único precio nacional (preciosPGM.csv) para toda Argentina, pero el propio proyecto ya
# calcula precios por provincia en preprocesamiento.R (precios-gestion-menstrual-limpio.RDS). Como el
# 70% de las respuestas viene de CABA+Buenos Aires, chequeamos si ese precio nacional (ponderado por
# población, no por dónde respondió la encuesta) está sub/sobreestimando el costo real fuera del AMBA.
# Nota: la base de precios con detalle provincial solo tiene "toallitas" y "tampones"; "protectores
# diarios" no tiene granularidad provincial disponible, así que ese rubro queda con el precio nacional.
precios_provinciales_raw <- readRDS("preprocesamiento/insumos_prepro/precios-gestion-menstrual-limpio.RDS")

precio_provincia <- precios_provinciales_raw %>%
  filter(Categoría %in% c("toallitas", "tampones")) %>%
  mutate(Provincia = as.character(Provincia)) %>%
  group_by(Categoría, Provincia) %>%
  summarise(precio_prov = mean(precio_unidad, trim = 0.05, na.rm = TRUE), n_obs = n(), .groups = "drop")

precio_nacional_ref <- preciosPGM %>%
  filter(Categoría %in% c("toallitas", "tampones")) %>%
  select(Categoría, precio_nacional)

comparacion_precios <- precio_provincia %>%
  left_join(precio_nacional_ref, by = "Categoría") %>%
  mutate(dif_pct_num = (precio_prov - precio_nacional) / precio_nacional) %>%
  arrange(Categoría, desc(dif_pct_num))

cat("\n== Precio provincial vs. precio nacional usado por la calculadora (toallitas y tampones) ==\n")
comparacion_precios %>%
  mutate(dif_pct = percent(dif_pct_num, accuracy = 0.1)) %>%
  select(Categoría, Provincia, precio_prov, precio_nacional, dif_pct, n_obs) %>%
  print(n = 50)

precio_provincia_wide <- precio_provincia %>%
  select(Categoría, Provincia, precio_prov) %>%
  pivot_wider(names_from = Categoría, values_from = precio_prov, names_prefix = "precio_prov_")

respuestas <- respuestas %>%
  left_join(precio_provincia_wide, by = c("province" = "Provincia")) %>%
  rowwise() %>%
  mutate(
    precio_toallitas_usar = ifelse(is.na(precio_prov_toallitas), p_toallas, precio_prov_toallitas),
    precio_tampones_usar  = ifelse(is.na(precio_prov_tampones), p_tampones, precio_prov_tampones),
    costo_menstrual_provincial = sum(
      qty_toallas * precio_toallitas_usar,
      qty_protectores * p_protectores, # sin precio provincial disponible para este rubro
      qty_tampones * precio_tampones_usar,
      na.rm = TRUE
    ),
    costo_total_mensual_provincial = sum(costo_menstrual_provincial, costo_medicamentos, na.rm = TRUE),
    dif_costo_provincial_vs_nacional = costo_total_mensual_provincial - costo_total_mensual
  ) %>%
  ungroup()

cat("\n== Costo estimado con precio nacional vs. con precio provincial, por zona ==\n")
respuestas %>%
  mutate(zona = ifelse(province %in% c("CABA", "Buenos Aires"), "AMBA", "Resto del país")) %>%
  group_by(zona) %>%
  summarise(
    n = n(),
    costo_prom_nacional = mean(costo_total_mensual, na.rm = TRUE),
    costo_prom_provincial = mean(costo_total_mensual_provincial, na.rm = TRUE),
    diferencia_prom = mean(dif_costo_provincial_vs_nacional, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print()

cat("\n== Detalle por provincia (costo nacional vs. provincial) ==\n")
respuestas %>%
  group_by(province) %>%
  summarise(
    n = n(),
    costo_prom_nacional = mean(costo_total_mensual, na.rm = TRUE),
    costo_prom_provincial = mean(costo_total_mensual_provincial, na.rm = TRUE),
    diferencia_prom = mean(dif_costo_provincial_vs_nacional, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(abs(diferencia_prom))) %>%
  print(n = 30)

grafico_precio_geografico <- comparacion_precios %>%
  ggplot(aes(x = reorder(paste(Provincia, Categoría), dif_pct_num), y = dif_pct_num, fill = Categoría)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  coord_flip() +
  scale_x_discrete(labels = function(x) str_remove(x, " (toallitas|tampones)$")) +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = c("toallitas" = "orange", "tampones" = "violetred")) +
  facet_wrap(~ Categoría, scales = "free_y") +
  theme_minimal() +
  tema_grilla_tenue +
  theme(legend.position = "none") +
  labs(
    title = "Precio provincial vs. precio nacional usado por la calculadora",
    x = "", y = "Diferencia % vs. precio nacional",
    caption = "Fuente: #MenstruAcción / #EcoFemiData - calcu.M"
  )

print(grafico_precio_geografico)
ggsave("analisis_respuestas/graficos_respuestas/precio_geografico_vs_nacional.png", grafico_precio_geografico, width = 10, height = 9, dpi = 150)
