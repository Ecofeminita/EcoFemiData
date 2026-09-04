# Helpers de costeo por fila usados en analisis_respuestas.R (misma lógica que
# cost_breakdown() en app.R). Dependen de detectar_opciones() (analisis_helpers.R)
# y de objetos ya cargados en el script al momento de llamarlos: MEDS, OTHERS_MEDS,
# SELFCARE_PRODUCTS, preciosMED, p_incontinencia, QTY_INCONTINENCIA_MES.

# Replica la extracción de app.R: de cada opción completa de medicamento se toma la primera
# palabra de 5+ letras (ej. "estradiol — terapia..." -> "estradiol") para matchear con preciosMED.
# Excepción: "Estradiol (gel o crema) — ..." matchea igual que "Estradiol (pastillas/...) — ..."
# en esa extracción (misma primera palabra), así que se distingue por separado como hace
# cost_breakdown() en app.R.
costo_medicamentos_fila <- function(meds, othersmeds) {
  opciones <- c(
    detectar_opciones(meds, MEDS),
    detectar_opciones(othersmeds, OTHERS_MEDS)
  )

  opciones <- str_to_lower(opciones)

  drogas <- case_when(
    str_detect(opciones, "^estradiol") & str_detect(opciones, "gel|crema") ~ "estradiol_gel",
    TRUE ~ str_extract(opciones, "^\\w{5,}((?=\\s)|$)")
  )

  preciosMED %>%
    filter(droga_costeo %in% drogas) %>%
    summarise(costo = sum(costo_mensual_promedio)) %>%
    pull(costo)
}

# Costo mensual estimado para "Protectores de incontinencias" (cantidad fija asumida,
# ver QTY_INCONTINENCIA_MES). Devuelve 0 mientras preciosPGM.csv no tenga una fila con
# Categoría == "protectores para incontinencia".
costo_incontinencia_fila <- function(selfcare) {
  opciones <- detectar_opciones(selfcare, SELFCARE_PRODUCTS)

  if ("Protectores de incontinencias" %in% opciones) {
    sum(QTY_INCONTINENCIA_MES * p_incontinencia, na.rm = TRUE)
  } else {
    0
  }
}
