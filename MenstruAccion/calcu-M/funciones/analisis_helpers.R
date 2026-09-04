# Helpers de tabulación usados en analisis_respuestas.R.

# Dado un campo de checkboxes guardado como texto y el vocabulario de opciones posibles,
# devuelve cuáles de esas opciones están efectivamente presentes en el texto.
detectar_opciones <- function(texto, vocabulario) {
  texto <- ifelse(is.na(texto), "", texto)
  vocabulario[map_lgl(vocabulario, ~ str_detect(texto, fixed(.x)))]
}

# Tabla de frecuencias (n y %) para una variable de respuesta única
tabla_freq <- function(df, var) {
  df %>%
    count({{ var }}, sort = TRUE) %>%
    mutate(porcentaje = percent(n / sum(n)))
}

# Tabla de frecuencias para un campo de checkboxes, usando el vocabulario de opciones reales
tabla_freq_multi <- function(campo, vocabulario, base_n, excluir_ninguno = TRUE) {
  tabla <- campo %>%
    map(detectar_opciones, vocabulario = vocabulario) %>%
    unlist() %>%
    tibble(opcion = .) %>%
    count(opcion, sort = TRUE)

  if (excluir_ninguno) tabla <- tabla %>% filter(opcion != "Ninguno/a")

  tabla %>% mutate(porcentaje = percent(n / base_n))
}
