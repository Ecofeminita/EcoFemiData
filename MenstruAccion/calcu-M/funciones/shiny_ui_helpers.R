# Helpers de validación/UI del wizard, usados desde server() en app.R.

is_empty <- function(x) {
  is.null(x) || length(x) == 0 || all(is.na(x)) || identical(x, "")
}

notify_required <- function(msg = "Para continuar, respondé todas las preguntas de esta pantalla.") {
  showNotification(msg, type = "error", duration = 5)
}

# Si "Ninguno/a" está seleccionado junto con otras opciones, se queda solo con
# la última opción tocada por la persona (session viene de server(input, output, session)).
modular_ninguno_logic <- function(input_id, input_val, session) {
  opcion_ninguno <- "Ninguno/a"

  if (opcion_ninguno %in% input_val && length(input_val) > 1) {
    ultimo_seleccionado <- tail(input_val, 1)

    if (ultimo_seleccionado == opcion_ninguno) {
      nuevos_seleccionados <- opcion_ninguno
    } else {
      nuevos_seleccionados <- input_val[input_val != opcion_ninguno]
    }

    updateCheckboxGroupInput(
      session,
      inputId = input_id,
      selected = nuevos_seleccionados
    )
  }
}
