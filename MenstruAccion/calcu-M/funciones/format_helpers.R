# Utilidades genéricas de formateo/coerción usadas en app.R.

safe_num <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x) || x == "") return(0)
  as.numeric(x)
}

money_fmt <- function(x) {
  # Formato simple (sin depender de paquetes)
  format(round(x, 0), big.mark = ".", decimal.mark = ",")
}

# infix helper for NULL coalescing
`%||%` <- function(a, b) if (!is.null(a)) a else b
