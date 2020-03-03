# Funciones de formato para la presentación de los números.
formato_porc <- function(numero, dec = 1){
  format(round(numero, digits = dec), nsmall = dec, decimal.mark = ",")
}

formato_pesos <- function(numero, dec = 2){
  paste0("$", format(round(numero, digits = dec), nsmall = dec, big.mark = ".", decimal.mark = ","))
}

formato_cantidad <- function(numero, dec = 0){
  format(round(numero, digits = dec), nsmall = dec, big.mark = ".", decimal.mark = ",")
}