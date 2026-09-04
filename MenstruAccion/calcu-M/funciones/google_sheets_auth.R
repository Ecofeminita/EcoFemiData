# Resuelve la ruta al JSON de la service account de Google usada por gs4_auth().
# Usada por app.R, analisis_respuestas.R y test_sheets_connection.R.
resolve_google_credentials <- function() {
  raw <- Sys.getenv("GOOGLE_CREDENTIALS_JSON", unset = ".secrets/service-account.json")
  if (file.exists(raw)) return(raw)
  # En shinyapps.io no se sube .secrets/ (ver .rscignore), así que ahí GOOGLE_CREDENTIALS_JSON
  # se configura como el JSON de la service account codificado en base64.
  decoded <- tryCatch(jsonlite::base64_dec(raw), error = function(e) NULL)
  if (is.null(decoded)) {
    stop(
      "No se pudo resolver GOOGLE_CREDENTIALS_JSON ('", raw, "'): no es un archivo existente ",
      "ni un JSON válido en base64. Revisá .secrets/.env (local) o la variable de entorno en shinyapps.io."
    )
  }
  tmp <- tempfile(fileext = ".json")
  writeBin(decoded, tmp)
  tmp
}
