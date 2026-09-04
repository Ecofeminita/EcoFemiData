# Verifica que la autenticación con Google Sheets funciona
# Ejecutar este script antes de modificar app.R

library(googlesheets4)

if (!dir.exists(".secrets") && dir.exists("MenstruAccion/calcu-M/.secrets")) {
  setwd("MenstruAccion/calcu-M")
}

source("funciones/google_sheets_auth.R", local = TRUE)

if (file.exists(".secrets/.env")) readRenviron(".secrets/.env")

SHEET_ID <- Sys.getenv("SHEET_ID")
if (identical(SHEET_ID, "")) {
  stop("SHEET_ID no está definida. Configurá .secrets/.env (local) o la variable de entorno en shinyapps.io.")
}

# Autenticación con service account
cat("Autenticando con service account...\n")
gs4_auth(path = resolve_google_credentials())

# Verificar que la conexión funciona leyendo metadata de la hoja
cat("Conectando con Google Sheets...\n")
meta <- gs4_get(SHEET_ID)
cat("Hoja encontrada:", gs4_get(SHEET_ID)$name, "\n")
cat("Hojas disponibles:", paste(sheet_names(meta), collapse = ", "), "\n")

# Leer las primeras filas para confirmar estructura actual
cat("\nPrimeras filas de la primera hoja:\n")
datos <- read_sheet(SHEET_ID)
print(head(datos))

# Probar escritura con una fila de prueba
cat("\nProbando escritura (fila de prueba)...\n")
fila_prueba <- data.frame(
  timestamp            = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  life_stage           = "TEST",
  gender               = "TEST",
  age_range            = "TEST",
  province             = "TEST",
  coverage             = "TEST",
  regular_period       = "TEST",
  menstrual_products   = "TEST",
  qty_toallas          = 0L,
  qty_protectores      = 0L,
  qty_tampones         = 0L,
  meds_used            = "TEST",
  othersmeds_used      = "TEST",
  practices_used       = "TEST",
  selfcare_used        = "TEST",
  supplements_used     = "TEST",
  exercise_weekly      = "TEST",
  other_costs          = "TEST",
  organizacion         = "TEST",
  other_meds_text      = "TEST",
  other_meds_cost      = 0,
  stringsAsFactors     = FALSE
)
sheet_append(SHEET_ID, fila_prueba)
cat("Fila de prueba escrita correctamente.\n")

cat("\nConexion y escritura exitosas.\n")
