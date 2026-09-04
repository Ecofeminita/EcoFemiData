# Persiste una respuesta completa del wizard como una fila nueva en SHEET_ID.
# g es group() ("menstrual" o "menopausia"), costos es el resultado de cost_breakdown().
guardar_respuesta <- function(input, g, costos) {
  collapse <- function(x) paste(x %||% "", collapse = ", ")
  bd <- costos$breakdown
  cost_for_category <- function(category, prefix = FALSE) {
    match_rows <- if (prefix) startsWith(bd$Rubro, category) else bd$Rubro == category
    round(sum(bd$Costo[match_rows]), 0)
  }
  fila <- data.frame(
    timestamp          = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    life_stage         = input$life_stage %||% "",
    gender             = input$gender %||% "",
    age_range          = input$age_range %||% "",
    province           = input$province %||% "",
    coverage           = input$coverage %||% "",
    regular_period     = input$regular_period %||% "",
    menstrual_products = collapse(input$menstrual_products_used),
    qty_toallas        = as.integer(input$qty_toallas %||% 0),
    qty_protectores    = as.integer(input$qty_protectores %||% 0),
    qty_tampones       = as.integer(input$qty_tampones %||% 0),
    meds_used          = collapse(input$meds_used),
    othersmeds_used    = collapse(input$othersmeds_used),
    practices_used     = collapse(input$practices_used),
    selfcare_used      = if (g == "menopausia") collapse(input$selfcare_used) else "",
    supplements_used   = if (g == "menopausia") collapse(input$supplements_used) else "",
    exercise_weekly    = if (g == "menopausia") (input$exercise_weekly %||% "") else "",
    other_costs        = if (g == "menopausia") (input$other_costs %||% "") else "",
    organizacion       = input$organizacion %||% "",
    other_meds_text    = if (g == "menopausia") (input$other_meds_text %||% "") else "",
    other_meds_cost    = if (g == "menopausia") as.numeric(input$other_meds_cost %||% 0) else 0,
    cost_toallas       = cost_for_category("Toallas higiénicas"),
    cost_protectores   = cost_for_category("Protectores diarios"),
    cost_tampones      = cost_for_category("Tampones"),
    cost_copa          = cost_for_category("Copa"),
    cost_meds          = cost_for_category("Medicamentos"),
    cost_selfcare      = cost_for_category("Productos de autocuidado"),
    cost_other_meds    = cost_for_category("Otros tratamientos o medicamentos", prefix = TRUE),
    cost_total_month   = round(safe_num(costos$total_month), 0),
    stringsAsFactors   = FALSE
  )
  tryCatch(
    sheet_append(SHEET_ID, fila),
    error = function(e) warning("No se pudo guardar la respuesta: ", e$message)
  )
}
