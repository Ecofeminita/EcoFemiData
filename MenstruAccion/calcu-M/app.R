# app.R
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(shinyFeedback))
suppressPackageStartupMessages(library(shinythemes))
suppressPackageStartupMessages(library(shinyWidgets))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(googlesheets4))

# Google Sheets-------------------------
SHEET_ID <- "1VaflzpcENN8XN8kpC-IU8jLdhD_mSAnrcPMt6VTjBvU"
gs4_auth(path = ".secrets/service-account.json")

# Helpers-------------------------
safe_num <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x) || x == "") return(0)
  as.numeric(x)
}

money_fmt <- function(x) {
  # Formato simple (sin depender de paquetes)
  format(round(x, 0), big.mark = ".", decimal.mark = ",")
}

guardar_respuesta <- function(input, g) {
  collapse <- function(x) paste(x %||% "", collapse = ", ")
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
    stringsAsFactors   = FALSE
  )
  tryCatch(
    sheet_append(SHEET_ID, fila),
    error = function(e) warning("No se pudo guardar la respuesta: ", e$message)
  )
}

# Constantes de encuesta-------------------------
PROVINCES <- c(
  "Buenos Aires", "CABA", "Catamarca", "Chaco", "Chubut", "Córdoba", "Corrientes",
  "Entre Ríos", "Formosa", "Jujuy", "La Pampa", "La Rioja", "Mendoza", "Misiones",
  "Neuquén", "Río Negro", "Salta", "San Juan", "San Luis", "Santa Cruz",
  "Santa Fe", "Santiago del Estero", "Tierra del Fuego", "Tucumán"
)

AGE_RANGES <- c(
  "Prefiero no responder", "18-24", "25-34", "35-44", "45-54", "55-64", "65+"
)

LIFE_STAGE <- c(
  "Menstruando y sin signos de climaterio/menopausia",
  "Experimentando signos de climaterio/menopausia",
  "Atravesando una menopausia \"inducida\" (por tratamiento médico u otra situación)",
  "Post menopausia (ya pasó un año o más desde la última menstruación)"
)

GENDER <- c(
  "Mujer cis", "Varón trans", "No binarie", "Varón cis", "Otra identidad de género", "Prefiero no responder"
)

COVERAGE <- c(
  "Obra social",
  "Prepaga",
  "No tengo cobertura"
)

MENSTRUAL_PRODUCTS <- c("Toallas higiénicas", "Protectores diarios", "Tampones", "Copa menstrual", "Ninguno/a")
# Nota: Copa menstrual no tiene cantidad mensual obligatoria, la dejamos como selección en step3

SELFCARE_PRODUCTS <- c(
  "Geles lubricantes o hidratantes vaginales",
  "Crema corporal/facial", 
  "Toallas para incontinencia leve",
  "Consultorio de Ginecología",
  "Consultorio de Psicología",
  "Consultorio de Sexología",
  "Consultorio de Psiquiatría",
  "Consultorio de Nutrición",
  "Terapias alternativas",
  "Hierbas medicinales u otros productos no farmacológicos",
  "Ninguno/a"
  )

MEDS <- c(
  "Estradiol — terapia hormonal con estrógenos (pastillas/comprimidos)",
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

SUPPLEMENTS <- c(
  "Nueces", 
  "Almendras", 
  "Jugo de arándanos", 
  "Vitaminas", 
  "Calcio",
  "Magnesio",
  "Hierro",
  "Ninguno/a"
  )

# Data-------------------------
preciosPGM <- read.csv("./insumos/preciosPGM.csv", encoding='UTF-8')
preciosMED <- read.csv("./insumos/preciosMED.csv", encoding='UTF-8')
textos <- readxl::read_excel("./insumos/textos_app.xlsx")

# UI-------------------------
ui <- fluidPage(
  shinyjs::useShinyjs(),
  shinyFeedback::useShinyFeedback(),
  theme = shinytheme("journal"),
  tags$head(tags$style(HTML("
  
    @import url('https://fonts.googleapis.com/css2?family=Martian+Mono:wght@300&display=swap');
    body, button, input, select, textarea, .btn, .form-control, h1, h2, h3, h4, h5, h6, .nav, .nav-tabs {
      font-family: 'Martian Mono', monospace !important;
      font-weight: 300;
      font-size: 13px;
      color: #232323 !important;
    }
    h1 { font-size: 28px !important; }
    h2 { font-size: 24px !important; }
    h3 { font-size: 20px !important; }
    h4 { font-size: 17px !important; }
    h5 { font-size: 15px !important; }
    
    .big-number { font-size: 34px; font-weight: 700; }
    .muted { color: #232323; }
    .wizard-nav { display:flex; gap:10px; margin-top: 15px; }
    .two-col { display: grid; grid-template-columns: 1fr 1fr; gap: 12px; }
    @media (max-width: 900px) { .two-col { grid-template-columns: 1fr; } }
    a { color: #c61e1b !important; }

    .step-header{
    padding: 14px 16px;
    border-radius: 12px;
    color: #fff;
    font-weight: 700;
    margin-bottom: 12px;
    box-shadow: 0 6px 16px rgba(0,0,0,.08);
    }
    .header-step1{ background: #c61e1b; } 
    .header-stepvaron{ background: #fed7ce; color: #93120f } 
    .header-step6{ background: #93120f; } 
    .question-block{
    border: 1px solid #c61e1b;
    padding: 16px;
    border-radius: 14px;
    margin-bottom: 15px;
    }
    .results-block{
    border: 1px solid #93120f;
    padding: 16px;
    border-radius: 14px;
    margin-bottom: 15px;
    }
    
    /* Wizard Nav específico */
    .wizard-nav .btn{
      border-radius: 999px !important;
      padding: 8px 22px !important;
      font-weight: 800 !important;
      border: none !important;
    }
    
    /* Botón Siguiente */
    .wizard-nav .btn-next{
      background-color: #fd81cd !important;
      color: #232323 !important;
    }
    
    /* Botón Atrás */
    .wizard-nav .btn-back{
      background-color: #fed7ce !important;
      color: #93120f !important;
    }
    
    /* Botón Resultado */
    .wizard-nav .btn-results{
      background-color: #93120f !important;
      color: #fed7ce !important;
    }
    
    /* Botón Restart */
    .wizard-nav .btn-restart{
      background-color: #c61e1b !important;
      color: #f0f0f0 !important;
    }
  "))),
  # chooseSliderSkin("Flat", color = "#e5616e"),
  titlePanel("ECOFEMIDATA - Menopausia y vida menstrual"),
  uiOutput("main_ui")
)

# SERVER-------------------------
server <- function(input, output, session) {
  
  # Wizard step (1..6, y un "final_out" especial)
  step <- reactiveVal(0)
  
  # Grupo: menstrual / menopausia / out
  group <- reactiveVal(NULL)
  
  is_empty <- function(x) {
    is.null(x) || length(x) == 0 || all(is.na(x)) || identical(x, "")
  }
  
  notify_required <- function(msg = "Para continuar, respondé todas las preguntas de esta pantalla.") {
    showNotification(msg, type = "error", duration = 5)
  }
  
  # Si "Ninguno/a" está seleccionado junto con otras opciones
  modular_ninguno_logic <- function(input_id, input_val) {
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
  
  # UI dinámica por step-------------------------
  output$main_ui <- renderUI({
    s <- step()
    g <- group()
    
    # STEP 0 (presentación del proyecto)-------------------------
    if (s == 0) {
      return(fluidRow(
        column(10,
               div(class="card",
                   div(class="step-header header-step1",
                       div("Presentación")
                   ),
                   div(class="question-block", style="text-align: justify;",
                       tags$div(
                         style = "text-align:center; margin-bottom:20px;",
                         tags$img(
                           src = "Logos_calcu.m-02.png",
                           width = "320px",
                           style = "max-width:100%;"
                         )
                       ),
                       HTML(textos$texto[textos$id=="explicacion_proyecto"]),
                       br(),br(),
                       HTML(textos$texto[textos$id=="disclaimer_estadistico"]),
                       br(),br(),
                       tags$div(
                         style = "text-align:center; margin-bottom:20px;",
                         tags$img(
                           src = "Firmas-02.png",
                           width = "90px",
                           style = "max-width:10%;"
                         ),
                         tags$img(
                           src = "Firmas-03.png",
                           width = "90px",
                           style = "max-width:10%;"
                         )
                       )
                   ),
                   div(class="wizard-nav",
                       actionButton("next_common", "Iniciar encuesta", class = "btn-next")
                   )
               )
        ))
      )
    }
    
    
    # STEP 1 (Inicio encuesta)-------------------------
    if (s == 1) {
      return(fluidRow(
        column(10,
               div(class="card",
                   div(class="step-header header-step1",
                       div("1 — Consentimiento y ciclo vital")
                   ),
                   div(class="question-block",
                       radioButtons("life_stage", "¿En qué momento del ciclo vital te ubicás?", choices = LIFE_STAGE, selected = character(0), width = '100%'),
                       hr(),
                       radioButtons("gender", "¿Con qué género te autopercibís?", choices = GENDER, selected = character(0), width = '100%')
                   ),
               ),
               div(class="question-block",
                   checkboxInput("consent", tags$b("Acepto participar y que mis respuestas se usen con fines de investigación."), value = FALSE, width = '100%'),
               ),
               div(class="wizard-nav",
                   actionButton("next1", "Siguiente", class = "btn-next")
               ),
               br(),
               p(class="muted", "Nota: si no aceptás el consentimiento, no podés continuar.")
        )
      ))
    }
    
    # Pantalla final fuera de flujo (VARON O PREFIRO NO RESPONDER)-------------------------
    if (!is.null(g) && g == "out") {
      return(fluidRow(
        column(8,
               div(class="card",
                   div(class="step-header header-stepvaron",
                       div("Muchas gracias por participar")
                   ),
                   div(class="results-block",
                       p("Esta encuesta busca medir costos asociados a la menstruación, la menopausia y el climaterio. Por el momento no se admiten respuestas de varones cis."),
                       br(), br(),
                       tags$div(
                         style = "text-align:center; margin-bottom:20px;",
                         tags$img(
                           src = "Iso+logo_calcu.m-01.png",
                           width = "320px",
                           style = "max-width:100%;"
                         )
                       )
                   )
               )
        )
      ))
    }
    
    # Pantalla final fuera de flujo (OTRAS IDENTIDADES DE GÉNERO)-------------------------
    if (!is.null(g) && g == "out_text") {
      others_identity_ui <- tagList(
        div(class="two-col",
            textInput("others_identity_text", "", value = "", width = '100%')
        )
      )
      
      return(fluidRow(
        column(8,
               div(class="card",
                   div(class="step-header header-stepvaron",
                       div("Contanos tu experiencia")
                   ),
                   div(class="results-block",
                       HTML("<b>Nos gustaría saber como transitas tu ciclo vital menstru-menopausia, abajo dejamos un espacio para que puedas escribir libremente.</b>"),
                       others_identity_ui,
                       br(), br(),
                       tags$div(
                         style = "text-align:center; margin-bottom:20px;",
                         tags$img(
                           src = "Iso+logo_calcu.m-01.png",
                           width = "320px",
                           style = "max-width:100%;"
                         )
                       )
                   ),
               ),
               div(class="wizard-nav",
                   actionButton("send_text", "Enviar", class = "btn-restart")
               ),
        )
      ))
    }
    
    
    # Si aún no hay grupo definido y no estamos en step1, fallback
    if (is.null(g) && s != 1) {
      return(fluidRow(
        column(8,
               div(class="card",
                   div(class="step-header header-step1",
                       div("Estado inválido")
                   ),
                   p("Volvé al inicio para continuar."),
                   div(class="wizard-nav",
                       actionButton("restart", "Reiniciar encuesta", class = "btn-restart")
                   )
               )
        )
      ))
    }
    
    # STEP 2 (común a ambos grupos)-------------------------
    if (s == 2) {
      return(fluidRow(
        column(10,
               div(class="card",
                   div(class="step-header header-step1",
                       div("2 — Perfil sociodemográfico")
                   ),
                   div(class="question-block",
                       selectInput("age_range", "Rango etario", choices = AGE_RANGES, selected = "Prefiero no responder"),
                       hr(),
                       selectInput("province", "¿En qué provincia vivís?", choices = PROVINCES, selected = "CABA"),
                       hr(),
                       radioButtons("coverage", "¿Qué cobertura de salud tenés?", choices = COVERAGE, selected = character(0))
                   )
               ),
               div(class="wizard-nav",
                   actionButton("back", "Atrás", class = "btn-back"),
                   actionButton("next_common", "Siguiente", class = "btn-next")
               )
        )
      ))
    }
    
    # STEP 3 (común a ambos grupos)-------------------------
    if (s == 3) {
      # Inputs de cantidades por categoría: default 0
      qty_ui <- tagList(
        div(class="two-col",
            disabled(numericInput("qty_toallas", "Toallas higiénicas", value = 0, min = 0, step = 1)),
            disabled(numericInput("qty_protectores", "Protectores diarios", value = 0, min = 0, step = 1))
        ),
        div(class="two-col",
            disabled(numericInput("qty_tampones", "Tampones", value = 0, min = 0, step = 1)),
            div() # placeholder
        )
      )
      
      return(fluidRow(
        column(10,
               div(class="card",
                   div(class="step-header header-step1",
                       div("3 — Costo directo")
                   ),
                   div(class="question-block",
                       radioButtons("regular_period", "¿Tenés un período regular?", choices = c("Si", "No", "No menstrúo"), selected = character(0)),
                       hr(),
                       checkboxGroupInput(
                         "menstrual_products_used",
                         "En cualquier caso, ¿Usás habitualmente alguno de estos productos de higiene íntima/personal? (puede ser más de uno)",
                         choices = MENSTRUAL_PRODUCTS,
                         selected = character(0),
                         width = "100%"
                       ),
                       hr(),
                       h4("¿Cuántas unidades utilizas aproximadamente en el lapso de un mes?"),
                       p(class="muted", "Dejá 0 si no usás ese producto o no aplica."),
                       qty_ui
                   )
               ),
               div(class="wizard-nav",
                   actionButton("back", "Atrás", class = "btn-back"),
                   actionButton("next_common", "Siguiente", class = "btn-next")
               )
        )
      ))
    }
    
    
    # STEP 4 (común a ambos grupos)-------------------------
    if (s == 4) {
      return(fluidRow(
        column(10,
               div(class="card",
                   div(class="step-header header-step1",
                       div("4 — Costo directo")
                   ),
                   div(class="question-block",
                       HTML("<b>¿Usás habitualmente alguno de estos medicamentos?</b>"),
                       br(),br(),
                       checkboxGroupInput(
                         "meds_used",
                         "Terapias hormonales",
                         choices = MEDS,
                         selected = character(0),
                         width = '100%'
                       ),
                       checkboxGroupInput(
                         "othersmeds_used",
                         "Otros tratamientos o suplementos",
                         choices = OTHERS_MEDS,
                         selected = character(0),
                         width = '100%'
                       ),
                       hr(),
                       checkboxGroupInput(
                         "practices_used",
                         "¿Recurrís a alguna de estas consultas y/o prácticas médicas?",
                         choices = PRACTICES,
                         selected = character(0),
                         width = '100%'
                       )
                   )
               ),
               div(class="wizard-nav",
                   actionButton("back", "Atrás", class = "btn-back"),
                   actionButton("next_common", "Siguiente", class = "btn-next")
               )
        )
      ))
    }
    
    # A partir de acá, solo continúa el grupo menopausia
    if (g == "menstrual") {
      step(6)
    }
    
    # STEP 5 (menopausia)-------------------------
    if (s == 5) {
      
      others_ui <- tagList(
        div(class="two-col",
            textInput("other_costs", "", value = "", width = '100%')
        )
      )
      
      
      return(fluidRow(
        column(10,
               div(class="card",
                   div(class="step-header header-step1",
                       div("5 — Otros gastos")
                   ),
                   div(class="question-block",
                       checkboxGroupInput(
                         "selfcare_used",
                         "¿Inició o incrementó el uso de alguno de los siguientes productos o prácticas de autocuidado a partir de la aparición de síntomas asociados a la menopausia?",
                         choices = SELFCARE_PRODUCTS,
                         selected = character(0),
                         width = '100%'
                       ),
                       hr(),
                       checkboxGroupInput(
                         "supplements_used",
                         "¿Consumís habitualmente alguno de estos alimentos y/o suplementos por recomendación para la menopausia?",
                         choices = SUPPLEMENTS,
                         selected = character(0),
                         width = '100%'
                       ),
                       hr(),
                       radioButtons("exercise_weekly", "¿Realizás ejercicio físico semanal?", choices = c("Si", "No"), selected = character(0)),
                       hr(),
                       HTML("<b>¿Agregarías otros gastos que hayas empezado a realizar como recomendación para la menopausa y que no fueron mencionados en la encuesta? (por ejemplo, vestimenta, ropa de cama, productos de higiene, ventilación/climatización, transporte a consultas médicas, entre otras)</b>"),
                       others_ui
                   )
               ),
               div(class="wizard-nav",
                   actionButton("back", "Atrás", class = "btn-back"),
                   actionButton("to_results", "Ver resultado", class = "btn-results")
               )
        )
      ))
    }
    
    # STEP 6 (resultado)-------------------------
    if (s == 6) {
      return(fluidRow(
        column(10,
               div(class="card",
                   div(class="step-header header-step6",
                       div("6 — Resultado: Costo estimado")
                   ),
                   div(class="results-block",
                       uiOutput("results_ui")
                   ),
                   
                   div(class="wizard-nav",
                       actionButton("restart", "Reiniciar encuesta", class = "btn-restart")
                   )
               )
        )
      ))
    }
    
    # Fallback
    fluidRow(
      column(10,
             div(class="card",
                 div(class="step-header header-step1",
                     div("Pantalla no encontrada")
                 ),
                 actionButton("restart", "Reiniciar encuesta", class = "btn-restart")
             )
      )
    )
  })
  
  # Para conditionalPanel (server -> UI): si es menopausia
  output$isMenopausia <- reactive({
    identical(group(), "menopausia")
  })
  outputOptions(output, "isMenopausia", suspendWhenHidden = FALSE)
  
  # Navegación-------------------------
  observeEvent(input$restart, {
    group(NULL)
    step(0)
    session$reload()
  })
  
  observeEvent(input$back, {
    s <- step()
    if (s > 1) step(s - 1)
  })
  
  # Step1 -> ruteo según criterio
  observeEvent(input$next1, {
    if (!isTRUE(input$consent)) {
      notify_required("Para continuar, necesitás aceptar el consentimiento.")
      return()
    }
    
    if (is_empty(input$life_stage) || is_empty(input$gender)) {
      notify_required()
      return()
    }
    
    if (input$gender %in% c("Varón cis")) {
      group("out")
      return(step(999))
    }
    
    if (input$gender %in% c("Otra identidad de género", "Prefiero no responder")) {
      group("out_text")
      return(step(99))
    }
    
    
    if (input$life_stage == "Menstruando y sin signos de climaterio/menopausia") {
      group("menstrual")
      return(step(2))
    }
    
    group("menopausia")
    step(2)
  })
  
  
  observeEvent(input$next_common, {
    s <- step()
    
    # Presentación del proyecto
    if (s == 0) {
      step(1)
    }
    
    # Inicio encuesta
    if (s == 2) {
      if (
        is_empty(input$age_range) ||
        is_empty(input$province) ||
        is_empty(input$coverage)
      ) {
        notify_required()
        return()
      }
      
      step(3)
    }
    
    # Costos directos 1 (ambos grupos)
    if (s == 3) {
      if (
        is_empty(input$regular_period) ||
        is_empty(input$menstrual_products_used) ||
        is_empty(input$qty_toallas) ||
        is_empty(input$qty_protectores) ||
        is_empty(input$qty_tampones)
      ) {
        notify_required()
        return()
      }
      
      if (
        input$qty_toallas < 0 ||
        input$qty_protectores < 0 ||
        input$qty_tampones < 0
      ) {
        notify_required("Las cantidades no pueden ser negativas.")
        return()
      }
      
      error_detectado <- FALSE
      productos <- input$menstrual_products_used
      
      # Toallitas
      if ("Toallas higiénicas" %in% productos && input$qty_toallas == 0) {
        showFeedbackDanger(inputId = "qty_toallas", text = "Debe ser mayor a 0.")
        error_detectado <- TRUE
      } else {
        hideFeedback("qty_toallas")
      }
      
      # Protectores
      if ("Protectores diarios" %in% productos && input$qty_protectores == 0) {
        showFeedbackDanger(inputId = "qty_protectores", text = "Debe ser mayor a 0.")
        error_detectado <- TRUE
      } else {
        hideFeedback("qty_protectores")
      }
      
      # Tampones
      if ("Tampones" %in% productos && input$qty_tampones == 0) {
        showFeedbackDanger(inputId = "qty_tampones", text = "Debe ser mayor a 0.")
        error_detectado <- TRUE
      } else {
        hideFeedback("qty_tampones")
      }
      
      # Si se detectó algún error de cantidad igual a 0, frenamos el avance
      if (error_detectado) {
        return()
      }
      
      step(4)
    }
    
    # Costos directos 2 (ambos grupos)
    if (s == 4) {
      if (
        is_empty(input$meds_used) ||
        is_empty(input$othersmeds_used) ||
        is_empty(input$practices_used)
      ) {
        notify_required()
        return()
      }
      
      if (identical(group(), "menopausia")) {
        step(5)
      } else {
        guardar_respuesta(input, group())
        step(6)
      }
    }
  })
  
  
  observeEvent(input$to_results, {
    if (
      is_empty(input$selfcare_used) ||
      is_empty(input$supplements_used) ||
      is_empty(input$exercise_weekly)
    ) {
      notify_required()
      return()
    }

    guardar_respuesta(input, group())
    step(6)
  })
  
  # Cantidades de PGM------------------------- 
  observe({
    productos <- input$menstrual_products_used
    
    # Control de Toallitas
    uso_toallas <- "Toallas higiénicas" %in% productos
    toggleState("qty_toallas", condition = uso_toallas)
    
    if (!uso_toallas) {
      hideFeedback("qty_toallas")
      updateNumericInput(session, "qty_toallas", value = 0) # Vuelve a 0 al destildar
    } else if (!is.na(input$qty_toallas) && input$qty_toallas > 0) {
      hideFeedback("qty_toallas")
    }
    
    # Control de Protectores
    uso_protectores <- "Protectores diarios" %in% productos
    toggleState("qty_protectores", condition = uso_protectores)
    
    if (!uso_protectores) {
      hideFeedback("qty_protectores")
      updateNumericInput(session, "qty_protectores", value = 0) # Vuelve a 0 al destildar
    } else if (!is.na(input$qty_protectores) && input$qty_protectores > 0) {
      hideFeedback("qty_protectores")
    }
    
    # 3. Control de Tampones
    uso_tampones <- "Tampones" %in% productos
    toggleState("qty_tampones", condition = uso_tampones)
    
    if (!uso_tampones) {
      hideFeedback("qty_tampones")
      updateNumericInput(session, "qty_tampones", value = 0) # Vuelve a 0 al destildar
    } else if (!is.na(input$qty_tampones) && input$qty_tampones > 0) {
      hideFeedback("qty_tampones")
    }
  })  
  
  # Condición para los Checkboxs de steps 3, 4 y 5-------------------------
  observeEvent(input$menstrual_products_used, {        
    modular_ninguno_logic("menstrual_products_used", input$menstrual_products_used) 
  }, ignoreNULL = TRUE)
  
  observeEvent(input$meds_used, {        
    modular_ninguno_logic("meds_used", input$meds_used) 
  }, ignoreNULL = TRUE)
  
  observeEvent(input$othersmeds_used, {  
    modular_ninguno_logic("othersmeds_used", input$othersmeds_used) 
  }, ignoreNULL = TRUE)
  
  observeEvent(input$practices_used, {   
    modular_ninguno_logic("practices_used", input$practices_used) 
  }, ignoreNULL = TRUE)
  
  observeEvent(input$selfcare_used, {    
    modular_ninguno_logic("selfcare_used", input$selfcare_used) 
  }, ignoreNULL = TRUE)
  
  observeEvent(input$supplements_used, { 
    modular_ninguno_logic("supplements_used", input$supplements_used) 
  }, ignoreNULL = TRUE)
  
  
  # Cálculo de costos (resultado)-------------------------
  cost_breakdown <- reactive({
    req(!is.null(group()))
    g <- group()
    
    # Cantidades mensuales (step3)
    qty_toallas <- safe_num(input$qty_toallas)
    qty_protectores <- safe_num(input$qty_protectores)
    qty_tampones <- safe_num(input$qty_tampones)
    
    # Precios unitarios (step6)
    p_toallas <- safe_num(preciosPGM$precio_nacional[preciosPGM$Categoría=="toallitas"])
    p_protectores <- safe_num(preciosPGM$precio_nacional[preciosPGM$Categoría=="protectores diarios"])
    p_tampones <- safe_num(preciosPGM$precio_nacional[preciosPGM$Categoría=="tampones"])
    # copa_month <- safe_num(preciosPGM$precio_nacional[preciosPGM$Categoría=="copa"])
    
    menstrual_cost <- data.frame(
      Rubro = c("Toallas higiénicas", "Protectores diarios", "Tampones"),
      Costo = c(qty_toallas * p_toallas,
                  qty_protectores * p_protectores,
                  qty_tampones * p_tampones
                  # copa_month
      )
    )
    
    # Costos mensuales estimados para medicamentos
    meds_used <- input$meds_used %>% stringr::str_to_lower()  
    meds_used <- stringr::str_extract(meds_used, "^\\w{5,}((?=\\s)|$)")
    
    othersmeds_used <- input$othersmeds_used %>% stringr::str_to_lower()  
    othersmeds_used <- stringr::str_extract(othersmeds_used, "^\\w{5,}((?=\\s)|$)")
    
    meds_used <- c(meds_used, othersmeds_used)
    
    cost_meds_month <- preciosMED %>% filter(droga_costeo %in% meds_used)
    cost_meds_month <- sum(cost_meds_month$costo_mensual_promedio)
    
    meds_month <- safe_num(cost_meds_month)
    
    other_costs <- data.frame(
      Rubro = c("Medicamentos"),
      Costo = c(meds_month)
    )
    
    bd <- rbind(menstrual_cost, other_costs)
    bd$Costo[is.na(bd$Costo)] <- 0
    total <- sum(bd$Costo)
    list(breakdown = bd, total_month = total, total_year = total * 12)
  })
  
  output$results_ui <- renderUI({
    req(group())
    g <- group()
    
    # Texto para nota metodologica (editar desde el excel)
    nota_metodologica <- textos$texto[textos$id=="nota_metodologica"]
    
    # Info contextual para mostrar en resultado
    stage <- input$life_stage %||% ""
    # gender <- input$gender %||% ""
    # prov <- input$province %||% ""
    # cov <- input$coverage %||% ""
    # age <- input$age_range %||% ""
    
    cov <- ifelse(input$coverage %in% c("Obra social", "Prepaga"), "", "no")
    
    # Otras practicas que también implican costos
    practices_used <- input$practices_used %||% "" 
    
    
    if(g != "menstrual"){
      selfcare_used <- input$selfcare_used %||% ""
      supplements_used <- input$supplements_used %||% ""
      exercise_weekly <- input$exercise_weekly %||% ""
      exercise_weekly <- ifelse(exercise_weekly=="Si", "Ejercicio físico", "")
      
      other_costs <- trimws(strsplit(input$other_costs, ",")[[1]]) %||% ""
      other_costs <- tools::toTitleCase(other_costs)
      
      other_costs <- c(practices_used, selfcare_used, supplements_used, exercise_weekly, other_costs)
      other_costs <- paste(other_costs[!other_costs%in%c("", "Ninguno/a")], collapse = ", ")
      
    }else{
      other_costs <- c(practices_used)
      other_costs <- paste(other_costs[!other_costs%in%c("", "Ninguno/a")], collapse = ", ")
    }
    
    # Si no existieran "otros costos"
    if(other_costs!=""){
      text_other_costs <- paste0(" tales como: ", other_costs)
    }else{text_other_costs <- ""}
    
    r <- cost_breakdown()
    bd <- r$breakdown
    total_m <- r$total_month
    
    tagList(
      div(class="card",
          h4("Resumen de perfil"),
          tags$p("En el ciclo vital menstru-menopausia te encuentras en la etapa: ", tags$b(stage), "."),
          br(),
          tags$p("Tu canasta actual estimada* es de ", tags$b("$", money_fmt(total_m)), 
                 " por mes asociada a esta etapa y ", cov, " tenes cobertura de salud.",
                 " Además, ", tags$b("este gasto puede verse incrementado por el uso de prácticas médicas, suplementos u otras
                  actividades", text_other_costs), ". Este monto no impacta de igual manera en todas las personas, el tipo de cobertura de salud puede modificar significativamente el gasto real asumido."),
          tags$em(tags$small("*Suponiendo que todos los estudios y prácticas suceden en el mismo mes."))
      ),
      hr(),
      h4("Desglose mensual"),
      tags$div(style = " display: flex; align-items: center; width: 100%; margin-top: 10px; margin-bottom: 10px; ",
        tags$div(style = " flex: 0 0 auto; ",
          tableOutput("breakdown_table")
        ),
        tags$div(style = " flex: 1; display: flex; justify-content: center; ",
          tags$img(
            src = "Iso+logo_calcu.m-04.png",
            style = " width: 160px; height: auto; "
          )
        )
      ),
      hr(),
      h5("Nota metodologica"),
      tags$div(id = "intro_metod", 
               style="text-align: justify;
                      font-size: 11px;
                      color: #666;
                      line-height: 1.3;",
               HTML(nota_metodologica)
      )
    )
  })
  
  output$breakdown_table <- renderTable({
    r <- cost_breakdown()
    bd <- r$breakdown
    bd$Costo <- as.numeric(bd$Costo)
    bd$Costo <- paste0("$ ", money_fmt(bd$Costo))
    bd
  })
  
  # Botón recalcular (no hace falta, pero lo dejamos por UX)
  observeEvent(input$recalc, {
    # no-op: reactive recalcula solo, pero el botón da sensación de control
    invisible(NULL)
  })
}

# infix helper for NULL coalescing
`%||%` <- function(a, b) if (!is.null(a)) a else b

shinyApp(ui, server)