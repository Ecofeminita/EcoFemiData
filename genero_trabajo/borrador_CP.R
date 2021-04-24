library(shiny)
library(eph)
library(dplyr)
library(tidyverse)
library(httr)
library(stringr)
library(readxl)
library(foreign)
library(spatstat)
library(tidyverse)
library(httr)
library(stringr)
library(xlsx)
library(haven)
library(ggthemes)
library(scales)
library(knitr)
library(ggalt)
library(kableExtra)
library(formattable)
library(zoo)
library(directlabels)
library(shinythemes)
library(ggvis)

#Subo los resultados y colores
resultados <- readRDS("Resultados/tablas_resultados.RDS") 
resultados$tablas_16_19[["horas_semanales_df"]] <- resultados$tablas_16_19[["horas_semanales_df"]] %>% gather(., key = indicador, value = valor, 4:5)


colores = c("#e5616e", "#8cddd3")

#Indicadores que vamos incorporando:

indicadores<- c("Actividad y empleo","Desocupación y subocupación", "Tasas de no registro",
                "Puestos en dirección y jefaturas","Cuentapropistas", "Asalariades", "Calificación profesional y técnica",                    "Calificación operativa y no calificados", "Nivel educativo: primaria y sin instrucción", 
                "Nivel educativo: secundaria y superior", "Extensión de la jornada laboral remunerada", 
                "Distribución de las tareas domésticas")

brechas <- c("Ingreso total individual", "Ingreso de la Ocupación Principal", "Ingreso de la Ocupación Principal - asalariados no registrados", "Ingreso de la Ocupación Principal - por Nivel educativo", "Ingreso de la Ocupación Principal - por calificación", "Ingreso horario de la Ocupación Principal", "Ingreso horario de la Ocupación Principal - por Nivel educativo", "Ingreso horario de la Ocupación Principal - por calificación")


deciles <- c("Ingreso total individual", "Ingreso per cápita familiar")

#Funciones
#separo la función que devuelve las tasas en serie y gráficos en una de serie y otra de gráficos.

#series:
series <- function(df="tablas_16_19[[df]]", 
                   filtro = FALSE,
                   variable = "indicador", 
                   valores_filter = c("Tasa Actividad", "Tasa Empleo"),
                   input1=input$aniosID[1],
                   input2=input$aniosID[2],
                   renombrar=FALSE,
                   cols="Proporción de no registrados"
                   ){
  
  datagraf<-resultados$tablas_16_19[[df]] %>% # Daraframe para 2016-19
                mutate(dummy = case_when(ANO4 %in% c(2004:2006) ~ "2004-2006",  # Identifico periodos
                                         TRUE ~ "2016-2019"),
                grp = paste0(Sexo, dummy),                                           # Grupos por Sexo y Período (4 grupos)
                # periodo = as.yearqtr(paste0(ANO4,".",TRIMESTRE), format="%Y.%q")), # Para trabajar con formato fecha 
                periodo = factor(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE)))%>%
                arrange(ANO4)
  
  datagraf <- (if (filtro==TRUE) {# Por si tengo que filtrar la base antes
    datagraf %>% 
      filter(eval(parse(text=variable)) %in% valores_filter)
  }
  else if (filtro == FALSE){
    datagraf
  })
  
  datagraf <- (if (renombrar==TRUE){
    datagraf <- datagraf %>% 
      rename("valor"=cols) %>% 
      filter(!is.na(valor))
  }
  else if (renombrar == FALSE){
    datagraf
  })

  df.filt <- reactive({
    datagraf %>% filter(ANO4>= input1 &
                    ANO4 <= input2)
  })
  
  
  return(df.filt)
}

#gráficos:
graficos <- function(df=df.filt, 
                    eje_x = "Período",
                    eje_y = "",
                    titulo = "Titulo", 
                    subtitulo = "Subtitulo",
                    doble_facet = FALSE,
                    faceta = "indicador",
                    porcentaje = TRUE){
  grafico <- ggplot(data=df, aes(periodo, valor, color = Sexo, fill = Sexo, group = grp)) +
    geom_col(position = "dodge")+
    theme_tufte(base_family = "Arial") +
    theme(axis.text.x = element_text(angle = 35, vjust = 0.7),
          legend.position = "bottom",
          panel.background = element_rect(fill = "gray99", color = "gray90"),
          #plot.background = element_rect(fill="gray99", color = NA),
          strip.text.y = element_text(angle = 0),
          panel.grid.minor.y = element_blank()) +
    scale_color_manual(values = colores) +
    labs(title = titulo,
         subtitle = subtitulo,
         x = eje_x,
         y = eje_y,
       caption = "Fuente: Elaboración propia en base a EPH-INDEC")
  #scale_x_yearqtr(format = "%yQ%q", n = 19)               # Para trabajar con formato fecha
  
  if(porcentaje){
    grafico <- grafico + 
      scale_y_continuous(labels = function(x) paste0(x, "%"))    # Para que se peque el valor y el signo de %
  }
  
  if (doble_facet) {
    grafico <- grafico +
      facet_grid(eval(parse(text=faceta)) ~ dummy, scales = "free_x", space = "free_x")
  }else{
    grafico <- grafico +
      facet_grid(.~ dummy, scales = "free_x", space = "free_x")
  }
  
  
  return(grafico)
  
}


#función que ordena períodos toda comentada porque todavía no se usó
ordeno_periodo <- function(df){
    df %>% 
    mutate(dummy = case_when(ANO4 %in% c(2004:2006) ~ "2004-2006",
                             TRUE ~ "2016-2018"),
           periodo = factor(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE),
                            levels = unique(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE))))
}


#comparaciones


comparaciones = function(df, anio, trimestre, filtro, variable_f, valores_filtro, val){
  
  
  tabla_comp1 = if(filtro == TRUE){
    
    bind_cols(
      Sexo <-  data.frame(df %>% filter(ANO4 == anio, TRIMESTRE == trimestre, eval(parse(text=variable_f)) %in% valores_filtro) %>% select(Sexo)),
      
      Valor_original <-  data.frame(df %>% filter(ANO4 == anio, TRIMESTRE == trimestre, eval(parse(text=variable_f)) %in% valores_filtro) %>% select(val)),
      
      trimestre_anterior <-
        if(trimestre != 1){
          df %>% filter(ANO4 == anio, TRIMESTRE == (trimestre - 1), eval(parse(text=variable_f)) %in% valores_filtro) %>% select(val)}else {
            df %>% filter(ANO4 == (anio - 1), TRIMESTRE == (trimestre + 3), eval(parse(text=variable_f)) %in% valores_filtro) %>% select(val)
          },
      
      anio_anterior <-  df %>% filter(ANO4 == (anio - 1), TRIMESTRE == trimestre, eval(parse(text=variable_f)) %in% valores_filtro) %>% select(val)
    )
    
  } 
  else{
    
    bind_cols(
      Sexo <-  data.frame(df %>% filter(ANO4 == anio, TRIMESTRE == trimestre) %>% select(Sexo)),
      
      Valor_original <-  data.frame(df %>% filter(ANO4 == anio, TRIMESTRE == trimestre) %>% select(val)),
      
      trimestre_anterior <-
        if(trimestre != 1){
          df %>% filter(ANO4 == anio, TRIMESTRE == (trimestre - 1)) %>% select(val)}else {
            df %>% filter(ANO4 == (anio - 1), TRIMESTRE == (trimestre + 3)) %>% select(val)
          },
      
      anio_anterior <-  df %>% filter(ANO4 == (anio - 1), TRIMESTRE == trimestre) %>% select(val)
    )
    
  }
  
  colnames(tabla_comp1) <- c("Sexo", "Valor_original", "trimestre_anterior", "anio_anterior")
  
  tabla_comp = tabla_comp1 %>%
    mutate(variacion_intertrimestral = 100*((Valor_original - trimestre_anterior)/Valor_original),
           variacion_interanual = 100*((Valor_original - anio_anterior)/Valor_original))%>% 
    select(Sexo, Valor_original, variacion_intertrimestral, variacion_interanual) %>%
    rename("Valor" = "Valor_original",
           "Variación intertrimestral (%)" = "variacion_intertrimestral",
           "Variación interanual (%)" = "variacion_interanual")
  
  
  
  return(tabla_comp)
}


series_brechas <- function(df="tablas_16_19[[df]]", 
                           input1=input$aniosID2[1],
                           input2=input$aniosID2[2]
){
  
  encuentro_brecha <- resultados$tablas_16_19[[df]]
  
  encuentro_brecha <- resultados$tablas_16_19[[df]] %>% 
    select_if(str_detect(colnames(encuentro_brecha), "^brecha") == TRUE) 
  
  colnames(encuentro_brecha) <- "brecha"
  
  tabla_brecha <- bind_cols(resultados$tablas_16_19[[df]], encuentro_brecha)
  
  datagraf<-tabla_brecha %>% # Daraframe para 2016-19
    mutate(dummy = case_when(ANO4 %in% c(2004:2006) ~ "2004-2006",  # Identifico periodos
                             TRUE ~ "2016-2019"),
           periodo = factor(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE)),
           ubi = (media.mujeres + media.varones)/2,
           brecha_lab = paste0(brecha, "%"))%>%
    arrange(ANO4)
  
  
  df.filt <- reactive({
    datagraf %>% filter(ANO4>= input1 &
                          ANO4 <= input2)
  })
  
  return(df.filt)
}


#gráficos:
graficos_brechas <- function(df=df.filt, 
                             eje_x = "",
                             titulo = "Titulo", 
                             subtitulo = "Subtitulo",
                             multiple_facet = FALSE,
                             faceta = "indicador"){
  grafico <-   ggplot(df, aes(x = media.mujeres, xend = media.varones, y = periodo, group = periodo,label = brecha_lab)) +
    geom_dumbbell(color= "#494949",
                  size_x = 3, size_xend = 3,
                  colour_x = colores[1],
                  colour_xend = colores[2]) +
    geom_text(data = df, aes(ubi, periodo, label = brecha_lab), nudge_y = .2)+
    labs(x=eje_x,
         title = titulo,
         subtitle = subtitulo,
         y="Período", 
         caption = "Fuente: Elaboración propia en base a EPH-INDEC")+
    scale_color_manual(values = colores)+
    theme_minimal(base_family = "Arial")
  
  
  if (multiple_facet) {
    grafico <- grafico +
      facet_grid(~eval(parse(text = faceta)), scales = "free_x", space = "free_x")
    
  }else{
    grafico <- grafico 
  }
  
  
  return(grafico)
  
}


graficos_deciles <- function(df, 
                             anio = "2019",
                             trimestre = "4",
                             variable = "DECINDR",
                             eje_x = "Decil del ingreso per cápita familiar",
                             titulo = "Composición según sexo de los deciles del ingreso per cápita familiar.", 
                             subtitulo = "Total de la población"){
  grafico <-  df %>% 
    filter(ANO4 == anio, TRIMESTRE == trimestre) %>% 
    ggplot(., aes(x = as.numeric(eval(parse(text = variable))), Prop, fill = Sexo, group = Sexo, label = paste0(Prop, "%"))) +
    geom_col(position = "dodge") +
    geom_text(position = position_dodge(width = .9), vjust =-.1, size = 2.5) +
    labs(y = '') +
    scale_fill_manual(values = colores) +
    scale_x_continuous(eje_x, breaks = c(1:10)) +
    labs(title = titulo,
         subtitle = paste0(subtitulo, trimestre, "° trimestre de ", anio),
         caption = "Fuente: Elaboración propia en base a EPH-INDEC") +
    theme_minimal() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
  
  
  return(grafico)
  
}

ui <- navbarPage("EcoFemiData",
        theme=shinythemes::shinytheme(theme = "paper"),
  tabPanel("Mercado de trabajo",
           titlePanel("Informes - La desigualdad de género se puede medir"),
  sidebarLayout(sidebarPanel(h4("Serie"),
                sliderInput(inputId = "aniosID",
                             "Años:",
                             min=2016,
                             max=2019,
                             value=c(2018,2019)),
                 selectInput(inputId="indicadoresID",
                             "Indicadores:",
                             choices=indicadores),
                 actionButton("boton1","Ver"),
                 
                 br(),
                br(),
                br(),
                 h4("Variación"),
                 numericInput("anio_var"
                              ,label = "Año:",
                              value = 2019, min = 2016, max = 2019),
                 numericInput("trim_var"
                               ,label = "Trimestre:",
                               value = 4, min = 1, max = 4),
                 actionButton("boton2","Calcular")),
    mainPanel(tags$style("#titulo_tabla1 {font-size:17px;}"),
      tags$style("#titulo_tabla2 {font-size:17px;}"),
      plotOutput('grafico_final'),
              br(),
              textOutput('titulo_tabla1'),
              tableOutput('tabla_comparacion1'),
              textOutput('titulo_tabla2'),
              tableOutput('tabla_comparacion2'))
  )
),
tabPanel("Ingresos",
         titlePanel("Informes - La desigualdad de género se puede medir"),
         sidebarLayout(sidebarPanel(h4("Serie"),
                                    sliderInput(inputId = "aniosID2",
                                                "Años:",
                                                min=2016,
                                                max=2019,
                                                value=c(2018,2019)),
                                    selectInput(inputId="indicadoresID2",
                                                "Indicadores:",
                                                choices=brechas),
                                    actionButton("boton3","Ver"),
                                    br(),
                                    br(),
                                    br(),
                                    h4("Deciles"),
                                    numericInput("anio_dec"
                                                 ,label = "Año:",
                                                 value = 2019, min = 2016, max = 2019),
                                    numericInput("trim_dec"
                                                 ,label = "Trimestre:",
                                                 value = 4, min = 1, max = 4),
                                    selectInput(inputId="indicadoresID3",
                                                "Indicadores:",
                                                choices=deciles),
                                    actionButton("boton4","Ver")),
                       mainPanel(plotOutput('grafico_final2'),
                                 br(),
                                 plotOutput('grafico_final3'))
         )
         
         )

)

server <- function(input,output,session){
  
  observeEvent(input$boton1,{
    if (input$indicadoresID == "Actividad y empleo"){
      
      df.filt <- series(df="tasas_por_sexo_df", 
                      filtro = TRUE, 
                      variable = "indicador", 
                      valores_filter = c("Tasa Actividad", "Tasa Empleo"),
                      input1=input$aniosID[1],
                      input2=input$aniosID[2],
                      renombrar=FALSE)
      
      graphPlot <- graficos(df=df.filt(), 
                            eje_x = "Período",
                            eje_y = "",
                            titulo = "Tasas de actividad y empleo", 
                            subtitulo = "Población de 14 años y más. Por sexo y período. Total 31 aglomerados urbanos.",
                            doble_facet = TRUE,
                            faceta = "indicador")
      
      output$grafico_final <- renderPlot(graphPlot)
      
    }
    
    else if (input$indicadoresID == "Desocupación y subocupación"){
      
      df.filt <- series(df="tasas_por_sexo_df", 
                   filtro = TRUE, 
                   variable = "indicador", 
                   valores_filter = c("Tasa Desocupación", "Tasa Subocupación"),
                   input1=input$aniosID[1],
                   input2=input$aniosID[2],
                   renombrar=FALSE)
      
      graphPlot <- graficos(df=df.filt(), 
                            eje_x = "Período",
                            eje_y = "",
                            titulo = "Tasas de desocupación y subocupación", 
                            subtitulo = "Población de 14 años y más. Por sexo y período. Total 31 aglomerados urbanos.",
                            doble_facet = TRUE,
                            faceta = "indicador")
      
      output$grafico_final <- renderPlot(graphPlot)
      
    }
    else if (input$indicadoresID == "Tasas de no registro"){
      
      df.filt <- series(df="tasas_no_registro_df", 
                        filtro = FALSE, 
                        input1=input$aniosID[1],
                        input2=input$aniosID[2],
                        renombrar= TRUE,
                        cols="Proporción de no Registrados"
                        )
      
      graphPlot <- graficos(df=df.filt(), 
                            eje_x = "Período",
                            eje_y = "",
                            titulo = "Tasas de no registro", 
                            subtitulo = "Ocupadas/os asalariadas/os. Por sexo y período. Total 31 aglomerados urbanos.",
                            doble_facet = FALSE)

      output$grafico_final<-renderPlot(graphPlot)
    }
    else if (input$indicadoresID == "Puestos en dirección y jefaturas"){
      
      df.filt <- series(df="sexo_segun_jerarquias_df", 
                        filtro = TRUE, 
                        variable = "JERARQUIA", 
                        valores_filter = c("Jefes", "Dirección"),
                        input1=input$aniosID[1],
                        input2=input$aniosID[2],
                        renombrar= TRUE,
                        cols="tasa")
      
      graphPlot <- graficos(df=df.filt(), 
                            eje_x = "Período",
                            eje_y = "",
                            titulo = "Sexo según jerarquías (jefatura y dirección)", 
                            subtitulo = "Ocupadas/os. Por periodos. Total 31 aglomerados urbanos.",
                            doble_facet = TRUE,
                            faceta = "JERARQUIA")
      
      output$grafico_final<-renderPlot(graphPlot)
      
    }
    else if (input$indicadoresID == "Cuentapropistas"){
      
      df.filt <- series(df="sexo_segun_jerarquias_df", 
                        filtro = TRUE, 
                        variable = "JERARQUIA", 
                        valores_filter = c("Cuentapropia"),
                        input1=input$aniosID[1],
                        input2=input$aniosID[2],
                        renombrar= TRUE,
                        cols="tasa")
      
      graphPlot <- graficos(df=df.filt(), 
                            eje_x = "Período",
                            eje_y = "",
                            titulo = "Sexo según jerarquías (cuentapropia)", 
                            subtitulo = "Ocupadas/os. Por periodos. Total 31 aglomerados urbanos.",
                            doble_facet = FALSE)
      
      output$grafico_final<-renderPlot(graphPlot)
      
    }
    else if (input$indicadoresID == "Asalariades"){
      
      df.filt <- series(df="sexo_segun_jerarquias_df", 
                        filtro = TRUE, 
                        variable = "JERARQUIA", 
                        valores_filter = c("Trabajadores Asalariados"),
                        input1=input$aniosID[1],
                        input2=input$aniosID[2],
                        renombrar= TRUE,
                        cols="tasa")
      
      graphPlot <- graficos(df=df.filt(), 
                            eje_x = "Período",
                            eje_y = "",
                            titulo = "Sexo según jerarquías (trabajo asalariado)", 
                            subtitulo = "Ocupadas/os. Por periodos. Total 31 aglomerados urbanos.",
                            doble_facet = FALSE)
      
      output$grafico_final<-renderPlot(graphPlot)
      
    }
    else if (input$indicadoresID == "Calificación profesional y técnica"){
      
      df.filt <- series(df="sexo_segun_calif_df", 
                        filtro = TRUE, 
                        variable = "CALIFICACION", 
                        valores_filter = c("Profesionales", "Técnicos"),
                        input1=input$aniosID[1],
                        input2=input$aniosID[2],
                        renombrar= TRUE,
                        cols="tasa")
      
      graphPlot <- graficos(df=df.filt(), 
                            eje_x = "Período",
                            eje_y = "",
                            titulo = "Sexo según calificación", 
                            subtitulo = "Ocupadas/os con calificación válida. Por periodos. Total 31 aglomerados urbanos.",
                            doble_facet = TRUE,
                            faceta = "CALIFICACION")
      
      output$grafico_final<-renderPlot(graphPlot)
      
    }
    else if (input$indicadoresID == "Calificación operativa y no calificados"){
      
      df.filt <- series(df="sexo_segun_calif_df", 
                        filtro = TRUE, 
                        variable = "CALIFICACION", 
                        valores_filter = c("Operativos", "No Calificados"),
                        input1=input$aniosID[1],
                        input2=input$aniosID[2],
                        renombrar= TRUE,
                        cols="tasa")
      
      graphPlot <- graficos(df=df.filt(), 
                            eje_x = "Período",
                            eje_y = "",
                            titulo = "Sexo según calificación", 
                            subtitulo = "Ocupadas/os con calificación válida. Por periodos. Total 31 aglomerados urbanos.",
                            doble_facet = TRUE,
                            faceta = "CALIFICACION")
      
      output$grafico_final<-renderPlot(graphPlot)
      
    }
    else if (input$indicadoresID == "Nivel educativo: secundaria y superior"){
      
      df.filt <- series(df="sexo_segun_nivel_educ_df", 
                        filtro = TRUE, 
                        variable = "NIVEL_EDUCATIVO", 
                        valores_filter = c("Superior", "Secundaria"),
                        input1=input$aniosID[1],
                        input2=input$aniosID[2],
                        renombrar= TRUE,
                        cols="tasa")
      
      graphPlot <- graficos(df=df.filt(), 
                            eje_x = "Período",
                            eje_y = "",
                            titulo = "Sexo según nivel educativo", 
                            subtitulo = "Ocupadas/os con nivel educativo válido. Por periodos. Total 31 aglomerados urbanos.",
                            doble_facet = TRUE,
                            faceta = "NIVEL_EDUCATIVO")
      
      output$grafico_final<-renderPlot(graphPlot)
      
    }
    else if (input$indicadoresID == "Nivel educativo: primaria y sin instrucción"){
      
      df.filt <- series(df="sexo_segun_nivel_educ_df", 
                        filtro = TRUE, 
                        variable = "NIVEL_EDUCATIVO", 
                        valores_filter = c("Primaria", "Sin Instrucción"),
                        input1=input$aniosID[1],
                        input2=input$aniosID[2],
                        renombrar = TRUE,
                        cols="tasa")
      
      graphPlot <- graficos(df=df.filt(), 
                            eje_x = "Período",
                            eje_y = "",
                            titulo = "Sexo según nivel educativo", 
                            subtitulo = "Ocupadas/os con nivel educativo válido. Por periodos. Total 31 aglomerados urbanos.",
                            doble_facet = TRUE,
                            faceta = "NIVEL_EDUCATIVO")
      
      output$grafico_final<-renderPlot(graphPlot)
      
    }
    else if (input$indicadoresID == "Extensión de la jornada laboral remunerada"){
      
      df.filt <- series(df= "horas_semanales_df", 
                        filtro = TRUE, 
                        variable = "indicador", 
                        valores_filter = c("Media.hs.ocup.princ", "Media.hs.total.ocup"),
                        input1=input$aniosID[1],
                        input2=input$aniosID[2],
                        renombrar = TRUE,
                        cols="valor")
      
      graphPlot <- graficos(df=df.filt(), 
                            eje_x = "Período",
                            eje_y = "",
                            titulo = "Extensión de la jornada laboral remunerada (principal ocupación y ocupaciones totales)", 
                            subtitulo = "Horas promedio semanales. Ocupadas/os. Por periodos. Total 31 aglomerados urbanos.",
                            doble_facet = TRUE,
                            faceta = "indicador",
                            porcentaje = FALSE)
      
      output$grafico_final<-renderPlot(graphPlot)
      
    }
    else if (input$indicadoresID == "Distribución de las tareas domésticas"){
      
      df.filt <- series(df="tareas_domesticas_sexo_df", 
                        filtro = FALSE, 
                        input1=input$aniosID[1],
                        input2=input$aniosID[2],
                        renombrar= TRUE,
                        cols="proporcion")
      
      graphPlot <- graficos(df=df.filt(), 
                            eje_x = "Período",
                            eje_y = "",
                            titulo = "Proporción de cada sexo entre las personas que realizan las tareas domésticas del hogar", 
                            subtitulo = "Por periodos. Total 31 aglomerados urbanos.",
                            doble_facet = FALSE)
      
      output$grafico_final<-renderPlot(graphPlot)
      
    }
    
  }
  )
  
  #agregado comparacion
  observeEvent(input$boton2, {
    
    if ((input$anio_var == 2016 & input$trim_var ==1)|
          (input$anio_var == 2016 & input$trim_var ==2)|
          (input$anio_var == 2016 & input$trim_var ==3)|
          (input$anio_var == 2016 & input$trim_var ==4)|
          (input$anio_var == 2017 & input$trim_var ==1)){
           
      output$titulo_tabla1 = renderText({}) 
      output$tabla_comparacion1 = renderTable({})
      output$titulo_tabla2 = renderText("Perdón, nuestra base todavía no permite calcular estos datos.") 
      output$tabla_comparacion2 = renderTable({})
          }
    
    else if (input$indicadoresID == "Actividad y empleo"){
      
      output$titulo_tabla1 = renderText("Tasa de actividad")
      
      calculo_comp1 = eventReactive(input$boton2, {
        
        comparaciones(df = resultados$tablas_16_19$tasas_por_sexo_df,
                      anio = isolate(input$anio_var),
                      trimestre = isolate(input$trim_var),
                      filtro = TRUE,
                      variable_f = "indicador",
                      valores_filtro = c("Tasa Actividad"),
                      val = "valor")
        
      }, ignoreNULL=F)
      
      output$tabla_comparacion1 = renderTable(calculo_comp1()
      )
      
      output$titulo_tabla2 = renderText("Tasa de empleo")
      
      calculo_comp2 = eventReactive(input$boton2, {
        
        comparaciones(df = resultados$tablas_16_19$tasas_por_sexo_df,
                      anio = isolate(input$anio_var),
                      trimestre = isolate(input$trim_var),
                      filtro = TRUE,
                      variable_f = "indicador",
                      valores_filtro = c("Tasa Empleo"),
                      val = "valor")
        
      }, ignoreNULL=F)
      
      output$tabla_comparacion2 = renderTable(calculo_comp2()
      )
      
    } 
    
    else if (input$indicadoresID == "Desocupación y subocupación"){
      
      output$titulo_tabla1 = renderText("Tasa de desocupación")
      
      calculo_comp1 = eventReactive(input$boton2, {
        
        comparaciones(df = resultados$tablas_16_19$tasas_por_sexo_df,
                      anio = isolate(input$anio_var),
                      trimestre = isolate(input$trim_var),
                      filtro = TRUE,
                      variable_f = "indicador",
                      valores_filtro = c("Tasa Desocupación"),
                      val = "valor")
        
      }, ignoreNULL=F)
      
      output$tabla_comparacion1 = renderTable(calculo_comp1()
      )
      
      output$titulo_tabla2 = renderText("Tasa de subocupación")
      
      calculo_comp2 = eventReactive(input$boton2, {
        
        comparaciones(df = resultados$tablas_16_19$tasas_por_sexo_df,
                      anio = isolate(input$anio_var),
                      trimestre = isolate(input$trim_var),
                      filtro = TRUE,
                      variable_f = "indicador",
                      valores_filtro = c("Tasa Subocupación"),
                      val = "valor")
        
      }, ignoreNULL=F)
      
      output$tabla_comparacion2 = renderTable(calculo_comp2()
      )
      
    }
    
    else if (input$indicadoresID == "Tasas de no registro"){
      
      output$titulo_tabla1 = renderText("Tasa de no registro")
      
      calculo_comp1 = eventReactive(input$boton2, {
        
        comparaciones(df = resultados$tablas_16_19$tasas_no_registro_df,
                      anio = isolate(input$anio_var),
                      trimestre = isolate(input$trim_var),
                      filtro = FALSE,
                      val = "Proporción de no Registrados")
        
      }, ignoreNULL=F)
      
      output$tabla_comparacion1 = renderTable(calculo_comp1()
      )
      
      output$titulo_tabla2 = renderText({})
      output$tabla_comparacion2 = renderTable({})
      
    }
    
    else if (input$indicadoresID == "Puestos en dirección y jefaturas") {
      
      output$titulo_tabla1 = renderText("Puestos en dirección")
      
      calculo_comp1 = eventReactive(input$boton2, {
        
        comparaciones(df = resultados$tablas_16_19$sexo_segun_jerarquias_df,
                      anio = isolate(input$anio_var),
                      trimestre = isolate(input$trim_var),
                      filtro = TRUE,
                      variable_f = "JERARQUIA",
                      valores_filtro = c("Dirección"),
                      val = "tasa")
        
      }, ignoreNULL=F)
      
      output$tabla_comparacion1 = renderTable(calculo_comp1()
      )
      
      output$titulo_tabla2 = renderText("Puestos en jefatura")
      
      calculo_comp2 = eventReactive(input$boton2, {
        
        comparaciones(df = resultados$tablas_16_19$sexo_segun_jerarquias_df,
                      anio = isolate(input$anio_var),
                      trimestre = isolate(input$trim_var),
                      filtro = TRUE,
                      variable_f = "JERARQUIA",
                      valores_filtro = c("Jefes"),
                      val = "tasa")
        
      }, ignoreNULL=F)
      
      output$tabla_comparacion2 = renderTable(calculo_comp2()
      )
      
    }
    
    else if (input$indicadoresID == "Cuentapropistas") {
      
      output$titulo_tabla1 = renderText("Trabajadores cuentapropistas")
      
      calculo_comp1 = eventReactive(input$boton2, {
        
        comparaciones(df = resultados$tablas_16_19$sexo_segun_jerarquias_df,
                      anio = isolate(input$anio_var),
                      trimestre = isolate(input$trim_var),
                      filtro = TRUE,
                      variable_f = "JERARQUIA",
                      valores_filtro = c("Cuentapropia"),
                      val = "tasa")
        
      }, ignoreNULL=F)
      
      output$tabla_comparacion1 = renderTable(calculo_comp1()
      )
      
      output$titulo_tabla2 = renderText({})
      
      
      output$tabla_comparacion2 = renderTable({})
      
    }
    
    else if (input$indicadoresID == "Asalariades") {
      
      output$titulo_tabla1 = renderText("Trabajadores asalariades")
      
      calculo_comp1 = eventReactive(input$boton2, {
        
        comparaciones(df = resultados$tablas_16_19$sexo_segun_jerarquias_df,
                      anio = isolate(input$anio_var),
                      trimestre = isolate(input$trim_var),
                      filtro = TRUE,
                      variable_f = "JERARQUIA",
                      valores_filtro = c("Trabajadores Asalariados"),
                      val = "tasa")
        
        
      }, ignoreNULL=F)
      
      output$tabla_comparacion1 = renderTable(calculo_comp1()
      )
      
      output$titulo_tabla2 = renderText({})
      
      
      output$tabla_comparacion2 = renderTable({})
      
    }
    
    else if (input$indicadoresID == "Calificación profesional y técnica") {
      
      output$titulo_tabla1 = renderText("Trabajadores con calificación profesional")
      
      calculo_comp1 = eventReactive(input$boton2, {
        
        comparaciones(df = resultados$tablas_16_19$sexo_segun_calif_df,
                      anio = isolate(input$anio_var),
                      trimestre = isolate(input$trim_var),
                      filtro = TRUE,
                      variable_f = "CALIFICACION",
                      valores_filtro = c("Profesionales"),
                      val = "tasa")
        
      }, ignoreNULL=F)
      
      output$tabla_comparacion1 = renderTable(calculo_comp1()
      )
      
      output$titulo_tabla2 = renderText("Trabajadores con calificación técnica")
      
      calculo_comp2 = eventReactive(input$boton2, {
        
        comparaciones(df = resultados$tablas_16_19$sexo_segun_calif_df,
                      anio = isolate(input$anio_var),
                      trimestre = isolate(input$trim_var),
                      filtro = TRUE,
                      variable_f = "CALIFICACION",
                      valores_filtro = c("Técnicos"),
                      val = "tasa")
        
      }, ignoreNULL=F)
      
      
      output$tabla_comparacion2 = renderTable(calculo_comp2()
                                              )
      
    }
    
    else if (input$indicadoresID == "Calificación operativa y no calificados") {
      
      output$titulo_tabla1 = renderText("Trabajadores con calificación operativa")
      
      calculo_comp1 = eventReactive(input$boton2, {
        
        comparaciones(df = resultados$tablas_16_19$sexo_segun_calif_df,
                      anio = isolate(input$anio_var),
                      trimestre = isolate(input$trim_var),
                      filtro = TRUE,
                      variable_f = "CALIFICACION",
                      valores_filtro = c("Operativos"),
                      val = "tasa")
        
        
      }, ignoreNULL=F)
      
      output$tabla_comparacion1 = renderTable(calculo_comp1()
      )
      
      output$titulo_tabla2 = renderText("Trabajadores no calificados")
      
      calculo_comp2 = eventReactive(input$boton2, {
        
        comparaciones(df = resultados$tablas_16_19$sexo_segun_calif_df,
                      anio = isolate(input$anio_var),
                      trimestre = isolate(input$trim_var),
                      filtro = TRUE,
                      variable_f = "CALIFICACION",
                      valores_filtro = c("No Calificados"),
                      val = "tasa")
        
        
      }, ignoreNULL=F)
      
      output$tabla_comparacion2 = renderTable(calculo_comp2()
      )
      
    }
    
    else if (input$indicadoresID == "Nivel educativo: secundaria y superior") {
      
      output$titulo_tabla1 = renderText("Trabajadores con nivel educativo: secundaria")
      
      calculo_comp1 = eventReactive(input$boton2, {
        
        comparaciones(df = resultados$tablas_16_19$sexo_segun_nivel_educ_df,
                      anio = isolate(input$anio_var),
                      trimestre = isolate(input$trim_var),
                      filtro = TRUE,
                      variable_f = "NIVEL_EDUCATIVO",
                      valores_filtro = c("Secundaria"),
                      val = "tasa")
        
        
      }, ignoreNULL=F)
      
      output$tabla_comparacion1 = renderTable(calculo_comp1()
      )
      
      output$titulo_tabla2 = renderText("Trabajadores con nivel educativo: superior")
      
      calculo_comp2 = eventReactive(input$boton2, {
        
        comparaciones(df = resultados$tablas_16_19$sexo_segun_nivel_educ_df,
                      anio = isolate(input$anio_var),
                      trimestre = isolate(input$trim_var),
                      filtro = TRUE,
                      variable_f = "NIVEL_EDUCATIVO",
                      valores_filtro = c("Superior"),
                      val = "tasa")
        
        
      }, ignoreNULL=F)
      
      output$tabla_comparacion2 = renderTable(calculo_comp2()
      )
      
    }
    
    else if (input$indicadoresID == "Nivel educativo: primaria y sin instrucción") {
      
      output$titulo_tabla1 = renderText("Trabajadores con nivel educativo: sin instrucción")
      
      calculo_comp1 = eventReactive(input$boton2, {
        
        comparaciones(df = resultados$tablas_16_19$sexo_segun_nivel_educ_df,
                      anio = isolate(input$anio_var),
                      trimestre = isolate(input$trim_var),
                      filtro = TRUE,
                      variable_f = "NIVEL_EDUCATIVO",
                      valores_filtro = c("Sin Instrucción"),
                      val = "tasa")
        
        
      }, ignoreNULL=F)
      
      output$tabla_comparacion1 = renderTable(calculo_comp1()
      )
      
      output$titulo_tabla2 = renderText("Trabajadores con nivel educativo: primario")
      
      calculo_comp2 = eventReactive(input$boton2, {
        
        comparaciones(df = resultados$tablas_16_19$sexo_segun_nivel_educ_df,
                      anio = isolate(input$anio_var),
                      trimestre = isolate(input$trim_var),
                      filtro = TRUE,
                      variable_f = "NIVEL_EDUCATIVO",
                      valores_filtro = c("Primaria"),
                      val = "tasa")
        
        
      }, ignoreNULL=F)
      
      output$tabla_comparacion2 = renderTable(calculo_comp2()
      )
      
    }
    
    else if (input$indicadoresID == "Extensión de la jornada laboral remunerada") {
      
      output$titulo_tabla1 = renderText("Horas promedio de trabajo remunerado semanal (ocupaciones totales)")
      
      calculo_comp1 = eventReactive(input$boton2, {
        
        comparaciones(df = resultados$tablas_16_19$horas_semanales_df,
                      anio = isolate(input$anio_var),
                      trimestre = isolate(input$trim_var),
                      filtro = TRUE,
                      variable_f = "indicador",
                      valores_filtro = c("Media.hs.total.ocup"),
                      val = "valor")
        
      }, ignoreNULL=F)
      
      output$tabla_comparacion1 = renderTable(calculo_comp1()
      )
      
      output$titulo_tabla2 = renderText("Horas promedio de trabajo remunerado semanal (ocupación principal)")
      
      calculo_comp2 = eventReactive(input$boton2, {
        
        comparaciones(df = resultados$tablas_16_19$horas_semanales_df,
                      anio = isolate(input$anio_var),
                      trimestre = isolate(input$trim_var),
                      filtro = TRUE,
                      variable_f = "indicador",
                      valores_filtro = c("Media.hs.ocup.princ"),
                      val = "valor")
        
        
      }, ignoreNULL=F)
      
      output$tabla_comparacion2 = renderTable(calculo_comp2()
      )
      
    }
    
    else if (input$indicadoresID == "Distribución de las tareas domésticas") {
      
      output$titulo_tabla1 = renderText("Proporción de cada sexo entre las personas que realizan las tareas domésticas del hogar")
      
      calculo_comp1 = eventReactive(input$boton2, {
        
        comparaciones(df = resultados$tablas_16_19$tareas_domesticas_sexo_df,
                      anio = isolate(input$anio_var),
                      trimestre = isolate(input$trim_var),
                      filtro = FALSE,
                      val = "proporcion")
        
      }, ignoreNULL=F)
      
      output$tabla_comparacion1 = renderTable(calculo_comp1()
      )
      
      output$titulo_tabla2 = renderText({})
      
      
      output$tabla_comparacion2 = renderTable({})
      
    }
    
  
  })
  
 
  observeEvent(input$boton3,{
    
    if (input$indicadoresID2 == "Ingreso de la Ocupación Principal - por Nivel educativo"){
      
      df.filt <- series_brechas(df="brecha_IOP_nivel_educ_df", 
                        input1=input$aniosID2[1],
                        input2=input$aniosID2[2])
      
      graphPlot <- graficos_brechas(df=df.filt(), 
                            eje_x = "Ingreso de la ocupación principal",
                            titulo = "Brecha de ingresos de la ocupación principal por nivel educativo", 
                            subtitulo = "Población ocupada (con nivel educativo válido). Por sexo y período. Total 31 aglomerados urbanos.",
                            multiple_facet = TRUE,
                            faceta = "NIVEL_EDUCATIVO")
      
      output$grafico_final2 <- renderPlot(graphPlot)
      
    } 
    
    else if (input$indicadoresID2 == "Ingreso total individual") {
      
      df.filt <- series_brechas(df="brecha_ITI_df", 
                                input1=input$aniosID2[1],
                                input2=input$aniosID2[2])
      
      graphPlot <- graficos_brechas(df=df.filt(), 
                                    eje_x = "Ingreso total individual",
                                    titulo = "Brecha de ingreso total individual", 
                                    subtitulo = "Población perceptora de ingresos. Por sexo y período. Total 31 aglomerados urbanos.",
                                    multiple_facet = FALSE)
      
      output$grafico_final2 <- renderPlot(graphPlot)
      
    }
  
    else if (input$indicadoresID2 == "Ingreso de la Ocupación Principal") {
    
    df.filt <- series_brechas(df="brecha_IOP_df", 
                              input1=input$aniosID2[1],
                              input2=input$aniosID2[2])
    
    graphPlot <- graficos_brechas(df=df.filt(), 
                                  eje_x = "Ingreso de la ocupación principal",
                                  titulo = "Brecha de ingresos de la ocupación principal", 
                                  subtitulo = "Población ocupada. Por sexo y período. Total 31 aglomerados urbanos.",
                                  multiple_facet = FALSE)
    
    output$grafico_final2 <- renderPlot(graphPlot)
    
  }
  
 else if (input$indicadoresID2 == "Ingreso de la Ocupación Principal - asalariados no registrados") {
  
  df.filt <- series_brechas(df="brecha_IOP_no_reg_df", 
                            input1=input$aniosID2[1],
                            input2=input$aniosID2[2])
  
  graphPlot <- graficos_brechas(df=df.filt(), 
                                eje_x = "Ingreso de la ocupación principal",
                                titulo = "Brecha de ingresos de la ocupación principal", 
                                subtitulo = "Asalariadas y Asalariados no registrados. Por sexo y período. Total 31 aglomerados urbanos.",
                                multiple_facet = FALSE)
  
  output$grafico_final2 <- renderPlot(graphPlot)
  
}

 else if (input$indicadoresID2 == "Ingreso de la Ocupación Principal - por calificación") {
  
  df.filt <- series_brechas(df="brecha_IOP_calif_df", 
                            input1=input$aniosID2[1],
                            input2=input$aniosID2[2])
  
  graphPlot <- graficos_brechas(df=df.filt(), 
                                eje_x = "Ingreso de la ocupación principal",
                                titulo = "Brecha de ingresos de la ocupación principal por calificación", 
                                subtitulo = "Población ocupada (con calificación válida). Por sexo y período. Total 31 aglomerados urbanos.",
                                multiple_facet = TRUE,
                                faceta = "CALIFICACION")
  
  output$grafico_final2 <- renderPlot(graphPlot)
  
}

 else if (input$indicadoresID2 == "Ingreso horario de la Ocupación Principal") {
  
  df.filt <- series_brechas(df="brecha_IOP_hr_df", 
                            input1=input$aniosID2[1],
                            input2=input$aniosID2[2])
  
  graphPlot <- graficos_brechas(df=df.filt(), 
                                eje_x = "Ingreso horario de la ocupación principal",
                                titulo = "Brecha de ingreso horario de la ocupación principal", 
                                subtitulo = "Población ocupada (con horas positivas). Por sexo y período. Total 31 aglomerados urbanos.",
                                multiple_facet = FALSE)
  
  output$grafico_final2 <- renderPlot(graphPlot)
  
}

 else if (input$indicadoresID2 == "Ingreso horario de la Ocupación Principal - por Nivel educativo") {
  
  df.filt <- series_brechas(df="brecha_IOP_hr_nivel_educ_df", 
                            input1=input$aniosID2[1],
                            input2=input$aniosID2[2])
  
  graphPlot <- graficos_brechas(df=df.filt(), 
                                eje_x = "Ingreso horario de la ocupación principal",
                                titulo = "Brecha de ingreso horario de la ocupación principal por nivel educativo", 
                                subtitulo = "Población ocupada (con horas positivas y nivel educativo válido). Por sexo y período. Total 31 aglomerados urbanos.",
                                multiple_facet = TRUE,
                                faceta = "NIVEL_EDUCATIVO")
  
  output$grafico_final2 <- renderPlot(graphPlot)
  
}

 else if (input$indicadoresID2 == "Ingreso horario de la Ocupación Principal - por calificación") {
  
  df.filt <- series_brechas(df="brecha_IOP_hr_calif_df", 
                            input1=input$aniosID2[1],
                            input2=input$aniosID2[2])
  
  graphPlot <- graficos_brechas(df=df.filt(), 
                                eje_x = "Ingreso horario de la ocupación principal",
                                titulo = "Brecha de ingreso horario de la ocupación principal por calificación", 
                                subtitulo = "Población ocupada (con horas positivas y calificación válida). Por sexo y período. Total 31 aglomerados urbanos.",
                                multiple_facet = TRUE,
                                faceta = "CALIFICACION")
  
  output$grafico_final2 <- renderPlot(graphPlot)
  
}
    
    
    
  })
  
  observeEvent(input$boton4, {
    
    if (input$indicadoresID3 == "Ingreso total individual"){
      
      graphPlot <- graficos_deciles(df=resultados$tablas_16_19$deciles_ITI_sexo_df, 
                                    anio = input$anio_dec,
                                    trimestre = input$trim_dec,
                                    variable = "DECINDR",
                                    eje_x = "Decil del ingreso total individual",
                                    titulo = "Composición según sexo de los deciles del ingreso total individual.", 
                                    subtitulo = "Total de la población.")
      
      output$grafico_final3 <- renderPlot(graphPlot)
      
    } 
    
    else if (input$indicadoresID3 == "Ingreso per cápita familiar"){
      
      graphPlot <- graficos_deciles(df=resultados$tablas_16_19$deciles_IPCF_sexo_df, 
                                    anio = input$anio_dec,
                                    trimestre = input$trim_dec,
                                    variable = "DECCFR",
                                    eje_x = "Decil del ingreso per cápita familiar",
                                    titulo = "Composición según sexo de los deciles del ingreso per cápita familiar.",
                                    subtitulo = "Total de la población.")
      
      output$grafico_final3 <- renderPlot(graphPlot)
      
    }
    
  })
  
  
  }


shinyApp(ui,server)



