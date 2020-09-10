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

#Subo los resultados y colores
resultados <- readRDS("Resultados/tablas_resultados.RDS") 
colores = c("#FE1764", "#00BDD6")

#Indicadores que vamos incorporando:

indicadores<- c("Actividad y empleo","Ocupación y subocupación", "Tasas de no registro",
                "Puestos en dirección y jefaturas","Cuentapropistas", "Asalariades")

#Funciones
#separo la función que devuelve las tasas en serie y gráficos en una de serie y otra de gráficos.

#series:
series <- function(df="tasas_por_sexo_df", 
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
  grafico <- ggplot(data=df, aes(periodo, valor, color = Sexo, group = grp)) +
    geom_line(size = 1, alpha = 0.75) +
    geom_point(size = 1) +
    theme_minimal() +
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

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(sliderInput(inputId = "aniosID",
                             "Años:",
                             min=2016,
                             max=2019,
                             value=c(2018,2019)),
                 selectInput(inputId="indicadoresID",
                             "Indicadores:",
                             choices=indicadores),
                 actionButton("boton1","Ver")),
    mainPanel(plotOutput('grafico_final'))
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
    
    else if (input$indicadoresID == "Ocupación y subocupación"){
      
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
  }
  )}


shinyApp(ui,server)
