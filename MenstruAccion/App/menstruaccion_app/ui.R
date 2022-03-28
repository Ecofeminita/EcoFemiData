library(shiny)
library(shinythemes)
library(dplyr)
library(ggpubr)

data <- read.csv('data_b.csv',encoding='UTF-8')

df_toallitas <- read.csv("data_toallitas_b.csv",encoding='UTF-8')

df_tampones<-read.csv("data_tampones_b.csv",encoding='UTF-8')

ui <- fluidPage( tags$head(
        tags$style(
        HTML('
        .nav > li > a[data-value="Home"] {background-color:#e5616e;
           color: #FFF} 
        .nav > li > a[data-value="EcoFemiData"] {background-color:#e5616e;
           color: #FFF} 
        .nav > li[class=active]    > a {background-color:#d8d8d8} 
        #sidebar {color: #f8f5ee}
        a {color: #8594c6}
        body, label, input, button, select { 
          font-family: "Arial";#
        }')
        )),
                navbarPage(
                        title= "¿Cuánto te cuesta menstruar en un año?",
                        id = "navibar",
                           tabPanel("Home", 
                 sidebarLayout(
                   sidebarPanel(
                     radioButtons('regularono',
                                  label=('¿Tenés un período regular?'),
                                  choices=c("Si","No"),
                                  selected = "Si"),
                     conditionalPanel('input.regularono == "No"',
                                      selectInput(inputId='noregular_1',
                                                  label=('¿Cuántos períodos solés tener en un año?'),
                                                  choices=c(1:20),
                                                  selected=1)),
                     numericInput(inputId='toallitasID', 
                                  label=("¿Cuántas toallitas usás aproximadamente en un período?"),
                                  min=0,
                                  max=50,
                                  value=22),
                     conditionalPanel('input.toallitasID > 0',
                                      selectInput(inputId = 'marcatoallitas',
                                                  label= '¿Qué marca?',
                                                  choices = c(" ",unique(df_toallitas$Marca)),
                                                  selected=NULL,
                                                  multiple = F)),
                     numericInput(inputId='tamponesID',
                                  label=("¿Cuántos tampones usás aproximadamente durante un período?"),
                                  min=0,
                                  max=50,
                                  value=1),
                     conditionalPanel('input.tamponesID > 0',
                                      selectInput(inputId = 'marcatampones',
                                                  label= '¿Qué marca?',
                                                  choices = c(" ", unique(df_tampones$Marca)),
                                                  selected = NULL,
                                                  multiple = F)),
                     br(),
                     actionButton("boton","Ver gasto",style="color: #fff;
                                  background-color: #8594c6"),
                     br(),
                     br(),
                     HTML('<center><img src="toallitas.png" width="90"></center>'),
                     br(), 
                     align = "center"
                     
                   ),
                   mainPanel(
                     tags$style("#tota_gasto {font-size:20px;
               color:#e5616e;
               display:block;
               font-weight:bold;}"),
                     tags$style("#sin_iva {font-size:15px;
               color:#e5616e;
               display:block;}"),
                     div(style="text-align:center;
        #box-shadow: 10px 10px 5px #888888;
        #width:200px;
        #height:200px;
        #padding-top:70px;
        #position:relative;",
                         br(),
                         textOutput('tota_gasto'),
                         h4(strong(actionLink("boton2","¿Por qué es importante saberlo?",style="color:#8cddd3'"))),
                         h4(htmlOutput('texto1')),
                         h4(htmlOutput('texto2')),
                         textOutput('sin_iva'),
                         br(),
                         img(src="menstru_logo.png",width="30%"),
                         br(),
                         br(),
                         (" Los gastos están calculados a partir de el precio medio por unidad de los productos de cada marca. Los datos sobre los precios fueron obtenidos de la página web de Precios Claros, actualizados a Marzo 2022."),
                         br()
                     ))
                   )
                 ),
                 h5(strong("App diseñada y mantenida por:"), a(href="https://twitter.com/layitx", "@layitx")),
                 tabPanel("EcoFemiData")
)
)

