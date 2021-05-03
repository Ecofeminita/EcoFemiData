library(shiny)
library(shinythemes)
library(dplyr)
library(ggpubr)

data <- read.csv('data_b.csv',encoding='UTF-8')

df_toallitas <- read.csv("data_toallitas_b.csv",encoding='UTF-8')

df_tampones<-read.csv("data_tampones_b.csv",encoding='UTF-8')

#data_group<-read.csv("menstruaccion_app/data_group.csv", encoding="UTF-8") %>% 
#        left_join(st_read
#        ("pxpciadatosok.shp"),by=c("Provincia"="provincia"))%>%
#        select(Provincia,CategorÃÂ­a,precio_x_unidad,geometry)

ui <- fluidPage( tags$head(tags$style(
        HTML('
         #sidebar {
            background-color: #dec4de;
        }

        body, label, input, button, select { 
          font-family: "Arial";
        }')
        )),
                navbarPage(title="¿Cuánto te cuesta menstruar en un año?",
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
                                  background-color: #ff1493"),
                     br(),        
                     br(),
                     br()
                     
                   ),
                   mainPanel(
                     tags$style("#tota_gasto {font-size:20px;
               color:deeppink;
               display:block;
               font-weight:bold;}"),
                     tags$style("#sin_iva {font-size:15px;
               color:deeppink;
               display:block;}"),
                     div(style="text-align:center;
        #box-shadow: 10px 10px 5px #888888;
        #width:200px;
        #height:200px;
        #padding-top:70px;
        #position:relative;",
                         textOutput('tota_gasto'),
                         h3(actionLink("boton2","¿Por qué es importante saberlo?",style="color:#FF1493'")),
                         h4(htmlOutput('texto1')),
                         h4(htmlOutput('texto2')),
                         br(),
                         textOutput('sin_iva'),
                         br(),
                         img(src="5aa040897bac3_1004x971.jpg",width="40%"),
                         br(),
                         br(),
                         (" Los gastos están calculados a partir de el precio medio por unidad de los productos de cada marca. Los datos sobre los precios fueron obtenidos de la página web de Precios Claros, actualizados a Marzo 2021."),
                         br()
                     ))
                   )#parentesis del sidebarlayout
                 )#,#parentesis del tabpanel
                 #tabPanel("Diferencias por provincia",
                 #         
                 #         renderTable('tabla_prueba')
                 #        
                 #)#parentesis del tabpanel
)
)

