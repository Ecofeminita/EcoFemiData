library(shiny)
library(eph)
library(dplyr)


#loop que abre y crea las tablas
#for (x in 1:length(anios)){
  
#  for (t in 1:length(tri)){
    
#    nombre <- paste0(anios[x],"_",tri[t])
    
#    base <- base_individual %>%
#      dplyr::filter(year==anios[x] & trimester==tri[t]) %>%
#      dplyr::select(microdata) %>%
#      tidyr::unnest(microdata)
    
#    assign(nombre,base)
#  }
#}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(sliderInput(inputId = "aniosID",
                             "Años:",
                             min=2006,
                             max=2020,
                             value=c(2018,2019)),
                 actionButton("boton1","Ver")),
    mainPanel(tableOutput('tabla'))
  )
)

server <- function(input,output,session){
  
  observeEvent(input$boton1,{
    base_individual<-get_microdata(year = input$aniosID[1]:input$aniosID[2],
                                   trimester = 1:2,
                                   type='individual')
    #base_individual <- base_individual %>% select(-microdata)
    anios <- input$aniosID[1]:input$aniosID[2]
    tri <- 1:2
    
    for (x in 1:length(anios)){
    
      for (t in 1:length(tri)){
    
        nombre <- paste0(anios[x],"_",tri[t])
    
        base <- base_individual %>%
          dplyr::filter(year==anios[x] & trimester==tri[t]) %>%
          dplyr::select(microdata) %>%
          tidyr::unnest(microdata)
    
        assign(nombre,base)
      }
      
    }
    
    
    #output$tabla<-renderTable(base_individual)
    
    })
  
  #output$tabla <- renderTable(get_microdata(year = input$aniosID[1]:input$aniosID[2],
  #                                                           trimester = 1:4,
  #                                                           type='individual'))

}

shinyApp(ui,server)
