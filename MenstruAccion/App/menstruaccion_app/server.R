library(shiny)
library(dplyr)
library(ggpubr)
library(viridis)

data <- read.csv('data_b.csv',encoding='UTF-8')

df_toallitas <- read.csv("data_toallitas_b.csv",encoding='UTF-8')

df_tampones<-read.csv("data_tampones_b.csv",encoding='UTF-8')



server = function(input, output,session) {
  
  globals <- reactiveValues(
    mydf=data
  )
  
  
  observe({
    newvar2 <- ifelse(input$regularono == "Si", 13,input$noregular_1)
    globals$mydf$Cant_periodos <- as.numeric(newvar2)
  })
  
  
  observe({
    newvar4 <- input$toallitasID
    globals$mydf$Cant_toallitas <- as.numeric(newvar4)
  })
  
  observe({
    newvar5 <- input$tamponesID
    globals$mydf$Cant_tampones <- as.numeric(newvar5)
  })
  
  observe({
    thedf <- globals$mydf
    
    globals$mydf$gasto_toallitas <- thedf$Cant_periodos*thedf$Cant_toallitas*thedf$Mean_precio

    globals$mydf$gasto_tampones<-thedf$Cant_periodos*thedf$Cant_tampones*thedf$Mean_precio

  })
  
  
  observeEvent(input$boton, {
    thedf <- globals$mydf
    
    if (input$marcatoallitas == " ") {
      newvar11 <- as.numeric(thedf[thedf$Marca==input$marcatampones & thedf$Categoria == "tampones", "gasto_tampones"])

        output$tota_gasto <- renderText({
        paste0("Gastás aproximadamente $",round(newvar11,2)," por año.")
      })
      newvar8 <- round(as.numeric(newvar11)-(as.numeric(newvar11)*0.21),2)
      output$sin_iva <- renderText({
        paste0("Para que te hagas una idea: con la eliminación del IVA de los productos menstruales, ese gasto bajaría aproximadamente a $",newvar8," por año")
      })
      
    } else if (input$marcatampones == " "){
      newvar7 <- as.numeric(thedf[thedf$Marca==input$marcatoallitas & thedf$Categoria == "toallitas", "gasto_toallitas"])

            output$tota_gasto <- renderText({
        paste0("Gastás aproximadamente $",round(newvar7,2)," por año.")
      })
      newvar8 <- round(as.numeric(newvar7)-(as.numeric(newvar7)*0.21),2)
      output$sin_iva <- renderText({
        paste0("Para que te hagas una idea: con la eliminación del IVA de los productos menstruales, ese gasto bajaría aproximadamente a $",newvar8," por año")
      })
    } else 
      {
    newvar7 <- as.numeric(thedf[thedf$Marca==input$marcatoallitas & thedf$Categoria == "toallitas", "gasto_toallitas"])
    newvar11 <- as.numeric(thedf[thedf$Marca==input$marcatampones & thedf$Categoria == "tampones", "gasto_tampones"])
    
    newvar12<-as.numeric(newvar7+newvar11)
    
    output$tota_gasto <- renderText({
      paste0("Gastás aproximadamente $",round(newvar12,2)," por año.")
    })
    newvar8 <- round(as.numeric(newvar12)-(as.numeric(newvar12)*0.21),2)
    output$sin_iva <- renderText({
      paste0("Para que te hagas una idea: con la eliminación del IVA de los productos menstruales, ese gasto bajaría aproximadamente a $",newvar8," por año")
    })
    }
  })
  
  observeEvent(input$boton2, {
    output$texto1 <- renderText({"Para aquellas personas que no pueden adquirir los medios para comprar estos productos, la menstruación serÃ¡ un factor de ausentismo escolar y laboral. En nuestro país, <b> siete de cada diez personas del grupo poblacional de menores ingresos son mujeres,</b> por lo que el gasto en productos de gestión menstrual recae sobre un gran grupo de la población que particularmente enfrenta una situación desventajosa."
      })
    output$texto2 <- renderText({
    paste0("Los productos menstruales son productos de primera necesidad, y deberían ser reconocidos como tal. Hoy en día,<b> el IVA en los productos menstruales es un impuesto por menstruar: </b>crea una desventaja real sobre los cuerpos que deben acceder a ellos. Por eso, la campaña ", "<b><a href='https://economiafeminita.com/cuanto-cuesta-menstruar-cual-es-la-inflacion-de-las-toallitas-y-tampones/'>#Menstruacción</a></b>" ," propone la eliminación del IVA de los productos menstruales, que hoy representa un impuesto por menstruar. Necesitamos que su distribución sea gratuita en escuelas, universidades, comedores, espacios comunitarios, cárceles y refugios para personas en situación de calle. ")
      })
  })
  
 #observe ({
   
#    newvar13<-as.numeric(input$toallitasID)*as.numeric(ifelse(input$regularono == "Si", 12,input$noregular_1))
#    newvar14<-as.numeric(input$tamponesID)*as.numeric(ifelse(input$regularono == "Si", 12,input$noregular_1))

#    dataf <- globals$mydf_b 

#    dataf$gasto_anual<-ifelse(dataf$CategorÃÂÃÂ­a=='toallitas',newvar13*dataf$precio_x_unidad,newvar14*dataf$precio_x_unidad)
    
#    })
 
# output$tabla<-renderTable({
#   dataf
#   })
 
  

  }


