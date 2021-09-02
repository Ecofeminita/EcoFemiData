library(rsconnect)
library(shiny)


rsconnect::setAccountInfo(name='layitx', 
                          token='token', 
                          secret='secret')

setwd("C:/Users/Usuario/Desktop/EcoFeminita/App/menstruaccion_app")
shinyAppDir(".")

#runApp()
#rsconnect::showLogs()
