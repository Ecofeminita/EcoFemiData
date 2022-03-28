library(rsconnect)
library(shiny)


rsconnect::setAccountInfo(name='layitx',
                          token='11B35ED566F7027828D8C9A2049362A8',
                          secret='O3b1/ZQ54a1xB6K2znBHPfVzSKYZE1hP2D/lJtaV')

runApp()
rsconnect::showLogs()
