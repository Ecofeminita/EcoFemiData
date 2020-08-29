library(eph)

anios <- 2016:2017
tri <- 1:4

base_individual<-get_microdata(year = anios,
                               trimester = 1:4,
                               type='individual')

base_hogar<-get_microdata(year = anios,
                               trimester = 1:4,
                               type='hogar')

for (x in anios){
  
  for (t in 1:length(tri)){
    
    nombre_individual <- paste0(x,"_individual_",tri[t],".txt")
    nombre_hogar <- paste0(x,"_hogar_",tri[t],".txt")
        
    indiv <- get_microdata(year = x,
                           trimester = t,
                           type='individual')
    
    hogar <- get_microdata(year = x,
                           trimester = t,
                           type='hogar')
    
    
    write.table(indiv,nombre_individual,sep=";",row.names=FALSE)
    write.table(hogar,nombre_hogar,sep=";",row.names=FALSE)
    
    
  }
  
}
