rm(list = ls())
library(tidyverse)
library(httr)
library(stringr)
library(xlsx)
library(haven)
library(readxl)

# Función que descarga bases de la página de indec, según el nombre que le pusieron al zip.
descarga_ultima_base <- function(base = 'EPH_usu_2_Trim_2017_txt.zip', individual = FALSE, hogar = FALSE){
  
  link = paste0('https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/', base)
  temp <- tempfile()
  download.file(link,temp)
  nombres <- as_vector(unzip(temp, list = TRUE)['Name'])
  base_hogar_name <- nombres[str_detect(nombres, 'hog')]
  base_individual_name <- nombres[str_detect(nombres, 'ind')]
  
  if (individual) {
    base_individual <<- read.table(unz(temp,base_individual_name), sep=";", dec=",", header = TRUE, fill = TRUE)
  }
  if (hogar) {
    base_hogar <<- read.table(unz(temp,base_hogar_name), sep=";", dec=",", header = TRUE, fill = TRUE) 
  }
  unlink(temp)
}

# Función que levanta bases, sea que estén en formato txt, sav o excel.
levantar_bases <- function(path){
  
  extension <- substr(path, start = nchar(path)-3, stop =nchar(path))
  
  if (extension == ".txt") {
    base <<- read.table(path, sep=";", dec=",", header = TRUE, fill = TRUE)
  }
  
  if (extension == ".sav") {
    base <<- read_spss(path)
  }

  if (extension == "xlsx") {
    base <<- read_xlsx(path)
  }

  if (extension == ".xls") {
    base <<- read_excel(path)
  }

}

# Funcion de redondeo para presentación (queda como character)
formato_porc <- function(numero, dec = 1){
  format(round(numero, digits = dec), nsmall = dec, decimal.mark = ",")
}

formato_pesos <- function(numero, dec = 2){
  format(round(numero, digits = dec), nsmall = dec, big.mark = ".", decimal.mark = ",")
}

formato_cantidad <- function(numero, dec = 0){
  format(round(numero, digits = dec), nsmall = dec, big.mark = ".", decimal.mark = ",")
}




















