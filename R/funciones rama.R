
# FUNCIONES PARA RAMAS


### 1. Función que "limpia" la variable PP04B_COD para que tenga 2 o 4 caracteres.

corregir.rama <- function(columna = PP04B_COD){
  columna <- as.character(columna)
  columna <- case_when(nchar(columna) == 1 ~ paste0("0", columna),
                       nchar(columna) == 2 ~ columna,
                       nchar(columna) == 3 ~ paste0("0", columna),
                       nchar(columna) == 4 ~ columna)
}

################################################################################################

### 2. Función que opera sobre el código de 2 o 4 dígitos, y devuelve 22 categorías según el
###    Clasificador de Actividades Económicas para Encuestas Sociodemográficas CAES - Mercosur 1.0.

rama.caes1.0 <- function(variable = rama){
  rama.caes <- case_when(
    ## A --> Agricultura, caza, silvicultura y pesca.
    (variable %in% c("01", "0101", "0102", "0103", "0104", "0105", 
                     "02", "0200", "03", "0300")) ~ 1,
    ## B --> Explotación de minas y canteras.
    (variable %in% c("05", "0500", "500", "06", "0600", "07", "0700", "08",
                     "0800", "09", "0900")) ~ 2,
    ## C --> Industria Manufacturera.
    (variable %in% c("10", "1001", "1002", "1003", "1009", "11", "1100", "12", 
                     "1200", "13", "1300", "14", "1400", "15", "1501", "1502",
                     "16", "1600", "17", "1700", "18", "1800", "19", "1901",
                     "1902", "20", "2001", "2002", "2009", "21", "2100", "22",
                     "2201", "2202", "23", "2301", "2309", "24", "2400", "25",
                     "2500", "26", "2601", "2602", "2603", "2604", "27", "2701",
                     "2709", "28", "2800", "29", "2900", "30", "3001", "3002",
                     "3003", "3009", "31", "3100", "32", "3200", "33", "3300")) ~ 3,
    ## D --> Suministro de electricidad, gas, vapor y aire acondicionado.
    (variable %in% c("35", "3501", "3502")) ~ 4,
    ## E --> Suministro de agua; alcantarillado, gestión de desechos y actividades de saneamiento.
    (variable %in% c("36", "3600", "37", "3700", "38", "3800", "39", "3900")) ~ 5,
    ## F --> Construcción.
    (variable %in% c("40", "4000")) ~ 6,
    ## G --> Comercio al por mayor y al por menor; reparaciòn de vehículos automotores y motocicletas.
    (variable %in% c("45", "4501", "4502", "4503", "4504", "48", "4801", "4802",
                     "4803", "4804", "4805", "4806", "4807", "4808", 
                     "4809", "4810", "4811")) ~ 7,
    ## H --> Transporte y almacenamiento.
    (variable %in% c("49", "4901", "4902", "4903", "4904", "4905", "4909", "50",
                     "5000", "51", "5100", "52", "5201", "5202", "53", "5300")) ~ 8,
    ## I --> Alojamiento y servicios de comidas.
    (variable %in% c("55", "5500", "56", "5601", "5602")) ~ 9,
    ## J --> Información y comunicación.
    (variable %in% c("58", "5800", "59", "5900", "60", "6000", "61", "6100",
                     "62", "6200", "63", "6300")) ~ 10,
    ## K --> Actividades financieras y de seguros.
    (variable %in% c("64", "6400", "65", "6500", "66", "6600")) ~ 11,
    ## L --> Actividades inmobiliarias.
    (variable %in% c("68", "6800")) ~ 12,
    ## M --> Actividades profesionales, científicas y ténicas.
    (variable %in% c("69", "6900", "70", "7000", "71", "7100", "72", "7200", "73",
                     "7301", "7302", "74", "7400", "75", "7500")) ~ 13,
    ## N --> Actividades administrativas y servicios de apoyo.
    (variable %in% c("77", "7701", "7702", "78", "7800", "79", "7900", "80", "8000",
                     "81", "8101", "8102", "82", "8200")) ~ 14,
    ## O --> Administración pública y defensa; planes de seguro social obligatorio.
    (variable %in% c("83", "8300","84", "8401", "8402", "8403")) ~ 15,
    ## P --> Enseñanza.
    (variable %in% c("85", "8501", "8509")) ~ 16,
    ## Q --> Salud humana y servicios sociales.
    (variable %in% c("86", "8600", "87", "8700", "88", "8800")) ~ 17,
    ## R --> Artes, entretenimiento y recreación.
    (variable %in% c("90", "9000", "91", "9100", "92", "9200", "93", "9301", "9302")) ~ 18,
    ## S --> Otras actividades de servicios.
    (variable %in% c("94", "9401", "9402", "9409", "95", "9501", "9502", "9503",
                     "96", "9601", "9602", "9603", "9606", "9609")) ~ 19,
    ## T --> Actividades de los hogares como empleadores de personal doméstico; actividades de los hogares como productores de bienes o servicios para uso propio.
    (variable %in% c("97", "9700", "98", "9800")) ~ 20,
    ## U --> Actividades de organizaciones y organismos extraterritoriales.
    (variable %in% c("99", "9900")) ~ 21,
    ## V-Z --> Descripción de Actividad Vacía / Actividad no Especificada claramente.
    variable == "9999" ~ 22)
}

################################################################################################

### 3. Función que opera sobre el código de 22 categorías, y devuelve la codificación de rama de 
###    actividad según la agrupación de EPH (14 categorías numeradas)

rama.caes1.0.pub <- function(variable = rama.caes){
  rama.eph <- case_when(
    variable == 1  | variable == 2 ~ 1,                     # Actividades primarias
    variable == 3  ~ 2,                                     # Industria manufacturera
    variable == 6  ~ 3,                                     # Construcción
    variable == 7  ~ 4,                                     # Comercio
    variable == 9  ~ 5,                                     # Hoteles y restaurantes
    variable == 8  | variable == 10 ~ 6,                    # Transporte, almacenamiento y comunicaciones
    variable >= 11 & variable <= 14 ~ 7,                    # Servicios financieros, de alquiler y empresariales
    variable == 15 ~ 8,                                     # Administración pública, defensa y seguridad social
    variable == 16 ~ 9,                                     # Enseñanza
    variable == 17 ~ 10,                                    # Servicios sociales y de salud
    variable == 20 ~ 11,                                    # Servicio doméstico
    variable == 18 | variable == 19 ~ 12,                   # Otros servicios comunitarios, sociales y personales
    variable == 5  | variable == 5  | variable == 21 ~ 13,  # Otras ramas
    variable == 22 | variable == 4 ~ 14)                    # Actividades no bien especificadas
}

################################################################################################

### 4. Función que opera sobre la rama numerada EPH y devuelve la rama según su descripción
###    (14 categorías nombradas). Es tipo factor y tiene los labels definidos según el orden 
###    del comunicado.

rama.eph.nombre <- function(variable = rama.eph){
  rama.nombre <- case_when(
    variable == 1  ~ "Actividades primarias",
    variable == 2  ~ "Industria manufacturera",
    variable == 3  ~ "Construccion",
    variable == 4  ~ "Comercio",
    variable == 5  ~ "Hoteles y restaurantes",
    variable == 6  ~ "Transporte, almacenamiento y comunicaciones",
    variable == 7  ~ "Servicios financieros, de alquiler y empresariales",
    variable == 8  ~ "Administracion publica, defensa y seguridad social",
    variable == 9  ~ "Ensenanza",
    variable == 10 ~ "Servicios sociales y de salud",
    variable == 11 ~ "Servicio domestico",
    variable == 12 ~ "Otros servicios comunitarios, sociales y personales",
    variable == 13 ~ "Otras ramas",
    variable == 14 ~ "Actividades no bien especificadas")
  
  rama.nombre <- factor(rama.nombre, 
                        levels = c("Actividades primarias", 
                                   "Industria manufacturera",
                                   "Construccion",
                                   "Comercio",
                                   "Hoteles y restaurantes",
                                   "Transporte, almacenamiento y comunicaciones",
                                   "Servicios financieros, de alquiler y empresariales",
                                   "Administracion publica, defensa y seguridad social",
                                   "Ensenanza",
                                   "Servicios sociales y de salud",
                                   "Servicio domestico",
                                   "Otros servicios comunitarios, sociales y personales",
                                   "Otras ramas",
                                   "Actividades no bien especificadas"))
  
}

################################################################################################

### 5. Función que directamente opera sobre la PP04B_COD y devuelve la rama con descripción directamente

entra.cuchillo.salen.las.ramas <- function(variable = PP04B_COD){
  
  rama.nombre <- corregir.rama(variable)
  rama.nombre <- rama.caes1.0(rama.nombre)
  rama.nombre <- rama.caes1.0.pub(rama.nombre)
  rama.nombre <- rama.eph.nombre(rama.nombre)
  
}

################################################################################################

### 6. Función que corrige los 7 casos del 4to trimestre 2016 que tienen un código (9109) que no existe y por tanto tiene rama CAES Missing.
###    Corrige directo sobre rama.eph y rama.nombre, no sobre las intermedias (rama y rama.caes). 
###    Se etiquetan como "12. Otros servicios comunitarios, sociales y personales"

correcion.7.casos <- function(base = base){
  
  base %>% 
    mutate(rama.eph = case_when(PP04B_COD == "9109" ~ 12,
                                TRUE ~ rama.eph),
           rama.nombre = rama.eph.nombre(rama.eph))
  
}



