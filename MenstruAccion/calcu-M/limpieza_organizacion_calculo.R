# 1. Limpieza y organizacion de los PGM----

## Librerias ----
options(scipen=100, digits=4)
options(warn = -1) # ignoro warnings
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(ggridges))
suppressPackageStartupMessages(library(viridis))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(scales))


## Insumos ----
# datos <- read.csv("MenstruAccion/Calcu-M/insumos/precios-gestion-menstrual.csv", header = TRUE, sep = ",", dec = ".",  fill = TRUE, encoding = "UTF-8")
datos_tamp <- read.csv("MenstruAccion/Calcu-M/insumos/precios-tampones.csv", header = TRUE, sep = ",", dec = ".",  fill = TRUE, encoding = "UTF-8")
datos_toall <- read.csv("MenstruAccion/Calcu-M/insumos/precios-toallitas.csv", header = TRUE, sep = ",", dec = ".",  fill = TRUE, encoding = "UTF-8")
datos_protect <- read.csv("MenstruAccion/Calcu-M/insumos/precios-protectores-diarios.csv", header = TRUE, sep = ",", dec = ".",  fill = TRUE, encoding = "UTF-8")
datos_protect_inc <- read.csv("MenstruAccion/Calcu-M/insumos/precios-protectores-incontinencia.csv", header = TRUE, sep = ",", dec = ".",  fill = TRUE, encoding = "UTF-8")
# datos_protect_inc <- datos_protect_inc %>% filter(!grepl("Pan", Nombre))

datos <- bind_rows(datos_tamp, datos_toall, datos_protect, datos_protect_inc)
# datos <- bind_rows(datos, datos_protect, datos_protect_inc)

rm(datos_tamp, datos_toall, datos_protect, datos_protect_inc)

regiones <- openxlsx::read.xlsx("MenstruAccion/Calcu-M/insumos/provincias_regiones.xlsx", sheet = 1)

datos$Marca <- trimws(datos$Marca) # quito espacios sobrantes en los nombres de las marcas (da problemas para el summarise)

(total_observaciones <- nrow(datos))

datos <- datos %>% mutate(Categoría = case_when(
  str_detect(Nombre, "Copa") == TRUE ~ "copa",
  TRUE ~ Categoría
))

# datos <- datos %>% filter(Categoría != "copa") # vamos a prorratear precios de copas
# total_sin_copas <- nrow(datos)

#Saco cantidades de cada categoría para automatizar

categ <- datos %>% 
  count(Categoría) %>% 
  arrange(-n) %>% 
  mutate(porcentaje = percent(n/sum(n))) 

cant_toallitas <- categ %>% filter(Categoría == "toallitas") %>% pull(n)
cant_tampones <- categ %>% filter(Categoría == "tampones") %>% pull(n)
cant_protectores <- categ %>% filter(Categoría == "protectores diarios") %>% pull(n)
cant_protectores_inc <- categ %>% filter(Categoría == "protectores para incontinencia") %>% pull(n)
cant_copas <- categ %>% filter(Categoría == "copa") %>% pull(n)

# Vista previa del dataset:
head(datos)

# Vista previa del diccionario de regiones:
head(regiones)

# función que nos va a devolver un número en formato character, con cero dígitos, con un punto para separar los miles, y una coma para separar los decimales.
cant <- function(x){
  format(x, digits = NULL, big.mark = ".", decimal.mark = ",")}

# función que nos va a automatizar la fecha y año en que estamos realizando la actualización
mes_anio_es <- function(fecha = today()) {
  Sys.setlocale("LC_TIME", "es_ES.UTF-8")  # en Linux/Mac, en Windows puede ser: "Spanish_Spain" o "es-ES"
  
  tools::toTitleCase(format(fecha, "%B %Y"))
}

# Guardamos el valor de la fecha a la que corresponde el ejercicio.
date <- mes_anio_es()

# 2. Exploración de los datos ----
# Al dataframe `datos` le pegamos los valores de las regiones, usando como variable de identificación a la `Provincia`. A continuación podemos borrar de nuestro entorno de trabajo el elemento `regiones`. 
datos <- left_join(datos, regiones, by = "Provincia")
rm(regiones)

names(datos) # Variables con las que contamos
cant(nrow(datos)) # Observaciones

## Categorías ----
categ

perc_toallitas <- categ %>% filter(Categoría == "toallitas") %>% pull(porcentaje)
perc_tampones <- categ %>% filter(Categoría == "tampones") %>% pull(porcentaje)
perc_protectores <- categ %>% filter(Categoría == "protectores diarios") %>% pull(porcentaje)
perc_protectores <- categ %>% filter(Categoría == "protectores para incontinencia") %>% pull(porcentaje)
perc_copa <- categ %>% filter(Categoría == "copa") %>% pull(porcentaje)

# Cabe destacar que esta composición corresponde a la disponibilidad de precios, no refleja la estructura del consumo. 
#Lo que sí sabemos es que la página de Precios Claros puede tener un sesgo por no incluir los comercios tradicionales 
#sino únicamente grandes cadenas.

## Marcas ----
marcas <- datos %>% 
  count(Marca) %>% 
  arrange(-n) %>% 
  mutate(porcentaje = n/sum(n),
         acumulado = cumsum(porcentaje),
         porcentaje = percent(porcentaje),
         acumulado = percent(acumulado)
         )

marcas

# Con 10 de las marcas se completa casi el 90% del total de productos. 
# Las primeras 3 marcas concentran más de la mitad de las observaciones. 
# Nuevamente aquí hay que tener en cuenta que no se trata exactamente de una "concentración de mercado".

## Presentación ----
datos %>% 
  count(Presentación) %>% 
  arrange(-n) %>% 
  mutate(porcentaje = n/sum(n),
         acumulado = cumsum(porcentaje),
         porcentaje = percent(porcentaje),
         acumulado = percent(acumulado))

# Más del 80% de los productos vienen en paquetes de 8 o 16 unidades. 
# Luego seguiremos analizando esta variable en particular, porque nos interesa para calcular los precios unitarios de los productos, 
# y hay valores (como "1.0 un") que son raros y, en caso de tratarse de errores, podrían arrastrar problemas hacia el cálculo de esos precios.

## Provincias ----
prov <- datos %>% 
  count(Provincia) %>% 
  arrange(-n) %>% 
  mutate(porcentaje = n/sum(n),
         acumulado = cumsum(porcentaje),
         porcentaje = percent(porcentaje),
         acumulado = percent(acumulado))

prov

# Obtengo valores para automatizar

concentracion_amba <- prov %>% filter(Provincia == "CABA" | Provincia == "Buenos Aires") %>% 
  filter(n == min(n)) %>% pull(acumulado)
concentracion_amba

provincias_min_casos <- prov %>% filter(n == min(n)) %>% pull(Provincia)

# Si bien un gran porcentaje de las observaciones se concentran en Buenos Aires y CABA, 
# es importante saber que se cuenta con información de todas las provincias del país, y que incluso en el peor de los casos 
# se trata de una cantidad pasible de proveernos estimaciones útiles.

## Regiones ----
r <- datos %>% 
  count(Region) %>% 
  arrange(-n) %>% 
  mutate(porcentaje = n/sum(n),
         acumulado = cumsum(porcentaje),
         porcentaje = percent(porcentaje),
         acumulado = percent(acumulado))
r

# En términos regionales seguimos viendo esa asimetría en la disponibilidad de datos, 
# pero hay que tener en cuenta que efectivamente hay diferencias de escala de mercados en términos regionales.

## Precio de lista (graficos) ----
# Ahora graficamos la distribución de los precios de lista (o sea, sin tener en cuenta las unidades por paquete) según cada categoría. 
# Para ello utilizamos `ggplot2`. 

# En la primer línea de código referida a `ggplot`, tomamos como fuente el dataset que venimos utilizando (a eso refiere el punto), 
# en el `eje x` queremos los precios, en el `eje y` queremos cada categoría, y el relleno de cada parte del gráfico también según 
# las categorías de productos. En la segunda línea aclaramos que queremos un gráfico de densidad 
#(que es como un histograma pero en versión continua), la escala hace a la superposición de los graficos y el `bandwidth` o "ancho de banda" 
# indica la extensión del intervalo de valores con los que se va a realizar la estimación del gráfico (o sea, si el gráfico va a ser más 
# detallista y por ende más "ruidoso", o más suave y "redondeado"). Se puede jugar con ambos valores para ver sus efectos. 
# En la tercer línea aclaramos que queremos un tema minimalista, en la cuarta que la paleta de colores la vamos a asignar de manera manual, 
# en la quinta que no queremos una leyenda aclarando los colores de cada categoría porque ya están aclaradas en el eje y. Y por último aclaramos 
# las etiquetas necesarias.

datos %>% 
  ggplot(., aes(x = Precio.de.lista, y = Categoría, fill = Categoría)) +
  geom_density_ridges(scale = 3, bandwidth = 20) +
  theme_minimal() +
  scale_fill_manual(values = c("orange", "red", "violetred", "green", "blue")) +
  theme(legend.position = "none") +
  labs(title = "Precio de lista de productos de gestión menstrual según categoría",
       subtitle = date,
       x = "Precio de lista",
       y = "",
       caption = "Fuente: #MenstruAcción")

# Por esta vez, filtramos de los datos los precios mayores a 3.000 pesos, 
# basicamente porque hay casos (outliers que de hecho vamos a tener que corregir) que superan ese monto y "rompen" la escala del gráfico. 
# Para una comprensión más acabada de este efecto, veamos qué sucede si quitamos estos valores que hasta superan los 3.000 pesos.
datos_copas <- datos %>% filter(Categoría == "copa")
datos <- datos %>% 
  filter(Precio.de.lista < 6000 & Categoría != "copa") #saco los outliers mayores a 3000 porque son demasiado altos. 

# El resto va a ser controlado a la hora de calcular la media de precios de forma "podada"

datos %>% 
  ggplot(., aes(x = Precio.de.lista, y = Categoría, fill = Categoría)) +
  geom_density_ridges(scale = 3, bandwidth = 20) +
  theme_minimal() +
  scale_fill_manual(values = c("orange", "red", "violetred", "green")) +
  theme(legend.position = "none") +
  labs(title = "Precio de lista de productos de gestión menstrual según categoría",
       subtitle = date,
       x = "Precio de lista",
       y = "",
       caption = "Fuente: #MenstruAcción")

# A primera vista, los niveles de precios de toallitas y tampones se distribuyen en una misma escala. 
# En el caso de los tampones hay una distribución bimodal, que puede deberse a la amplia cantidad de presentaciones de 8 y 16 unidades.

# También podemos graficar los precios de cada categoría por regiones:
datos %>% 
  ggplot(., aes(x = Precio.de.lista, y = Region, fill = Categoría, alpha = Region)) +
  geom_density_ridges(scale = 2, bandwidth = 20) + # el bandwidth anterior estaba en 10
  theme_minimal() +
  scale_fill_manual(values = c("orange", "red", "violetred", "green"))+
  facet_wrap(. ~ Categoría) +
  theme(legend.position = "none") +
  labs(title = "Precio de lista de productos de gestión menstrual según categoría y región",
       subtitle = date,
       x = "Precio de lista",
       y = "",
       caption = "Fuente: #MenstruAcción")

# En este caso se encuentran diferencias más que nada entre los tampones, no así en el caso de las toallitas. 
# Dado el peso de GBA entre las observaciones, tiene sentido que la distribución general se asemeje a la de dicha región. 
# En este caso, Cuyo presenta la distribución más anómala, pero podemos pensar que esto se debe a la baja cantidad de casos que tenemos registrados. 

#3. Limpieza de la cantidad de unidades (presentación) ----
# Como mencionábamos anteriormente, es necesario prestarle particular atención a la variable de `Presentación`, 
# que indica la cantidad de unidades por cada paquete. En primer lugar, teniendo en cuenta que está expresada como "X un.", 
# modificamos la variable para quedarnos únicamente con las unidades en formato numérico. Esto lo hacemos convirtiendo la variable al 
# tipo character, y luego quedándonos con aquellos caracteres entre la primer posición y aquella que se encuentra 4 posiciones 
# por detrás de la última (omitiendo así ".0 un"). En la tabla `presentacion_nros` resumimos el comportamiento de esta nueva variable `unidades`.

datos <- datos %>% 
  mutate(Presentación = as.character(Presentación),
         unidades = as.numeric(substr(Presentación, 1, nchar(Presentación)-4)))

presentacion_nros <- datos %>% 
  count(unidades) %>% 
  arrange(-n) %>% 
  mutate(porcentaje = n/sum(n),
         acumulado = cumsum(porcentaje),
         porcentaje = percent(porcentaje),
         acumulado = percent(acumulado))

presentacion_nros

# Sin embargo, en la base puede verse que hay casos que, de acuerdo a la descripción en `Nombre`, 
# tienen mal la cantidad de unidades en `Presentacion` (y por ende en unidades), afectando el calculo de precios unitarios.
# Utilizando expresiones regulares podemos quedarnos con los dígitos, sean uno o más, que están seguidos de "Un" o "un" en la variable `Nombre`. 
# Nos quedamos con esta información en la variable `unidades_regex` y resumimos su comportamiento.

datos <- datos %>% 
  mutate(unidades_regex = as.numeric(str_extract(Nombre, '\\d+(?=\\s*([Uu]n|[Cc]u)?)')))

presentacion_regex <- datos %>% 
  count(unidades_regex) %>% 
  arrange(-n) %>% 
  mutate(porcentaje = n/sum(n),
         acumulado = cumsum(porcentaje),
         porcentaje = percent(porcentaje),
         acumulado = percent(acumulado))
presentacion_regex

# Nótese que hay pequeñas diferencias respecto del cuadro anterior. 
# En este caso, las unidades se encuentran más concentradas en valores populares. 
# Podemos identificar cuándo ambas informaciones (la contenida en `unidades` y en `unidades_regex`) coinciden y cuándo no. 
# Creamos la variable `igual` que toma valor TRUE cuando son iguales, y FALSE cuando no. 
# A continuación, en la tabla `comparo_unidades` se realiza un conteo de observaciones para ambas variables, 
# se identifican las situaciones de coincidencia y diferencia y se ordenan los datos para ver primero las diferencias, según la magnitud.

datos <- datos %>% 
  mutate(igual = case_when(unidades == unidades_regex ~ T,
                           unidades != unidades_regex ~ F))

comparo_unidades <- datos %>% 
  count(unidades, unidades_regex) %>% 
  mutate(igual = case_when(unidades == unidades_regex ~ T,
                           unidades != unidades_regex ~ F)) %>% 
  arrange(igual, -n)

comparo_unidades

comparo_unidades %>% filter(igual == FALSE) %>% summarise(n=sum(n)) # si != 0 ~ hay casos que fueron informados sin una especificación de unidades clara.

cants_iguales <- datos %>% 
  group_by(igual) %>% 
  summarise(n = n()) %>% 
  mutate(porc = percent(n/sum(n)))

cants_iguales # si NA ~ casos en que no había información sobre la cantidad de unidades en la descripción del artículo.

# Distribucion de los datos que queremos sacar entre las regiones
datos %>% 
  filter(igual == FALSE) %>% 
  count(Region)

# Distribucion de los datos que queremos sacar entre las marcas
datos %>% 
  filter(igual == FALSE) %>% 
  count(Marca)

# Este codigo esta anulado. Es lo que estaba en el ejercicio anterior
# unidades.faltantes <- datos %>% 
#   filter(is.na(unidades.regex)) %>% 
#   select(Nombre, unidades.regex, Presentación, unidades) %>% 
#   group_by(Nombre, unidades.regex, Presentación, unidades) %>% 
#   summarise(n())
# 
# datos <- datos %>% 
#   filter(!is.na(unidades.regex))
# 
# kable(unidades.faltantes) %>% 
#   kable_styling(bootstrap_options = "striped",
#                 full_width = F)
# 
# verif <- datos %>% 
#   filter(unidades.regex != unidades) %>% 
#   group_by(Nombre, unidades, unidades.regex) %>% 
#   summarise(n=n()) %>% 
#   arrange(-n)
# 
# kable(verif) %>% 
#   kable_styling(bootstrap_options = "striped",
#                 full_width = F)

# Esta vez, dado que el porcentaje de aquellos casos en que no coinciden `unidades` y `unidades_regex`, junto a los casos en que `unidades_regex` figura como `NA`, alcanzan solamente un porcentaje muy bajo, se prescindirá de ellos. 
if(length(cants_iguales %>% filter(igual == FALSE) %>% pull(porc))!=0){
  datos <- datos %>% 
    filter(igual == TRUE)
}

# Obervaciones luego de la limpieza
nrow(datos)

# Descarto las variables que no voy a usar
datos <- datos %>% 
  select(-Presentación, -unidades_regex, -igual)


# Extraigo las unidades de las copas
datos_copas <- datos_copas %>% 
  mutate(Presentación = as.character(Presentación),
         unidades = as.numeric(substr(Presentación, 1, nchar(Presentación)-4))) %>% 
  select(-Presentación)


#4. Cálculo del precio por unidad ----
# Creo la variable `precio_unidad`, dividiendo el `Precio.de.lista` por las `unidades`.
datos <- datos %>% 
  bind_rows(datos_copas) %>% 
  mutate(precio_unidad = round(Precio.de.lista/unidades, 2))

## Gráficos del precio por unidad ----
# Ahora podemos observar la distribución de esta nueva variable con los mismos gráficos que utilizamos antes.
ggplot(datos, aes(x = precio_unidad, y = Categoría, fill = Categoría)) +
  geom_density_ridges(scale = 2, bandwidth = 1.25) + # el bandwidth anterior estaba en 10
  theme_minimal() +
  scale_fill_manual(values = c("orange", "red", "violetred", "green", "blue")) +
  theme(legend.position = "none") +
  labs(title = "Precio por unidad de productos de gestión menstrual según categoría",
       subtitle = date,
       x = "Precio por unidad",
       y = "",
       caption = "Fuente: #MenstruAcción")

# Esta vez, para realizar el gráfico por categorías y regiones, podemos ordenar a estas últimas según su precio promedio en el gráfico. 
# Para ello construimos un vector que las aloje en orden, llamado `reg_ordenadas`. 
# Luego, pisamos la variable `Region` en los datos para que sea de tipo factor, y el orden de la misma esté determinado según el vector 
# previamente definido.
reg_ordenadas <- datos %>% 
  group_by(Region) %>% 
  summarise(promedio = mean(precio_unidad)) %>% 
  arrange(promedio) %$% Region

datos <- datos %>% 
  mutate(Region = factor(Region, levels = reg_ordenadas))

ggplot(datos, aes(x = precio_unidad, y = Region, fill = Categoría, alpha = Region)) +
  geom_density_ridges(scale = 2, bandwidth = 1.25) + # el bandwidth anterior estaba en 1
  theme_minimal() +
  scale_fill_manual(values = c("orange", "red", "violetred", "green", "blue"))+
  facet_wrap(. ~ Categoría, scales = 'free') +
  theme(legend.position = "none") +
  labs(title = "Precio por unidad de productos de gestión menstrual según categoría y región",
       subtitle = date,
       x = "Precio por unidad",
       y = "",
       caption = "Fuente: #MenstruAcción")

# 2,5 % de los extremos de la distribución
# Adicionalmente, como el cálculo lo realizaremos agregando los precios de acuerdo a una media alpha podada con alpha = 2,5% 
# (o sea, ignorando los valores superiores e inferiores para evitar la intrusión de outliers a pesar de la limpieza), 
# presentamos gráficos que justamente muestran las "colas" de la distribución que se estarían obviando.
ggplot(datos, aes(x = precio_unidad, y = Categoría, fill = factor(..quantile..))) +
  stat_density_ridges(geom = "density_ridges_gradient", 
                      calc_ecdf = TRUE, quantiles = c(0.025, 0.975),
                      bandwidth = 1.25, scale = 1.5) + # El bandwidth anterior estaba en 0.75
  scale_fill_manual(name = "Probabilidad", 
                    values = c("violetred1", "red2", "violetred1"),
                    labels = c("2,5 %", "95,0 %", "2,5 %")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Precio por unidad de productos de gestión menstrual según categoría",
       subtitle = paste(date),
       x = "Precio por unidad",
       y = "",
       caption = "Fuente: #MenstruAcción")

# Realizamos el mismo gráfico por cada provincia, ya que esa será la primer unidad de agregación para el cálculo total a nivel nacional.
prov_ordenadas <- datos %>% 
  group_by(Provincia) %>% 
  summarise(promedio = mean(precio_unidad)) %>% 
  arrange(promedio) %$% 
  Provincia

datos <- datos %>% 
  mutate(Provincia = factor(Provincia, levels = prov_ordenadas))

ggplot(datos, aes(x = precio_unidad, y = Provincia, 
                  fill = factor(..quantile..))) +
  stat_density_ridges(geom = "density_ridges_gradient", 
                      calc_ecdf = TRUE, quantiles = c(0.025, 0.975),
                      bandwidth = 1.25, scale = 2) +
  scale_fill_manual(name = "Probabilidad", 
                    values = c("violetred1", "red2", "violetred1"),
                    labels = c("2,5 %", "95,0 %", "2,5 %")) +
  #scale_x_continuous(limits = c(0, 30)) + # El limite anterior estaba en 20
  facet_wrap(. ~ Categoría) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Precio por unidad de productos de gestión menstrual según categoría y provincia",
       subtitle = paste(date, ". "),
       x = "Precio por unidad",
       y = "",
       caption = "Fuente: #MenstruAcción")

# Finalmente, guardamos esta nueva versión del dataset en formato .RDS para continuar en el siguiente script con el cálculo de cuánto cuesta menstruar.
saveRDS(datos, file = "MenstruAccion/Calcu-M/insumos/precios-gestion-menstrual-limpio.RDS")

#5. Calculo costo anual de menstruar ----
# datos <- readRDS("MenstruAccion/Calcu-M/insumos/precios-gestion-menstrual-limpio.RDS")

# menstruan <- readxl::read_excel("MenstruAccion/Calcu-M/insumos/poblaciones.xls") %>% select(Provincia, Menstruan) # Para sacar las columnas con comentarios
# menstruan %>% mutate(total = sum(Menstruan))
menstruan <- openxlsx::read.xlsx("MenstruAccion/Calcu-M/insumos/poblaciones_2022_2040_mujeres_edad.xlsx", sheet = 1)

# Se carga la estimación, por cada provincia, de personas que menstrúan. Como está aclarado en la carpeta de insumos, 
# esto sale de las proyecciones poblacionales del INDEC, teniendo en cuenta que la menarca se estima en los 13 años y la menopausia en 49 años. 
# Estos datos se van a utilizar para ponderar el precio promedio de cada provincia a la hora de agregar en un indicador nacional. 
cants_categorias <- datos %>% group_by(Categoría) %>% summarise(n=n())

# Calculamos el total a nivel nacional de personas que menstrúan, sumando los datos de todas las provincias. 
# Y, dividiendo los datos de cada provincia por dicho total, obtenemos las proporciones que nos van a servir para ponderar 
# los promedios de precio según el peso de cada una de las provincias. Como guardo estas modificaciones bajo el mismo nombre, 
#la información será pisada en la misma tabla `menstruan`, de la que se vuelve a presentar una vista previa.
menstruan <- menstruan %>% 
  mutate(total = sum(Menstruan),
         pond = Menstruan/total) %>% 
  select(Provincia, pond)

head(menstruan)

# Además, considerando que hay outliers (valores atípicos) en los precios unitarios, a la hora de calcular los promedios de precios provinciales 
# se remueven "las colas" de la distribución, utilizando una media alfa podada: alfa = 2.5%  

## Cálculo y Resultados ----
# Procedemos a calcular los precios promedio de toallitas y tampones. 
# Para el caso del total nacional, primero habrá que calcular los precios promedio (por unidad) de cada provincia, 
# para luego agregarlos en un promedio general. Una vez que tenemos precios representativos de toallitas y tampones, 
# podremos estimar un costo anual por persona de acuerdo a las estimaciones de cuántos ciclos por año presentan las personas que menstrúan, 
# y cuántos productos utilizan por ciclo.

### Total Nacional ----

#### Toallitas ----
# Promedio del precio por unidad

# Para calcular el precio promedio de cada provincia tomamos los datos, los filtramos para quedarnos únicamente con las toallitas, 
# agrupando las observaciones según la variable `Provincia`. Con `summarise()` podemos definir indicadores que agregan la información para 
# cada grupo, así que definimos el promedio provincial como una media podada. Es en el parámetro `trim` que se aclara la proporción de los 
# datos que se podan. Finalmente, anexamos en una nueva columna los ponderadores de cada provincia, así toda la información necesaria para 
# el cálculo del promedio nacional se encuentra en la misma tabla.

toallitas <- datos %>% 
  filter(Categoría == "toallitas") %>%
  group_by(Provincia) %>% 
  summarise(prom = mean(precio_unidad, trim = 0.025)) %>% 
  left_join(., menstruan, by = "Provincia")

head(toallitas)

# Luego, el valor promedio para el total nacional se calcula como una media ponderada. 
#La función `weighted.mean()` nos permite aclarar la variable a promediar, y la variable que funciona como ponderador.

valor_toallitas <- weighted.mean(toallitas$prom, toallitas$pond)

# Gasto por año por persona

# El gasto anual por persona se calcula teniendo en cuenta:
# - ciclos por año: 13
# - productos utilizados por ciclo: 22
# (Ambos datos se encuentran aclarados en las `Fuentes`)

ciclos_por_anio <- 13
productos_por_ciclo <- 22
productos_por_anio <- ciclos_por_anio * productos_por_ciclo

gasto_por_anio_toallitas <- valor_toallitas * productos_por_anio

#### Tampones ----
# Promedio del precio por unidad

# Se realiza un procedimiento semejante para el caso de los tampones.
tampones <- datos %>% 
  filter(Categoría == "tampones") %>%
  group_by(Provincia) %>% 
  summarise(prom = mean(precio_unidad, trim = 0.025)) %>% 
  left_join(., menstruan, by = "Provincia")

valor_tampones <- weighted.mean(tampones$prom, tampones$pond)

# Gasto por año por persona

# El gasto anual por persona se calcula teniendo en cuenta:
# - ciclos por año: 13
# - productos utilizados por ciclo: 22

gasto_por_anio_tampones <- valor_tampones * productos_por_anio

### Por provincia ----
# Para obtener los datos de cada provincia el procedimiento es aún más sencillo. 
# En la variable `gasto_por_anio`, cada precio promedio se multiplica por la cantidad de productos que se utilizan anualmente. 
# Por una cuestión de exposición, se quitan de la tabla las columnas con aquellos precios promedios por unidad, y los ponderadores provinciales.

#### Toallitas ----

# Gasto por año por persona

toallitas <- toallitas %>% 
  mutate(gasto_por_anio = round(prom * productos_por_anio, 2)) %>% 
  select(-prom, -pond) %>% 
  arrange(gasto_por_anio)

toallitas %>%
  DT::datatable(
    extensions = 'Buttons', 
    options = list(dom = 'Bfrtip', 
                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))

#### Tampones ----

# Gasto por año por persona

tampones <- tampones %>% 
  mutate(gasto_por_anio = round(prom * productos_por_anio, 2)) %>% 
  select(-prom, -pond) %>% 
  arrange(gasto_por_anio)

tampones %>% 
  DT::datatable(
    extensions = 'Buttons', 
    options = list(dom = 'Bfrtip', 
                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))

## Aumentos ----
# Como esta es la onceava vez que realizamos este ejercicio de estimación semestral, podemos ver cómo evolucionaron los precios de toallitas 
# y tampones en el último año. También, con los datos del Índice de Precios al Consumidor (IPC - INDEC), 
# se puede realizar una comparación entre la evolución de estos productos de gestión menstrual y el nivel general de precios, 
# el rubro alimentos y bebidas no alcohólicas, y el de Salud. 
# Ambas series se levantan desde archivos de extensión .xlsx en las tablas `serie_inflacion` y `precios_pgm`.

serie_inflacion <- readxl::read_excel("MenstruAccion/Calcu-M/insumos/serie_inflacion.xlsx")
precios_pgm <- read_csv("MenstruAccion/Calcu-M/insumos/serie_precios_pgm_viejo.csv")

# A continuación, agrego los precios actuales de toallitas y tampones a la serie histórica y la guardo. 

precios_pgm <- precios_pgm %>% 
  add_row(
    Mes = as.Date(paste(year(today()), month(today())-1, "01", sep = "-")),
    Toallitas = round(valor_toallitas, 2),
    Tampones = round(valor_tampones, 2)
  )


write_csv(precios_pgm, 'MenstruAccion/Calcu-M/insumos/serie_precios_pgm_nuevo.csv')

head(serie_inflacion)
head(precios_pgm)

# Asegurándonos de que los datos se encuentran ordenados por fecha, podemos transformar ambas series para que se encuentren expresadas 
# con el mes de marzo 2019 = 100. De esta forma podremos comparar la evolución de ambas. 
# Esto lo hacemos modificando las series con las que contamos dividiendo todos los valores por el primero de ellos (y multiplicando por 100). 
# La selección del primer elemento de la serie se realizó con la función `first()`.

base100 <- serie_inflacion %>% 
  mutate(`Nivel General` = `Nivel General`/first(`Nivel General`) * 100,
         `Alimentos y bebidas no alcohólicas` = `Alimentos y bebidas no alcohólicas`/
           first(`Alimentos y bebidas no alcohólicas`) * 100,
         Salud = Salud/first(Salud) * 100)

head(base100)

pgm_100 <- precios_pgm %>% 
  mutate(Toallitas = Toallitas/first(Toallitas) * 100,
         Tampones = Tampones/first(Tampones) * 100)

head(pgm_100)

# Podemos anexar la evolución de los precios de los productos de gestión menstrual a la serie de inflación, 
# por más que esta última tenga valores mensuales y nuestros datos se hayan recolectado cada seis meses, 
# utilizando `Mes` como variable de link. Luego reordenamos la información para que todas las variables, excepto `Mes`, 
# se presenten en una variable `Categoría` (con valores "Nivel General", "Salud", "Toallitas", etc.), y los valores de cada serie se agrupen 
# en la variable `Indice`. No habrá datos en diversos meses para toallitas y tampones, así que filtramos esas filas vacías, y finalmente 
# creamos la variable `Grupo` para identificar como "IPC" a las categorías del índice de precios al consumidor, y como "PGM" a los productos 
# de gestión menstrual.

# Esto se puede correr "de a poco" para ver cómo cambian las cosas en cada paso
base100 <- base100 %>% 
  left_join(., pgm_100, by = "Mes") %>% 
  pivot_longer(-Mes, names_to = "Categoria", values_to = "Indice") %>% 
  filter(!is.na(Indice)) %>% 
  mutate(Grupo = case_when(Categoria %in% c("Nivel General", "Alimentos y bebidas no alcohólicas", "Salud") ~ "IPC",
                           Categoria %in% c("Toallitas", "Tampones") ~ "PGM"))

head(base100)

# A fines de graficar esta información, por esta vez se prescinde de la serie de "Alimentos y bedidas no alcohólicas" 
# porque no agrega tanta información en la comparabilidad, y sí ocupa espacio visual que puede obstruir la comparación de las demás series.
base100bis <- base100 %>% 
  filter(Categoria != "Alimentos y bebidas no alcohólicas")

# Para el gráfico puedo definir un vector de colores que sirva para distinguir las series de IPC (en la gama de azules) y 
# las de productos de gestión menstrual (en la gama del rojo).
colorcitos <- c('royalblue4', 'royalblue', 
                'violetred', 'red')

# Finalmente, definimos nuestro gráfico. Tomando la tabla `base100bis`, 
suppressPackageStartupMessages(library(ggrepel))

ggplot(base100bis, aes(x = Mes, y = Indice, group = Categoria, color = Categoria)) +
  geom_line(data = base100bis %>% filter(Grupo == "PGM"), size = 1, alpha = 1) +
  geom_line(data = base100bis %>% filter(Grupo == "IPC"), alpha = 1, size = 1) +
  geom_point(data = base100bis %>% filter(Grupo == "PGM"), size = 2) +
  theme_minimal() +
  theme(legend.position = "top",
        text = element_text(size = 13)) +
  scale_color_manual(values = colorcitos) +
  expand_limits(x = as.POSIXct(c("2019-03-01", "2026-04-01"))) + 
  labs(title = "Inflación de Toallitas y Tampones vs. Salud y Nivel General del IPC-INDEC",
       subtitle = "Marzo 2019 - Marzo 2026", #<------------------------------------------------- CAMBIAR
       x = "",
       y = "Indice de precios",
       caption = "Fuente: #MenstruAcción y #EcoFemiData (EcoFeminita)") +
  scale_x_datetime(breaks = seq(as.POSIXct('2019-03-01'), by='6 months', length = 13), #<------------------------ CAMBIAR
                   labels = c("03-2019", "09-2019", "03-2020", "09-2020", "03-2021", "09-2021", "03-2022", "09-2022", "03-2023", "09-2023", "03-2024", "09-2024", "03-2025")) #<------------- CAMBIAR

