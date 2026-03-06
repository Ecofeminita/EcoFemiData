# PAQUETES ####
library(tidyverse)
library(dplyr)
library(highcharter)
library(gsheet)
library(osmdata)
# library(georefar)
# library(xlsx)

# Datos Argentina ####
url <- "https://docs.google.com/spreadsheets/d/1XH_HTs7jUnlFsiQh-lRjvYq9GRWcsqWXErcA5pk8vVU/edit#gid=0"
archivo <- gsheet2text(url, format='csv')
data_argentina <- read_csv(archivo, skip = 0) %>% janitor::clean_names()

# Proyectos Nacionales
data_nacionales <- data_argentina %>% 
  filter(alcance == 'Nacional')

# Provinciales y Municipales
data_argentina <- data_argentina %>% 
  filter(!is.na(provincia)) %>% 
  mutate(provincia = case_when(
    provincia=='Provincia de Buenos Aires' ~ 'Buenos Aires',
    TRUE ~ provincia))

# Para traer coordenadas de OSM
data_municipios <- data_argentina %>%
  filter(!is.na(ciudad)) %>% 
  distinct(ciudad, provincia) %>% 
  group_by(ciudad) %>% 
  mutate(mun_prov = ifelse(ciudad != "Ciudad Autónoma de Buenos Aires", 
                           paste(ciudad, provincia, "Argentina", sep = ", "), 
                           paste(ciudad, "Argentina", sep = ", "))) %>% 
  ungroup() %>% 
  select(mun_prov)

# Data Municipios ####
# a <- 0
# for (i in unique(data$provincia)) {
#   a <<- 1 + a
#   municipios <<-  get_municipios(provincia = "Santiago del Estero", max = 1000)
#   
#   if(a == 1){
#     data_muni <<- municipios 
#   }else{
#     data_muni <<- bind_rows(data_muni, municipios)
#   }
# }
# rm(municipios, a, i)
# municipios <-  xlsx::read.xlsx2(file = "DASERPA/municipiosok.xlsx", sheetIndex = 1)
a <- 0
data_ <- data.frame(ciudad = NA,
                    lat = NA,
                    lon = NA)
for (i in unique(data_municipios$mun_prov)) {
  a <<- 1 + a
  bbox1 <<- getbb(i) %>% as.data.frame()
  bbox1 <- rownames_to_column(bbox1)
  bbox1 <- bbox1 %>% 
    group_by(rowname) %>% 
    mutate(prom = round((min+max)/2, 4)) %>% 
    ungroup()
  data_$lon <- bbox1[1,4] %>% as.character()
  data_$lat <- bbox1[2,4] %>% as.character()
  data_$ciudad <- i
  
  if(a==1){
    data_coord <<- data_ 
  }else{
    data_coord <<- bind_rows(data_coord, data_)
  }
}

data_coord <- data_coord %>% 
  mutate(ciudad = str_extract(ciudad, "((\\w+)|(\\w+ \\w+)|(\\w+ \\w+ \\w+)|(\\w+ \\w+ \\w+ \\w+)|(\\w+ \\w+ \\w+ \\w+ \\w+))(?=,)"))

# Etiquetas colores datos argentina
argentina <- get_data_from_map(download_map_data("countries/ar/ar-all"))

data_ <- data_argentina %>% 
  mutate(proyecto_provincia_level = case_when(alcance=='Provincial' & aprobado=='Si' ~ 3,
                                              alcance=='Provincial' & aprobado=='No' ~ 2,
                                              TRUE ~ 1),
         proyecto_municipio_level = case_when(alcance=='Municipal' & aprobado=='Si' ~ 2,
                                              alcance=='Municipal' & aprobado=='No' ~ 1)) %>% 
  group_by(provincia) %>% 
  mutate(proyecto_provincia_level = max(proyecto_provincia_level)) %>% 
  ungroup() %>% 
  group_by(ciudad) %>% 
  mutate(proyecto_municipio_level = max(proyecto_municipio_level)) %>% 
  ungroup()

data_provincias <- data_ %>% 
  filter(alcance %in% c('Provincial', NA)) %>% 
  select(-c(ciudad, pais, proyecto_municipio_level))

data_provincias[is.na(data_provincias)] <- "-"

# data_provincias <- data_ %>% 
#   filter(alcance=='Provincial')

data_categorias <- data_ %>% 
  distinct(proyecto_provincia_level) %>% 
  arrange(desc(proyecto_provincia_level)) %>% 
  mutate(name = case_when(proyecto_provincia_level==3 ~ "Aprobado",
                          proyecto_provincia_level==2 ~ "Presentado",
                          proyecto_provincia_level==1 ~ "Ninguno"),
         from = proyecto_provincia_level,
         to = from,
         color = c('#e5616e', '#ed9ca1', '#f8f5ee')) %>%
  select(-proyecto_provincia_level, name, from, to, color) %>% 
  list_parse()

# Datos Ciudades
ciudades <- data_ %>% 
  filter(!is.na(ciudad)) %>%
  left_join(data_coord, by = "ciudad")

ciudades[is.na(ciudades)] <- "-"

# Tema para plot
thm <- hc_theme(colors = c('red','green','blue'),
                chart = list(backgroundColor = "transparent"),
                title = list(style = list(color ='#494949',
                                          fontFamily = "Montserrat")),
                subtitle = list(style = list(color ='#494949',
                                             fontFamily = "Montserrat")),
                legend = list(itemStyle = list(fontFamily ='Montserrat',
                                               color ='black'),
                              itemHoverStyle = list(color ='gray'),
                              style = list(fontFamily ='Montserrat',
                                           color ='#030303')),
                tooltip = list(style = list(color ='#030303',
                                            fontFamily = "Montserrat")))
# Mapa Argentina ####
arg_mapa <- 
  hcmap("countries/ar/ar-all", 
        showInLegend = FALSE,
        data = data_provincias %>% filter(proyecto_provincia_level != 1),
        value = 'proyecto_provincia_level',
        name = 'Proyecto Provincial',
        joinBy = c('name', 'provincia'),
        borderColor = '#494949',
        tooltip = list(pointFormat = "<b>Provincia:</b> {point.provincia} <br>
                                                <b>Tipo:</b> {point.tipo_de_proyecto} <br>
                                                <b>Exige:</b> {point.exige} <br>
                                                <b>Aprobado:</b> {point.aprobado} <br>
                                                <b>Presentado por:</b> {point.presentado_por} <br>
                                                <b>Aclaraciones:</b> {point.comentarios}"),
        dataLabels = list(enabled = FALSE)) %>% 
  hc_colorAxis(showInLegend = TRUE,
               dataClassColor = "category",
               dataClasses = data_categorias) %>% 
  hc_add_series(data = ciudades,
                type = 'mappoint',
                name = 'Ordenanza Municipal',
                tooltip = list(pointFormat = "<b>Municipio:</b> {point.ciudad} <br>
                                              <b>Tipo:</b> {point.tipo_de_proyecto} <br>
                                              <b>Exige:</b> {point.exige} <br>
                                              <b>Aprobado:</b> {point.aprobado} <br>
                                              <b>Presentado por:</b> {point.presentado_por} <br>
                                              <b>Aclaraciones:</b> {point.comentarios}"),
                minSize = "1%",
                maxSize = "5%",
                color = '#d61326',
                showInLegend = FALSE) %>% 
 hc_legend(title = list(text = 'Proyectos Provinciales',
                         style = list(fontFamily = "Montserrat",
                                      fontWeight = "bold")),
            align = 'right',
            verticalAlign = 'middle',
            layout = 'vertical',
            x = -15) %>% 
  hc_tooltip(outside = TRUE,
             snap = -1) %>% 
  hc_plotOptions(series = list(stickyTracking = FALSE)) %>% 
  hc_caption(text = "Colaboración: Ecofeminita + EcoFemiData",
             align = "right",
             style = list(fontFamily = "Montserrat",
                          color = "#494949")) %>% 
  hc_add_theme(thm)
  # hc_mapNavigation(enabled = TRUE) %>% 
  # hc_title(text = 'Mapa MenstruAccion',
  #          align = 'left')

arg_mapa

htmlwidgets::saveWidget(arg_mapa, file = "./MenstruAccion/MapasMenstru/MenstruArg.html")

# Datos Mundo ####
url <- "https://docs.google.com/spreadsheets/d/1XH_HTs7jUnlFsiQh-lRjvYq9GRWcsqWXErcA5pk8vVU/edit#gid=1197174787"
archivo <- gsheet2text(url, format='csv')
data_mundo <- read_csv(archivo, skip = 0) %>% janitor::clean_names()
data_mundo[is.na(data_mundo)] <- "No"
data_mundo <- data_mundo %>% 
  mutate(proyecto_level = case_when(!is.na(tipo_de_proyecto) ~ 1,
                                    TRUE ~ 0))

data_categorias_mundo <- data.frame(proyecto_level = 0:1) %>% 
  distinct(proyecto_level) %>% 
  arrange(desc(proyecto_level)) %>% 
  mutate(name = case_when(proyecto_level==1 ~ "Política Pública Menstrual",
                          TRUE ~ "No Tiene"
                          # proyecto_level==1 ~ "Otra Política",
                          # proyecto_level==0 ~ "No Tiene"
                          ),
         from = proyecto_level,
         to = from,
         color = c('#e5616e', '#f8f5ee')) %>%
  select(-proyecto_level, name, from, to, color) %>% 
  list_parse()

# Mapa Mundo ####
mundo <- get_data_from_map(download_map_data("custom/world")) #custom/world-robinson-highres

mundo_mapa <- hcmap("custom/world", 
                    showInLegend = FALSE,
                    data = data_mundo,
                    value = 'proyecto_level',
                    name = 'MenstruAccion en el Mundo',
                    joinBy = c('name', 'country'),
                    borderColor = '#494949',
                    backgroundColor = '#f8f5ee',
                    tooltip = list(pointFormat = "<b>Pais:</b> {point.pais} <br>
                                                  <b>Tipo:</b> {point.tipo_de_proyecto} <br>
                                                  <b>Aclaraciones:</b> {point.comentarios}"),
                    dataLabels = list(enabled = FALSE)) %>% 
  hc_colorAxis(showInLegend = TRUE,
               dataClassColor = "category",
               dataClasses = data_categorias_mundo) %>% 
  hc_legend(title = list(text = 'Proyectos',
                         style = list(fontFamily = "Montserrat",
                                      fontWeight = "bold")),
            align = 'center') %>% 
  hc_tooltip(outside = TRUE,
             snap = -1) %>% 
  hc_plotOptions(series = list(stickyTracking = FALSE)) %>% 
  hc_caption(text = "Colaboración: Ecofeminita + EcoFemiData",
             align = "right",
             style = list(fontFamily = "Montserrat",
                          color = "#494949")) %>% 
  hc_add_theme(thm)
  # hc_mapNavigation(enabled = TRUE) %>% 
  # hc_title(text = 'Mapa MenstruAccion Mundo',
  #          align = 'left')

mundo_mapa

htmlwidgets::saveWidget(mundo_mapa, file = "./MenstruAccion/MapasMenstru/MenstruMundo.html")
