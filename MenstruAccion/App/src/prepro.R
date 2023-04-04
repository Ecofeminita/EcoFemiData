library(tidyverse)

df <- readRDS("data/precios-gestion-menstrual-2023-03-19-limpio.RDS")

df <- df %>% group_by(Categoría, Marca) %>%
  summarise(Mean_precio = mean(precio_unidad)) %>%
  rename(Categoria = Categoría)

tampones <- df %>% filter(Categoria == "tampones")
toallitas <- df %>% filter(Categoria == "toallitas")

write.csv(tampones, "../menstruaccion_app/data_tampones_b.csv")
write.csv(toallitas, "../menstruaccion_app/data_toallitas_b.csv")
write.csv(df, "../menstruaccion_app/data_b.csv")
