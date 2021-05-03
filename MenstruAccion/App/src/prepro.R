library(tidyverse)

df <- readRDS("data/precios-gestion-menstrual-2021-03-12-limpio.RDS")

df <- df %>% group_by(Categoría, Marca) %>%
  summarise(Mean_precio = mean(precio_unidad)) %>%
  rename(Categoria = Categoría)

tampones <- df %>% filter(Categoria == "tampones")
toallitas <- df %>% filter(Categoria == "toallitas")

write.csv(tampones, "../ui-server-deploy/data_tampones_b.csv")
write.csv(toallitas, "../ui-server-deploy/data_toallitas_b.csv")
write.csv(df, "../ui-server-deploy/data_b.csv")
