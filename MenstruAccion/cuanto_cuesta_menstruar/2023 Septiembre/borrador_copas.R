copas <- datos %>% filter(Categoría == "copa")

copas %>% group_by(Marca) %>%
  summarise(n=n())

copas %>% group_by(Provincia) %>%
  summarise(n=n()) %>%
  mutate(prop = n/sum(n)*100) %>%
  arrange(-prop)

datos %>% group_by(Provincia) %>%
  summarise(n=n()) %>%
  mutate(prop = n/sum(n)*100) %>%
  arrange(-prop)


copas %>% group_by(Provincia) %>% 
  summarise(precio_mean = median(Precio.de.lista))

copas %>% 
  summarise(precio_mean = median(Precio.de.lista))
