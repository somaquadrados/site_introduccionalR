
# Paquetes --------------------------------------------------
library(tidyverse)
library(lubridate)

## Datos -----------------------------------------------------

# Dato de tipo 'date' (tiempo)
fecha <- c("2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01") %>%
  as.Date()

head(fecha)

class(fecha)

# Numero de individuos recorridos en campo
rural <- sample(15:60, 5)
urbano <- sample(5:15, 5)
bosque <- sample(30:100, 5)

# Todo en un data.frame
datos <- data.frame(rural, urbano, bosque, fecha) %>% # data frame con los datos
  pivot_longer(-fecha, # paa cambiar el formato (menos la columna fecha)
               names_to = "uso_tierra", # para los nombres para col "uso_tierra"
               values_to = "n_ind") # numero de individuos por uso de la tierra

head(datos) # tab final - tibble
names(datos) # variables en la tabla

## Graficos temp. ---------------------------------------------

# Grafico de linea
datos %>% 
  ggplot(aes(x = fecha, y = n_ind, color = uso_tierra)) +
  geom_line() + 
  geom_point() 

# Grafico barplot
datos %>% 
  ggplot(aes(x = fecha, y = n_ind, fill = uso_tierra)) +
  geom_bar(position="dodge", stat="identity")

# Grafico de área
datos %>% 
  ggplot(aes(x = fecha, y = n_ind, fill = uso_tierra)) +
  geom_area(alpha=0.5)

