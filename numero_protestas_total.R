### Autor: Cristian Vergara

## Objetivos: 

## A partir del número de protestas encontradas en las comunas de los cuestionarios ELSOC, y las comunas aledañas, contar el número de protestas que ocurrieron en un 
## en un plazo de un mes, tres semanas, dos semanas, y una semana antes de la aplicación de un cuestionario de actitudes.

# leer datos del geopackage

layers <- st_layers("C:/projectos/dizzi/datos_espaciales/res_protestas/protestas_comuna.gpkg")

# leer nombes para la iteración
macro_names <- layers$name

# modificar nombres
names <- substr(macro_names,1,nchar(macro_names)-4)

setwd("~/github/protest-analysis")

source("funcion_conteo_protestas_macrocomuna.R")

# Bucle para contar el numero de protestas utilizando la funcion (funcion_conteo_protestas_macrocomunas)
for (i in macro_names){
  funcion_conteo_protestas_macrocomuna("C:/projectos/dizzi/datos_espaciales/res_protestas/protestas_comuna.gpkg",
                                       i,"C:/projectos/dizzi/datos_espaciales/res_protestas_tiempo")
  
}

##########
# De los resultados anteriores vamos a genrar un solo fichero utilizando la función load_data
setwd("~/github/protest-analysis")

source(merge_csv_function.R)

protestas_comunas_chile_completo <- load_data("C:/projectos/dizzi/datos_espaciales/res_protestas_tiempo")
 
write.csv(protestas_comunas_chile_completo, file = "C:/projectos/dizzi/datos_espaciales/res_protestas_tiempo/protestas_comunas_chile_completo.csv")


################################
# II Parte
# Relacionar la tabla de cuestionarios de actitud, con el número de protestas para los distintos umbrales de tiempo.  

setwd("C:/projectos/dizzi/datos_espaciales/res_protestas_tiempo")
protestas_chile_completo <- read.csv(file = "protestas_comunas_chile_completo.csv")

setwd("C:/projectos/dizzi/csv")

ELSOC_complete_temporal <- read.csv(file = "ELSOC_complete_temporal.csv", stringsAsFactors = FALSE) %>% as_tibble() %>% 
  mutate(date = mdy(date)) %>%
  mutate(date_min1 = mdy(date_min1))%>% 
  mutate(date_min2 = mdy(date_min2))%>% 
  mutate(date_min3 = mdy(date_min3))%>% 
  mutate(date_min4 = mdy(date_min4))%>% 
  mutate(date_min6 = mdy(date_min6))%>% 
  mutate(date_min8 = mdy(date_min8))


# Clave común la clave ID_ELSOC
ELSOC_complete_protestas <- left_join(ELSOC_complete_temporal, protestas_chile_completo, by = "ID_ELSOC") %>%
  select(-c("X","X.1"))

setwd("C:/projectos/dizzi/protestas feministas")

write.csv(ELSOC_complete_temporal, file = "ELSOC_complete_protestas_macro.csv")



