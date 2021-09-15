library(dplyr)
library(sf)
library(tidyr)
library(lubridate)
library(tigris)
library(tidyselect)

options(scipen = 9999)


st_layers("C:/projectos/dizzi/datos_espaciales/res_protestas/protestas_comuna.gpkg")

macro_protestas_comuna <- st_read(dsn = "C:/projectos/dizzi/datos_espaciales/res_protestas/protestas_comuna.gpkg",
                layer="macro_QUILICURA.shp,") # revisar como se escribieron o leyeron los nombres


macro_protestas_comuna ## Me falta la fecha de la protesta


#### importar datos cuestionarios #### 

setwd("C:/projectos/dizzi/csv")

ELSOC_complete_temporal <- read.csv(file = "ELSOC_complete_temporal.csv") %>% as_tibble() %>% 
  mutate(date = mdy(date)) %>%
  mutate(date_min1 = mdy(date_min1))%>% 
  mutate(date_min2 = mdy(date_min2))%>% 
  mutate(date_min3 = mdy(date_min3))%>% 
  mutate(date_min4 = mdy(date_min4))%>% 
  mutate(date_min6 = mdy(date_min6))%>% 
  mutate(date_min8 = mdy(date_min8))


# Vamos a hacer el join en el que relacionaremos cada cuestionario para cada proesta, juntando por comuna, debido a que la clave es cod_comuna 

ELSOC_protestas_feministas_comuna <- left_join(ELSOC_complete_temporal, macro_protestas_comuna, by = c("cod_comuna" = "p8")) # 1908907 casos

# Seleccionamos solo las variables que utilizaremos en el analisis 

ELSOC_protestas_feministas_comuna_sel <- select(ELSOC_protestas_feministas_comuna, ID_ELSOC, wave, cod_comuna, #date,
                                         date_min1, date_min2, date_min3, date_min4, #date_prot, 
                                         protestas, violento, feminista, viol_genero, 
                                         des_genero, non_fem, non_fem_viol, 
                                         non_fem_nonviol, fem_nonviol)#, #fem_viol) # En la seleccion del script macro falta femvio



# Vamos a reemplazar los NA con 0

ELSOC_protestas_feministas_sel_na <- replace_na(ELSOC_protestas_feministas_sel,
                                                list(violento = 0, feminista = 0, 
                                                     viol_genero = 0, des_genero = 0, non_fem = 0, non_fem_viol = 0, 
                                                     fem_nonviol = 0))#, #fem_viol = 0)
                                                     
                                                     
    # Aqui luego vienen las fechas. Volver a hacer la seleccion de las protestas pero seleccionar
# la fecha de la protesta como pro_date
                                                     