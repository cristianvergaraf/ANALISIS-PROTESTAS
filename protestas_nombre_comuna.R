### El objetivo de este fichero es generar un fichero csv que identifique en que comuna 
## se encuentra cada protesta,  y seleccionar las variables mas importantes que serán 
## utilizada para el conteo del numero de protestas

library(dplyr)
library(sf)
library(tidyr)
library(lubridate)
library(tigris)
library(tidyselect)

options(scipen = 9999)

setwd("C:/projectos/dizzi/datos_espaciales")

comunas <- st_read(dsn = ".", layer = "comunas")

codigo_comunas_ine <- st_drop_geometry(select(comunas, NOM_COMUNA, COMUNA)) %>% mutate(across(where(is.factor), as.character)) %>%
 mutate(COMUNA = as.integer(COMUNA))

setwd("C:/projectos/dizzi/protestas femininas")


protestas_feministas <- read.csv(file = "OdeC_16_19_feminista.csv") 



protestas_feministas$protestas <- 1


protestas_nombre_comuna <- left_join(protestas_feministas, codigo_comunas_ine, by = c("p8"="COMUNA"))


protestas_feministas_nombre_comuna_na <- replace_na(protestas_nombre_comuna,list(violento = 0, feminista = 0, viol_genero = 0, des_genero = 0, 
                                                                non_fem = 0, non_fem_viol = 0, non_fem_nonviol = 0, fem_nonviol = 0,
                                                                fem_viol = 0)) %>%
  select( p8, NOM_COMUNA, p5a, p5b, p5c,x,y, protestas, violento, feminista, viol_genero, des_genero, 
         non_fem, non_fem_viol,non_fem_nonviol, fem_nonviol,fem_viol)



write.csv(protestas_feministas_nombre_comuna_na, "protestas_feministas_nombre_comuna.csv")

