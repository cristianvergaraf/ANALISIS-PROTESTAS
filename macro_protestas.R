##
## EL objetivo de este script es identificar las protestas ocurridas en las macro comunas identificadas anteriormente ##
## para esto aplicarame los función protestas_macro definida en el script de macro_unidades.R

library(dplyr)
library(sf)
library(tidyr)
library(lubridate)
library(tigris)
library(tidyselect)
library(stringr) 

options(scipen = 9999)

setwd("C:/projectos/disi/protestas feministas")

# cargar datos protestas, crear variable fechas date_prot, y un ID_prot asociadao a cada propuesta
protestas_feministas <- read.csv(file = "protestas_centroide.csv") %>% as_tibble()  %>% 
  select(p8,NOM_COMUNA, ID_prot, date_prot, violento, feminista, viol_genero, des_genero, non_fem, 
                                                                                       non_fem_viol,non_fem_nonviol, fem_nonviol,fem_viol,x,y, x_completa, y_completa) %>%
  mutate(protestas = 1) %>%replace_na(list(violento = 0, feminista = 0, viol_genero = 0, des_genero = 0, 
                                     non_fem = 0, non_fem_viol = 0, non_fem_nonviol = 0, fem_nonviol = 0,
                                     fem_viol = 0)) 

# Transformamos a objeto espacial basado en las coordenadas completadas con los centroides
# de las comunas para los casos sin datos

# dos comunas con codigo de nuble provincia

#View(filter(protestas_feministas, is.na(protestas_feministas$x_completa)))

#protestas_feministas_16REG <- mutate(protestas_feministas, p8_16 = ifelse(cod_comuna == 8404, 16203,ifelse(cod_comuna==8413,16107,cod_comuna))) %>% 
  #relocate(.after = cod_comuna, cod_comuna16)


protestas_feministas_completas <- filter(protestas_feministas, !is.na(protestas_feministas$x_completa))


protestas_feministas_sf <- st_as_sf(protestas_feministas_completas, coords = c("x_completa", "y_completa"), crs = 4326, agr = "constant")

protestas_feministas_3857 <- st_transform(protestas_feministas_sf, crs = 3857)

setwd("~/github/protest-analysis")

source("macro_unidades.R")

setwd("C:/projectos/disi/datos_espaciales/resultados_macrocomuna")

layers <- st_layers("C:/projectos/disi/datos_espaciales/resultados_macrocomuna/macro_comuna.gpkg")
nombre <- layers$name

nombres_macro <- substr(nombre,1,nchar(nombre)-4)

for (i in nombre){
  protestas_macro(i,protestas_feministas_3857, "C:/projectos/disi/datos_espaciales/resultados_macrocomuna",
                  "C:/projectos/disi/datos_espaciales/resultados_macroprotestas")
}




