library(dplyr)
library(sf)
library(tidyr)
library(lubridate)
library(tigris)
library(tidyselect)

options(scipen = 9999)

layers <- st_layers("C:/projectos/dizzi/datos_espaciales/resultados_buffer/buf_protestas2km.gpkg")

nombre <- layers$name

source("~/GitHub/protest-analysis/funcion_conteo_protestas_buffer.R")



funcion_conteo_protestas_buffer("C:/projectos/dizzi/datos_espaciales/resultados_buffer/buf_protestas2km.gpkg", nombre[[2]],
                                "C:/projectos/dizzi/datos_espaciales/res_conteo_protestas_buf/buffer 2000m")


for (i in nombre){
  print(i)
  funcion_conteo_protestas_buffer("C:/projectos/dizzi/datos_espaciales/resultados_buffer/buf_protestas2km.gpkg", i,
                                  "C:/projectos/dizzi/datos_espaciales/res_conteo_protestas_buf/buffer 2000m")
  
}

layers_1000 <- st_layers("C:/projectos/dizzi/datos_espaciales/resultados_buffer/buf_protestas_1km.gpkg")

nombre_1000 <- layers_1000$name

for (i in nombre_1000){
  print(i)
  funcion_conteo_protestas_buffer("C:/projectos/dizzi/datos_espaciales/resultados_buffer/buf_protestas_1km.gpkg", i,
                                  "C:/projectos/dizzi/datos_espaciales/res_conteo_protestas_buf/buffer 1000m")
  
}

