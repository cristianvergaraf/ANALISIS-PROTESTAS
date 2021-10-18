##
## En este script vamos a utilizar la funcion macro_pais, que fue definida en el fichero macro_unidades.R, para generar a partir de la capa shape de todas las comunas de Chile
## macro unidades que contempla una comuna central, y todas las comunas vecinas a esta. Los resultados son exportardos a un geopackage, el nombre de cada macrocomuna
## incluye el codigo de la comuna central.
##

library(sp)
library(sf)
library(rgdal)
library(dplyr)

#################

setwd("~/github/protest-analysis")

source("macro_unidades.R")

setwd("C:/projectos/disi/datos_espaciales")

chile <- st_read(dsn = ".", layer = "comunas")

chile$COMUNA
########################################################

# Ahora aplicamos la funcion para todas las comunas y obtemos un shape 
# para cada comuna, que es exportado a un geopackage creado en el directorio indicado.

comunas <- as.character(chile$COMUNA)

for (i in 1:length(comunas)){
  macro_pais(comunas[i],"C:/projectos/disi/datos_espaciales/resultados_macrocomuna")
}

# 
