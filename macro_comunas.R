####
## Este program ejecuta las funciones que creamos 

library(sp)
library(sf)
library(rgdal)
library(dplyr)

#################

setwd("~/github/protest-analysis")

source("macro_unidades.R")

setwd("C:/projectos/dizzi/datos_espaciales")

chile <- st_read(dsn = ".", layer = "comunas")

chile$COMUNA
########################################################

macro_pais("8312","C:/projectos/dizzi/datos_espaciales/resultados" )

##### Ahora debemos aplicar el calcola para todas las comunas y obtemos un shape para cada comuna. 

comunas <- as.character(chile$COMUNA)

for (i in 1:length(comunas)){
  macro_pais(comunas[i],"C:/projectos/dizzi/datos_espaciales/resultados")
}

# 