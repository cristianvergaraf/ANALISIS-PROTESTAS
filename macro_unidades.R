library(sf)
library(dplyr)


#### Esta funcion define un area de influencia a partir de un shape seleccionado

#### compuesto por las unidades que colindan con nuestra seleccion ####

macro_pais <- function(comuna, dir_salida){
  variable <- filter(chile, NOM_COMUNA == comuna)
  border <- st_intersects(chile, variable, sparse = FALSE)
  macro_comuna <- chile[border,]
  carpeta_salida <- dir_salida
  st_write(macro_comuna, paste(carpeta_salida,"/", "macro_",comuna,".shp", sep=""), update = TRUE)
  
}

#####

protestas_macro <- function(m_comuna, protestas, dir_entrada, dir_salida){
  setwd(dir_entrada)
  ma_comuna <- st_read(dsn =".", layer = m_comuna)
  variable <- st_combine(ma_comuna)
  protestas_macrocomuna <- st_intersection(protestas_feministas_3857, variable)
  carpeta_salida <- dir_salida
  st_write(protestas_macrocomuna, dsn=paste(carpeta_salida,"/", "protestas_comuna.gpkg", sep=""), 
           layer=paste(m_comuna,".shp,", sep=""))
  
}

