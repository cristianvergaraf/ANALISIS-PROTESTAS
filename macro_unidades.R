library(sf)
library(dplyr)
library(stringr)



#### Esta funcion define un area de influencia a partir de un shape seleccionado

#### compuesto por las unidades que colindan con nuestra seleccion ####

macro_pais <- function(comuna, dir_salida){
  variable <- filter(chile, COMUNA == comuna)
  border <- st_intersects(chile, variable, sparse = FALSE)
  macro_comuna <- chile[border,]
  carpeta_salida <- dir_salida
  #st_write(macro_comuna, paste(carpeta_salida,"/", "macro_",comuna,".shp", sep=""), update = TRUE)
  st_write(macro_comuna, dsn=paste(carpeta_salida,"/", "macro_comuna.gpkg", sep=""), 
           layer=paste("macro_",comuna,".shp", sep=""))
  
}
#####

protestas_macro <- function(m_comuna, protestas, dir_entrada, dir_salida){
  setwd(dir_entrada)
  ma_comuna <- st_read(dsn ="C:/projectos/dizzi/datos_espaciales/resultados/macro_comuna.gpkg", layer = m_comuna)
  variable <- st_combine(ma_comuna)
  protestas_macrocomuna <- st_intersection(protestas, variable)
  protestas_macrocomuna_p8 <- mutate(protestas_macrocomuna, macro_p8 = str_extract(m_comuna, "[[:digit:]]+"))
  carpeta_salida <- dir_salida
  st_write(protestas_macrocomuna_p8, dsn=paste(carpeta_salida,"/", "protestas_comuna.gpkg", sep=""), 
           layer=paste(m_comuna, sep=""))
  
}
