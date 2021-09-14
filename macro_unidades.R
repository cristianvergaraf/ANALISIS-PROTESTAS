library(sf)
library(dplyr)


#### Esta funcion define un area de influencia a partir de un shape seleccionado

#### compuesto por las unidades que colindan con nuestra seleccion ####

macro_pais <- function(comuna, dir_salida){
  chile <- st_read(dsn =".", layer = "chile")
  variable <- filter(chile, NOM_COM == comuna)
  border <- st_intersects(chile, variable, sparse = FALSE)
  macro_comuna <- chile[border,]
  carpeta_salida <- dir_salida
  st_write(macro_comuna, paste(carpeta_salida,"/", "macro_",comuna,".shp", sep=""), update = TRUE)
  
}

#####

protestas_macro <- function(m_comuna, directorio){
  vacio <-list()
  setwd(directorio)
  ma_comuna <- st_read(dsn =".", layer = m_comuna)
  variable <- st_combine(ma_comuna)
  border <- st_intersects(protestas_chile, variable, sparse = FALSE)
  macro_comuna <- protestas_chile[border,]
  carpeta_salida <- "E:/RD/resultados_protestas"
  st_write(macro_comuna, paste(carpeta_salida,"/", "prot_",m_comuna,".shp", sep=""), update = TRUE)
  
}