## Este script define dos funciones:
## Funcion macro_pais que permite crear macro comunas a partir de una comuna central.
## Funcion macro_protestas que permite identificar las protestas ocurridas en cada macro 
## comuna a partir de una interseccion espacial de ambos datos.


library(sf)
library(dplyr)
library(stringr)

macro_pais <- function(comuna, dir_salida){
  variable <- filter(chile, COMUNA == comuna)
  border <- st_intersects(chile, variable, sparse = FALSE)
  macro_comuna <- chile[border,]
  carpeta_salida <- dir_salida
  st_write(macro_comuna, dsn=paste(carpeta_salida,"/", "macro_comuna.gpkg", sep=""), 
           layer=paste("macro_",comuna,".shp", sep=""))
  
}

protestas_macro <- function(m_comuna, protestas, dir_entrada, dir_salida){
  setwd(dir_entrada)
  ma_comuna <- st_read(dsn ="C:/projectos/disi/datos_espaciales/resultados_macrocomuna/macro_comuna.gpkg", layer = m_comuna)
  variable <- st_combine(ma_comuna)
  protestas_macrocomuna <- st_intersection(protestas, variable)
  protestas_macrocomuna_p8 <- mutate(protestas_macrocomuna, macro_p8 = as.integer(str_extract(m_comuna, "[[:digit:]]+")))
  carpeta_salida <- dir_salida
  st_write(protestas_macrocomuna_p8, dsn=paste(carpeta_salida,"/", "protestas_comuna.gpkg", sep=""), 
           layer=paste("protesta_",m_comuna, sep=""))
  
}


buffer_centroides_elsoc <- function(manzanas,ELSOC_CHILE, buffer_dist, dir_salida){
  manzanas_elsoc_chile <- right_join(manzanas,ELSOC_CHILE, by = "MANZENT")
  cent_elsoc_chile <- st_centroid(manzanas_elsoc_chile)
  buf_elsoc_chile <- st_buffer(cent_elsoc_chile, dist = as.integer(buffer_dist))
  carpeta_salida <- dir_salida
  st_write(buf_elsoc_chile, dsn=paste(carpeta_salida,"/", paste("buf_elsoc_chile_",buffer_dist,".gpkg", sep=""), 
                                      layer=paste("buf_elsoc_chile_",buffer_dist,".shp", sep="")))
}



