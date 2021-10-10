## Cargar librerias necesarias

library(dplyr)
library(sf)
library(tidyr)
library(lubridate)
library(tigris)
library(tidyselect)
library(stringr) 



### Este script tiene el objetivo de crea un area buffer alrededor de las manzanas 
### donde se realizaron entrevistas el soc

setwd("C:/projectos/dizzi/csv")

ELSOC_complete_temporal <- read.csv(file = "ELSOC_complete_temporal.csv") %>% as_tibble() %>% 
  mutate(date = mdy(date))%>% 
  mutate(date_min1 = mdy(date_min1))%>% 
  mutate(date_min2 = mdy(date_min2))%>% 
  mutate(date_min3 = mdy(date_min3))%>% 
  mutate(date_min4 = mdy(date_min4))%>% 
  mutate(date_min6 = mdy(date_min6))%>% 
  mutate(date_min8 = mdy(date_min8))

setwd("C:/projectos/dizzi/manzanas")

manzanas <- st_read(dsn = ".",  layer = "Microdatos_Censo_20173A_Manzana")

manzanas_sim <- select(manzanas, MANZENT, MANZANA,geometry)

# Las manzanas a UTM 18s
manzanas_sim_32718 <- st_transform(manzanas_sim, crs = 32718)

# cargamos la funcion manzanas perdida
setwd("~/GitHub/protest-analysis")

source("manzanas_perdidas.R")

## Todas las manzanas donde se aplicó el cuestionario ELSOC
ELSOC_manzanas_unique <- unique(ELSOC_complete_temporal$manzana)
length(ELSOC_manzanas_unique)

# Con esto podemos identificar el numero de manzanas que no existen en la base de datos ELSOC
registros_faltantes <- manzanas_perdidas(ELSOC_manzanas_unique,manzanas_sim$MANZENT, 1078)

length(registros_faltantes) ### 275 registro faltantes

# Esto me da las manznaas unicas sin registros faltantes en formato vector
ELSOC_manzanas_unique_sin_registros_faltantes <- ELSOC_manzanas_unique[!ELSOC_manzanas_unique %in% registros_faltantes]

length(ELSOC_manzanas_unique_sin_registros_faltantes) # 803 registros 

# Ahora seleccionamos los registros de ELSOC que solo tienen las manzanas que existen 
# En la cartografía del INE
ELSOC_sin_registros_manzanas_faltantes <- ELSOC_complete_temporal[!ELSOC_complete_temporal$manzana %in% registros_faltantes, ]

## Agregamos un campo para hacer el join
ELSOC_sin_registros_manzanas_faltantes$MANZENT <- ELSOC_sin_registros_manzanas_faltantes$manzana

# Transformamos a data frame el vector con los valore unicos de manzanas que tienen correspondencia con el 
# el INE
df_ELSOC_chile <- data.frame(MANZENT = ELSOC_manzanas_unique_sin_registros_faltantes)

# ahora creamos una función para generar el buffer considerando un buffer aplicado a 
# a cada manzana que existe.

# Este primera funcion entrega como resultado una capa donde se encuentran todas las 
# entidades de los buffer

buffer_centroides_elsoc <- function(manzanas,ELSOC_CHILE, buffer_dist, dir_salida){
  manzanas_elsoc_chile <- right_join(manzanas,ELSOC_CHILE, by = "MANZENT")
  cent_elsoc_chile <- st_centroid(manzanas_elsoc_chile)
  buf_elsoc_chile <- st_buffer(cent_elsoc_chile, dist = as.integer(buffer_dist))
  carpeta_salida <- dir_salida
  st_write(buf_elsoc_chile, dsn=paste(carpeta_salida,"/", paste("buf_elsoc_chile_",buffer_dist,".gpkg", sep=""), sep = ""), 
           layer=paste("buf_elsoc_chile_",buffer_dist,".shp", sep=""))
}

## Esta segunda funcion se asemeja más al flujo de trabajo que tienen 

buffer_centroides_elsoc_for <- function(manzanas,ELSOC_CHILE, buffer_dist, dir_salida){
  manzanas_elsoc_chile <- right_join(manzanas,ELSOC_CHILE, by = "MANZENT")
  for (i in 1:length(ELSOC_CHILE$MANZENT)){
    cent_elsoc_chile <- st_centroid(manzanas_elsoc_chile[i,])
    buf_elsoc_chile <- st_buffer(cent_elsoc_chile, dist = as.integer(buffer_dist))
    carpeta_salida <- dir_salida
    st_write(buf_elsoc_chile, dsn=paste(carpeta_salida,"/", paste("for_buf_elsoc_chile_",buffer_dist,".gpkg", sep=""), sep = ""), 
             layer=paste(ELSOC_CHILE[i,],buffer_dist,"shp", sep="."))
  }
 
}

## Con esto ejecutamos el buffer

## Primer caso 1000 m
buffer_centroides_elsoc_for(manzanas_sim_32718, df_ELSOC_chile,1000,"C:/projectos/dizzi/datos_espaciales/resultados_buffer" )
## Segundo caso 2000 m
buffer_centroides_elsoc_for(manzanas_sim_32718, df_ELSOC_chile,2000,"C:/projectos/dizzi/datos_espaciales/resultados_buffer" )


buffer_centroides_elsoc_for(manzanas_sim_32718, df_ELSOC_chile,5000,"C:/projectos/dizzi/datos_espaciales/resultados_buffer" )

buffer_centroides_elsoc_for(manzanas_sim_32718, df_ELSOC_chile,10000,"C:/projectos/dizzi/datos_espaciales/resultados_buffer" )

####################################################################################
####################################################################
