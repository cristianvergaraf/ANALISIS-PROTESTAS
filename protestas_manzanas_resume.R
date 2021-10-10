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

setwd("~/GitHub/protest-analysis")

source("manzanas_perdidas.R")

ELSOC_manzanas_unique <- unique(ELSOC_complete_temporal$manzana)
length(ELSOC_manzanas_unique)

registros_faltantes <- manzanas_perdidas(ELSOC_manzanas_unique,manzanas_sim$MANZENT, 1078)

length(registros_faltantes) ### 275 registro faltantes

ELSOC_manzanas_unique_sin_registros_faltantes <- ELSOC_manzanas_unique[!ELSOC_manzanas_unique %in% registros_faltantes]

length(ELSOC_manzanas_unique_sin_registros_faltantes)

ELSOC_sin_registros_manzanas_faltantes <- ELSOC_complete_temporal[!ELSOC_complete_temporal$manzana %in% registros_faltantes, ]

## Agregamos un campo para hacer el join
ELSOC_sin_registros_manzanas_faltantes$MANZENT <- ELSOC_sin_registros_manzanas_faltantes$manzana

# df_ELSOC_temuco_19_manz_INE <- data.frame(MANZENT = ELSOC_manzanas_unique_sin_registros_faltantes)


df_ELSOC_chile <- data.frame(MANZENT = ELSOC_manzanas_unique_sin_registros_faltantes)

## verificar funcion paso a paso primero ##

manzanas_elsoc_chile <- right_join(manzanas_sim,df_ELSOC_chile, by = "MANZENT")





buffer_centroides_elsoc <- function(manzanas,ELSOC_CHILE, buffer_dist, dir_salida){
  manzanas_elsoc_chile <- right_join(manzanas,ELSOC_CHILE, by = "MANZENT")
  cent_elsoc_chile <- st_centroid(manzanas_elsoc_chile)
  buf_elsoc_chile <- st_buffer(cent_elsoc_chile, dist = as.integer(buffer_dist))
  carpeta_salida <- dir_salida
  st_write(buf_elsoc_chile, dsn=paste(carpeta_salida,"/", paste("buf_elsoc_chile_",buffer_dist,".gpkg", sep=""), sep = ""), 
           layer=paste("buf_elsoc_chile_",buffer_dist,".shp", sep=""))
}



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

buffer_centroides_elsoc_for(manzanas_sim, df_ELSOC_chile,2000,"C:/projectos/dizzi/datos_espaciales/resultados_buffer" )


####################################################################################
####################################################################

setwd("C:/projectos/dizzi/datos_espaciales/resultados_buffer")

layers <- st_layers("C:/projectos/dizzi/datos_espaciales/resultados_buffer/for_buf_elsoc_chile_2000.gpkg")

nombre <- layers$name

nombres_buffer <- substr(nombre,1,nchar(nombre)-9)



protestas_buffer <- function(buffer, protestas, dir_salida, distancia){
  #setwd(dir_entrada)
  ma_manzana <- st_read(dsn ="C:/projectos/dizzi/datos_espaciales/resultados_buffer/for_buf_elsoc_chile_2000.gpkg", layer = buffer)
  ma_manzana_32718 <- st_transform(ma_manzana, crs = 32718)
  protestas_buffer <- st_intersection(protestas, ma_manzana_32718)
  protestas_buffer_p8 <- mutate(protestas_buffer, m_MANZENT = as.numeric(substr(buffer,1,nchar(buffer)-9)))
  carpeta_salida <- dir_salida
  #st_write(protestas_buffer_p8, dsn=paste(carpeta_salida,"/", "buf_protestas",distancia,".gpkg", sep=""), 
           #layer=paste("prot",buffer, sep=""))
}




"C:/projectos/dizzi/datos_espaciales/resultados_buffer"


for (i in 1:length(nombre)){
  protestas_buffer(nombre[i],protestas_feministas_32718,"C:/projectos/dizzi/datos_espaciales/resultados_buffer","2km")
}




library(dplyr)
library(sf)
library(tidyr)
library(lubridate)
library(tigris)
library(tidyselect)
library(stringr) 

options(scipen = 9999)

## Cargar protestas feministas

setwd("C:/projectos/dizzi/protestas feministas")


protestas_feministas <- read.csv(file = "protestas_feministas_nombre_comuna.csv") %>% as_tibble() %>%
  unite(fecha, c(p5a, p5b, p5c), sep="/")%>% mutate(date_prot = dmy(fecha), ID_prot = row_number()) %>% select(p8,NOM_COMUNA, ID_prot, date_prot, violento, feminista, viol_genero, des_genero, non_fem, 
                                                                                                               non_fem_viol,non_fem_nonviol, fem_nonviol,fem_viol,x,y) %>%
  mutate(protestas = 1) %>%replace_na(list(violento = 0, feminista = 0, viol_genero = 0, des_genero = 0, 
                                           non_fem = 0, non_fem_viol = 0, non_fem_nonviol = 0, fem_nonviol = 0,
                                           fem_viol = 0))

protestas_feministas_sf <- st_as_sf(protestas_feministas, coords = c("x", "y"), crs = 4326, agr = "constant")

protestas_feministas_32718 <- st_transform(protestas_feministas_sf, crs = 32718)

