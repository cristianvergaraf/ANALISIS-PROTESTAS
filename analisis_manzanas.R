

# https://www.censo2017.cl/
# http://www.censo2017.cl/microdatos/
# Considera una variable llave (key) que permite incorporar esta información como una capa en
# la cartografía censal. Esa variable es ID_ MANZENT

## ID_MANZENT_15R Identificador de manzana/entidad
## Llave o identificador único de manzanas o entidades con la
## división geográfica antes de Ñuble.

## ID_MANZENT Identificador de manzana/entidad Llave 
## o identificador único de manzanas o entidades
## https://www.ine.cl/herramientas/portal-de-mapas/geodatos-abiertos
## https://www.ine.cl/herramientas/portal-de-mapas/geodatos-abiertos
## https://geoine-ine-chile.opendata.arcgis.com/search?q=manzanas
## https://geoine-ine-chile.opendata.arcgis.com/search?q=manzana

setwd("C:/projectos/disi/Censo2017_16R_ManzanaEntidad_CSV")

censo <- read.csv("Censo2017_Manzanas.csv", sep = ";")
head(censo)

library(dplyr)
options(scipen = 9999)
censo_sel <- select(censo, ID_MANZENT, ID_MANZENT_15R)

length(unique(censo_sel$ID_MANZENT)) # 180499 En total
length(unique(censo_sel$ID_MANZENT_15R)) # 180499 Total

head(censo_sel)

filter(censo_sel, ID_MANZENT_15R == 1101031004002)

setwd("C:/projectos/disi/Censo2017_16R_ManzanaEntidad_CSV/Censo2017_Identificación_Geográfica")

microdato_censo2017 <- read.csv("Microdato_Censo2017-Geografia_Manzanas.csv", sep = ";")

length(unique(microdato_censo2017$ID_MANZENT)) # Tenemos 180499 manzanas

filter(microdato_censo2017, ID_MANZENT == 1101031004002)

### Los siguientes son los datos espaciales que usamos descargados de la pagina
## geodatos INE


setwd("C:/projectos/disi/manzanas")

manzanas <- st_read(dsn = ".",  layer = "Microdatos_Censo_20173A_Manzana")

manzanas_sim <- select(manzanas, MANZENT, MANZANA,geometry)

manzanas_INE_20173A <- unique(manzanas_sim$MANZENT)


manzanas_chile_20173A <- data.frame(MANZENT_INE_20173A = manzanas_INE_20173A)
manzanas_chile_20173A

length(unique(manzanas_chile_20173A$MANZENT_INE_20173A))

######################################################

setwd("C:/projectos/disi/manzanas")

manzanas_indeterminada <- st_read(dsn = ".",  layer = "Microdatos_Manzana_Indeterminada")

length(unique(manzanas_indeterminada$MANZENT_I))

manzanas_indeterminada$MANZENT_I

######

setwd("C:/projectos/dizzi/manzanas")

manzanas <- st_read(dsn = ".",  layer = "Microdatos_Censo_20173A_Manzana")

manzanas_sim <- select(manzanas, MANZENT, MANZANA,geometry)

manzanas_INE_20173A <- unique(manzanas_sim$MANZENT)


manzanas_chile_20173A <- data.frame(MANZENT_INE_20173A = manzanas_INE_20173A)
manzanas_chile_20173A

length(unique(manzanas_chile_20173A$MANZENT_INE_20173A))# 151545

setwd("C:/projectos/disi/csv")

ELSOC_complete_temporal <- read.csv(file = "ELSOC_complete_temporal.csv") %>% as_tibble() %>% 
  mutate(date = mdy(date))%>% 
  mutate(date_min1 = mdy(date_min1))%>% 
  mutate(date_min2 = mdy(date_min2))%>% 
  mutate(date_min3 = mdy(date_min3))%>% 
  mutate(date_min4 = mdy(date_min4))%>% 
  mutate(date_min6 = mdy(date_min6))%>% 
  mutate(date_min8 = mdy(date_min8))

##############################################
##############################################
# Analysis Manzanas #
# https://geoine-ine-chile.opendata.arcgis.com/
# https://www.ine.cl/herramientas/portal-de-mapas/geodatos-abiertos

ELSOC_unique_manzanas <- unique(ELSOC_complete_temporal$manzana)

setwd("~/GitHub/protest-analysis")

source("manzanas_perdidas.R")

ELSOC_manzanas_unique <- unique(ELSOC_complete_temporal$manzana)

registros_faltantes_sim_manzent <- manzanas_perdidas(ELSOC_manzanas_unique,manzanas_sim$MANZENT, 1078)
length(registros_faltantes_sim_manzent)

#################################################################


# Con esto podemos identificar el numero de manzanas que no existen en la base de datos ELSOC
  registros_faltantes <- manzanas_perdidas(ELSOC_manzanas_unique,manzanas_sim$MANZENT, 1078)

  length(unique(manzanas_sim$MANZENT)) # 151545
  length(registros_faltantes) # 275
  
  registros_faltantes_ind <- manzanas_perdidas(ELSOC_manzanas_unique,manzanas_indeterminada$MANZENT_I, 1078)
  
  length(unique(manzanas_indeterminada$MANZENT_I)) # 151545
  length(registros_faltantes_ind) # 275
  
  
  registros_faltantes_ine_microdato <- manzanas_perdidas(ELSOC_manzanas_unique, microdato_censo2017$ID_MANZENT, 1078)
  registros_faltantes_ine_microdato_15R <- manzanas_perdidas(ELSOC_manzanas_unique, microdato_censo2017$ID_MANZENT_15R, 1078)
  
  length(unique(microdato_censo2017$ID_MANZENT)) # 180499
  length(unique(microdato_censo2017$ID_MANZENT_15R)) # 180499
  
  length(registros_faltantes_ine_microdato) # 275
  length(registros_faltantes_ine_microdato_15R) # 257 ### ENCONTRAMOS MAS CODIGOS MANZENT ELSOC EN 15R

#####################################################################################  

  registros_faltantes_ine_censo_sel <- manzanas_perdidas(ELSOC_manzanas_unique, censo_sel$ID_MANZENT, 1078)
  registros_faltantes_ine_censo_sel_15R <- manzanas_perdidas(ELSOC_manzanas_unique, censo_sel$ID_MANZENT_15R, 1078)
  

  length(registros_faltantes_ine_censo_sel) # 275    # Esto es igual que el anterio
  length(registros_faltantes_ine_censo_sel_15R) # 257
  
  ################################################3
  
  registros_faltantes16R <- data.frame(RF_INE_CENSO16R = registros_faltantes_ine_censo_sel)
  registros_faltantes15R <- data.frame(RF_INE_CENSO15R = registros_faltantes_ine_censo_sel_15R)
  
  setwd("C:/projectos/disi/manzanas")
  
  write.csv(registros_faltantes16R, file = "registros_faltantes16R.csv")
  write.csv(registros_faltantes15R, file = "registros_faltantes15R.csv")
  