##
## EL objetivo de este script es identificar las protestas ocurridas en las macro comunas identificadas anteriormente ##
## para esto aplicarame los función protestas_macro definida en el script de macro_unidades.R

library(dplyr)
library(sf)
library(tidyr)
library(lubridate)
library(tigris)
library(tidyselect)
library(stringr) 

options(scipen = 9999)

setwd("C:/projectos/disi/protestas feministas")

# cargar datos protestas, crear variable fechas date_prot, y un ID_prot asociadao a cada propuesta
protestas_feministas <- read.csv(file = "protestas_feministas_nombre_comuna.csv", stringsAsFactors = FALSE) %>% as_tibble() %>%
  unite(fecha, c(p5a, p5b, p5c), sep="/")%>% mutate(date_prot = dmy(fecha), ID_prot = row_number()) %>% select(p8,NOM_COMUNA, ID_prot, date_prot, violento, feminista, viol_genero, des_genero, non_fem, 
                                                                                                               non_fem_viol,non_fem_nonviol, fem_nonviol,fem_viol,x,y) %>%
  mutate(protestas = 1) %>%replace_na(list(violento = 0, feminista = 0, viol_genero = 0, des_genero = 0, 
                                           non_fem = 0, non_fem_viol = 0, non_fem_nonviol = 0, fem_nonviol = 0,
                                           fem_viol = 0))

setwd("C:/projectos/disi/datos_espaciales")

comunas <- st_read(dsn = ".", layer = "comunas")
comunas_32718 <- st_transform(comunas, crs = 32718)

centr_comunas <- st_centroid(comunas_32718)
centr_comunas_coord <- select(centr_comunas, COMUNA, NOM_COMUNA, geometry)
centr_comunas_4326 <- st_transform(centr_comunas_coord, crs = 4326) 

centr_comunas_4326_coord <-  mutate(centr_comunas_4326, coordenadas = st_coordinates(centr_comunas_4326),
                                    COMUNA = as.character(COMUNA))  

centr_comunas_4326_drop <- st_drop_geometry(centr_comunas_4326_coord)

centroide_comunas <- data.frame(nom_comuna = centr_comunas_4326_drop$NOM_COMUNA, 
           cod_comuna = as.integer(centr_comunas_4326_drop$COMUNA),
           X_centroide = centr_comunas_4326_drop$coordenadas[,1],
           Y_centroide = centr_comunas_4326_drop$coordenadas[,2])

protestas_sin_coordenadas <- filter(protestas_feministas, x == 0 | y == 0)

protestas_centroide <- left_join(protestas_feministas, centroide_comunas, by =c("p8" = "cod_comuna"))

protestas_centroide_mutate <- mutate(protestas_centroide, x_completa = ifelse(x==0, X_centroide, x), y_completa = ifelse(y==0, Y_centroide, y))

filter(protestas_centroide_mutate, x == 0 | y == 0)


setwd("C:/projectos/disi/protestas feministas")

write.csv(protestas_centroide_mutate, file =  "protestas_centroide.csv")

