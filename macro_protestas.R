
library(dplyr)
library(sf)
library(tidyr)
library(lubridate)
library(tigris)
library(tidyselect)
library(stringr) 

options(scipen = 9999)

setwd("C:/projectos/dizzi/protestas femininas")


protestas_feministas <- read.csv(file = "protestas_feministas_nombre_comuna.csv") %>% as_tibble() %>%
  unite(fecha, c(p5a, p5b, p5c), sep="/")%>% mutate(date_prot = dmy(fecha), ID_prot = row_number()) %>% select(p8,NOM_COMUNA, ID_prot, date_prot, violento, feminista, viol_genero, des_genero, non_fem, 
                                                                                       non_fem_viol,non_fem_nonviol, fem_nonviol,fem_viol,x,y) %>%
  mutate(protestas = 1) %>%replace_na(list(violento = 0, feminista = 0, viol_genero = 0, des_genero = 0, 
                                     non_fem = 0, non_fem_viol = 0, non_fem_nonviol = 0, fem_nonviol = 0,
                                     fem_viol = 0))

protestas_feministas_sf <- st_as_sf(protestas_feministas, coords = c("x", "y"), crs = 4326, agr = "constant")

protestas_feministas_3857 <- st_transform(protestas_feministas_sf, crs = 3857)

setwd("~/github/protest-analysis")

source("macro_unidades.R")

setwd("C:/projectos/dizzi/datos_espaciales/resultados")

layers <- st_layers("C:/projectos/dizzi/datos_espaciales/resultados/macro_comuna.gpkg")
nombre <- layers$name

nombres_macro <- substr(nombre,1,nchar(nombre)-4)

as.integer(str_extract(nombres_macro[1], "[[:digit:]]+"))

macro_12102.shp <- st_read(dsn = "C:/projectos/dizzi/datos_espaciales/resultados/macro_comuna.gpkg",
                                  layer="macro_12102.shp") # revisar como se escribieron o leyeron los nombres


#protestas_macro("macro_10101.shp", protestas_feministas_3857, "C:/projectos/dizzi/datos_espaciales/resultados",
 #               "C:/projectos/dizzi/datos_espaciales/res_protestas")


for (i in nombre){
  protestas_macro(i,protestas_feministas_3857, "C:/projectos/dizzi/datos_espaciales/resultados",
                  "C:/projectos/dizzi/datos_espaciales/res_protestas")
}





