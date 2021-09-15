
library(dplyr)
library(sf)
library(tidyr)
library(lubridate)
library(tigris)
library(tidyselect)


options(scipen = 9999)

setwd("C:/projectos/dizzi/protestas femininas")



protestas_feministas <- read.csv(file = "OdeC_16_19_feminista.csv") %>% as_tibble() %>%
  unite(fecha, c(p5a, p5b, p5c), sep="/")%>% mutate(date_prot = dmy(fecha)) %>% select(p8, violento, feminista, viol_genero, des_genero, non_fem, non_fem_viol,non_fem_nonviol, fem_nonviol,x,y) %>%
  mutate(protestas = 1) %>%replace_na(list(violento = 0, feminista = 0, viol_genero = 0, des_genero = 0, 
                                     non_fem = 0, non_fem_viol = 0, non_fem_nonviol = 0, fem_nonviol = 0,
                                     fem_viol = 0))

numero_protestas_com <- read.csv("numero_protestas_feministas_sel_com.csv", stringsAsFactors = FALSE)

protestas_feministas_sf <- st_as_sf(protestas_feministas, coords = c("x", "y"), crs = 4326, agr = "constant")

protestas_feministas_3857 <- st_transform(protestas_feministas_sf, crs = 3857)


setwd("~/github/protest-analysis")

source("macro_unidades.R")


setwd("C:/projectos/dizzi/datos_espaciales/resultados")

nombre <- list.files(pattern = "*.shp$")

nombres_macro <- substr(nombre,1,nchar(nombre)-4)

for (i in nombres_macro){
  protestas_macro(i,protestas_feministas_3857, "C:/projectos/dizzi/datos_espaciales/resultados",
                  "C:/projectos/dizzi/datos_espaciales/res_protestas")
}


