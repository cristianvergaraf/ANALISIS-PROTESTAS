###
# Este script es aquel que 
layers <- st_layers("C:/projectos/dizzi/datos_espaciales/res_protestas/protestas_comuna.gpkg")

macro_names <- layers$name

names <- substr(macro_names,1,nchar(macro_names)-4)



setwd("~/github/protest-analysis")

source("funcion_conteo_protestas_macrocomuna.R")


funcion_conteo_protestas_macrocomuna("C:/projectos/dizzi/datos_espaciales/res_protestas/protestas_comuna.gpkg",
                                     macro_names[100],"C:/projectos/dizzi/datos_espaciales/res_protestas_tiempo")

for (i in macro_names){
  funcion_conteo_protestas_macrocomuna("C:/projectos/dizzi/datos_espaciales/res_protestas/protestas_comuna.gpkg",
                                       i,"C:/projectos/dizzi/datos_espaciales/res_protestas_tiempo")
  
}


################################
setwd("C:/projectos/dizzi/datos_espaciales/res_protestas_tiempo")
protestas_chile_completo <- read.csv(file = "protestas_comunas_chile_completo.csv")

ELSOC_complete_temporal

setwd("C:/projectos/dizzi/csv")

ELSOC_complete_temporal <- read.csv(file = "ELSOC_complete_temporal.csv", stringsAsFactors = FALSE) %>% as_tibble() %>% 
  mutate(date = mdy(date)) %>%
  mutate(date_min1 = mdy(date_min1))%>% 
  mutate(date_min2 = mdy(date_min2))%>% 
  mutate(date_min3 = mdy(date_min3))%>% 
  mutate(date_min4 = mdy(date_min4))%>% 
  mutate(date_min6 = mdy(date_min6))%>% 
  mutate(date_min8 = mdy(date_min8))


ELSOC_complete_protestas <- left_join(ELSOC_complete_temporal, protestas_chile_completo, by = "ID_ELSOC") %>%
  select(-c("X","X.1"))

setwd("C:/projectos/dizzi/protestas feministas")

write.csv(ELSOC_complete_temporal, file = "ELSOC_complete_protestas_macro.csv")



########################################################################################
### REVISION DEL LA FUNCION MACRO PROTESTAS ###


library(dplyr)
library(sf)
library(tidyr)
library(lubridate)
library(tigris)
library(tidyselect)
options(scipen = 9999)

macro_protestas_comuna <- st_read(dsn = "C:/projectos/dizzi/datos_espaciales/res_protestas/protestas_comuna.gpkg",
                           layer = "macro_10101.shp")


setwd("C:/projectos/dizzi/csv")

ELSOC_complete_temporal <- read.csv(file = "ELSOC_complete_temporal.csv", stringsAsFactors = FALSE) %>% as_tibble() %>% 
  mutate(date = mdy(date)) %>%
  mutate(date_min1 = mdy(date_min1))%>% 
  mutate(date_min2 = mdy(date_min2))%>% 
  mutate(date_min3 = mdy(date_min3))%>% 
  mutate(date_min4 = mdy(date_min4))%>% 
  mutate(date_min6 = mdy(date_min6))%>% 
  mutate(date_min8 = mdy(date_min8))

p_comuna <- "macro_10101.shp"
ELSOC_selecion_comuna <- filter(ELSOC_complete_temporal, cod_comuna == as.integer(str_extract(p_comuna, "[[:digit:]]+"))
)

ELSOC_protestas_feministas_comuna <- left_join(ELSOC_selecion_comuna, macro_protestas_comuna, by = c("cod_comuna" = "macro_p8")) # 1908907 casos

ELSOC_protestas_feministas_comuna_sel <- select(ELSOC_protestas_feministas_comuna, ID_ELSOC, wave, cod_comuna, p8, NOM_COMUNA, date,
                                                date_min1, date_min2, date_min3, date_min4, date_prot, ID_prot, 
                                                protestas, violento, feminista, viol_genero, 
                                                des_genero, non_fem, non_fem_viol, 
                                                non_fem_nonviol, fem_nonviol, fem_viol)

ELSOC_protestas_feministas_sel_na <- replace_na(ELSOC_protestas_feministas_comuna_sel,
                                                list(violento = 0, feminista = 0, 
                                                     viol_genero = 0, des_genero = 0, non_fem = 0, non_fem_viol = 0, 
                                                     fem_nonviol = 0, fem_viol = 0))


resultados_fechas <- mutate(ELSOC_protestas_feministas_sel_na, 
                            RANGO_DATE_cuatro = (date_min4 <= date_prot) & (date_prot <= date),
                            RANGO_DATE_tres = (date_min3 <= date_prot) & (date_prot <= date),
                            RANGO_DATE_dos = (date_min2 <= date_prot) & (date_prot <= date),
                            RANGO_DATE_una = (date_min1 <= date_prot) & (date_prot <= date))

resultados_fechas_protestas <-filter(resultados_fechas,(!is.na(date_prot)))

resultados_por_grupo_test <-group_by(resultados_fechas_protestas, ID_ELSOC) %>%
  summarize(
    
    protestas_4 =  sum((RANGO_DATE_cuatro) & (protestas ==1)),
    protestas_3 = sum((RANGO_DATE_tres) & (protestas ==1)),
    protestas_2 = sum((RANGO_DATE_dos) & (protestas ==1)),
    protestas_1 = sum((RANGO_DATE_una) & (protestas ==1)),
    
    
    violento_4 =  sum((RANGO_DATE_cuatro) & (violento==1)),
    violento_3 = sum((RANGO_DATE_tres) & (violento==1)),
    violento_2 = sum((RANGO_DATE_dos) & (violento==1)),
    violento_1 = sum((RANGO_DATE_una) & (violento==1)),
    
    feminista_4 =  sum((RANGO_DATE_cuatro) & (feminista==1)),
    feminista_3 = sum((RANGO_DATE_tres) & (feminista==1)),
    feminista_2 = sum((RANGO_DATE_dos) & (feminista==1)),
    feminista_1 = sum((RANGO_DATE_una) & (feminista==1)),
    
    
    viol_genero_4 =  sum((RANGO_DATE_cuatro) & (viol_genero==1)),
    viol_genero_3 = sum((RANGO_DATE_tres) & (viol_genero==1)),
    viol_genero_2 = sum((RANGO_DATE_dos) & (viol_genero==1)),
    viol_genero_1 = sum((RANGO_DATE_una) & (viol_genero==1)),
    
    
    des_genero_4 =  sum((RANGO_DATE_cuatro) & (des_genero ==1)),
    des_genero_3 = sum((RANGO_DATE_tres) & (des_genero ==1)),
    des_genero_2 = sum((RANGO_DATE_dos) & (des_genero ==1)),
    des_genero_1 = sum((RANGO_DATE_una) & (des_genero ==1)),
    
    non_fem_4 =  sum((RANGO_DATE_cuatro) & (non_fem ==1)),
    non_fem_3 = sum((RANGO_DATE_tres) & (non_fem ==1)),
    non_fem_2 = sum((RANGO_DATE_dos) & (non_fem ==1)),
    non_fem_1 = sum((RANGO_DATE_una) & (non_fem ==1)),
    
    non_fem_viol_4 =  sum((RANGO_DATE_cuatro) & (non_fem_viol ==1)),
    non_fem_viol_3 = sum((RANGO_DATE_tres) & (non_fem_viol ==1)),
    non_fem_viol_2 = sum((RANGO_DATE_dos) & (non_fem_viol ==1)),
    non_fem_viol_1 = sum((RANGO_DATE_una) & (non_fem_viol ==1)),
    
    non_fem_nonviol_4 =  sum((RANGO_DATE_cuatro) & (non_fem_nonviol ==1)),
    non_fem_nonviol_3 = sum((RANGO_DATE_tres) & (non_fem_nonviol ==1)),
    non_fem_nonviol_2 = sum((RANGO_DATE_dos) & (non_fem_nonviol ==1)),
    non_fem_nonviol_1 = sum((RANGO_DATE_una) & (non_fem_nonviol ==1)),
    
    fem_nonviol_4 =  sum((RANGO_DATE_cuatro) & (fem_nonviol ==1)),
    fem_nonviol_3 = sum((RANGO_DATE_tres) & (fem_nonviol ==1)),
    fem_nonviol_2 = sum((RANGO_DATE_dos) & (fem_nonviol ==1)),
    fem_nonviol_1 = sum((RANGO_DATE_una) & (fem_nonviol ==1)),
    
    fem_viol_4 =  sum((RANGO_DATE_cuatro) & (fem_viol == 1)),
    fem_viol_3 = sum((RANGO_DATE_tres) & (fem_viol == 1)),
    fem_viol_2 = sum((RANGO_DATE_dos) & (fem_viol == 1)),
    fem_viol_1 = sum((RANGO_DATE_una) & (fem_viol == 1))
    
  )


resultados_por_grupo_test2 <-group_by(resultados_fechas_protestas, ID_ELSOC, cod_comuna) %>%
  summarize(
    
    protestas_4 =  sum((RANGO_DATE_cuatro) & (protestas ==1)),
    protestas_3 = sum((RANGO_DATE_tres) & (protestas ==1)),
    protestas_2 = sum((RANGO_DATE_dos) & (protestas ==1)),
    protestas_1 = sum((RANGO_DATE_una) & (protestas ==1)),
    
    
    violento_4 =  sum((RANGO_DATE_cuatro) & (violento==1)),
    violento_3 = sum((RANGO_DATE_tres) & (violento==1)),
    violento_2 = sum((RANGO_DATE_dos) & (violento==1)),
    violento_1 = sum((RANGO_DATE_una) & (violento==1)),
    
    feminista_4 =  sum((RANGO_DATE_cuatro) & (feminista==1)),
    feminista_3 = sum((RANGO_DATE_tres) & (feminista==1)),
    feminista_2 = sum((RANGO_DATE_dos) & (feminista==1)),
    feminista_1 = sum((RANGO_DATE_una) & (feminista==1)),
    
    
    viol_genero_4 =  sum((RANGO_DATE_cuatro) & (viol_genero==1)),
    viol_genero_3 = sum((RANGO_DATE_tres) & (viol_genero==1)),
    viol_genero_2 = sum((RANGO_DATE_dos) & (viol_genero==1)),
    viol_genero_1 = sum((RANGO_DATE_una) & (viol_genero==1)),
    
    
    des_genero_4 =  sum((RANGO_DATE_cuatro) & (des_genero ==1)),
    des_genero_3 = sum((RANGO_DATE_tres) & (des_genero ==1)),
    des_genero_2 = sum((RANGO_DATE_dos) & (des_genero ==1)),
    des_genero_1 = sum((RANGO_DATE_una) & (des_genero ==1)),
    
    non_fem_4 =  sum((RANGO_DATE_cuatro) & (non_fem ==1)),
    non_fem_3 = sum((RANGO_DATE_tres) & (non_fem ==1)),
    non_fem_2 = sum((RANGO_DATE_dos) & (non_fem ==1)),
    non_fem_1 = sum((RANGO_DATE_una) & (non_fem ==1)),
    
    non_fem_viol_4 =  sum((RANGO_DATE_cuatro) & (non_fem_viol ==1)),
    non_fem_viol_3 = sum((RANGO_DATE_tres) & (non_fem_viol ==1)),
    non_fem_viol_2 = sum((RANGO_DATE_dos) & (non_fem_viol ==1)),
    non_fem_viol_1 = sum((RANGO_DATE_una) & (non_fem_viol ==1)),
    
    non_fem_nonviol_4 =  sum((RANGO_DATE_cuatro) & (non_fem_nonviol ==1)),
    non_fem_nonviol_3 = sum((RANGO_DATE_tres) & (non_fem_nonviol ==1)),
    non_fem_nonviol_2 = sum((RANGO_DATE_dos) & (non_fem_nonviol ==1)),
    non_fem_nonviol_1 = sum((RANGO_DATE_una) & (non_fem_nonviol ==1)),
    
    fem_nonviol_4 =  sum((RANGO_DATE_cuatro) & (fem_nonviol ==1)),
    fem_nonviol_3 = sum((RANGO_DATE_tres) & (fem_nonviol ==1)),
    fem_nonviol_2 = sum((RANGO_DATE_dos) & (fem_nonviol ==1)),
    fem_nonviol_1 = sum((RANGO_DATE_una) & (fem_nonviol ==1)),
    
    fem_viol_4 =  sum((RANGO_DATE_cuatro) & (fem_viol == 1)),
    fem_viol_3 = sum((RANGO_DATE_tres) & (fem_viol == 1)),
    fem_viol_2 = sum((RANGO_DATE_dos) & (fem_viol == 1)),
    fem_viol_1 = sum((RANGO_DATE_una) & (fem_viol == 1))
    
  )


resultados_por_grupo_test1 <-group_by(resultados_fechas_protestas, ID_ELSOC, cod_comuna, NOM_COMUNA) %>%
  summarize(
    
    protestas_4 =  sum((RANGO_DATE_cuatro) & (protestas ==1)),
    protestas_3 = sum((RANGO_DATE_tres) & (protestas ==1)),
    protestas_2 = sum((RANGO_DATE_dos) & (protestas ==1)),
    protestas_1 = sum((RANGO_DATE_una) & (protestas ==1)),
    
    
    violento_4 =  sum((RANGO_DATE_cuatro) & (violento==1)),
    violento_3 = sum((RANGO_DATE_tres) & (violento==1)),
    violento_2 = sum((RANGO_DATE_dos) & (violento==1)),
    violento_1 = sum((RANGO_DATE_una) & (violento==1)),
    
    feminista_4 =  sum((RANGO_DATE_cuatro) & (feminista==1)),
    feminista_3 = sum((RANGO_DATE_tres) & (feminista==1)),
    feminista_2 = sum((RANGO_DATE_dos) & (feminista==1)),
    feminista_1 = sum((RANGO_DATE_una) & (feminista==1)),
    
    
    viol_genero_4 =  sum((RANGO_DATE_cuatro) & (viol_genero==1)),
    viol_genero_3 = sum((RANGO_DATE_tres) & (viol_genero==1)),
    viol_genero_2 = sum((RANGO_DATE_dos) & (viol_genero==1)),
    viol_genero_1 = sum((RANGO_DATE_una) & (viol_genero==1)),
    
    
    des_genero_4 =  sum((RANGO_DATE_cuatro) & (des_genero ==1)),
    des_genero_3 = sum((RANGO_DATE_tres) & (des_genero ==1)),
    des_genero_2 = sum((RANGO_DATE_dos) & (des_genero ==1)),
    des_genero_1 = sum((RANGO_DATE_una) & (des_genero ==1)),
    
    non_fem_4 =  sum((RANGO_DATE_cuatro) & (non_fem ==1)),
    non_fem_3 = sum((RANGO_DATE_tres) & (non_fem ==1)),
    non_fem_2 = sum((RANGO_DATE_dos) & (non_fem ==1)),
    non_fem_1 = sum((RANGO_DATE_una) & (non_fem ==1)),
    
    non_fem_viol_4 =  sum((RANGO_DATE_cuatro) & (non_fem_viol ==1)),
    non_fem_viol_3 = sum((RANGO_DATE_tres) & (non_fem_viol ==1)),
    non_fem_viol_2 = sum((RANGO_DATE_dos) & (non_fem_viol ==1)),
    non_fem_viol_1 = sum((RANGO_DATE_una) & (non_fem_viol ==1)),
    
    non_fem_nonviol_4 =  sum((RANGO_DATE_cuatro) & (non_fem_nonviol ==1)),
    non_fem_nonviol_3 = sum((RANGO_DATE_tres) & (non_fem_nonviol ==1)),
    non_fem_nonviol_2 = sum((RANGO_DATE_dos) & (non_fem_nonviol ==1)),
    non_fem_nonviol_1 = sum((RANGO_DATE_una) & (non_fem_nonviol ==1)),
    
    fem_nonviol_4 =  sum((RANGO_DATE_cuatro) & (fem_nonviol ==1)),
    fem_nonviol_3 = sum((RANGO_DATE_tres) & (fem_nonviol ==1)),
    fem_nonviol_2 = sum((RANGO_DATE_dos) & (fem_nonviol ==1)),
    fem_nonviol_1 = sum((RANGO_DATE_una) & (fem_nonviol ==1)),
    
    fem_viol_4 =  sum((RANGO_DATE_cuatro) & (fem_viol == 1)),
    fem_viol_3 = sum((RANGO_DATE_tres) & (fem_viol == 1)),
    fem_viol_2 = sum((RANGO_DATE_dos) & (fem_viol == 1)),
    fem_viol_1 = sum((RANGO_DATE_una) & (fem_viol == 1))
    
  )

resultados_por_grupo_test_3 <- group_by(resultados_fechas_protestas, ID_ELSOC, NOM_COMUNA) %>%
  summarize(
    
    protestas_4 =  sum((RANGO_DATE_cuatro) & (protestas ==1)),
    protestas_3 = sum((RANGO_DATE_tres) & (protestas ==1)),
    protestas_2 = sum((RANGO_DATE_dos) & (protestas ==1)),
    protestas_1 = sum((RANGO_DATE_una) & (protestas ==1)),
    
    
    violento_4 =  sum((RANGO_DATE_cuatro) & (violento==1)),
    violento_3 = sum((RANGO_DATE_tres) & (violento==1)),
    violento_2 = sum((RANGO_DATE_dos) & (violento==1)),
    violento_1 = sum((RANGO_DATE_una) & (violento==1)),
    
    feminista_4 =  sum((RANGO_DATE_cuatro) & (feminista==1)),
    feminista_3 = sum((RANGO_DATE_tres) & (feminista==1)),
    feminista_2 = sum((RANGO_DATE_dos) & (feminista==1)),
    feminista_1 = sum((RANGO_DATE_una) & (feminista==1)),
    
    
    viol_genero_4 =  sum((RANGO_DATE_cuatro) & (viol_genero==1)),
    viol_genero_3 = sum((RANGO_DATE_tres) & (viol_genero==1)),
    viol_genero_2 = sum((RANGO_DATE_dos) & (viol_genero==1)),
    viol_genero_1 = sum((RANGO_DATE_una) & (viol_genero==1)),
    
    
    des_genero_4 =  sum((RANGO_DATE_cuatro) & (des_genero ==1)),
    des_genero_3 = sum((RANGO_DATE_tres) & (des_genero ==1)),
    des_genero_2 = sum((RANGO_DATE_dos) & (des_genero ==1)),
    des_genero_1 = sum((RANGO_DATE_una) & (des_genero ==1)),
    
    non_fem_4 =  sum((RANGO_DATE_cuatro) & (non_fem ==1)),
    non_fem_3 = sum((RANGO_DATE_tres) & (non_fem ==1)),
    non_fem_2 = sum((RANGO_DATE_dos) & (non_fem ==1)),
    non_fem_1 = sum((RANGO_DATE_una) & (non_fem ==1)),
    
    non_fem_viol_4 =  sum((RANGO_DATE_cuatro) & (non_fem_viol ==1)),
    non_fem_viol_3 = sum((RANGO_DATE_tres) & (non_fem_viol ==1)),
    non_fem_viol_2 = sum((RANGO_DATE_dos) & (non_fem_viol ==1)),
    non_fem_viol_1 = sum((RANGO_DATE_una) & (non_fem_viol ==1)),
    
    non_fem_nonviol_4 =  sum((RANGO_DATE_cuatro) & (non_fem_nonviol ==1)),
    non_fem_nonviol_3 = sum((RANGO_DATE_tres) & (non_fem_nonviol ==1)),
    non_fem_nonviol_2 = sum((RANGO_DATE_dos) & (non_fem_nonviol ==1)),
    non_fem_nonviol_1 = sum((RANGO_DATE_una) & (non_fem_nonviol ==1)),
    
    fem_nonviol_4 =  sum((RANGO_DATE_cuatro) & (fem_nonviol ==1)),
    fem_nonviol_3 = sum((RANGO_DATE_tres) & (fem_nonviol ==1)),
    fem_nonviol_2 = sum((RANGO_DATE_dos) & (fem_nonviol ==1)),
    fem_nonviol_1 = sum((RANGO_DATE_una) & (fem_nonviol ==1)),
    
    fem_viol_4 =  sum((RANGO_DATE_cuatro) & (fem_viol == 1)),
    fem_viol_3 = sum((RANGO_DATE_tres) & (fem_viol == 1)),
    fem_viol_2 = sum((RANGO_DATE_dos) & (fem_viol == 1)),
    fem_viol_1 = sum((RANGO_DATE_una) & (fem_viol == 1))
)
