#####                                       ###### 
#### El objetivo de este script es relacionar y contar el numero de
#### protestas feministas ocurridas por comuna 1,2,3,4 semanas antes 
###  de aplicar el cuestionario.


# Cargar librerias necesarias

library(dplyr)
library(sf)
library(tidyr)
library(lubridate)
library(tigris)
library(tidyselect)

options(scipen = 9999)

# Cargar protestas feministas
setwd("C:/projectos/disi/protestas feministas")

# Leer protestas feministas
protestas_feministas <- read.csv(file = "OdeC_16_19_feminista.csv") %>% as_tibble() %>%
  unite(fecha, c(p5a, p5b, p5c), sep="/")%>% mutate(date_prot = dmy(fecha))

# Creamos una variable para contar el numero de protestas
protestas_feministas$protestas <- 1

# P5: Fecha de la acción. Corresponde a la fecha en que sucedió la acción contenciosa 
# codificada en 3 campos númericos de día (dd, P5a), mes (mm, P5b) y año (aa, P5c); 

## P8: Comuna. Campo numérico que describe la comuna en donde sucede la acción contenciosa. 
## Toma valores correspondientes a cada comuna del país según el código otorgado por

## El INE, conformado por el(los) primer (os) dígito(s) para la región, el siguiente dígito para el
## número de la provincia dentro de la región y el siguiente dígito para el número de la comuna
## dentro de la provincia.

##################################################################################################################
######################################################################################################
#####################################################
# Leer datos espaciales de comunas chile

setwd("C:/projectos/disi/datos_espaciales")

comunas <- st_read(dsn = ".", layer = "comunas")

# obtenemos los codigos y nombres de las comunas
codigo_comunas_ine <- st_drop_geometry(select(comunas, NOM_COMUNA, COMUNA))

# cargamos el paquete dplyr y transformamos los factores a caracter (numeoros fueron leidos como factores)
library(dplyr)
codigo_comunas_ine %>% mutate(across(where(is.factor), as.character)) -> codigo_comunas_ine ### Transformar datos a Character

# transformamos el codigo de comunasa a enteros
codigo_comunas_ine$COMUNA <- as.integer(codigo_comunas_ine$COMUNA)

###########################################################################################3

filter(protestas_feministas,  is.na(date_prot)) ## hay un no dato debido a que hay una fecha que es 31 de abril. Abril no tine 31.
# Se cambio la fecha de la protesta a la fecha del dia valido mas cercano en el mismo mes

# transformamos las protestas a datos espacial usando las coordenadas
protestas_feministas_sf <- st_as_sf(protestas_feministas, coords = c("x", "y"), crs = 4326, agr = "constant")


## Visualizar las protestas
library(tmap)
library(tmaptools)
tmap_mode("view")

tm_shape(protestas_feministas_sf)+
  tm_dots(size = 0.01, col = "green")


## Rellenar los na con 0 en las variables de las protestas
protestas_feministas_na <- replace_na(protestas_feministas,list(violento = 0, feminista = 0, viol_genero = 0, des_genero = 0, 
                                                                non_fem = 0, non_fem_viol = 0, non_fem_nonviol = 0, fem_nonviol = 0,
                                                                fem_viol = 0))
# vamos a seleccionar y contar el numero de variables por comuna  y por tipo cruzando el codigo de las comunas
# de comuna de propuesta p8 con el codigo comuna de la capa espacial

numero_protestas_feministas_comunas <- select(protestas_feministas_na, p8, violento, feminista, viol_genero, des_genero, 
                                             non_fem, non_fem_viol,non_fem_nonviol, fem_nonviol,fem_viol, protestas) %>% group_by(p8) %>%
  summarise(across(everything(), sum)) %>% left_join(codigo_comunas_ine, by = c("p8"="COMUNA"))%>%
  select(NOM_COMUNA, p8, protestas, violento, feminista, viol_genero, des_genero, 
         non_fem, non_fem_viol,non_fem_nonviol, fem_nonviol, fem_viol) %>% 
  rename_with(~ paste("t_com", .x, sep = "_"), -c(NOM_COMUNA, p8))

# Exportar resultados
setwd("C:/projectos/disi/protestas feministas/Resultados a nivel de comuna")

write.csv(numero_protestas_feministas_comunas, file = "numero_protestas_feministas_comuna.csv")



#######################
### PARTE II. CONTAR EL NUMERO DE PROTESTAS POR RANGO DE TIEMPO PARA CADA CUESTIONARIO ELSOC


#### importar datos cuestionarios ELSOC temporal #### 

setwd("C:/projectos/disi/csv")

ELSOC_complete_temporal <- read.csv(file = "ELSOC_complete_temporal.csv") %>% as_tibble() %>% 
  mutate(date = mdy(date)) %>%
  mutate(date_min1 = mdy(date_min1))%>% 
  mutate(date_min2 = mdy(date_min2))%>% 
  mutate(date_min3 = mdy(date_min3))%>% 
  mutate(date_min4 = mdy(date_min4))%>% 
  mutate(date_min6 = mdy(date_min6))%>% 
  mutate(date_min8 = mdy(date_min8))

# Cambiamos los codigos de las comunas de nuble adaptandolos a la nueva region de nuble

ELSOC_complete_temporal_16REG <- mutate(ELSOC_complete_temporal, cod_comuna16 = ifelse(cod_comuna == 8404, 16203,ifelse(cod_comuna==8413,16107,cod_comuna))) %>% 
  relocate(.after = cod_comuna, cod_comuna16)

# Hamos un left join con el que relacionaremos cada cuestionario elsoc con las protestas
# con las protestas ocurridas en cada comuna (by "cod_comuna16 = p8") 

ELSOC_protestas_feministas <- left_join(ELSOC_complete_temporal_16REG, protestas_feministas_na, 
                                        by = c("cod_comuna16" = "p8")) # 1908907 casos con nuble y 1909279 con nuble

# Seleccionamos solo las variables que utilizaremos en el analisis 

ELSOC_protestas_feministas_sel <- select(ELSOC_protestas_feministas, ID_ELSOC, wave, cod_comuna, cod_comuna16,
                                         date, date_min1, date_min2, date_min3, date_min4, date_prot, 
                                         protestas, violento, feminista, viol_genero, 
                                         des_genero, non_fem, non_fem_viol, 
                                         non_fem_nonviol, fem_nonviol, fem_viol)


# Vamos a reemplazar los NA con 0

ELSOC_protestas_feministas_sel_na <- replace_na(ELSOC_protestas_feministas_sel,list(protestas = 0, violento = 0, feminista = 0, viol_genero = 0, 
                                                                                    des_genero = 0, non_fem = 0, non_fem_viol = 0, 
                                                                                    non_fem_nonviol = 0, fem_nonviol = 0, fem_viol = 0))

### A continuación creamos las 4 variables que prueban la cuatro condiciones temporales

resultados_fechas <- mutate(ELSOC_protestas_feministas_sel_na, 
                            RANGO_DATE_cuatro = (date_min4 <= date_prot) & (date_prot <= date),
                            RANGO_DATE_tres = (date_min3 <= date_prot) & (date_prot <= date),
                            RANGO_DATE_dos = (date_min2 <= date_prot) & (date_prot <= date),
                            RANGO_DATE_una = (date_min1 <= date_prot) & (date_prot <= date))


#

### Hay muchos cuestionarios sin protestas (Revisar cuando se pueda los cuestionarios a que comunas correspodenden)

cuestionario_comunas_sin_protestas <- filter(resultados_fechas,(is.na(ELSOC_protestas_feministas_sel$date_prot))) ### caso de cuestionarios sin protestas


### Son 3 comunas sin protestas. 

codigo_comunas_sin_protestas <- unique(cuestionario_comunas_sin_protestas$cod_comuna16)


## Ahora vamos a sumar los positivos de cada comuna

numero_protestas_cod_ELSOC <- group_by(resultados_fechas, ID_ELSOC) %>% 
  summarize(
    number_cases_protestas4 = sum(RANGO_DATE_cuatro),
    number_cases_protestas3 = sum(RANGO_DATE_tres),
    number_cases_protestas2 = sum(RANGO_DATE_dos),
    number_cases_protestas1 = sum(RANGO_DATE_una))


### Creamos el data frame con la suma de las tipos protestas que cumplen para cada rango de tiempo

resultados_por_grupo <-group_by(resultados_fechas, ID_ELSOC) %>%
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

##### Resultados por grupo tenemos que vincularlo al dataframe original del ELSOC usando el ID_ELSOC

ELSOC_protestas_comuna <- left_join(ELSOC_complete_temporal_16REG, resultados_por_grupo, by = "ID_ELSOC")%>%
  left_join(codigo_comunas_ine, by = c("cod_comuna16" = "COMUNA")) %>%
  relocate(NOM_COMUNA, .after = cod_comuna16)

setwd("C:/projectos/disi/protestas feministas/Resultados a nivel de comuna")

write.csv(ELSOC_protestas_comuna, file = "ELSOC_protestas_comuna.csv")

### Tambien vamos a vincular los resultados con el numero de protestas total por comuna

#ELSOC_comunas_final <- left_join(ELSOC_final, numero_protestas_feministas_comunas, by = c("cod_comuna.x" = "p8"))%>% 
  #relocate(NOM_COMUNA, .after = cod_comuna.x, cod.comuna = cod_comuna.x)

#write.csv(ELSOC_comunas_final, file = "ELSOC_comunas_final.csv")
