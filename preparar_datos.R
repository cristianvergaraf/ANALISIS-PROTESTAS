##
## El objetivo de este script es leer y exportar tanto la tabla del cuestionario ELSOC, como la tabla de protestas.
##
setwd("C:/projectos/dizzi")
library(dplyr)
library(sf)
library(tidyr)
library(lubridate)
library(tigris)



setwd("C:/projectos/dizzi/csv")

options(scipen = 9999)

ELSOC <- (read.csv(file = "../csv/ELSOC_id_fechas_manzanas_corregido_1.csv")) %>% as_tibble() %>% unite(fecha, c(annio_entr_w0, mes_entr_w0, dia_entr_w0), sep="/") %>% 
  mutate(date = ymd(fecha)) 


paste("El nuemro de NA desde el año 2016 es", sum(is.na(ELSOC$date))) ### Esto corresponde a las olas.

#filter(ELSOC,  is.na(date))

ELSOC_complete <- filter(ELSOC,  !is.na(date))

# LOS NA ANTERIORES TIENEN QUE VER CON FILAS VACIAS EN LA ENCUESTA ELSOC. PRINCIPALMENTE ASOCIADAS A LAS OLAS 1 y 2. 

### Consulta de relaciones de fecha entre los datos de las protestas y del COE

ELSOC_complete_temporal <- mutate(ELSOC_complete, ID_ELSOC  = row_number(), date_min1 = date - 7, date_min2 = date - 14, 
                                  date_min3 = date - 21, date_min4 = date - 28, date_min6 = date - 42, 
                                  date_min8 = date - 56)


write.csv(ELSOC_complete_temporal, file = "../csv/ELSOC_complete_temporal.csv")

#################################################################################################

## Parte II ##

## DATOS COES ##

## Protestas de la Base del Observatorio de Conflictos espacializadas (2019 y 2016-2018)
## Los datos a combinar son los de ubicación geográfica (latitud y longitud) y 
## fecha de la acción: p5a (día), p5b (mes) y año (p5c)
## La fechas estan bien.

options(scipen = 9999)

coes_16_18 <- (read.csv(file = "../csv/COES_Georreferenciada.csv")) %>% as_tibble() %>% unite(fecha, c(p5a, p5b, p5c), sep="/") %>% mutate(date = dmy(fecha))
  
filter(coes_16_18,  is.na(date)) ## hay un no dato debido a que hay una fecha que es 31 de abril. Abril no tine 31.

coes_19 <- (read.csv(file = "../csv/BBDD COES 2019_Georef.csv")) %>% as_tibble() %>% unite(fecha, c(P5a, P5b, P5c), sep="/") %>% mutate(date = dmy(fecha)) #%>% dplyr::select(ID,direccion,Latitud,Longitud, comuna, date, fecha, MANZENT) Probablemente estas fechas estan mal


### En este paso tenemos que seleccionar las variables que se van a sumar que caracterizan las protestas, las variables deberian estar en ambos archivos.

options(scipen = 9999)

#names(coes_16_18_sel) <- c("ID", "Longitud", "Latitud", "date", "MANZENT")

### Antes de hacer esto debemos revisar la codificación de las columnas de interés para luego sumarlas.

coes_completo <- rbind.data.frame(coes_19, coes_16_18) %>% as_tibble()

write.csv(coes_completo, file = "../csv/coes_completo_sel.csv")


