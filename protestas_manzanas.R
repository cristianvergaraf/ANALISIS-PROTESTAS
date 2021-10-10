###
### El objetivo de este script es contar el numero de protestas ocurridas en el área de influencia alrededor
### del centroide de la manzana donde se realizo el cuestionario ELSOC
###

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

####  Cargar elsoc ELSOC ####

options(scipen = 9999)

setwd("C:/projectos/dizzi/csv")

ELSOC_complete_temporal <- read.csv(file = "ELSOC_complete_temporal.csv") %>% as_tibble() %>% 
  mutate(date = mdy(date))%>% 
  mutate(date_min1 = mdy(date_min1))%>% 
  mutate(date_min2 = mdy(date_min2))%>% 
  mutate(date_min3 = mdy(date_min3))%>% 
  mutate(date_min4 = mdy(date_min4))%>% 
  mutate(date_min6 = mdy(date_min6))%>% 
  mutate(date_min8 = mdy(date_min8))
  
  
### Ahora vamos a leer los datos espaciales de las manzanas INE ###
  
  setwd("C:/projectos/dizzi/manzanas")
  
  manzanas <- st_read(dsn = ".",  layer = "Microdatos_Censo_20173A_Manzana")
  
  # Vamos a realizar un pequeño piloto en la comuna de temuco
  
  # Ahora para el piloto vamos a seleccionar las protestas que solo ocurrren en temuco, 
  # y las vamos transformar a PSG 32718
  
  # Seleccionamos las manzanas que componen la comuna de Temuco
  
  manz_temuco <- filter(manzanas, COMUNA == "TEMUCO")
  sel_temuco_manzana <- dplyr::select(manz_temuco, MANZENT, MANZANA,geometry)
  
  # Vamos a transformar las manzanas de temuco a UTM18s para poder trabajar con distancias. 
  # Luego calculamos el centroide de cada manzana y el buffer de 2000 metros
  
  sel_temuco_manzana_18s <- st_transform(sel_temuco_manzana, crs = 32718)
  
  protestas_temuco <- st_crop(protestas_feministas_32718, sel_temuco_manzana_18s)
  
  plot(protestas_temuco)
  
  ### cuestionarios en temuco, filtrados por codigo de comuna
  
  ELSOC_temuco <- filter(ELSOC_complete_temporal, cod_comuna == 9101) # el total son 255
  
  manzanas_elsoc <- (unique(ELSOC_temuco$manzana)) 
  manzanas_elsoc
  
  length(manzanas_elsoc) # 23 manzanas distintas 
  
  
  ### Aqui creamos el data frame con los codigos de las 23 manzanas a las que se les aplicaron 
  ### un cuestionario de ELSOC.
  
  ### Buscamos cruzar las manzanas de temuco, con los codigos de las manzanas ELSOC, 
  ### usando la version ELSOC ### 
  
  ### por qué no un inner o geo_join ### EL problema esta en el tipo de join
  
  ### Este join deberia ser solo con las manzanas donde se realizo ELSOC para poder
  ### tener relacion manzanas con entrevistas y protestas asociadas
  
  # ESto hay que cambiarlo. Para que vamos a cruzar todas las manzanas con todas las protestas.
  # Si tenemos solo algunas manzanas es mas rapido.
  # vamos identificar las manzanas presentes.
  
  
  length(unique(ELSOC_temuco$manzana)) # De estas 23, sabemos que hay 4 que no existen.
  
 setwd("~/GitHub/protest-analysis")
  
  
  source("manzanas_perdidas.R")
  
  registros_faltantes <- manzanas_perdidas(ELSOC_temuco$manzana,sel_temuco_manzana_18s$MANZENT)
  
  ## Ahora verificamos si estos numeros de manzanas se encuentran dentro de todo Chile 
  
  manzanas_perdidas(ELSOC_temuco$manzana, manzanas$MANZENT)
  
  
  ## nos ha resultado que los registros no existen dentro de Chile. Esto quiere decir
  ## que probablemente esten mal ingresados
  
  ### Por lo tanto vamos a eliminar estos registros de las comunas el SOC ###
  
  
  ELSOC_temuco_19_manz_INE <- filter(ELSOC_temuco, !(manzana == registros_faltantes[1] | manzana == registros_faltantes[2] | 
                                                manzana == registros_faltantes[3] | manzana == registros_faltantes[4])) 
  
  
  
  ## Agregamos un campo para hacer el join
  ELSOC_temuco_19_manz_INE$MANZENT <- ELSOC_temuco_19_manz_INE$manzana
  
  
  ## Ahora vaamos a cruzar los ELSOC de temuco depurados 
  ## Con los datos de los buffer
  ## Pero antes vamos a hacer los buffer solo de las manzanas donde hubo entrevistas.
  
  df_ELSOC_temuco_19_manz_INE <- data.frame(MANZENT = unique(ELSOC_temuco_19_manz_INE$manzana), CIUDAD = c("TEMUCO"))
  
  ## Ahora podemos quedarnos con las manzanas haciendo un join para identificar espacialmente
  ## Las 19 manzanas donde se realizaron los cuestionarios
  
  manzanas_elsoc_temuco_19_INE <- right_join(sel_temuco_manzana_18s,df_ELSOC_temuco_19_manz_INE, by = "MANZENT")
  
  # Ahora vamos a calcular el centroide de esas manzanas
  
  cent_manznas_elsoc_temuco_19_INE <- st_centroid(manzanas_elsoc_temuco_19_INE)
  
  ## Y ahora el buffer de 2000 metros
  
  buf_elsoc_tem_19manz_INE <- st_buffer(cent_manznas_elsoc_temuco_19_INE, dist = 2000)
  
  # ahora tenemos que cruzar las protestas con intersect y los buffer
  
  # protestas por buffer # Revisar protestas temuco a que se refiere
  
  resultados_prot_buf <- list()
  for (i in 1:19){
    resultados_prot_buf[[i]] <-(st_intersection(buf_elsoc_tem_19manz_INE[i,],protestas_temuco))
  }
  
  
  resultados_prot_buf1 <- list()
  for (i in 1:19){
    resultados_prot_buf1[[i]] <-(st_intersection(protestas_temuco, buf_elsoc_tem_19manz_INE[i,]))
  }
  
  resultados_prot_buf1[[5]] ### ESTO SERIA EQUIVALENTE A EXPORTAR ESTO COMO SHAPE 
                         ### Y LUEGO TRABAJAR COMO MACRO COMUNA. 
                         ### O AHORA APLICAR LAS CONDICIONES A CADA UNA DE LOS ELEMENTOS
                         ### DE LAS LISTAs.
  
  
  ### TENGO LAS PROTESTAS OCURRIDAS EN CADA BUFFER. QUE ES EQUIVALENTE AL MACRO_PROTESTAS
  ### EL ANALISIS DE LAS PROTESAS POR MACRO_COMUNA
  ### Siguiente paso es juntar el soc temuco con cada una de las protestas
  
  resultados_prot_buf1[[5]]
  
resultados_prot_buf1_5 <- mutate(resultados_prot_buf1[[5]], macro_p8 = as.integer("9101"))
  
  
  ELSOC_protestas_feministas_buf_5 <- left_join(ELSOC_temuco, resultados_prot_buf1_5, by = c("cod_comuna" = "macro_p8")) # 1908907 casos
  
  resultados_fechas_buf_5 <- mutate( ELSOC_protestas_feministas_buf_5, 
                              RANGO_DATE_cuatro = (date_min4 <= date_prot) & (date_prot <= date),
                              RANGO_DATE_tres = (date_min3 <= date_prot) & (date_prot <= date),
                              RANGO_DATE_dos = (date_min2 <= date_prot) & (date_prot <= date),
                              RANGO_DATE_una = (date_min1 <= date_prot) & (date_prot <= date))
  
  
  resultados_fechas_protestas_buf_5 <- filter(resultados_fechas_buf_5,(!is.na(date_prot)))
  
  
  resultados_por_grupo_test_buf_5 <- group_by(resultados_fechas_protestas_buf_5, ID_ELSOC) %>%
    summarize(
      
      protestas_4_2km =  sum((RANGO_DATE_cuatro) & (protestas ==1)),
      protestas_3_2km = sum((RANGO_DATE_tres) & (protestas ==1)),
      protestas_2_2km = sum((RANGO_DATE_dos) & (protestas ==1)),
      protestas_1_2km = sum((RANGO_DATE_una) & (protestas ==1)),
      
      
      violento_4_2km =  sum((RANGO_DATE_cuatro) & (violento==1)),
      violento_3_2km = sum((RANGO_DATE_tres) & (violento==1)),
      violento_2_2km = sum((RANGO_DATE_dos) & (violento==1)),
      violento_1_2km = sum((RANGO_DATE_una) & (violento==1)),
      
      feminista_4_2km =  sum((RANGO_DATE_cuatro) & (feminista==1)),
      feminista_3_2km = sum((RANGO_DATE_tres) & (feminista==1)),
      feminista_2_2km = sum((RANGO_DATE_dos) & (feminista==1)),
      feminista_1_2km = sum((RANGO_DATE_una) & (feminista==1)),
      
      
      viol_genero_4_2km =  sum((RANGO_DATE_cuatro) & (viol_genero==1)),
      viol_genero_3_2km = sum((RANGO_DATE_tres) & (viol_genero==1)),
      viol_genero_2_2km = sum((RANGO_DATE_dos) & (viol_genero==1)),
      viol_genero_1_2km = sum((RANGO_DATE_una) & (viol_genero==1)),
      
      
      des_genero_4_2km =  sum((RANGO_DATE_cuatro) & (des_genero ==1)),
      des_genero_3_2km = sum((RANGO_DATE_tres) & (des_genero ==1)),
      des_genero_2_2km = sum((RANGO_DATE_dos) & (des_genero ==1)),
      des_genero_1_2km = sum((RANGO_DATE_una) & (des_genero ==1)),
      
      non_fem_4_2km =  sum((RANGO_DATE_cuatro) & (non_fem ==1)),
      non_fem_3_2km = sum((RANGO_DATE_tres) & (non_fem ==1)),
      non_fem_2_2km = sum((RANGO_DATE_dos) & (non_fem ==1)),
      non_fem_1_2km = sum((RANGO_DATE_una) & (non_fem ==1)),
      
      non_fem_viol_4_2km =  sum((RANGO_DATE_cuatro) & (non_fem_viol ==1)),
      non_fem_viol_3_2km = sum((RANGO_DATE_tres) & (non_fem_viol ==1)),
      non_fem_viol_2_2km = sum((RANGO_DATE_dos) & (non_fem_viol ==1)),
      non_fem_viol_1_2km = sum((RANGO_DATE_una) & (non_fem_viol ==1)),
      
      non_fem_nonviol_4_2km =  sum((RANGO_DATE_cuatro) & (non_fem_nonviol ==1)),
      non_fem_nonviol_3_2km = sum((RANGO_DATE_tres) & (non_fem_nonviol ==1)),
      non_fem_nonviol_2_2km = sum((RANGO_DATE_dos) & (non_fem_nonviol ==1)),
      non_fem_nonviol_1_2km = sum((RANGO_DATE_una) & (non_fem_nonviol ==1)),
      
      fem_nonviol_4_2km =  sum((RANGO_DATE_cuatro) & (fem_nonviol ==1)),
      fem_nonviol_3_2km = sum((RANGO_DATE_tres) & (fem_nonviol ==1)),
      fem_nonviol_2_2km = sum((RANGO_DATE_dos) & (fem_nonviol ==1)),
      fem_nonviol_1_2km = sum((RANGO_DATE_una) & (fem_nonviol ==1)),
      
      fem_viol_4_2km =  sum((RANGO_DATE_cuatro) & (fem_viol == 1)),
      fem_viol_3_2km = sum((RANGO_DATE_tres) & (fem_viol == 1)),
      fem_viol_2_2km = sum((RANGO_DATE_dos) & (fem_viol == 1)),
      fem_viol_1_2km = sum((RANGO_DATE_una) & (fem_viol == 1))
      
    )
  
  
  
  
  
  
  ###############################################################################
  ### HASTA AQUI TRABAJO VIERNES###
  
  resultados <- list()
  for (i in 1:208){
    resultados[[i]] <-(st_intersection(spatial_buf_tem_filt[i,],protestas_temuco))
  }
  
  
  # Ahora vamos a calcular el buffer
  
  left_join(sel_temuco_manzana_18s,df_ELSOC_temuco_19_manz_INE, by = "MANZENT")
  
  
  manzanas_entrevistadas_tem <- left_join(sel_temuco_manzana_18s, df_ELSOC_temuco_19_manz_INE, by=c("MANZENT")) 

  length(unique(manzanas_entrevistadas_tem$MANZENT)) # 3036 entrevistas en manzanas 
  

  
  #############
  
  manzanas_ELSOC_temuco <- filter(manzanas_entrevistadas_tem, cuestionario == "ELSOC")
  
  plot(manzanas_ELSOC_temuco) # Estas son las manzanas donde se realizaron cuestionarios
  length(manzanas_ELSOC_temuco$MANZENT) # 19 manzanas en Temuco
  unique(manzanas_ELSOC_temuco$MANZENT) # Los codigos manzent de estas 19 manzanas son

######
### Aqui registros faltantes funcion que hicimos 
  
  # Falta calcular la funcion registro faltantes
  
  
# Resultado del script anterior  de aplicar la funcion para identificar los registros faltantes que en temuco son 4
  
ELSOC_temuco_filt <- filter(ELSOC_temuco, !(manzana == registros_faltantes[1] | manzana == registros_faltantes[2] | 
                                                manzana == registros_faltantes[3] | manzana == registros_faltantes[4])) 
  
  
ELSOC_temuco_filt # Esto representa todos los datos de ELSOC temuco, despues de eliminar los
  
# Vamos a crear  MANZENT que no existen
  

ELSOC_temuco_filt$MANZENT <- ELSOC_temuco_filt$manzana
  

df_manzanas_elsoc <- data.frame(manzana = manzanas_elsoc, cuestionario = "ELSOC")

length(unique(df_manzanas_elsoc$manzana))

centroide_manz_18s <- st_centroid(sel_temuco_manzana_18s)

buffer_2km_centroide_18s <- st_buffer(centroide_manz_18s, dist = 2000)

# Ahora vamos a cruzar los ELSOC de temuco depurados con los datos de las manzanas!!

library(tigris)

spatial_buf_tem_filt <- geo_join(buffer_2km_centroide_18s, ELSOC_temuco_filt, by = "MANZENT", 
                                 how = "inner")

# Ahora tenemos los 208 registros que aparecen al revisar los datos en EXCEl, todo el 
# el desarreglo parte debido a que existen MANZENT de ELSOC que no existen.

# Ahora tenemos que agregar la condicion final del cruce con el tiempo #


resultados <- list()
for (i in 1:208){
  resultados[[i]] <-(st_intersection(spatial_buf_tem_filt[i,],protestas_temuco))
}

# Graficamente vamos a ver las protestas que se encuentran en cada uno de los buffer que
# surgieron del cruce. Sabemos que se han hecho 208 encuestas SOC dentro de las 
# manzanas que corresponden a TEMUCO.

# Esto lo podemos verificar en la base de datos excel (REVISAR)  
  
    
library(tmap)
library(tmaptools)
data("World")

tmap_mode("view")

tm_shape(resultados[[20]]) + 
  tm_dots(size = 0.1) +
  
  tm_shape(spatial_buf_tem_filt[20,])+
  tm_borders("black", lwd = .5) +
  
  tm_shape(resultados[[10]]) + 
  tm_dots(size = 0.1) +
  
  tm_shape(spatial_buf_tem_filt[10,])+
  tm_borders("black", lwd = .5) +
  
  
  tm_shape(resultados[[208]]) + 
  tm_dots(size = 0.1) +
  
  tm_shape(spatial_buf_tem_filt[208,])+
  tm_borders("black", lwd = .5)




revisar <-function(numero){
  
  library(tmap)
  library(tmaptools)
  data("World")
  
  tmap_mode("view")
  
  tm_shape(resultados[[numero]]) + 
    tm_dots(size = 0.1)+
    
    tm_shape(spatial_buf_tem_filt[numero,])+
    tm_borders("black", lwd = .5) 
}

revisar(1)
revisar(2)
revisar(3)
revisar(4)
revisar(5)
revisar(10)

################### PROTESTAS MANZANAS ####################################  
  