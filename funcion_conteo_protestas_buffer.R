library(dplyr)
library(sf)
library(tidyr)
library(lubridate)
library(tigris)
library(tidyselect)

options(scipen = 9999)

funcion_conteo_protestas_buffer <- function(ruta_base_datos, p_buffer, dir_salida){
  
  macro_protestas_buffer <- st_read(dsn = ruta_base_datos, layer = p_buffer)
  
  #### importar datos cuestionarios #### 

  setwd("C:/projectos/dizzi/csv")

  ELSOC_complete_temporal <- read.csv(file = "ELSOC_complete_temporal.csv", stringsAsFactors = FALSE) %>% as_tibble() %>% 
    mutate(date = mdy(date)) %>%
    mutate(date_min1 = mdy(date_min1))%>% 
    mutate(date_min2 = mdy(date_min2))%>% 
    mutate(date_min3 = mdy(date_min3))%>% 
    mutate(date_min4 = mdy(date_min4))%>% 
    mutate(date_min6 = mdy(date_min6))%>% 
    mutate(date_min8 = mdy(date_min8))

  
  ## Vamos a seleccionar los cuestionarios ELSOC realizados en la manzana desde donde se origino el buffer
  ELSOC_seleccion_manzana <- filter(ELSOC_complete_temporal, manzana == as.numeric(substr(p_buffer,5,nchar(p_buffer)-9)))
  
  ## Cruzamos los cuestionarios realizados en la manzana con cada una de las protestas
  ## que se encuentran dentro del buffer
  ## Hasta el avance del dia domingo 10.10.2021.
  
  ELSOC_protestas_feministas_manzana <- left_join(ELSOC_seleccion_manzana, macro_protestas_buffer, by = c("manzana" = "buf_manzent")) # 1908907 casos
  
  ## Seleccionamos solo las variables que utilizaremos en el analisis 

  ELSOC_protestas_feministas_manzana_sel <- select(ELSOC_protestas_feministas_manzana, ID_ELSOC, wave, cod_comuna, p8, NOM_COMUNA, date,
                                         date_min1, date_min2, date_min3, date_min4, date_prot, ID_prot, 
                                         protestas, violento, feminista, viol_genero, 
                                         des_genero, non_fem, non_fem_viol, 
                                         non_fem_nonviol, fem_nonviol, fem_viol) # En la seleccion del script macro falta femvio

  # Vamos a reemplazar los NA con 0

  ELSOC_protestas_feministas_sel_na <- replace_na(ELSOC_protestas_feministas_manzana_sel,
                                                list(violento = 0, feminista = 0, 
                                                     viol_genero = 0, des_genero = 0, non_fem = 0, non_fem_viol = 0, 
                                                     fem_nonviol = 0, fem_viol = 0))
                                                     
  ### A continuación creamos las 4 variables que prueban la cuatro condiciones temporales

  resultados_fechas <- mutate(ELSOC_protestas_feministas_sel_na, 
                            RANGO_DATE_cuatro = (date_min4 <= date_prot) & (date_prot <= date),
                            RANGO_DATE_tres = (date_min3 <= date_prot) & (date_prot <= date),
                            RANGO_DATE_dos = (date_min2 <= date_prot) & (date_prot <= date),
                            RANGO_DATE_una = (date_min1 <= date_prot) & (date_prot <= date))


  # Esto me da el resultado donde los cuestionarios tienen protestas asociadas, estos pueden corresponder a formularios todas las macro comunas
  # Sin embargo deberiamos seleccionar solo aquellos formularios aplicados a la comuna centro de la macro 
  # comuna 

  resultados_fechas_protestas <-filter(resultados_fechas,(!is.na(date_prot)))  

  ### Creamos el data frame con la suma de las tipos protestas que cumplen para cada rango de tiempo

  resultados_por_grupo <- group_by(resultados_fechas_protestas, ID_ELSOC) %>%
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
  
  write.csv(resultados_por_grupo, paste(dir_salida,"/", p_buffer,  "_protestas_buffer.csv", sep = ""))
  
}          

