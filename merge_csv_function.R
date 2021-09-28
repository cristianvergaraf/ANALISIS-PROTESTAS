
### Funcion obtenida de stackOverflow para leer todos los csv de un directorio y rbind todos ellos creando 
### un solo data.frame


load_data <- function(path) { 
  files <- dir(path, pattern = '\\.csv', full.names = TRUE)
  tables <- lapply(files, read.csv)
  do.call(rbind, tables)
}

protestas_comunas_chile_completo <- load_data("C:/projectos/dizzi/datos_espaciales/res_protestas_tiempo")
 
write.csv(protestas_comunas_chile_completo, file = "C:/projectos/dizzi/datos_espaciales/res_protestas_tiempo/protestas_comunas_chile_completo.csv")
