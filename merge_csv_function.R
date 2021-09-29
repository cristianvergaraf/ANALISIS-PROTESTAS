
### Funcion obtenida de stackOverflow para leer todos los csv de un directorio, y generar un fichero con toda la información (rbind) 
### un solo data.frame


load_data <- function(path) { 
  files <- dir(path, pattern = '\\.csv', full.names = TRUE)
  tables <- lapply(files, read.csv)
  do.call(rbind, tables)
}

