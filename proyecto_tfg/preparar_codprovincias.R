

preparar_cod_provincias <- function(path_proyecto){
  
  library(tidyr)
  library(rio)
  
  # setwd("C:\\Users\\UX430U\\Desktop\\TFG")
  
  setwd(path_proyecto)
  
  # importar fichero original del INE
  excel <- import("datos/20_cod_prov.xls")
  
  # borrar filas nulas
  excel <- drop_na(excel)
  
  # generar nombre de columnas correctamente
  excel <- excel[-c(1),]
  colnames(excel) <- c("cod_provincia", "nombre_provincia" )
  
  # almacenar los códigos como "cadena de caracteres" para que se mantengan con
  # dos dígitos, y no se consideren como números 
  excel$cod_provincia <- as.character(excel$cod_provincia)
  write.table(excel, "datos/cod_provincias.csv", row.names=FALSE, col.names=TRUE, sep = ';')
  
}
