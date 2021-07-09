# Alonso Bueno Herrero

################################################################################
#' FUNCION PARA PROCESAR EL FICHERO AUXILIAR CON EL CÓDIGO INE DE LAS PROVINCIAS
#' @param path_proyecto es la ruta a la carpeta del proyecto
preparar_cod_provincias <- function(path_proyecto = getwd()){
  
  library(tidyr)
  library(rio)
  
  setwd(path_proyecto)
  
  # importar fichero original del INE
  excel <- import("datos/20_cod_prov.xls")
  
  # borrar filas nulas
  excel <- drop_na(excel)
  
  # generar nombre de columnas correctamente
  excel <- excel[-c(1),]
  colnames(excel) <- c("cod_provincia", "nombre_provincia" )
  
  # almacenar fichero
  excel$cod_provincia <- as.character(excel$cod_provincia)
  write.table(excel, "datos/cod_provincias.csv", row.names=FALSE, 
              col.names=TRUE, sep = ';')
  
}
