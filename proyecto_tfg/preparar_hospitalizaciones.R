# ALONSO BUENO HERRERO
#
# Script para trabajar con el fichero de datos a nivel de provincia no acumulados

#setwd("C:\\Users\\UX430U\\Desktop\\TFG\\datos")

preparar_datos_hospitalizaciones <- function(path = getwd()){
  
  library('tidyr')   # para el fill
  library('readr')
  library("rio")
  
  setwd(path)
  
  excel <- import("sit_hospitalaria.xls")
  
  # delete head non-valid rows
  excel <- excel[-c(1,2,3,4),]  
  
  # renombrar cabecera con nombres auxiliares para evitar problemas de codifica-
  #  ción de acentos 
  colnames(excel) <- c("V1", "V2", "V3", "V4","V5","V6","V7")
  
  # rellenar primera columna con la fecha correspondiente (primera fila del día)
  excel <- excel %>% fill(V1, .direction = "down")
  
  # eliminar filas de datos totales (Andalucía), pues se calcularán al hacer
  #  roll-up por el cubo 
  #excel <- excel[!(excel$V2 == "Andalucía"),]
  
  colnames(excel) <- excel[1,]
  excel <- excel[-c(1),]
  
  
  
  View(excel)
  
  write.table(excel, "datos_provincia_diarios.csv", row.names=FALSE, col.names=TRUE, sep = ';')
  
}


#preparar_datos_hospitalizaciones()

