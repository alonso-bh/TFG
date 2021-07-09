# ALONSO BUENO HERRERO - 2021


################################################################################
#' FICHERO PARA TRANSFORMAR EL DATASET CON DATOS DE CADA DÍA DEL AÑO (DÍAS 
#' NATURALES)
#' 
#' @param path_fichero es un parámetro obligatorio que espefica la ruta del
#' fichero XLS descargado del portal IECA. La ruta debería ser relativa (aunque
#' si es absoluta, evidentemente, no importa) dentro de la carpeta de trabajo
#' "TFG".  
preparar_datos_dias_naturales <- function(path_fichero = getwd()){
  
  library('tidyr')   # para el fill
  library('readr')
  library("rio")

  # path_fichero <- path_dias_naturales  # descomentar para las pruebas

  excel <- import(path_fichero)
  
  # delete head non-valid rows
  excel <- excel[-c(1,2,3,4),]  
  
  # renombrar cabecera temporalmente para más comodidad
  colnames(excel) <- c("V1", "V2", "V3", "V4","V5","V6","V7")
  
  # rellenar primera columna con la fecha correspondiente (primera fila del día)
  excel <- excel %>% fill(V1, .direction = "down")
  
  # extraer nombre real de las columnas 
  cabecera <- excel[1,]    # fila con los nombres
  excel <- excel[-c(1),]          # borrar esta fila
  
  # eliminar filas de datos totales (Andalucía), pues se calcularán al hacer
  #  roll-up por el cubo 
  source("proyecto_tfg/utils.R")
  provincias <- obtener_provincias()
  #excel <- excel[!(excel$V2 == "Andalucía"),]
  excel <- excel[ (is.element(excel$V2, provincias)), ]
  
  
  # salvar los datos
  write.table(excel, "datos/datos_dias_naturales.csv", row.names=FALSE, col.names=TRUE, sep = ';')
  
}


# source("proyecto_tfg/utils.R")
# preparar_datos_dias_naturales()

