
# Script para generar las dimensiones aplicando los casos de diseño 
# especificados en la documentación del proyecto.

################################################################################
#' Función para generar la dimensión Cuándo a partir de las fechas:
#' - días naturales de los datos (1 dataset)
#' - días en que se han notificado datos (el resto de datasets)
#' @param path_proyecto Es el camino al proyecto (carpeta que contiena a su vez
#' al diretorio 'datos/', donde a su vez encontraremos todos los datasets 
#' que necesitamos). 
generar_dimension_cuando <- function(path_proyecto){
  
  library(rio)
  
  setwd(path_proyecto)
  # setwd("C:\\Users\\UX430U\\Desktop\\TFG")
  
  cuando <- import("datos/datos_provincia_diarios.csv")
  
  #install.packages("lubridate")
  library(lubridate)

  # añadir las columnas de mes, año y semana del año (niveles de la dimensión)
  cuando$Mes <- month(dmy(cuando$`Fecha diagnóstico`) )
  cuando$Año <- year(dmy(cuando$`Fecha diagnóstico`))
  cuando$Semana <- week(dmy(cuando$`Fecha diagnóstico`))
  
  # eliminar las columnas que no son de la dimensión 
  cuando <- cuando[,c(1,8,9,10)]
  
  # borrar duplicados
  cuando <- cuando[!duplicated(cuando),]
  colnames(cuando) <- c("Fecha", "Mes", "Año", "Semana", "Nombre del Mes")

  # añadir la columna mes (inicialmente vacía) "Nombre del Mes" 
  # para rellenarla con el nombre del mes asociado a la columna Mes (número)
  cuando$`Nombre del Mes` <- ""
  for(i in 1:nrow(cuando)){
    if(cuando[i,"Mes"] == 1){
      cuando[i,"Nombre del Mes"] <- "Enero"
    } else if(cuando[i,"Mes"] == 2){
      cuando[i,"Nombre del Mes"] <- "Febrero"
    } else if(cuando[i,"Mes"] == 3){
      cuando[i,"Nombre del Mes"] <- "Marzo"
    } else if(cuando[i,"Mes"] == 4){
      cuando[i,"Nombre del Mes"] <- "Abril"
    } else if(cuando[i,"Mes"] == 5){
      cuando[i,"Nombre del Mes"] <- "Mayo"
    } else if(cuando[i,"Mes"] == 6){
      cuando[i,"Nombre del Mes"] <- "Junio"
    } else if(cuando[i,"Mes"] == 7){
      cuando[i,"Nombre del Mes"] <- "Julio"
    } else if(cuando[i,"Mes"] == 8){
      cuando[i,"Nombre del Mes"] <- "Agosto"
    } else if(cuando[i,"Mes"] == 9){
      cuando[i,"Nombre del Mes"] <- "Septiembre"
    } else if(cuando[i,"Mes"] == 10){
      cuando[i,"Nombre del Mes"] <- "Octubre"
    } else if(cuando[i,"Mes"] == 11){
      cuando[i,"Nombre del Mes"] <- "Noviembre"
    } else if(cuando[i,"Mes"] == 12){
      cuando[i,"Nombre del Mes"] <- "Diciembre"
    } 
  }
  
  # aplicación del caso de diseño: dimensión con varios roles 
  # vamos a añadir el atributo "Día natural" para indicar que las fechas del
  # dataset actual son por días naturales (L a D), frente a los que añadiremos
  # después, que serán por fecha de notificación (días hábiles -> L a V).
  cuando$`Tipo de Fecha` <- "Día natural"

  # la tabla que tenemos hasta ahora la dejamos como está
  
  # Segundo tipo de Fecha: escoger cualquiera de otros datasets con datos COVID
  # descargados del IECA que contenga la fecha de notitifación de esos datos
  
  
  
}