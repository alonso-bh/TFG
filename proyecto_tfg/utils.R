# fichero con funciones útiles a usar a lo largo del proyecto
# ALONSO BUENO HERRERO



################################################################################
#' FUNCION PARA CONCATENAR CADENAS DE CARACTERES
#' 
#' Recibe una lista de cadenas de caracteres y devuelve una nueva cadena 
#' de caracteres con la concatenación, sin ningún separador entre 
#' ellas.
#' 
#' @param cadenas Es una lista cadena de caracteres (tipo "character").
#' @return Cadena de caracteres (tipo "character") con las cadenas 'cad1' y
#' 'cad2' concatenadas, en ese orden, y sin ningún separador entre ambas
concatenar_strings <- function(cadenas = c()){
  tmp <- cbind(cadenas)
  library(stringr)
  str_c(tmp, collapse = "")
}


################################################################################
#' Obtener array con el camino relativo (dentro de la carpeta de trabajo TFG) 
#' a cada fichero de cada provincia. Ejemplo: datos/25-04/Covid_04.xls (Almería)
#' @param path_dir_hoy es la cadena formada por "datos/" y la carpeta asociada
#' al día, con el formato DD-MM. Por ejemplo: 'datos/25-04'. 
obtener_path_provincias_hoy <- function (path_dir_hoy = ""){
  
  path_provincias <- c(
    concatenar_strings(c(path_dir_hoy, "/", "Covid_04.xls")),
    concatenar_strings(c(path_dir_hoy, "/", "Covid_11.xls")),
    concatenar_strings(c(path_dir_hoy, "/", "Covid_14.xls")),
    concatenar_strings(c(path_dir_hoy, "/", "Covid_18.xls")),
    concatenar_strings(c(path_dir_hoy, "/", "Covid_21.xls")),
    concatenar_strings(c(path_dir_hoy, "/", "Covid_23.xls")),
    concatenar_strings(c(path_dir_hoy, "/", "Covid_29.xls")),
    concatenar_strings(c(path_dir_hoy, "/", "Covid_41.xls")) 
  )
  
  return(path_provincias)
}


################################################################################
#' Función para obtener la fecha de ayer (día anterior al día en que se llame
#' a esta función) usando el objeto Sys.Date
#' 
#' @return Camino completo a la carpeta del día anterior
#obtener_fecha_ayer <- function(){
  # obtener fecha de ayer con el formato de "DD-MM", que es el usado en este
  #  proyecto
  #date_yesterday <- format(Sys.Date()-1, "%d-%m")
  
  # construir path al directorio del día anterior (yesterday)
  #path_yesterday <- concatenar_strings("C:\\Users\\UX430U\\Desktop\\TFG\\datos\\", date_yesterday)
  
  #return (path_yesterday)
#}


