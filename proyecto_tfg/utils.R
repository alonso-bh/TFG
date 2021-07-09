# fichero con funciones útiles a usar a lo largo del proyecto
# ALONSO BUENO HERRERO


################################################################################
#' OBTENER UNA LISTA CON LAS PROVINCIAS DE ANDALUCÍA
#'
#' Recibe opcionalmente el path del proyecto (carpeta raíz TFG), aunque lo
#' habitual será que no se le pase, y tome getwd(), suponiendo que se le llama
#' desde una función que ya ha fijado previamente este valor.
#' @param path_proyecto es un parámetro opcional para indicar el path donde 
#' está la carpeta de trabajo. Lo habitual es que no se especifique, y se tome
#' el valor por defecto. 
obtener_provincias <- function(path_proyecto = getwd()) {
  library(rio)
  setwd(path_proyecto)
  
  provincias <- import("datos/cod_provincias.csv")
  provincias <- provincias$nombre_provincia
  provincias <- provincias[c(3,13,17,22,24,26,31,42)]
  
  return(provincias)
}



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
#' a cada fichero de cada provincia. Ejemplo: datos/25-04-21/Covid_04.xls (Almería)
#' @param path_dir_hoy es la cadena formada por "datos/" y la carpeta asociada
#' al día, con el formato dd-mm-yy. Por ejemplo: 'datos/25-04-21'. 
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

