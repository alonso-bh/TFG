install.packages("RCurl")
library(RCurl)


# función para concatenar
concatenar_strings <- function(cad1, cad2){
  tmp <- cbind(cad1, cad2)
  library(stringr)
  str_c(tmp, collapse = "")
}

# preparar el formato de fecha para nombrar así a la carpeta de hoy
hoy <- format(Sys.time(), "%d-%m") # formato establecido: dd-mm

# crear la carpeta para el día de hoy


setwd("datos")
nombre_carpeta <- concatenar_strings("\\datos\\", hoy)
nombre_carpeta
dir.create(nombre_carpeta)
setwd(hoy)

# descargar ficheros de datos por provincia-dist.sanit.-pueblo
# almería
download.file("https://www.juntadeandalucia.es/institutodeestadisticaycartografia/salud/static/xls/Covid_04.xls",destfile="Covid_04.xls",method="libcurl")

# descargar datos de hospitalizaciones por provincias


