# install.packages("RCurl")
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
setwd("C:\\Users\\UX430U\\Desktop\\TFG")
nombre_carpeta <- concatenar_strings("datos/", hoy)
nombre_carpeta
dir.create(nombre_carpeta)
setwd(nombre_carpeta)
getwd()
# descargar ficheros de datos por provincia-dist.sanit.-pueblo
download.file("https://www.juntadeandalucia.es/institutodeestadisticaycartografia/salud/static/xls/Covid_04.xls",destfile="Covid_04.xls",method="curl")
download.file("https://www.juntadeandalucia.es/institutodeestadisticaycartografia/salud/static/xls/Covid_11.xls",destfile="Covid_11.xls",method="curl")
download.file("https://www.juntadeandalucia.es/institutodeestadisticaycartografia/salud/static/xls/Covid_14.xls",destfile="Covid_14.xls",method="curl")
download.file("https://www.juntadeandalucia.es/institutodeestadisticaycartografia/salud/static/xls/Covid_18.xls",destfile="Covid_18.xls",method="curl")
download.file("https://www.juntadeandalucia.es/institutodeestadisticaycartografia/salud/static/xls/Covid_21.xls",destfile="Covid_21.xls",method="curl")
download.file("https://www.juntadeandalucia.es/institutodeestadisticaycartografia/salud/static/xls/Covid_23.xls",destfile="Covid_23.xls",method="curl")
download.file("https://www.juntadeandalucia.es/institutodeestadisticaycartografia/salud/static/xls/Covid_29.xls",destfile="Covid_29.xls",method="curl")
download.file("https://www.juntadeandalucia.es/institutodeestadisticaycartografia/salud/static/xls/Covid_41.xls",destfile="Covid_41.xls",method="curl")


# descargar datos de residencias
download.file("https://www.juntadeandalucia.es/institutodeestadisticaycartografia/badea/stpivot/stpivot/Print?cube=484ce06f-403a-457e-b75b-750f47561823&type=0&foto=si&ejecutaDesde=&codConsulta=38528&consTipoVisua=JP",destfile="residencias.xls",method="curl")


# datos diarios hospitalizaciones y otros
setwd("C:\\Users\\UX430U\\Desktop\\TFG\\datos")
download.file("https://www.juntadeandalucia.es/institutodeestadisticaycartografia/badea/stpivot/stpivot/Print?cube=c4a0a3cd-43ec-4a09-9184-2e5bf28c348e&type=0&foto=si&ejecutaDesde=&codConsulta=39409&consTipoVisua=JP",destfile="sit_hospitalaria.xls",method="curl")
