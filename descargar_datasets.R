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
# download.file("https://www.juntadeandalucia.es/institutodeestadisticaycartografia/salud/static/xls/Covid_04.xls",destfile="Covid_04.xls",method="curl")
# download.file("https://www.juntadeandalucia.es/institutodeestadisticaycartografia/salud/static/xls/Covid_11.xls",destfile="Covid_11.xls",method="curl")
# download.file("https://www.juntadeandalucia.es/institutodeestadisticaycartografia/salud/static/xls/Covid_14.xls",destfile="Covid_14.xls",method="curl")
# download.file("https://www.juntadeandalucia.es/institutodeestadisticaycartografia/salud/static/xls/Covid_18.xls",destfile="Covid_18.xls",method="curl")
# download.file("https://www.juntadeandalucia.es/institutodeestadisticaycartografia/salud/static/xls/Covid_21.xls",destfile="Covid_21.xls",method="curl")
# download.file("https://www.juntadeandalucia.es/institutodeestadisticaycartografia/salud/static/xls/Covid_23.xls",destfile="Covid_23.xls",method="curl")
# download.file("https://www.juntadeandalucia.es/institutodeestadisticaycartografia/salud/static/xls/Covid_29.xls",destfile="Covid_29.xls",method="curl")
# download.file("https://www.juntadeandalucia.es/institutodeestadisticaycartografia/salud/static/xls/Covid_41.xls",destfile="Covid_41.xls",method="curl")


# acceso a datos covid: https://www.juntadeandalucia.es/institutodeestadisticaycartografia/badea/informe/anual?CodOper=b3_2314&idNode=42348

# alternativa por badea (estar pendiente si este es el enlace definitivo)
download.file("https://www.juntadeandalucia.es/institutodeestadisticaycartografia/badea/stpivot/stpivot/Print?cube=89e6d2a9-cfcd-4f0c-8d31-a6290fe8c4d8&type=0&foto=si&ejecutaDesde=&codConsulta=38665&consTipoVisua=JP" ,destfile="Covid_04.xls",method="curl")
download.file("https://www.juntadeandalucia.es/institutodeestadisticaycartografia/badea/stpivot/stpivot/Print?cube=49db88b3-a373-4b06-9f05-0041ec9fad7b&type=0&foto=si&ejecutaDesde=&codConsulta=38637&consTipoVisua=JP" ,destfile="Covid_11.xls",method="curl")
download.file("https://www.juntadeandalucia.es/institutodeestadisticaycartografia/badea/stpivot/stpivot/Print?cube=e7748177-b666-4b81-a7a5-aa8f30ebaef0&type=0&foto=si&ejecutaDesde=&codConsulta=38666&consTipoVisua=JP" ,destfile="Covid_14.xls",method="curl")
download.file("https://www.juntadeandalucia.es/institutodeestadisticaycartografia/badea/stpivot/stpivot/Print?cube=50f4d76c-fab7-48b1-abd6-f12662c50a08&type=0&foto=si&ejecutaDesde=&codConsulta=38667&consTipoVisua=JP" ,destfile="Covid_18.xls",method="curl")
download.file("https://www.juntadeandalucia.es/institutodeestadisticaycartografia/badea/stpivot/stpivot/Print?cube=a59d4919-b0eb-40c1-a357-729cb2231269&type=0&foto=si&ejecutaDesde=&codConsulta=38668&consTipoVisua=JP" ,destfile="Covid_21.xls",method="curl")
download.file("https://www.juntadeandalucia.es/institutodeestadisticaycartografia/badea/stpivot/stpivot/Print?cube=68de058c-07fe-47a4-83d3-c3088a00de7a&type=0&foto=si&ejecutaDesde=&codConsulta=38669&consTipoVisua=JP" ,destfile="Covid_23.xls",method="curl")
download.file("https://www.juntadeandalucia.es/institutodeestadisticaycartografia/badea/stpivot/stpivot/Print?cube=fe4bef26-fb17-4eac-8b22-2d1b0768c80d&type=0&foto=si&ejecutaDesde=&codConsulta=38674&consTipoVisua=JP" ,destfile="Covid_29.xls",method="curl")
download.file("https://www.juntadeandalucia.es/institutodeestadisticaycartografia/badea/stpivot/stpivot/Print?cube=3e87a401-0d32-47c5-947c-b69cf26a4f5e&type=0&foto=si&ejecutaDesde=&codConsulta=38676&consTipoVisua=JP" ,destfile="Covid_41.xls",method="curl")


# descargar datos de residencias
download.file("https://www.juntadeandalucia.es/institutodeestadisticaycartografia/badea/stpivot/stpivot/Print?cube=484ce06f-403a-457e-b75b-750f47561823&type=0&foto=si&ejecutaDesde=&codConsulta=38528&consTipoVisua=JP",destfile="residencias.xls",method="curl")


# datos diarios hospitalizaciones y otros
setwd("C:\\Users\\UX430U\\Desktop\\TFG\\datos")
download.file("https://www.juntadeandalucia.es/institutodeestadisticaycartografia/badea/stpivot/stpivot/Print?cube=c4a0a3cd-43ec-4a09-9184-2e5bf28c348e&type=0&foto=si&ejecutaDesde=&codConsulta=39409&consTipoVisua=JP",destfile="sit_hospitalaria.xls",method="curl")


# comprobaciones previas a la descarga (no descargar si no están actualizados)
#install.packages("rvest")
#library('rvest')

#ppal <- read_html('https://www.juntadeandalucia.es/institutodeestadisticaycartografia/salud/static/index.html5')

#obtener parrafo
# parrafo <- ppal  %>%
#   html_nodes("#arboTxt") %>%
#   html_text()
# parrafo

# obtener la línea donde está la fecha
# linea_fecha <- parrafo %>% 
#   html_nodes("strong") %>% html_text()
