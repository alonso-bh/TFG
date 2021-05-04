
# Leer el camino a la carpeta de trabajo

args = commandArgs(trailingOnly=TRUE)

if (length(args)==0) {
  stop("Debes proporcionar el camino (absoluto) a la carpeta de trabajo 'TFG'.")
} else if (length(args)==1) {
  # default output file
  sprintf("El camino introducido es: %s", args[1])
  setwd(args[1])
}


#setwd("C:/Users/UX430U/Desktop/TFG")

source("proyecto_tfg/utils.R")

# generar la carpeta para guardar los datos de hoy 
hoy <- format(Sys.time(), "%d-%m") # formato establecido: dd-mm
nombre_carpeta <- concatenar_strings(c("datos/", hoy))
nombre_carpeta
dir.create(nombre_carpeta)
#setwd(nombre_carpeta)


#########################
# DESCARGA DE FICHEROS 
#########################

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

path_provincias <- obtener_path_provincias_hoy(nombre_carpeta)

# alternativa por badea (parece que es más fiable)
download.file("https://www.juntadeandalucia.es/institutodeestadisticaycartografia/badea/stpivot/stpivot/Print?cube=89e6d2a9-cfcd-4f0c-8d31-a6290fe8c4d8&type=0&foto=si&ejecutaDesde=&codConsulta=38665&consTipoVisua=JP" ,
              destfile=path_provincias[1],
              method="curl")
download.file("https://www.juntadeandalucia.es/institutodeestadisticaycartografia/badea/stpivot/stpivot/Print?cube=49db88b3-a373-4b06-9f05-0041ec9fad7b&type=0&foto=si&ejecutaDesde=&codConsulta=38637&consTipoVisua=JP" ,
              destfile=path_provincias[2],
              method="curl")
download.file("https://www.juntadeandalucia.es/institutodeestadisticaycartografia/badea/stpivot/stpivot/Print?cube=e7748177-b666-4b81-a7a5-aa8f30ebaef0&type=0&foto=si&ejecutaDesde=&codConsulta=38666&consTipoVisua=JP" , 
              destfile=path_provincias[3],
              method="curl")
download.file("https://www.juntadeandalucia.es/institutodeestadisticaycartografia/badea/stpivot/stpivot/Print?cube=50f4d76c-fab7-48b1-abd6-f12662c50a08&type=0&foto=si&ejecutaDesde=&codConsulta=38667&consTipoVisua=JP" ,
              destfile=path_provincias[4],
              method="curl")
download.file("https://www.juntadeandalucia.es/institutodeestadisticaycartografia/badea/stpivot/stpivot/Print?cube=a59d4919-b0eb-40c1-a357-729cb2231269&type=0&foto=si&ejecutaDesde=&codConsulta=38668&consTipoVisua=JP" ,
              destfile=path_provincias[5],
              method="curl")
download.file("https://www.juntadeandalucia.es/institutodeestadisticaycartografia/badea/stpivot/stpivot/Print?cube=68de058c-07fe-47a4-83d3-c3088a00de7a&type=0&foto=si&ejecutaDesde=&codConsulta=38669&consTipoVisua=JP" ,
              destfile=path_provincias[6],
              method="curl")
download.file("https://www.juntadeandalucia.es/institutodeestadisticaycartografia/badea/stpivot/stpivot/Print?cube=fe4bef26-fb17-4eac-8b22-2d1b0768c80d&type=0&foto=si&ejecutaDesde=&codConsulta=38674&consTipoVisua=JP" ,
              destfile=path_provincias[7],
              method="curl")
download.file("https://www.juntadeandalucia.es/institutodeestadisticaycartografia/badea/stpivot/stpivot/Print?cube=3e87a401-0d32-47c5-947c-b69cf26a4f5e&type=0&foto=si&ejecutaDesde=&codConsulta=38676&consTipoVisua=JP" ,
              destfile=path_provincias[8],
              method="curl")


# descargar datos de residencias
path_residencias <- concatenar_strings(c(nombre_carpeta, "/residencias.xls"))
download.file("https://www.juntadeandalucia.es/institutodeestadisticaycartografia/badea/stpivot/stpivot/Print?cube=484ce06f-403a-457e-b75b-750f47561823&type=0&foto=si&ejecutaDesde=&codConsulta=38528&consTipoVisua=JP",
              destfile=path_residencias, 
              method="curl")


# descargar datos de residencias por rangos de edades
path_residencias_edad_sexo <- concatenar_strings(c(nombre_carpeta, "/residencias_edad.xls"))
download.file("https://www.juntadeandalucia.es/institutodeestadisticaycartografia/badea/stpivot/stpivot/Print?cube=618a9dcc-cd79-4075-b933-48f33c69c737&type=0&foto=si&ejecutaDesde=&codConsulta=38378&consTipoVisua=JP", 
              destfile=path_residencias_edad_sexo, 
              method="curl")
source("proyecto_tfg/preparar_residencias_edad_sexo.R")
#preparar_datos_residencias_edad_sexo(getwd())


# descargar datos diarios sobre personal profesional de riesgo
path_profesionales <- concatenar_strings(c(nombre_carpeta, "/profesionales.xls"))
download.file("https://www.juntadeandalucia.es/institutodeestadisticaycartografia/badea/stpivot/stpivot/Print?cube=753943e2-375b-4d77-80bd-368aac3e255e&type=0&foto=si&ejecutaDesde=&codConsulta=40256&consTipoVisua=JP", 
              destfile=path_profesionales, 
              method="curl")
source("proyecto_tfg/preparar_datos_profesionales.R")
#preparar_datos_profesionales(getwd()) 

# vacunas
path_vacunas <- concatenar_strings(nombre_carpeta, "/vacunas.ods")
download.file("https://www.mscbs.gob.es/profesionales/saludPublica/ccayes/alertasActual/nCov/documentos/Informe_Comunicacion_20210423.ods", 
              destfile = "vacunas.ods", 
              method = "curl")


# datos diarios hospitalizaciones y otros
path_dias_naturales <- concatenar_strings("datos", "/")
source("proyecto_tfg/preparar_hospitalizaciones.R")
download.file("https://www.juntadeandalucia.es/institutodeestadisticaycartografia/badea/stpivot/stpivot/Print?cube=c4a0a3cd-43ec-4a09-9184-2e5bf28c348e&type=0&foto=si&ejecutaDesde=&codConsulta=39409&consTipoVisua=JP",
              destfile="datos/sit_hospitalaria.xls", 
              method="curl")
#preparar_datos_hospitalizaciones(getwd()) 


