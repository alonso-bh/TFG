# script para generar las tablas de hechos a partir de las tablas estáticas
# extraídas de las fuentes de datos y las tablas de las dimensiones, previamente
# generadas con el script 'generar_dimensiones.R' en formato CSV


#setwd("C:\\Users\\UX430U\\Desktop\\TFG")


# HECHOS 1: Días naturales
generar_hechos_dias_naturales <- function(path_proyecto) {
  
  library(rio)
  library(tidyr)
  library(dplyr)
  
  setwd(path_proyecto)
  
  # cargar fichero plano de residencias.csv
  hechos <- import("datos/datos_dias_naturales.csv")
  dimension_cuando <- import("datos/dimension_cuando.csv")
  dimension_donde_provincia <- import("datos/dimension_donde_provincia.csv")
  
  hechos <- hechos[!(hechos$Territorio == "Andalucía"),]
  dimension_cuando <- dimension_cuando[!(dimension_cuando$tipo_fecha == 'Día hábil'),]
  
    
  # normalizar nombres de columnas ("lower_case", y sin acentos)
  colnames(hechos) <- tolower(gsub(' ', '_', colnames(hechos)))
  colnames(hechos)[c(1,2)] <- c("fecha","nombre_provincia")
  
  # hacer left_join para obtener las llaves generadas y guardarlas en la tabla de hechos
  hechos <- left_join(hechos, dimension_cuando, by= "fecha")

  # quitar columnas que no sirven 
  hechos <- hechos[,-c(1,8,9,10,11,12)]
  
  # añadir cod_donde_provincia (dimensión geográfica)
  hechos <- left_join(hechos, dimension_donde_provincia, by="nombre_provincia")
  
  # eliminar columnas inservibles
  hechos <- hechos[,-c(1,9)]
  
  # almacenar esta tabla de hechos
  write.table(hechos, "datos/hechos_dias_naturales.csv", row.names=FALSE, col.names=TRUE, sep = ';')
  
}


# HECHOS 2: Municipios
generar_hechos_municipios <- function(path_proyecto){
  
  library(rio)
  
  setwd(path_proyecto)
  
  hechos <- import("datos/municipios.csv")

  colnames(hechos) <- c()
    
}


# HECHOS 3: Residencias geográfico
generar_hechos_residencias <- function(path_proyecto) {
  
  library(rio)
  library(dplyr)

  setwd(path_proyecto)
  
  hechos <- import("datos/residencias.csv")  
  
  # cambiar nombre a tabla
  colnames(hechos) <- c("distrito_sanitario", 
                        "confirmados_pdia", 
                        "confirmados_pdia_14d", 
                        "confirmados_pdia_7d", 
                        "total_confirmados", 
                        "curados", 
                        "fallecidos", 
                        "tipo_residencia", 
                        "fecha")
  
  donde <- import("datos/dimension_donde_ds.csv")

  # hacer JOINs con las dimensiones para obtener las llaves generadas
  hechos <- left_join(hechos, donde, by="distrito_sanitario")
    
  hechos <- hechos[,-c(1,11,12)]
  
  cuando <- import("datos/dimension_cuando.csv")
  cuando <- cuando[ (cuando$tipo_fecha == "Día hábil") ,]
  
  hechos <- left_join(hechos, cuando, by = "fecha")
  hechos <- hechos[, -c(8,10,11,12,13,14)]  
  
  residencia_dim <- import("datos/dimension_residencia.csv")
  hechos <- left_join(hechos, residencia_dim, by = "tipo_residencia")
  hechos <- hechos[,-7]    
  
  write.table(hechos, "datos/hechos_residencias.csv", row.names=FALSE, col.names=TRUE, sep = ';')
  
}


# Hechos 5: Vacunas
generar_hechos_vacunas <- function(path_proyecto = getwd()){
  
  library(rio)
  library(dplyr)
  
  setwd(path_proyecto)
  
  hechos <- import("datos/vacunas.csv")

  colnames(hechos) <- c("nombre_provincia", 
                        "n_personas_1_dosis", 
                        "%personas_1_dosis" ,  
                        "rango_edad", 
                        "n_personas_pauta_completa", 
                        "%personas_pauta_completa", 
                        "fecha")

  # hacer los JOIN con las dimensiones participantes de este foco de atención:
  
  # dimensión cuándo
  cuando <- import("datos/dimension_cuando.csv")
  cuando <- cuando[(cuando$tipo_fecha == 'Día hábil'),] # solo días hábiles
  hechos <- left_join(hechos, cuando, by='fecha') 
  hechos <- hechos[,-c(7,8,9,10,11,12)]
  
  # dimensión donde-provincia
  provincia <- import("datos/dimension_donde_provincia.csv")
  hechos <- left_join(hechos, provincia, by = "nombre_provincia")
  hechos <- hechos[,-c(1,9)]
  
  # dimension quien (edades vacunas)
  edades <- import("datos/dimension_quien_vacunas.csv")
  hechos <- left_join(hechos, edades, by = "rango_edad")
  hechos <- hechos[,-3]
  
  # una vez preparada la tabla de hechos, la almacenamos
  write.table(hechos, "datos/hechos_vacunas.csv", row.names=FALSE, col.names=TRUE, sep = ';')
  
}