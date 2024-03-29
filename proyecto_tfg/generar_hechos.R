# script para generar las tablas de hechos a partir de las tablas estáticas
# extraídas de las fuentes de datos y las tablas de las dimensiones, previamente
# generadas con el script 'generar_dimensiones.R' en formato CSV



##########################
# PROCEDIMIENTOS PARA GENERAR TABLAS DE HECHOS EN FORMATO .CSV
##########################


################################################################################
# HECHOS 1: Días naturales
generar_hechos_dias_naturales <- function() {
  
  library(rio)
  library(tidyr)
  library(dplyr)
  source("proyecto_tfg/utils.R")
  

  hechos <- import("datos/datos_dias_naturales.csv")
  
  # cargar dimensiones 
  dimension_cuando <- import("datos/dimension_cuando.csv")
  dimension_donde_provincia <- import("datos/dimension_donde_provincia.csv")
  
  # quitar valores acumulados
  provincias <- obtener_provincias()  
  hechos <- hechos[ is.element(hechos$Territorio, provincias), ] 
  
  # normalizar nombres de columnas ("lower_case", y sin acentos);
  # es una alternativa a establecer explícitamente (a través de una lista de 
  # strings) dichos nombres 
  colnames(hechos) <- tolower(gsub(' ', '_', colnames(hechos)))
  colnames(hechos)[c(1,2)] <- c("fecha","nombre_provincia")
  
  # hacer left_join para obtener las llaves generadas y guardarlas en la tabla de hechos
  hechos <- left_join(hechos, dimension_cuando, by= "fecha")

  # quitar columnas que no sirven 
  hechos <- hechos[,-c(1,8,9,10,11,12,13)]
  
  # añadir cod_donde_provincia (dimensión geográfica)
  hechos <- left_join(hechos, dimension_donde_provincia, by="nombre_provincia")
  
  # eliminar columnas inservibles
  hechos <- hechos[,-c(1,9)]
  
  # almacenar esta tabla de hechos
  write.table(hechos, "datos/hechos_dias_naturales.csv", quote = FALSE , row.names=FALSE, col.names=TRUE, sep = ';')
}



################################################################################
# HECHOS 2: Municipios
generar_hechos_municipios <- function(){
  
  library(rio)
  library(dplyr)
  library(stringr)
  

  hechos <- import("datos/municipios.csv")

  colnames(hechos) <- c("nombre_municipio", 
                        "poblacion", 
                        "confirmados_pdia", 
                        "confirmados_pdia_14d", 
                        "tasa_confirmados_pdia_14d", 
                        "confirmados_pdia_7d", 
                        "total_confirmados", 
                        "curados", 
                        "fallecidos",
                        "fecha")
    
  
  # JOINs con las dimensiones para obtener llaves generadas
  # dimensión cuándo
  cuando <- import("datos/dimension_cuando.csv")
  cuando <- cuando[(cuando$es_dia_habil == TRUE ),] # solo días hábiles
  hechos <- left_join(hechos, cuando, by='fecha') 
  hechos <- hechos[,-c(10,11,12,13,14,15,16)]
  
  # dimension dónde
  donde <- import("datos/dimension_donde_municipio.csv")
  hechos <- left_join(hechos, donde, by = "nombre_municipio")  
  hechos <- hechos[,-c(1,12,13,14,15)]
  
  # reduciendo decimales de la tasa
  # hechos$tasa_confirmados_pdia_14d <- str_replace(hechos$tasa_confirmados_pdia_14d , ',' , '\\.')
  hechos$tasa_confirmados_pdia_14d <- as.double(hechos$tasa_confirmados_pdia_14d)
  hechos$tasa_confirmados_pdia_14d <- round(hechos$tasa_confirmados_pdia_14d, digits = 2)
  hechos$tasa_confirmados_pdia_14d <- str_replace(hechos$tasa_confirmados_pdia_14d , '\\.' , ',')  
  
  # revisar que no haya nulos en las mediciones
  hechos[ (is.na(hechos$tasa_confirmados_pdia_14d)) , "tasa_confirmados_pdia_14d"] <- 0
  
  # salvar tabla de hechos
  write.table(hechos, "datos/hechos_municipios.csv", quote = FALSE, row.names=FALSE, col.names=TRUE, sep = ';')
  
}




################################################################################
# HECHOS 3: Residencias geográfico
generar_hechos_residencias <- function() {
  
  library(rio)
  library(dplyr)

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

  
  # hacer JOINs con las dimensiones para obtener las llaves generadas
  donde <- import("datos/dimension_donde_ds.csv")
  hechos <- left_join(hechos, donde, by="distrito_sanitario")
  hechos <- hechos[,-c(1,11,12)]
  
  cuando <- import("datos/dimension_cuando.csv")
  cuando <- cuando[ (cuando$es_dia_habil == TRUE ), ]
  
  hechos <- left_join(hechos, cuando, by = "fecha")
  hechos <- hechos[, -c(8,10,11,12,13,14,15)]  
  
  residencia_dim <- import("datos/dimension_residencia.csv")
  hechos <- left_join(hechos, residencia_dim, by = "tipo_residencia")
  hechos <- hechos[,-7]    
  
  write.table(hechos, "datos/hechos_residencias.csv", quote = FALSE , row.names=FALSE, col.names=TRUE, sep = ';')
  
}


################################################################################
# hecho4 : residencias por edad-sexo
generar_hechos_residencias_edad_sexo <- function(){
  
  library(rio)
  library(dplyr)
  
  hechos <- import("datos/residencias_edad_sexo.csv")  
  
  # JOIN con dimension quién
  quien <- import("datos/dimension_quien.csv")
  colnames(hechos) <- c("tipo_residencia", "rango_edad", "confirmados_pdia", "total_confirmados", "curados", "fallecidos", "sexo", "fecha")
  hechos <- left_join(hechos, quien, by = c("tipo_residencia", "rango_edad", "sexo") )  
  
  # eliminar columnas inservibles tras la fusión
  hechos <- hechos[,-c(1,2,7)]
  
  # hacer JOIN con la dimensión Cuándo 
  cuando <- import("datos/dimension_cuando.csv")
  cuando <- cuando[ (cuando$es_dia_habil == TRUE ), ]
  hechos <- left_join(hechos, cuando, by = "fecha")
  
  # eliminar columnas inservibles tras el join
  hechos <- hechos[, c("confirmados_pdia", 
                       "total_confirmados", 
                       "curados", 
                       "fallecidos", 
                       "cod_quien", 
                       "cod_cuando")]

  # almacenar la tabla
  write.table(hechos, "datos/hechos_residencias_edad_sexo.csv", row.names=FALSE, col.names=TRUE, quote = FALSE , sep = ';')
  
}



################################################################################
# Hechos 5: Vacunas
generar_hechos_vacunas <- function(){
  
  library(rio)
  library(dplyr)
  
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
  cuando <- cuando[(cuando$es_dia_habil == TRUE ),] # solo días hábiles
  hechos <- left_join(hechos, cuando, by='fecha') 
  hechos <- hechos[,-c(7,8,9,10,11,12,13)]
  
  # dimensión donde-provincia
  provincia <- import("datos/dimension_donde_provincia.csv")
  hechos <- left_join(hechos, provincia, by = "nombre_provincia")
  hechos <- hechos[,-c(1,9)]
  
  # dimension quien (edades vacunas)
  edades <- import("datos/dimension_quien_vacunas.csv")
  hechos <- left_join(hechos, edades, by = "rango_edad")
  hechos <- hechos[,-c(3,9)]
  
  # una vez preparada la tabla de hechos, la almacenamos
  write.table(hechos, "datos/hechos_vacunas.csv", row.names=FALSE, quote = FALSE, 
              col.names=TRUE, sep = ';')
  
}




################################################################################
generar_hechos_profesionales <- function (){ 
  
  library(rio)

  hechos <- import("datos/profesionales.csv")

  colnames(hechos) <- c("sexo",
                        "categoria_profesional",
                        "confirmados_pdia",
                        "confirmados_pdia_14d",
                        "confirmados_pdia_7d",
                        "total_confirmados", 
                        "fallecidos",
                        "curados",
                        "fecha")  
  
  # hacer JOINs con dimensiones QUIEN-PROFESIONALES Y CUANDO
  library(dplyr)
  
  # dimensión cuándo
  cuando <- import("datos/dimension_cuando.csv")
  cuando <- cuando[(cuando$es_dia_habil == TRUE ),] # solo días hábiles
  hechos <- left_join(hechos, cuando, by='fecha') 
  hechos <- hechos[,-c(9,10,11,12,13,14,15)]

  # JOIN con dimension quién-profesional
  quien <- import("datos/dimension_quien_profesionales.csv")
  hechos <- left_join(hechos, quien)
  hechos <- hechos[,-c(1,2)]      
  
  # guardar en CSV la tabla de hechos
  write.table(hechos, "datos/hechos_profesionales.csv", row.names=FALSE, 
              quote = FALSE, col.names=TRUE, sep = ';')
} 




# ----------------- EJECUCIÓN DE LOS PROCEDIMIENTOS PARA ACTUALIZAR CSV --------

# Leer el camino a la carpeta de trabajo
args = commandArgs(trailingOnly=TRUE)

if (length(args)==0) {
  stop("Debes proporcionar el camino (absoluto) a la carpeta de trabajo 'TFG'.")
} else if (length(args)==1) {
  sprintf("El camino introducido es: %s", args[1])
  setwd(args[1])
} 


generar_hechos_dias_naturales()
generar_hechos_municipios()
generar_hechos_profesionales()
generar_hechos_residencias()
generar_hechos_residencias_edad_sexo()
generar_hechos_vacunas()

