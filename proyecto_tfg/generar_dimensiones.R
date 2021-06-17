
# Script para generar las dimensiones aplicando los casos de diseño 
# especificados en la documentación del proyecto.

#setwd("C:\\Users\\UX430U\\Desktop\\TFG")


################################################################################
#' Función para generar la dimensión Cuándo a partir de las fechas:
#' - días naturales de los datos (1 dataset)
#' - días en que se han notificado datos (el resto de datasets)
#' @param path_proyecto Es el camino al proyecto (carpeta que contiena a su vez
#' al diretorio 'datos/', donde a su vez encontraremos todos los datasets 
#' que necesitamos). 
generar_dimension_cuando <- function(path_proyecto){
  
  library('rio')
  library('readr')
  
  # setwd(path_proyecto)
  # setwd("C:\\Users\\UX430U\\Desktop\\TFG")
  
  cuando <- import("datos/datos_dias_naturales.csv")
  
  cuando <- cuando[!(cuando$Territorio == "Andalucía"),]
  
  
  #install.packages("lubridate")
  library(lubridate)

  # añadir las columnas de mes, año y semana del año (niveles de la dimensión)
  cuando$Mes <- month(dmy(cuando$`Fecha diagnóstico`) )
  cuando$Año <- year(dmy(cuando$`Fecha diagnóstico`))
  cuando$Semana <- week(dmy(cuando$`Fecha diagnóstico`))
  
  # eliminar las columnas que no son de la dimensión 
  cuando <- cuando[, c("Fecha diagnóstico", "Mes", "Año", "Semana")]
  
  # borrar duplicados
  cuando <- cuando[!duplicated(cuando),]
  colnames(cuando) <- c("Fecha", "Mes", "Año", "Semana")

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
  cuando2 <- import("datos/municipios.csv")
  
  # añadir las columnas de mes, año y semana del año (niveles de la dimensión)
  cuando2$Mes <- month(dmy(cuando2$`Fecha`) )
  cuando2$Año <- year(dmy(cuando2$`Fecha`))
  cuando2$Semana <- week(dmy(cuando2$`Fecha`))
  
  
  # seleccionar solo atributos de tiempo
  cuando2 <- cuando2[,c("Fecha", "Mes", "Año", "Semana")]
  
  # eliminar duplicados 
  cuando2 <- cuando2[!duplicated(cuando2),]
  
  # añadir columna con el nombre del mes
  cuando2$`Nombre del Mes` <- ""
  
  for(i in 1:nrow(cuando2)){
    if(cuando2[i,"Mes"] == 1){
      cuando2[i,"Nombre del Mes"] <- "Enero"
    } else if(cuando2[i,"Mes"] == 2){
      cuando2[i,"Nombre del Mes"] <- "Febrero"
    } else if(cuando2[i,"Mes"] == 3){
      cuando2[i,"Nombre del Mes"] <- "Marzo"
    } else if(cuando2[i,"Mes"] == 4){
      cuando2[i,"Nombre del Mes"] <- "Abril"
    } else if(cuando2[i,"Mes"] == 5){
      cuando2[i,"Nombre del Mes"] <- "Mayo"
    } else if(cuando2[i,"Mes"] == 6){
      cuando2[i,"Nombre del Mes"] <- "Junio"
    } else if(cuando2[i,"Mes"] == 7){
      cuando2[i,"Nombre del Mes"] <- "Julio"
    } else if(cuando2[i,"Mes"] == 8){
      cuando2[i,"Nombre del Mes"] <- "Agosto"
    } else if(cuando2[i,"Mes"] == 9){
      cuando2[i,"Nombre del Mes"] <- "Septiembre"
    } else if(cuando2[i,"Mes"] == 10){
      cuando2[i,"Nombre del Mes"] <- "Octubre"
    } else if(cuando2[i,"Mes"] == 11){
      cuando2[i,"Nombre del Mes"] <- "Noviembre"
    } else if(cuando2[i,"Mes"] == 12){
      cuando2[i,"Nombre del Mes"] <- "Diciembre"
    } 
  }
  
  # aplicación del caso de diseño: dimensión con varios roles 
  # vamos a añadir el atributo "Día natural" para indicar que las fechas del
  # dataset actual son por días naturales (L a D), frente a los que añadiremos
  # después, que serán por fecha de notificación (días hábiles -> L a V).
  cuando2$`Tipo de Fecha` <- "Día hábil"
  
  # paso final: solapar los dos dataset (mismas columnas en número y nombre=>OK)
  cuando_global <- rbind(cuando, cuando2)

  # añadir columna de los periodos covid (datos IECA)
  #info_periodos <- import("datos/periodos_metadatos.xlsx")
  #View(info_periodos)
  # for(i in 1:nrow(cuando_global)){
  #   if(cuando_global[i,"Fecha"] == 1){
  #     cuando_global[i,"periodo_covid"] <- "Enero"
  #   } else if (cuando_global[i,"Fecha"] == )
  # }
    
  # volvemos a eliminar duplicados para asegurarnos
  cuando_global <- cuando_global[!duplicated(cuando_global),]

  colnames(cuando_global) <- c("fecha", 
                               "mes", 
                               "anio", 
                               "semana", 
                               "nombre_mes", 
                               "tipo_fecha" )
  
  # añadir llave generada
  cuando_global$cod_cuando <- 1:nrow(cuando_global)
  
  # cuando_global$fecha <- as.Date(cuando_global$fecha, format = "%d/%m/%y")
  
  # almacenar dimensión 
  write.table(cuando_global, "datos/dimension_cuando2.csv", row.names=FALSE, 
              col.names=TRUE, sep = ';', quote = FALSE)
}

# generar_dimension_cuando("C:/Users/UX430U/Desktop/TFG/")


################################################################################
#' GENERAR DIMENSION QUIEN
#' @param path_proyecto es el camino a la carpeta principal de trabajo (TFG),
#' que tendrá el mismo orden de ficheros y directorios actual para que funcionen
#' las funciones implementadas tal cual están.

generar_dimension_quien <- function(path_proyecto){
  
  library("rio")
  
  # setwd(path_proyecto)
  
  dimension <- import("datos/residencias_edad_sexo.csv")
  
  dimension <- dimension[,c("Vive en residencia", "Edad", "Sexo")]

  # eliminar duplicados
  dimension <- dimension[!duplicated(dimension),]
  
  colnames(dimension) <- c("tipo_residencia", "rango_edad", "sexo")
  
  # añadir llave generada
  dimension$cod_quien <- 1:nrow(dimension)
  
  # guardar dimension
  write.table(dimension, "datos/dimension_quien.csv", row.names=FALSE, 
              quote = FALSE , col.names=TRUE, sep = ';')
}



################################################################################
generar_dimension_donde_provincia <- function(path_proyecto){
  
  library(rio)
  
  # setwd(path_proyecto)
  
  provincias <- import("datos/datos_dias_naturales.csv")
  provincias$cod_donde_provincia <- " "
  
  provincias <- provincias[!(provincias$Territorio == "Andalucía"),]
  
  provincias <- provincias[,c("Territorio","cod_donde_provincia")]
  provincias <- provincias[!duplicated(provincias),]
  
  # añadir la llave generada
  provincias$cod_donde_provincia <- 1:nrow(provincias)
  
  # cambiar nombre de la primera columna al de "nombre_provincia", más específico
  # que "Territorio"
  colnames(provincias)[1] <- "nombre_provincia"
  
  # enriquecimiento de la dimensión: usando directamente cod_provincias.csv
  # añadimos el código de cada provincia correspondiente
  codprov <- import("datos/cod_provincias.csv")
  
  library(dplyr)
  
  # añadir las claves de la provincia del INE
  provincias <- left_join(provincias, codprov, by="nombre_provincia")
  
  # almacenar la tabla de la dimensión
  write.table(provincias, "datos/dimension_donde_provincia.csv", 
              quote = FALSE , row.names=FALSE, col.names=TRUE, sep = ';')
}

################################################################################
#' FUNCION PARA GENERAR LA TABLA DE LA DIMENSION DONDE-DS 
#' 
#' @param path_proyecto Es el path a la carpeta principal del proyecto.
generar_dimension_donde_distrito_sanitario <- function(path_proyecto){
  
  library(rio)

  #path_proyecto <- "C:/Users/UX430U/Desktop/TFG"  # descomentar para pruebas 
  # setwd(path_proyecto)
  
  ds <- import("datos/residencias.csv")  

  # View(excel)
  ds$cod_donde_ds <- ""
  ds <- ds[,c("Territorio", "cod_donde_ds")]  
  ds <- ds[!duplicated(ds),]
  colnames(ds)[1] <- "distrito_sanitario"
  ds$cod_donde_ds <- 1:nrow(ds)
  
  # añadimos la provincia asociada a cada DS
  # la información se extrae de la web del IECA, de los propios ficheros de datos
  
  ds$nombre_provincia <- ""
  # provincia de Almería
  for(i in 1:3){
    ds[i, "nombre_provincia"] <- "Almería"
  }

  # cádiz 
  for(i in 4:8){
    ds[i, "nombre_provincia"] <- "Cádiz"
  }
    
  # córdoba
  for(i in 9:12){
    ds[i, "nombre_provincia"] <- "Córdoba"
  }
  
  # granada
  for(i in 13:16){
    ds[i, "nombre_provincia"] <- "Granada"
  }

  # huelva
  for(i in 17:19){
    ds[i, "nombre_provincia"] <- "Huelva"
  }

  # jaén
  for(i in 20:23){
    ds[i, "nombre_provincia"] <- "Jaén"
  }
  
  # málaga
  for(i in 24:29){
    ds[i, "nombre_provincia"] <- "Málaga"
  }
  
  # sevilla
  for(i in 30:34){
    ds[i, "nombre_provincia"] <- "Sevilla"
  }

  codprovincias <- import("datos/cod_provincias.csv")  

  library(dplyr)
  
  ds <- left_join(ds, codprov, by="nombre_provincia")
  
  # almacenar la tabla de la dimensión
  write.table(ds, "datos/dimension_donde_ds.csv", row.names=FALSE, 
              quote = FALSE , col.names=TRUE, sep = ';')
  
}


generar_dimension_donde_municipio (path_proyecto = getwd()){
  
  library(rio)
  library(dplyr)
  library(tidyr)
  source("proyecto_tfg/utils.R")

  # setwd(path_proyecto)
  
  dimension <- import("datos/municipios.csv")
  
  dimension$cod_donde_municipio = ""
  dimension <- dimension[,c("Lugar de residencia", 
                            "Población" ,  
                            "cod_donde_municipio")]  
  colnames(dimension) <- c("nombre_municipio", 
                           "poblacion_ine", 
                           "cod_donde_municipio" )
  
  dimension <- dimension[!duplicated(dimension),]
  
  
  # enriquecer la dimension con el código de municipio del INE
  info_ine <- import("datos/20codmun.xlsx")
  colnames(info_ine) <- info_ine[1,]
  info_ine <- info_ine[-1,]    
  info_ine <- info_ine[ (info_ine$CODAUTO == '01') ,]
  info_ine <- info_ine[,c("CMUN", "NOMBRE")]
  colnames(info_ine) <- c("codigo_municipio_ine", "nombre_municipio")  

  # cambiar la forma de escribir los determinantes definidos al final del 
  #  nombre del pueblo. Ejemplo: Luisiana, La --> Luisiana (La) que es como 
  #  está en los datos del IECA 
  info_ine$nombre_municipio <- gsub(", El", " (El)", info_ine$nombre_municipio)
  info_ine$nombre_municipio <- gsub(", Las", " (Las)", info_ine$nombre_municipio)
  info_ine$nombre_municipio <- gsub(", La", " (La)", info_ine$nombre_municipio)
  info_ine$nombre_municipio <- gsub(", Los", " (Los)", info_ine$nombre_municipio)
  
  # añadir en info_ine el prefijo " (capital)" a los nombres de las 8 capitales
  #  de provincia 
  for(i in 1:nrow(info_ine)){
    if (info_ine[i,"nombre_municipio" ] == "Almería" ||
        info_ine[i,"nombre_municipio" ] == "Cádiz"   ||
        info_ine[i,"nombre_municipio" ] == "Córdoba" ||
        info_ine[i,"nombre_municipio" ] == "Granada" ||
        info_ine[i,"nombre_municipio" ] == "Huelva"  ||
        info_ine[i,"nombre_municipio" ] == "Jaén"    ||
        info_ine[i,"nombre_municipio" ] == "Málaga"  ||
        info_ine[i,"nombre_municipio" ] == "Sevilla"  ){
      
      capital <- concatenar_strings(c(info_ine[i,"nombre_municipio"], " (capital)"))
      info_ine[i,"nombre_municipio"] <- capital
    }
  }
  
  # añadir codigos de municipios a la dimension con un LEFT-JOIN
  dimension <- left_join(dimension, info_ine, by = "nombre_municipio")
  dimension$cod_donde_municipio <- 1:nrow(dimension)
  
  
  # info sobre distrito sanitario y provincia de cada municipio
  info_dsprov <- import("datos/distritos_sanitarios.xlsx")
  colnames(info_dsprov) <- c("nombre_municipio", 
                             "distrito_sanitario", 
                             "nombre_provincia")
  
  # añadir info extra: DS, Provincia, y cod INE de la provincia 
  dimension <- left_join(dimension, info_dsprov, by="nombre_municipio")  
  dimension <- left_join(dimension, codprov, by="nombre_provincia")
  
  # arreglar filas de municipios sin especificar
  dimension <- dimension %>% fill(nombre_provincia, .direction = "down")
  dimension <- dimension %>% fill(cod_provincia, .direction = "down")
  dimension[ (is.na(dimension$distrito_sanitario)), "distrito_sanitario"] <- "DS no especificado"
  dimension[ (is.na(dimension$codigo_municipio_ine)),"codigo_municipio_ine"] <- 0
    
  
  # salvar dimensión en su fichero
  write.table(dimension, "datos/dimension_donde_municipio.csv", 
              quote = FALSE , row.names=FALSE, col.names=TRUE, sep = ';')
  
}


################################################################################
#' GENERAR DIMENSION QUIEN PARA EL FICHERO DE VACUNAS
#' 
#' Nota: Ya teníamos una dimensión quién, con la edad y el sexo, pero solo
#' para las residencias. Aquí tendremos solo un nivel: el de la edad, y con
#' rangos de edad diferentes a aquellos, por lo que no podemos usar aquella 
#' dimensión y por eso creamos esta. 
#' @param path_proyecto Es el path a la carpeta principal del proyecto
generar_dimension_quien_vacunas <- function(path_proyecto = getwd()){
  
  library(rio)

  dimension <- import("datos/vacunas.csv")

  dimension$cod_quien_vacunas <- ""

  dimension <- dimension[,c("Edad", "cod_quien_vacunas")]
  colnames(dimension) <- c("rango_edad", "cod_quien_vacunas")

  dimension <- dimension[!duplicated(dimension),]

  dimension$cod_quien_vacunas <- 1:nrow(dimension)          

  # añadimos el nivel "mayores de edad"
  dimension$adultos <- TRUE
  for(i in 1:nrow(dimension)){
    if( grepl( "^Menores de 16", dimension[i,"rango_edad"]) | 
        grepl( "^De 16 a 17", dimension[i,"rango_edad"] ) ){
      dimension[i,"adultos"] <- FALSE
    }
  }
  
  # almacenar la dimension 
  write.table(dimension, "datos/dimension_quien_vacunas.csv", row.names=FALSE, 
              quote = FALSE , col.names=TRUE, sep = ';')
}


generar_dimension_residencia(path_proyecto = getwd()){
  
  library(rio)

  dimension <- import("datos/residencias.csv")
  dimension$cod_residencia = ""
  dimension <- dimension[,c("cod_residencia", "Vive en residencia")]

  colnames(dimension)[2] <- "tipo_residencia"

  dimension <- dimension[!duplicated(dimension),]

  dimension$cod_residencia <- 1:nrow(dimension)

  write.table(dimension, "datos/dimension_residencia.csv", row.names=FALSE, 
              quote = FALSE , col.names=TRUE, sep = ';')
  
}



generar_dimension_quien_profesionales <- function(path_proyecto = getwd()){
  
  library(rio)
  #setwd(path_proyecto)
  
  # importar la tabla con los datos de profesionales
  dimension <- import("datos/profesionales.csv")

  # quedarnos con las columnas de la dimensión: rango edad y tipo de sanitario
  dimension <- dimension[,c(1,2)]
  
  # cambiar los nombrse de las cols
  colnames(dimension) <- c("sexo", "categoria_profesional")
  
  # eliminar filas repetidas (recordemos que estamos en una dimensión)
  dimension <- dimension[!duplicated(dimension),]
  
  # añadir la llave generada
  dimension$cod_quien_profesionales <- 1:nrow(dimension)
  
  # almacenar dimensión en formato CSV 
  write.table(dimension, "datos/dimension_quien_profesionales.csv", row.names=FALSE, 
              quote = FALSE , col.names=TRUE, sep = ';')
  
}