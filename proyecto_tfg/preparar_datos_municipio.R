# ALONSO BUENO HERRERO


####### FUNCIONES PARA TRABAJAR LOS DATOS DE CADA PROVINCIA


################################################################################
#' PREPARAR DATASET DE CADA PROVINCIA PARA PODER FUSIONARLA AL DATASET GLOBAL
#' 
#' @param data_xls es una cadena de caracteres (character) con la ruta al 
#' archivo Excel asociado a los datos de la provincia.
#' @param provincia es un identificador de cada provincia. Toma como valores 
#' las dos primeras letras, en MAYÚSCULA, del nombre de cada provincia. 
#' 
preparar_provincia <- function(data_xls, provincia){
  codprov <- c('AL', 'CA','CO', 'GR',  'HU', 'JA', 'MA', 'SE')
  
  fecha = format(Sys.time(), "%d/%m/%Y")  # get current date 

  # import xls as dataframe in order to transform the data
  datos <- import(data_xls)
  
  # eliminar filas innecesarias y filas asociadas a los distritos sanitarios
  #  (datos agrupados) 
  if (provincia == codprov[1]){  # almería
    datos <- datos[-c(1,2,3,4,5,8,56,98),]  
  }   else if(provincia == codprov[2]){  # cadiz
    datos <- datos[-c(1,2,3,4,5,59,60,61,62,8,12,18,31,38),] 
  } else if(provincia == codprov[3]){   # cordoba
    datos <- datos[-c(1,2,3,4,5,91,92,8,10,40,64),] 
  }  else if(provincia == codprov[4]){  # granada
    datos <- datos[-c(1,2,3,4,5,187,188,189,190,8,52,99,104),] 
  }  else if(provincia == codprov[5]){  # huelva
    datos <- datos[-c(1,2,3,4,5,86,87,8,45,65),] 
  }  else if(provincia == codprov[6]){  # jaén
    datos <- datos[-c(1,2,3,4,5,110,111,112,113,8,21,45,71),] 
  }  else if(provincia == codprov[7]){  # málaga
    datos <- datos[-c(1,2,3,4,5,118,119,120,121, 8,37,49,69,76,102),] 
  }  else if(provincia == codprov[8]){  # sevilla 
    datos <- datos[-c(1,2,3,4,5,125,126,8,43,45,70,107),] 
  }
  
  # renombrar columnas con el nombre correcto, de la primera fila actual del 
  #  dataset 
  colnames(datos) <- datos[c(1),]
  
  # borra cabecera duplicada y fila con datos agrupados de la provincia
  datos <- datos[-c(1,2),]
  
  # add date column in order to identify the date asssociated to the data
  datos$Fecha = fecha
  return(datos)
}



# ------------------------------------------------------------------------------
# Tareas a realizar:
#   
# a. Get the dataset of the day.
# b. Extract null rows and add "Fecha" (date) column with today's date 
# c. Append the data to the general dataset of the "provincia" (csv)
# 
# ------------------------------------------------------------------------------



preparar_datos_municipio <- function(path_dir_hoy = ""){ # recibiría 'datos/25-02', por ejemplo 
  
  library("rio")
  library('readr')
  library(stringr)
  
  source("proyecto_tfg/utils.R")
  
  path_dir_hoy <- "datos/25-02"
  
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
  
  
  almeria <- preparar_provincia(path_provincias[1], 'AL')
  cadiz   <- preparar_provincia(path_provincias[2], 'CA')
  cordoba <- preparar_provincia(path_provincias[3], 'CO')
  granada <- preparar_provincia(path_provincias[4], 'GR')
  huelva  <- preparar_provincia(path_provincias[5], 'HU')
  jaen    <- preparar_provincia(path_provincias[6], 'JA')
  malaga  <- preparar_provincia(path_provincias[7], 'MA')
  sevilla <- preparar_provincia(path_provincias[8], 'SE')
  
  
  datos_globales <- rbind(almeria, cadiz, cordoba, granada, huelva, jaen, malaga, sevilla)
  
  # comprobar que solo quedan los municipios en el dataset, 
  #  más 8 registros tipo "Municipio de la provincia X sin especificar", propio 
  #  de los datos originales, y que no podemos eliminar. 
  if( nrow(datos_globales) != (785+8) ){
    stop("ERROR: Es posible que no se hayan extraído los municipios correctamente.")
    
  }
  
  
  # reemplazar NA (nulos) por el 0 
  datos_globales[is.na(datos_globales)] <- 0
  #datos_globales$`Tasa PDIA 14 días` <- gsub('-', '0', datos_globales$`Tasa PDIA 14 días`)
  
  
  # preparar dataset del día anterior para hacer la resta de casos/fallecidos/...
  #setwd("C:\\Users\\UX430U\\Desktop\\TFG\\datos\\ayer")
  dataset_ayer <- "datos/ayer/municipios_ayer.csv" 
  
  if ( file.exists(dataset_ayer) ) {
    
    ayer <- import(dataset_ayer)
    
    # ahora 2 datasets: el de hoy y el de ayer, tenemos que restar dos a dos las
    # columnas del mismo nombre que contienen datos acumulados, que son:
    # 3,7,8,9
    
    hoy <- datos_globales  # "renombrar" tabla de datos de hoy
    
    # Almacenar los datos de hoy (sin restarlos, claro) para poder restarlos a
    # los de el día siguiente. Se reescribe el fichero municipios_ayer.csv
    write.table(hoy, dataset_ayer , row.names=FALSE, col.names=TRUE, sep = ';')
    
  
    # restar cada columna y modificarla en el dataset final del día de hoy 
    # columna 3: Confirmados PDIA 
    aux1 <- c(as.numeric( hoy[,3]))
    aux2 <- c(as.numeric(ayer[,3]))
    aux <- aux1-aux2
    hoy$`Confirmados PDIA` <- as.character(aux)
    
    # columna 7: Total Confirmados
    aux1 <- c(as.numeric( hoy[,7]))
    aux2 <- c(as.numeric(ayer[,7]))
    aux <- aux1-aux2   #
    hoy$`Total Confirmados` <- as.character(aux)
    
    # columna 8: Curados
    aux1 <- c(as.numeric( hoy[,8]))
    aux2 <- c(as.numeric(ayer[,8]))
    aux <- aux1-aux2   #
    hoy$Curados <- as.character(aux)
    
    # columna 9: Fallecidos 
    aux1 <- c(as.numeric( hoy[,9]))
    aux2 <- c(as.numeric(ayer[,9]))
    aux <- aux1-aux2   #
    hoy$Fallecidos <- as.character(aux)
    
    #View(hoy)
    
    # añadir al dataset de municipios los datos de hoy
    #setwd("C:/Users/UX430U/Desktop/TFG/datos/ayer")
    
    dataset_historico <- "datos/municipios.csv" # datos preparados de cada día notificado
    
    # sustituir los puntos por comas para trabajar con ellos en las herramientas
    # de Microsoft (SSMS/SSAS/Power BI/...)
    hoy$`Tasa PDIA 14 días` <- str_replace(hoy$`Tasa PDIA 14 días`, '\\.',',')
    
    
    if( file.exists(dataset_historico) ){ 
      print("El archivo de municipios.csv ya tiene datos, vamos a solapar los de hoy. ")
      
      todo_municipios <- import(dataset_historico)
      todo_municipios <- rbind(todo_municipios, hoy)  #append data
      
      
      write.table(todo_municipios, dataset_historico, row.names=FALSE, col.names=TRUE, sep = ';')
      
    } else {
      print("El fichero municipios.csv no existía. Lo creamos hoy. ")
      write.table(hoy, dataset_historico, row.names=FALSE, col.names=TRUE, sep = ';')
    }
    
    
  } else{ 
    # esto solo ocurre una vez, con el primer día del que tengamos datos, y 
    # sirve para restarlo a los del día siguiente, que será el primer día válido
    
    # guardamos directamente nuestro dataset tal cual está
    print("El archivo municipios_ayer.csv no existía aún. ")
    write.table(excel, dataset_ayer, row.names=FALSE, col.names=TRUE, sep = ';')
  }
  
}

