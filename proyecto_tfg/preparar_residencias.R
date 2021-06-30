

################################################################################
#' Función para preparar y guardar los datos sobre residencias
#' 
#' @param path es una cadena de caracteres (string) que indica el 
#' camino a la carpeta del proyecto (TFG).
preparar_datos_residencias <- function(path_fichero){ 

  library('flattabler')
  library("rio") 
  library('readr')
  
  setwd("C:/Users/UX430U/Desktop/TFG")
  # path_fichero <- "datos/24-06/residencias.xls"
  # esta_fecha   <-       "24/06/2021"

  source("proyecto_tfg/utils.R")
  
  # import excel
  excel <- import(path_fichero)
  
  # borrar filas nulas (primeras y últimas)
  excel <- excel[-c(1,2,3,4,5,6,7,53,54),]
  type_residencias <- c(excel[1,2], excel[1,8])
  excel <- excel[-c(1),]
  
  # remove aggregation rows (data for Andalucia and Provincias)
  colnames(excel) <- list("V1","V2","V3","V4","V5","V6","V7", "V8", "V9", "V10", "V11", "V12", "V13")  # simplify columns name of dataset
  
  # eliminar filas con valores acumulados (de provincia y de la CA)
  excel <- excel[-2,]                       # eliminar fila de Andalucía 
  provincias <- obtener_provincias()  # necesario import de utils.R 
  residencias_today <- excel[! is.element(excel$V1, provincias), ]
  
  # ---------------------------------
  # Ahora podemos hacer "unpivot" sobre la tabla para borrar la cabecera con el 
  #  tipo de residencia. Pasos son: 
  #
  # 1. obtener primera columna
  column1 <- residencias_today[,1]
  
  # 2. unpivot (we generate and then fix 2 subdatasets for both type of "residencias")
  new_col <- "Vive en residencia"  # source: https://www.juntadeandalucia.es/institutodeestadisticaycartografia/badea/operaciones/consulta/anual/38378?CodOper=b3_2314&codConsulta=38378 (casos por edad y donde vive)
  
  # 2a. first type of "residencia" -> "Residencias de mayores"
  data_resid_mayores <- residencias_today[,c(1,2,3,4,5,6,7)]
  var_resid_mayores <- type_residencias[1]
  
  # Crear y solapar la nueva columna
  data_resid_mayores$V8 = var_resid_mayores
  data_resid_mayores[1,8] <- new_col
  
  
  # 2b. second type of "residencia" -> "Otro tipo de instituci?n"
  data_other_resid <- residencias_today[,c(1,8,9,10,11,12,13)]
  
  # rename R colnames (V1,V2,...) in order to homogenize the manipulation of both sub-datasets
  colnames <- list("V1","V2","V3","V4","V5","V6","V7")
  colnames(data_other_resid) <- colnames
  
  var_otra_resid <- type_residencias[2] 
  data_other_resid <- data_other_resid[-c(1),] # we don't need the row -> (Confirmados PDIA, UCI, etc) because we already have it in the previous subdataset 
  
  data_other_resid$V8 = var_otra_resid
    
  
  # append the subtables in a single one, then we need to add the today's date column 
  data_clear <- rbind(data_resid_mayores, data_other_resid)
  
  # replace null cells (NA) with a zero (0) -> NA <equiv> 0 for us in measures (UCI, Casos, etc)
  data_clear[is.na(data_clear)] <- 0
  
  # add date (Fecha) column and fill it
  date_today <- format(Sys.time(), "%d/%m/%Y")  # get current date 
  #date_today <- esta_fecha  # descomentar para pruebas 
  
  data_clear$V9 <- date_today 
  data_clear[1,9] <- "Fecha" 
  
  # change the column names to the names we need (Territorio, Fallecidos, etc)
  colnames(data_clear) <- data_clear[1,]
  data_clear <- data_clear[-c(1),]
  
  #View(data_clear)
  
  # preparar dataset del día anterior para hacer la resta de casos/fallecidos/...
  dataset_ayer <- "datos/ayer/residencias_ayer.csv" 
  
  if ( file.exists(dataset_ayer) ) {
    
    ayer <- import(dataset_ayer)
    
    # ahora 2 datasets: el de hoy y el de ayer, tenemos que restar dos a dos las
    # columnas del mismo nombre que contienen datos acumulados, que son:
    # columna 
    hoy <- data_clear

    # Almacenar los datos de hoy (sin restarlos, claro) para poder restarlos a
    # los de el día siguiente. Se reescribe el fichero residencias_ayer.csv
    write.table(hoy, dataset_ayer, row.names=FALSE, col.names=TRUE, sep = ';')
    
    
    # restar cada columna y modificarla en el dataset final del día de hoy 
    # columna 2 
    aux1 <- c(as.numeric(hoy[,2]))
    aux2 <- c(as.numeric(ayer[,2]))
    aux <- aux1-aux2
    hoy$`Confirmados PDIA` <- as.character(aux)
    
    # columna 5
    aux1 <- c(as.numeric(hoy[,5]))
    aux2 <- c(as.numeric(ayer[,5]))
    aux <- aux1-aux2   #
    hoy$`Total Confirmados` <- as.character(aux)
    
    # columna 6
    aux1 <- c(as.numeric(hoy[,6]))
    aux2 <- c(as.numeric(ayer[,6]))
    aux <- aux1-aux2   #
    hoy$Curados <- as.character(aux)
    
    # columna 7
    aux1 <- c(as.numeric(hoy[,7]))
    aux2 <- c(as.numeric(ayer[,7]))
    aux <- aux1-aux2   #
    hoy$Fallecidos <- as.character(aux)
    
    # View(hoy)
    
    # añadir al dataset de residencias.csv los datos de hoy
    residencias_csv <- "datos/residencias.csv"
    
    if( file.exists(residencias_csv) ){ 
      print("El archivo de residencias.csv ya tiene datos, vamos a solapar los de hoy. ")
      
      todo_residencias <- import(residencias_csv)
      todo_residencias <- rbind(todo_residencias, hoy)  #append data
      
      write.table(todo_residencias, residencias_csv, row.names=FALSE, col.names=TRUE, sep = ';')
      
    } else {
      print("No existía aún el fichero residencias.csv. Lo creamos hoy. ")
      write.table(hoy, residencias_csv, row.names=FALSE, col.names=TRUE, sep = ';')
    }
    
    
  } else{ 
    # esto solo ocurre una vez, con el primer día del que tengamos datos, y 
    # sirve para restarlo a los del día siguiente, que será el primer día válido
    
    print("El archivo residencias_ayer.csv no existía aún. Se va a crear por primera vez con los datos de hoy.  ")
    write.table(data_clear, dataset_ayer, row.names=FALSE, col.names=TRUE, sep = ';')
  }
  
}

#setwd("C:\\Users\\UX430U\\Desktop\\TFG")
#preparar_datos_residencias("datos/13-05/residencias.xls")
