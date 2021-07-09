# Alonso Bueno Herrero


################################################################################
#' Función para preparar y guardar los datos sobre residencias por edad y sexo
#' 
#' @param path es una cadena de caracteres (string) que indica el 
#' camino a la carpeta del proyecto (TFG).  
preparar_datos_residencias_edad_sexo <- function(path_fichero){ 

  library("rio")
  library('readr')
  library('tidyr') # for fill function 
  
  source("proyecto_tfg/utils.R")
  
  # esta_fecha <- "30/06/2021"
  # path_fichero <- "datos/30-06/residencias_edad.xls"

  # import excel
  excel <- import(path_fichero)
  
  # guardar los sexos en una lista auxiliar 
  sexos <- c(excel[8,3], excel[8,7]) 
  
  # eliminar filas vacías
  excel <- excel[-c(1,2,3,4,5,6,7,30,31),]
  excel <- excel[-c(1),]
  
  colnames(excel) <- list("V1","V2","V3","V4","V5","V6","V7", "V8", "V9", "V10")  # simplify columns name of dataset
  
  # rellenar la primera fila hacia abajo con el valor de la fila de referencia (celda múltiple) 
  excel <- excel %>% fill(V1, .direction = "down")
  
  # remove aggregation rows (data for Andalucia and Provincias)
  excel <- excel[!(excel$V2=="TOTAL"),]
  
  
  # ---------------------------------
  # Desdinamizar tabla
  
  # 1. unpivot (we generate and then fix 2 subdatasets for both type of "Sexo")
  
  new_col <- "Sexo" 
  
  # 1a. Hombres
  datos_hombres <- excel[,c(1,2,3,4,5,6)]
  var_hombres <- sexos[1]
  
  
  # creating and filling the (new) date column
  datos_hombres$V7 = var_hombres
  datos_hombres[1,7] <- new_col
  
  
  # 1b. second type of "sexo" -> "Mujeres"
  datos_mujeres <- excel[,c(1,2,7,8,9,10)]
  
  # renombrar columnas para manejarlas mejor 
  colnames <- list("V1","V2","V3","V4","V5","V6" )
  colnames(datos_mujeres) <- colnames
  
  var_mujeres <- sexos[2]
  datos_mujeres <- datos_mujeres[-c(1),] # we don't need the row -> (Confirmados PDIA, UCI, etc) because we already have it in the previous subdataset 
  
  datos_mujeres$V7 = var_mujeres
  
  # fusionar tablas 
  datos_globales <- rbind(datos_hombres, datos_mujeres)
  
  # cambiar NAs (nulos) por 0
  datos_globales[is.na(datos_globales)] <- 0
  
  # añadir columna con la fecha de hoy 
  fecha_hoy <- format(Sys.time(), "%d/%m/%Y")  # get current date 
  # fecha_hoy <- esta_fecha   # descomentar para pruebas
  
  datos_globales$V9 <- fecha_hoy 
  datos_globales[1,8] <- "Fecha" 
  
  # renombrar las columnas con los nombres originales
  colnames(datos_globales) <- datos_globales[1,]
  datos_globales <- datos_globales[-c(1),]
  
  # preparar dataset del día anterior para hacer la resta de casos/fallecidos/...
  dataset_ayer <- "datos/ayer/residencias_edad_sexo_ayer.csv" 
  
  if ( file.exists(dataset_ayer) ) { 
    
    ayer <- import(dataset_ayer)
    
    hoy <- datos_globales
    
    # Almacenar los datos de hoy (sin restarlos, claro) para poder restarlos a
    # los de el día siguiente. Se reescribe el fichero residencias_edad_sexo_ayer.csv
    write.table(hoy, dataset_ayer , row.names=FALSE, col.names=TRUE, sep = ';')
    
    # restar cada columna y modificarla en el dataset final del día de hoy 
    # columna 2 
    aux1 <- c(as.numeric(hoy[,3]))
    aux2 <- c(as.numeric(ayer[,3]))
    aux <- aux1-aux2
    hoy$`Confirmados PDIA` <- as.character(aux)
    
    # columna 5
    aux1 <- c(as.numeric(hoy[,4]))
    aux2 <- c(as.numeric(ayer[,4]))
    aux <- aux1-aux2   #
    hoy$`Total Confirmados` <- as.character(aux)
    
    # columna 6
    aux1 <- c(as.numeric(hoy[,5]))
    aux2 <- c(as.numeric(ayer[,5]))
    aux <- aux1-aux2   #
    hoy$Curados <- as.character(aux)
    
    # columna 7
    aux1 <- c(as.numeric(hoy[,6]))
    aux2 <- c(as.numeric(ayer[,6]))
    aux <- aux1-aux2   #
    hoy$Fallecidos <- as.character(aux)
    

    # añadir al dataset de residencias_edad_sexo.csv los datos de hoy
    residencias_edadsexo_csv <- "datos/residencias_edad_sexo.csv"
    
    if( file.exists(residencias_edadsexo_csv) ){ 
      print("El archivo de residencias_edad_sexo.csv ya tiene datos, vamos a solapar los de hoy. ")
      
      todo_residencias <- import(residencias_edadsexo_csv)
      todo_residencias <- rbind(todo_residencias, hoy)  #append data
      
      write.table(todo_residencias, residencias_edadsexo_csv, row.names=FALSE, col.names=TRUE, sep = ';')
      
    } else {
      print("No existía aún el fichero residencias_edad_sexo.csv. Los creamos hoy. ")
      write.table(hoy, residencias_edadsexo_csv, row.names=FALSE, col.names=TRUE, sep = ';')
    }
    
  
  } else{ 
    # esto solo ocurre una vez, con el primer día del que tengamos datos, y 
    # sirve para restarlo a los del día siguiente, que será el primer día válido
    
    # guardamos directamente nuestro dataset tal cual está
    print("El archivo residencias_edad_sexo_ayer.csv no existía aún. Se va a crear por primera vez con los datos de hoy.  ")
    write.table(datos_globales, dataset_ayer, row.names=FALSE, col.names=TRUE, sep = ';')
  }
  
  
} # fin de la función 




