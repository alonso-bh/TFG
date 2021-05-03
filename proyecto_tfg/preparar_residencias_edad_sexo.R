

preparar_datos_residencias_edad_sexo <- function(path_dir_hoy = getwd()){ 

  library('flattabler')
  library("rio")
  library('readr')
  library('tidyr') # for fill function 
  
  setwd(path_dir_hoy)
  #-setwd("C:\\Users\\UX430U\\Desktop\\TFG\\datos\\27-04")
  #-esta_fecha <- "27/04/2021"
  
  # import excel
  excel <- import("residencias_edad.xls")
  #View(excel)
  
  sexos <- c(excel[8,3], excel[8,7]) 
  
  # remove non-valid rows, but before I save the first row with the type of "residencias" used
  excel <- excel[-c(1,2,3,4,5,6,7,30,31),]
  excel <- excel[-c(1),]
  
  colnames(excel) <- list("V1","V2","V3","V4","V5","V6","V7", "V8", "V9", "V10")  # simplify columns name of dataset
  
  # rellenar la primera fila hacia abajo con el valor de la fila de referencia (celda múltiple) 
  excel <- excel %>% fill(V1, .direction = "down")
  
  # remove aggregation rows (data for Andalucia and Provincias)
  excel <- excel[!(excel$V2=="TOTAL"),]
  
  
  # ---------------------------------
  # Now we can manually  "unpivot" the table in orden to delete the row of the type of "Sex" (H/M)
  # 1. get the first column
  column1 <- excel[,1]
  column2 <- excel[,2]
  
  # 2. unpivot (we generate and then fix 2 subdatasets for both type of "Sexo")
  new_col <- "Sexo" 
  
  # 2a. first type of "Sexo" -> "Hombres"
  data_hombres <- excel[,c(1,2,3,4,5,6)]
  var_hombres <- sexos[1]
  
  
  # creating and filling the (new) date column
  data_hombres$V7 = var_hombres
  data_hombres[1,7] <- new_col
  
  
  # 2b. second type of "sexo" -> "Mujeres"
  data_mujeres <- excel[,c(1,2,7,8,9,10)]
  
  # rename R colnames (V1,V2,...) in order to homogenize the manipulation of both sub-datasets
  colnames <- list("V1","V2","V3","V4","V5","V6" )
  colnames(data_mujeres) <- colnames
  
  var_mujeres <- sexos[2]
  data_mujeres <- data_mujeres[-c(1),] # we don't need the row -> (Confirmados PDIA, UCI, etc) because we already have it in the previous subdataset 
  
  data_mujeres$V7 = var_mujeres
  
  
  # append the subtables in a single one, then we need to add the today's date column 
  data_clear <- rbind(data_hombres, data_mujeres)
  
  # replace null cells (NA) with a zero (0) -> NA <equiv> 0 for us in measures (UCI, Casos, etc)
  data_clear[is.na(data_clear)] <- 0
  
  # añadir columna con la fecha de hoy 
  date_today <- format(Sys.time(), "%d/%m/%Y")  # get current date 

  data_clear$V9 <- date_today 
  data_clear[1,8] <- "Fecha" 
  
  # renombrar las columnas con los nombres originales (los que necesitamos): 
  #  (Territorio, Fallecidos, etc)
  colnames(data_clear) <- data_clear[1,]
  data_clear <- data_clear[-c(1),]
  
  View(data_clear)
  
  # preparar dataset del día anterior para hacer la resta de casos/fallecidos/...
  setwd("C:\\Users\\UX430U\\Desktop\\TFG\\datos\\ayer")
  dataset_ayer <- "residencias_edad_sexo_ayer.csv" 
  
  if ( file.exists(dataset_ayer) ) {
    
    ayer <- import(dataset_ayer)
    
    # ahora 2 datasets: el de hoy y el de ayer, tenemos que restar dos a dos las
    # columnas del mismo nombre que contienen datos acumulados, que son:
    # columna 
    hoy <- data_clear
    
    # Almacenar los datos de hoy (sin restarlos, claro) para poder restarlos a
    # los de el día siguiente. Se reescribe el fichero residencias_edad_sexo_ayer.csv
    write.table(hoy, "residencias_edad_sexo_ayer.csv", row.names=FALSE, col.names=TRUE, sep = ';')
    
    
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
    
    View(hoy)
    
    # añadir al dataset de residencias_edad_sexo.csv los datos de hoy
    setwd("C:\\Users\\UX430U\\Desktop\\TFG\\datos")
    
    if( file.exists("residencias_edad_sexo.csv") ){ 
      print("El archivo de residencias_edad_sexo.csv ya tiene datos, vamos a solapar los de hoy. ")
      
      todo_residencias <- import("residencias_edad_sexo.csv")
      todo_residencias <- rbind(todo_residencias, hoy)  #append data
      
      write.table(todo_residencias, "residencias_edad_sexo.csv", row.names=FALSE, col.names=TRUE, sep = ';')
      
    } else {
      print("No existía aún el fichero residencias_edad_sexo.csv. Los creamos hoy. ")
      write.table(hoy, "residencias_edad_sexo.csv", row.names=FALSE, col.names=TRUE, sep = ';')
    }
    
  
  } else{ 
    # esto solo ocurre una vez, con el primer día del que tengamos datos, y 
    # sirve para restarlo a los del día siguiente, que será el primer día válido
    
    # guardamos directamente nuestro dataset tal cual está
    print("El archivo residencias_edad_sexo_ayer.csv no existía aún. Se va a crear por primera vez con los datos de hoy.  ")
    write.table(data_clear, dataset_ayer, row.names=FALSE, col.names=TRUE, sep = ';')
  }

  
} # fin de la función 


