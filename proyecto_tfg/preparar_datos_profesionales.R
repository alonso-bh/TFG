# código para procesar (ETL) el fichero con el informe diario sobre la situación
#  de profesionales de riesgo


#' FUNCIÓN PARA PROCESAR  Y PREPARAR DATOS DE PROFESIONALES. 
#' 
#' Para que funcione correctamente es imprescindible pasar como parámetro a la 
#' función el camino hacia el directorio con el fichero con los datos. 
#' 
#' @param Camino hacia el directorio donde están los datos de hoy sobre 
#' profesionales, previamente descargados de la web de la Junta 
#' 
preparar_datos_profesionales <- function(path_fichero, version=2){
  
  library('readr')
  library("rio")
  library("tidyr") 

  # esta_fecha   <-       "05/07/2021"
  # path_fichero <- "datos/05-07/profesionales.xls"

  # import excel
  excel <- import(path_fichero)
  
  # quitar primeras y últimas filas "sucias" (sin datos)
  if (version == 1){
    excel <- excel[-c(1,2,3,4,5,6,38,39),]
  } else if (version == 2){
    excel <- excel[-c(1,2,3,4,5,6,35,36),]
  }
  
  # asignar los nombres correctos a las columnas
  colnames <- excel[1,]
  colnames(excel) <- colnames
  excel <- excel[-c(1),]
  
  # rellenar primera columna con los datos que faltan hacia abajo (Sexo)
  excel <- excel %>% fill(Sexo, .direction = "down")
  
  # quitar filas con datos agregados (aquellas filas con Sexo='Ambos sexos') ya
  #  que eso se mostrará al hacer "Roll-up"
  excel <- excel[!(excel$Sexo == "Ambos sexos"),]
  
  # añadir columna con la fecha de hoy (notificación)
  date_today <- format(Sys.Date(), "%d/%m/%Y")  # get current date 
  # date_today <- esta_fecha # descomentar para pruebas 
  
  excel$Fecha <- date_today
  
  # preparar dataset del día anterior para hacer la resta de casos/fallecidos/...
  dataset_ayer <- "datos/ayer/profesionales_ayer.csv" 
  
  if ( file.exists(dataset_ayer) ) {
    
    ayer <- import(dataset_ayer)
    
    if(nrow(ayer) == 20 & version == 2){
      # esto solo ocurre un día, pero hay que comprobarlo y adaptar el fichero
      # para que la resta de datos con los de hoy se haga correctamente
      ayer <- ayer[-c(6,16),]
    }
    
    # ahora 2 datasets: el de hoy y el de ayer, tenemos que restar dos a dos las
    # columnas del mismo nombre que contienen datos acumulados, que son:
    # columna 3. Confirmados PDIA
    # columna 6. Total Confirmados
    # columna 7. Curados
    # columna 8. Fallecidos 
    hoy <- excel
    
    # Almacenar los datos de hoy (sin restarlos, claro) para poder restarlos a
    # los de el día siguiente. Se reescribe el fichero profesionales_ayer.csv
    write.table(hoy, dataset_ayer, row.names=FALSE, col.names=TRUE, sep = ';')
    
    
    # restar cada columna y modificarla en el dataset final del día de hoy 
    # 1. 
    aux1 <- c(as.numeric(hoy[,3]))
    aux2 <- c(as.numeric(ayer[,3]))
    aux <- aux1-aux2
    hoy$`Confirmados PDIA` <- as.character(aux)
    
    # 2.
    aux1 <- c(as.numeric(hoy[,6]))
    aux2 <- c(as.numeric(ayer[,6]))
    aux <- aux1-aux2   #
    hoy$`Total Confirmados` <- as.character(aux)
    
    # 2.
    aux1 <- c(as.numeric(hoy[,7]))
    aux2 <- c(as.numeric(ayer[,7]))
    aux <- aux1-aux2   #
    hoy$Fallecidos <- as.character(aux)
    
    # 2.
    aux1 <- c(as.numeric(hoy[,8]))
    aux2 <- c(as.numeric(ayer[,8]))
    aux <- aux1-aux2   #
    hoy$Curados <- as.character(aux)
    
    View(hoy)
    
    # añadir al dataset de profesionales los datos de hoy
    profesionales_csv <- "datos/profesionales.csv"

    if( file.exists(profesionales_csv) ){ 
      print("El archivo de profesionales.csv ya tiene datos, vamos a solapar los de hoy. ")
      
      todo_profesionales <- import(profesionales_csv)
      todo_profesionales <- rbind(todo_profesionales, hoy)  #append data
      
      
      write.table(todo_profesionales, profesionales_csv , row.names=FALSE, col.names=TRUE, sep = ';')
      
    } else {
      write.table(hoy, profesionales_csv, row.names=FALSE, col.names=TRUE, sep = ';')
    }
    
    
  } else{ 
    # esto solo ocurre una vez, con el primer día del que tengamos datos, y 
    # sirve para restarlo a los del día siguiente, que será el primer día válido
    
    # guardamos directamente nuestro dataset tal cual está
    print("El archivo profesionales_ayer.csv no existía aún. ")
    write.table(excel, dataset_ayer, row.names=FALSE, col.names=TRUE, sep = ';')
  }
  
} # fin de la función de trabajo con datos d eprofesionales 


# preparar_datos_profesionales("datos/10-05/profesionales.xls")

