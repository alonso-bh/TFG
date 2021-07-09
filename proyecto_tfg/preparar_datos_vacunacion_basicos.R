# script para trabajar con los datos de vacunación 


################################################################################
#' FUNCIÓN AUXILIAR PARA PROCESAR CADA UNO DE LOS DATASETS DE VACUNACIÓN
#' 
#' Debido a que son datasets idénticos en campos y el contenido, se ha construido
#' esta función para facilitar la reusabilidad de código.
#' 
#' @param datos es el dataset a procesar, el de 1 dosis o el de la pauta completa
#' @param tipo es el tipo de dataset; toma los valores:
#'     'I' : dataset pauta incompleta
#'     'C' : dataset pauta completa
tabla_plana <- function(datos, tipo, version){
  
  library(stringr)
  
  # Borrar filas iniciales y finales nulas 
  if(version == 3){
    if (nrow(datos) == 18){
      datos <- datos[-c(1:7),]
    } else{
      datos <- datos[-c(1:6),]
    }
  } else{
    datos <- datos[-c(1:6),]
  }
  
  
  # poner nombres correctos a columnas
  colnames(datos) <- datos[2,]

  # eliminar fila de Andalucía (datos agregados) y columnas de total (datos agregados)
  datos <- datos[-3,-c(2,3)]

  # guardar rango de edades (valores de la dimensión)  
  edades <- datos[1,]
  edades <- edades[!is.na(edades)]
  
  # cambios en función del tipo de dataset: 
  # - versión 1: basta con eliminar las filas de cabecera (para desdinamizar)
  # - versión 2: - lo mismo que en versión 1 
  #              - fusionar rangos nuevos al antiguo (caso SCD) 
  # - versión 3: - tomar los rangos nuevos (solo los nuevos y almacenarlos junto a los antiguos)
  if(version == 2){
    
    edades <- gsub("De 25 a 39", "De 25 a 49", edades)
    
    for(i in 1:length(edades)){
      if (grepl("^De 40 a 49", edades[i])){    # uso de Expresión Regular
        edades <- edades[-i]
      }
    }
    
    # borrar dos primeras filas
    datos <- datos[-c(1,2),]
    
    # sumamos los valores de ambos rangos (columnas 10-11 y 12-13) por separado,
    # y las colocamos en el lugar de las anteriores
    rango1_num  <- datos[,10]
    rango1_porc <- datos[,11]
    rango2_num  <- datos[,12]
    rango2_porc <- datos[,13]

    # añadir nuevos datos en la posición que irían en el formato "antiguo"
    datos[,10] <- as.numeric(rango1_num)  + as.numeric(rango2_num)
    datos[,11] <- as.numeric(rango1_porc) + as.numeric(rango2_porc)
    
    # eliminar las columnas "sobrantes" (rango2)
    datos <- datos[,-c(12,13)]
    
  } else if (version == 1 | version == 3){
    
    # borrar filas superiores, ya tenemos los datos guardados correctamente
    datos <- datos[-c(1,2),]
    
  }
  
  # des-dinamizar (unpivot) el dataset 
  grupo1 <- datos[,c(1,2,3)]
  grupo2 <- datos[,c(1,4,5)]
  grupo3 <- datos[,c(1,6,7)]
  grupo4 <- datos[,c(1,8,9)]
  grupo5 <- datos[,c(1,10,11)]
  grupo6 <- datos[,c(1,12,13)]
  grupo7 <- datos[,c(1,14,15)]
  grupo8 <- datos[,c(1,16,17)]
  
  # asignas a cada fragmento su rango de edad 
  grupo1$Edad <- edades[1]
  grupo2$Edad <- edades[2]
  grupo3$Edad <- edades[3]
  grupo4$Edad <- edades[4]
  grupo5$Edad <- edades[5]
  grupo6$Edad <- edades[6]
  grupo7$Edad <- edades[7]
  grupo8$Edad <- edades[8]
  
  # cabecera (común) para cada fragmento
  if (tolower(tipo) == 'i'){
    cabecera <- c("Provincia", "Num personas con 1 dosis", "% personas con 1 dosis", "Edad")
  } else if (tolower(tipo) == 'c') {
    cabecera <- c("Provincia", "Num personas con pauta completa", "% personas con pauta completa", "Edad")
  }
  
  # solapar fragmentos en cada dataset: 
  colnames(grupo1) <- cabecera
  colnames(grupo2) <- cabecera
  colnames(grupo3) <- cabecera
  colnames(grupo4) <- cabecera
  colnames(grupo5) <- cabecera
  colnames(grupo6) <- cabecera
  colnames(grupo7) <- cabecera
  colnames(grupo8) <- cabecera
  
  if(version == 3){
    
    # hay que usar un grupo más, el grupo 9, para el NUEVO rango de edad
    grupo9 <- datos[,c(1,18,19)]
    
    grupo9$Edad <- edades[9]
    
    colnames(grupo9) <- cabecera
    
    datos <- rbind(grupo1, grupo2, grupo3, grupo4, 
                   grupo5, grupo6, grupo7, grupo8, grupo9 )
    
  } else if (version == 1 | version == 2 ){
    datos <- rbind(grupo1, grupo2, grupo3, grupo4, 
                   grupo5, grupo6, grupo7, grupo8 )
  }
  
  return (datos)
}



################################################################################
#' FUNCION PARA PROCESAR LOS DATOS DE VACUNACIÓN USADOS
#' 
#' Recibe un array -c()- con dos cadenas de caracteres: 
#'   1) el path al fichero datos_pauta_incompleta.xls
#'   2) el path al fichero datos_pauta_completa.xls
#' @param paths_vacunacion es un array de dos componentes: path al fichero con 
#' los datos sobre la pauta incompleta, y la pauta completa, en ese orden 
#' (NO INVERTIR EL ORDEN)
preparar_datos_vacunacion_basicos <- function(paths_vacunacion, version){
  
  library(rio)
  library(tidyr)
  library(dplyr)
  
  # incompleta  <- import("datos/dd-mm-aa/datos_pauta_incompleta.xls") #pruebas
  # completa    <- import("datos/dd-mm-aa/datos_pauta_completa.xls")   #pruebas

  incompleta <- import(paths_vacunacion[1])
  completa   <- import(paths_vacunacion[2])
  
  # procesar tablas
  incompleta <- tabla_plana (incompleta, 'i', version)
  completa   <- tabla_plana (  completa, 'c', version)
  
  # fusionar tablas horizontalmente 
  # paso previo: quitar columnas repetidas en uno de los 2 datasets
  completa <- completa[,c("Num personas con pauta completa", "% personas con pauta completa")]
  
  # fusionar (una al lado de otra)
  datos_globales <- cbind(incompleta, completa)
  
  # añadir columna con la fecha de hoy
  fecha = format(Sys.time(), "%d/%m/%Y")  # get current date 
  datos_globales$Fecha <- fecha
  
  # restar los datos del día anterior para ver los valores netos de hoy
  dataset_ayer <- "datos/ayer/vacunas_ayer.csv"
  
  if ( file.exists(dataset_ayer) & datos_globales[1,"Fecha"] != "17/06/2021" ) { 
    # la segunda condición es debido a la versión 3 (dimensión lentamente cambiante)
    # no podemos restar los datos del 17/7/21 a los del día anterior porque tienen
    # distintos rangos de edad => distinto nº de filas => error 
    
    # importar datos de ayer para restarlos 
    ayer <- import(dataset_ayer)
    
    # usar nombre más intituivo
    hoy <- datos_globales
    
    # Almacenar los datos de hoy (sin restarlos) 
    write.table(hoy, dataset_ayer, row.names=FALSE, col.names=TRUE, sep = ';')
    
    # restar cada columna y modificarla en el dataset final del día de hoy 
    # col 2 
    aux1 <- c(as.numeric(hoy[,2]))
    aux2 <- c(as.numeric(ayer[,2]))
    aux <- aux1-aux2
    hoy$`Num personas con 1 dosis` <- as.character(aux)
    
    # col 5
    aux1 <- c(as.numeric(hoy[,5]))
    aux2 <- c(as.numeric(ayer[,5]))
    aux <- aux1-aux2   #
    hoy$`Num personas con pauta completa` <- as.character(aux)
    
    # sustituir los puntos por comas para que SSIS/SSAS reconozcan que es float
    hoy$`% personas con 1 dosis` <- str_replace(hoy$`% personas con 1 dosis`, '\\.', ',')
    hoy$`% personas con pauta completa` <- str_replace(hoy$`% personas con pauta completa`, '\\.', ',' )
    
    # añadir al dataset de profesionales los datos de hoy
    vacunas_csv <- "datos/vacunas.csv"
    
    if( file.exists(vacunas_csv) ){ 
      print("El archivo de vacunas.csv ya tiene datos, vamos a solapar los de hoy. ")
      
      todo_vacunas <- import(vacunas_csv)
      todo_vacunas <- rbind(todo_vacunas, hoy)  #fusionar tablas (una bajo otra)
      
      # grabar datos 
      write.table(todo_vacunas, vacunas_csv , row.names=FALSE, col.names=TRUE, sep = ';')
      
    } else {
      write.table(hoy, vacunas_csv, row.names=FALSE, col.names=TRUE, sep = ';')
    }
    
    
  } else{ 
    # sólo ocurre una vez
    # guardamos dataset sin modificarlo
    
    print("El archivo vacunas_ayer.csv no existía aún o bien estamos en el día 17/06/2021 ")
    write.table(datos_globales, dataset_ayer, row.names=FALSE, col.names=TRUE, sep = ';')
  }
  
}

