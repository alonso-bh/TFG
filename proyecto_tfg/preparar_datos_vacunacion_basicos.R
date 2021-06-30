# script para trabajar con los datos de vacunación 

#' FUNCIÓN PARA PROCESAR CADA UNO DE LOS DATASETS DE VACUNACIÓN
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
  
  # datos  <- import("datos/01-06/datos_pauta_incompleta.xls")
  # datos  <- import("datos/31-05/datos_pauta_completa.xls")
  
  # Borrar filas iniciales y finales nulas
  datos <- datos[-c(1:6),]
  #completa   <- completa[-c(1:6),] 
  
  # poner nombres correctos a columnas
  colnames(datos) <- datos[2,]
  #colnames(completa)   <- completa[2,]
  
  # eliminar fila de Andalucía (datos agregados) y columnas de total (datos agregados)
  datos <- datos[-3,-c(2,3)]
  #completa   <- completa[-3,-c(2,3)]
  
  # guardar rango de edades (valores de la dimensión)  
  edades <- datos[1,]
  edades <- edades[!is.na(edades)]
  
  # cambios en función del tipo de dataset: 
  # - versión 1: basta con eliminar las filas de cabecera (para desdinamizar)
  # - versión 2: - lo mismo que en versión 1 
  #              - fusionar rangos nuevos al antiguo (caso SCD) 
  if(version == 2){
    
    edades <- gsub("De 25 a 39", "De 25 a 49", edades)
    
    for(i in 1:length(edades)){
      if (grepl("^De 40 a 49", edades[i])){    # uso de Expr.Regs. y grepl (lib. base-R)
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
    
  } else if (version == 1){
    datos <- datos[-c(1,2),]
  }

  #completa <- completa[-c(1,2),]
  
  # en función de la versión, tenemos que añadir una columna u otra
  # desdinamizar el dataset de la paunta datos
  grupo1 <- datos[,c(1,2,3)]
  grupo2 <- datos[,c(1,4,5)]
  grupo3 <- datos[,c(1,6,7)]
  grupo4 <- datos[,c(1,8,9)]
  grupo5 <- datos[,c(1,10,11)]
  grupo6 <- datos[,c(1,12,13)]
  grupo7 <- datos[,c(1,14,15)]
  grupo8 <- datos[,c(1,16,17)]
  
  grupo1$Edad <- edades[1]
  grupo2$Edad <- edades[2]
  grupo3$Edad <- edades[3]
  grupo4$Edad <- edades[4]
  grupo5$Edad <- edades[5]
  grupo6$Edad <- edades[6]
  grupo7$Edad <- edades[7]
  grupo8$Edad <- edades[8]
  
  # cabecera genérica para cada fragmento
  if (tolower(tipo) == 'i'){
    cabecera <- c("Provincia", "Num personas con 1 dosis", "% personas con 1 dosis", "Edad")
  } else if (tolower(tipo) == 'c') {
    cabecera <- c("Provincia", "Num personas con pauta completa", "% personas con pauta completa", "Edad")
  }
  
  # solapar fragmentos en cada dataset: 'completa' e 'datos'
  colnames(grupo1) <- cabecera
  colnames(grupo2) <- cabecera
  colnames(grupo3) <- cabecera
  colnames(grupo4) <- cabecera
  colnames(grupo5) <- cabecera
  colnames(grupo6) <- cabecera
  colnames(grupo7) <- cabecera
  colnames(grupo8) <- cabecera
  
  datos <- rbind(grupo1, grupo2, grupo3, grupo4, 
                 grupo5, grupo6, grupo7, grupo8 )
  
  return (datos)
}


################################################################################
#' FUNCION PARA PROCESAR LOS DATOS DE VACUNACIÓN USADOS
#' 
#' Recibe un array básico -c()- con dos cadenas de caracteres: 
#'   1) el path al fichero datos_pauta_incompleta.xls
#'   2) el path al fichero datos_pauta_completa.xls
#' @param paths_vacunacion es un array de dos componentes: path al fichero con 
#' los datos sobre la pauta incompleta, y la pauta completa, en ese orden 
#' (NO INVERTIR EL ORDEN)
preparar_datos_vacunacion_basicos <- function(paths_vacunacion){
  
  library(rio)
  library(tidyr)
  library(dplyr)

  # setwd("C:/Users/UX430U/Desktop/TFG")
  # incompleta  <- import("datos/11-06/datos_pauta_incompleta.xls")
  # completa    <- import("datos/11-06/datos_pauta_completa.xls")

  incompleta <- import(paths_vacunacion[1])
  completa   <- import(paths_vacunacion[2])
  
  # procesar tablas
  incompleta <- tabla_plana (incompleta, 'i', 2)
  completa   <- tabla_plana (  completa, 'c', 2)
  
  # fusionar tablas horizontalmente 
  
  # paso previo: quitar columnas repetidas en uno de los 2 datasets
  completa <- completa[,c("Num personas con pauta completa", "% personas con pauta completa")]
  
  datos_globales <- cbind(incompleta, completa)
  
  # añadir columna con la fecha de hoy
  fecha = format(Sys.time(), "%d/%m/%Y")  # get current date 
  # fecha <- "11/06/2021"
  datos_globales$Fecha <- fecha
  
  # restar los datos del día anterior para ver los valores netos de hoy
  dataset_ayer <- "datos/ayer/vacunas_ayer.csv"
  
  if ( file.exists(dataset_ayer) ) { 
    
    ayer <- import(dataset_ayer)
    
    # ahora 2 datasets: el de hoy y el de ayer, tenemos que restar dos a dos las
    # columnas del mismo nombre que contienen datos acumulados

    hoy <- datos_globales
    
    # Almacenar los datos de hoy (sin restarlos, claro) para poder restarlos a
    # los de el día siguiente. Se reescribe el fichero profesionales_ayer.csv
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
    
    # sustituir los puntos por comas para trabajar con ellos en las herramientas
    # de Microsoft (SSMS/SSAS/Power BI/...)
    hoy$`% personas con 1 dosis` <- str_replace(hoy$`% personas con 1 dosis`, '\\.', ',')
    hoy$`% personas con pauta completa` <- str_replace(hoy$`% personas con pauta completa`, '\\.', ',' )
    
    # añadir al dataset de profesionales los datos de hoy
    vacunas_csv <- "datos/vacunas.csv"
    
    if( file.exists(vacunas_csv) ){ 
      print("El archivo de vacunas.csv ya tiene datos, vamos a solapar los de hoy. ")
      
      todo_vacunas <- import(vacunas_csv)
      todo_vacunas <- rbind(todo_vacunas, hoy)  #append data
      
      # grabar datos 
      write.table(todo_vacunas, vacunas_csv , row.names=FALSE, col.names=TRUE, sep = ';')
      
    } else {
      write.table(hoy, vacunas_csv, row.names=FALSE, col.names=TRUE, sep = ';')
    }
    
    
  } else{ 
    # esto solo ocurre una vez, con el primer día del que tengamos datos, y 
    # sirve para restarlo a los del día siguiente, que será el primer día válido
    
    # guardamos directamente nuestro dataset tal cual está
    print("El archivo vacunas_ayer.csv no existía aún. ")
    write.table(datos_globales, dataset_ayer, row.names=FALSE, col.names=TRUE, sep = ';')
  }
  
}

