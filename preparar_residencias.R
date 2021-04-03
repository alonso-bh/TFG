library('flattabler')
library("rio")
library('readr')


setwd("C:\\Users\\UX430U\\Desktop\\TFG\\datos_prueba")

# import excel
excel <- import("residencias.xls")
#View(excel)

# remove non-valid rows, but before I save the first row with the type of "residencias" used
excel <- excel[-c(1,2,3,4,5,6,7,53,54),]
type_residencias <- c(excel[1,2], excel[1,8])
excel <- excel[-c(1),]

# remove aggregation rows (data for Andalucia and Provincias)
colnames(excel) <- list("V1","V2","V3","V4","V5","V6","V7", "V8", "V9", "V10", "V11", "V12", "V13")  # simplify columns name of dataset

residencias_today<-excel[!(excel$V1=="Andalucía" | excel$V1=="Almería" | excel$V1=="Cádiz" | excel$V1=="Huelva" | excel$V1=="Sevilla" | excel$V1=="Jaén" | excel$V1=="Granada" | excel$V1=="Córdoba" | excel$V1=="Málaga"),]

#write.table(excel2, "residencias.csv",row.names=FALSE, col.names=FALSE,  sep = ';')
#residencias_today <- read_text_file("residencias.csv", sep = ';', encoding = 'ASCII')


# ---------------------------------
# Now we can manually  "unpivot" the table in orden to delete the row of the type os "residencia"
# 1. get the first column
column1 <- residencias_today[,1]

# 2. unpivot (we generate and then fix 2 subdatasets for both type of "residencias")
new_col <- "Vive en residencia"  # source: https://www.juntadeandalucia.es/institutodeestadisticaycartografia/badea/operaciones/consulta/anual/38378?CodOper=b3_2314&codConsulta=38378 (casos por edad y donde vive)

# 2a. first type of "residencia" -> "Residencias de mayores"
data_resid_mayores <- residencias_today[,c(1,2,3,4,5,6,7)]
var_resid_mayores <- type_residencias[1]


# creating and filling the (new) date column
data_resid_mayores$V8 = var_resid_mayores
data_resid_mayores[1,8] <- new_col


# 2b. second type of "residencia" -> "Otro tipo de institución"
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
date_today = format(Sys.time(), "%d/%m/%Y")  # get current date 

date_today <- "05/04/2021"
data_clear$V9 <- date_today 
data_clear[1,9] <- "Fecha" 

# change the column names to the names we need (Territorio, Fallecidos, etc)
colnames(data_clear) <- data_clear[1,]
data_clear <- data_clear[-c(1),]

View(data_clear)

# ----------------------------------------------
# LAST STEP: Add this dataset to the dataset of the rest "residencias.csv"

setwd("C:\\Users\\UX430U\\Desktop\\TFG\\datos_prueba")

if( file.exists("residencias.csv") ){ # there is already a file with past data

  print("El archivo de residencias.csv ya tiene datos. ")
  
  todo_residencias <- import("residencias.csv")
  todo_residencias <- rbind(todo_residencias, data_clear)  #append data
  
  View(todo_residencias)
  #print("Se van a escribir " + nrow(todo_residencias) + " filas (antiguas más nuevas). ")
  
  write.table(todo_residencias, "residencias.csv", row.names=FALSE, col.names=TRUE, sep = ';')
  
} else {
  write.table(data_clear, "residencias.csv", row.names=FALSE, col.names=TRUE, sep = ';')
}


aux <- import("residencias.csv")
View(aux)
