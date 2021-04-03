library('flattabler')
library("rio")
library('readr')
library(tidyr) # for fill function 


setwd("C:\\Users\\UX430U\\Desktop\\TFG\\datos_prueba")

# import excel
excel <- import("residencias_edad.xls")
View(excel)

sexos <- c(excel[8,3], excel[8,7]) 

# remove non-valid rows, but before I save the first row with the type of "residencias" used
excel <- excel[-c(1,2,3,4,5,6,7,30,31),]
excel <- excel[-c(1),]

colnames(excel) <- list("V1","V2","V3","V4","V5","V6","V7", "V8", "V9", "V10")  # simplify columns name of dataset

# fill the first column with the date in each row 
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

# add date (Fecha) column and fill it
date_today = format(Sys.time(), "%d/%m/%Y")  # get current date 

date_today <- "05/04/2021"
data_clear$V9 <- date_today 
data_clear[1,8] <- "Fecha" 

# change the column names to the names we need (Territorio, Fallecidos, etc)
colnames(data_clear) <- data_clear[1,]
data_clear <- data_clear[-c(1),]

View(data_clear)

# ----------------------------------------------
# LAST STEP: Add this dataset to the dataset of the rest "residencias.csv"

setwd("C:\\Users\\UX430U\\Desktop\\TFG\\datos_prueba")

if( file.exists("residencias_edad.csv") ){ # there is already a file with past data
  
  print("El archivo de residencias_edad.csv ya tiene datos. ")
  
  todo_residencias <- import("residencias_edad.csv")
  todo_residencias <- rbind(todo_residencias, data_clear)  #append data
  
  write.table(todo_residencias, "residencias_edad.csv", row.names=FALSE, col.names=TRUE, sep = ';')
  
} else {
  write.table(data_clear, "residencias_edad.csv", row.names=FALSE, col.names=TRUE, sep = ';')
}


aux <- import("residencias_edad.csv")
View(aux)
