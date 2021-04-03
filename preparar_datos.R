# ALONSO BUENO HERRERO

library("rio")
library('readr')


# function to prepare the new dataset before appending it to the past data 
clear_dataset<- function(data_xls, the_date){
  # import xls as dataframe in order to transform the data
  granada <- import(data_xls)
  granada

  # drop unnecesary rows
  granada_clear <- granada[-c(1,2,3,4,5,6,187,188,189,190),]  

  # add date column in order to identify the date asssociated to the data
  granada_clear$Fecha = the_date
  return(granada_clear)
}

concatenar_strings <- function(cad1, cad2){
  tmp <- cbind(cad1, cad2)
  library(stringr)
  str_c(tmp, collapse = "")
}


# ------------------------------------------------------------------------------
# Tareas a realizar:
#   
# a. Get the dataset of the day.
# b. Extract null rows and add "Fecha" (date) column with today's date 
# c. Append the data to the general dataset of the "provincia" (csv)
# 
# ------------------------------------------------------------------------------

path = concatenar_strings("C:\\Users\\UX430U\\Desktop\\TFG\\datos_prueba\\", 
                          format(Sys.time(), "%d-%m"))

path
setwd("C:\\Users\\UX430U\\Desktop\\TFG\\datos_prueba\\25-02")

date_today = format(Sys.time(), "%d/%m/%Y")  # get current date 
date_today = "25/02/2021"

granada_clear <- clear_dataset("Covid_18.xls", the_date = date_today) 

View(granada_clear)

head = c("Lugar de residencia",
         "Población",
         "Confirmados PDIA",
         "Confirmados PDIA 14 días",
         "Tasa PDIA 14 días",
         "Confirmados PDIA 7 días",
         "Total Confirmados",
         "Curados",
         "Fallecidos",
         "Fecha")
head


colnames(granada_clear) <- c(head)


# append the new data to the rest of the data of the "provincia"
setwd("C:\\Users\\UX430U\\Desktop\\TFG\\datos_prueba")

if( file.exists("granada.csv") ){
  print("El archivo de granada.csv ya tiene datos. ")
  
  todo_granada <- import("granada.csv")

  todo_granada <- rbind(todo_granada, granada_clear)  #append data
  View(todo_granada)
  
  write.table(todo_granada, "granada.csv", row.names=FALSE, col.names=TRUE, sep = ';')

  } else {
  write.table(granada_clear, "granada.csv", row.names=FALSE, col.names=TRUE, sep = ';')
}


aux <- import("granada.csv")
View(aux)

