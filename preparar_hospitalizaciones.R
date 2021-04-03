# script to work with hospitalizados.xls file
library('tidyr')   # para el fill
library('readr')
library("rio")


setwd("C:\\Users\\UX430U\\Desktop\\TFG\\datos")

excel <- import("sit_hospitalaria.xls")

# delete head non-valid rows
excel <- excel[-c(1,2,3,4),]  

# fill the first column with the date in each row 
excel <- excel %>% fill("Informe COVID-19 en Andalucía", .direction = "down")

# delete rows with "roll-up" values 
excel <- excel[!(excel$...2 == "Andalucía"),]

View(excel)


# ---------------------------------------------
# generate table for dimensions and fact/s 

# extract column for "When" dimension
fecha_column <- excel[,1]

View(fecha_column)

