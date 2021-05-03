library(tidyr)
library(rio)

setwd("C:\\Users\\UX430U\\Desktop\\TFG\\datos")

excel <- import("20_cod_prov.xls")

excel <- drop_na(excel)
  
excel <- excel[-c(1),]
colnames(excel) <- c("Cod Provincia", "Provincia" )

View(excel)
excel$`Cod Provincia` <- as.character(excel$`Cod Provincia`)
write.table(excel, "cod_provincias.csv", row.names=FALSE, col.names=TRUE, sep = ';')

