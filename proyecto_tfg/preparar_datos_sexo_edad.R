#install.packages("jsonlite")
# library("jsonlite")
library(rjson)

setwd("C:\\Users\\UX430U\\Desktop\\TFG\\datos_prueba")

download.file("https://www.juntadeandalucia.es/institutodeestadisticaycartografia/badea/stpivot/stpivot/Print?cube=37fdb68a-524e-43ab-b27a-6d6f2f5c4626&type=3&codConsulta=40402&consTipoVisua=TJP", destfile = "sexo_edad.zip" , method = "curl")

tabla <- fromJSON(file='and_sexo_edad.json')
tabla <- as.data.frame(tabla$data) 
View(tabla$data)
