library(readODS)


fichero_vacunas <- "C:\\Users\\UX430U\\Desktop\\TFG\\datos_prueba\\vacunas.ods"

df1 <- read_ods(path = fichero_vacunas)

# Seleccionar solo la fila con datos de Andalucía 
df_andalucia <- df[c(1),-c(1)]

df_andalucia$Fecha <- format(Sys.time(), "%d/%m/%Y")  # fecha de descarga/notificación (hoy)

df2 <- read_ods(path = fichero_vacunas, sheet = 2 )
df2 <- df2[c(1),-c(1)]

df3 <- read_ods(path = fichero_vacunas, sheet = 3)
df3 <- df3[c(1),-c(1)]

df4 <- read_ods(path = fichero_vacunas, sheet = 4)
df4 <- df4[c(1),-c(1)]


df_andalucia <- cbind(df_andalucia, df2, df3, deparse.level = 1)
