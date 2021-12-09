# Ajuste de cosecha para remover el efecto temporal de la predicción

library(ggplot2)

# Directorio
current_working_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_working_dir)

# Leer datos
datos <- read.csv('datos_ml.csv', fileEncoding = "latin1")[,-1]
datos2 <- read.csv('datos_ml_2.csv', fileEncoding = "latin1")[,-1]

datos <- rbind(datos, datos2)

# Renombrar columna con el tipo de suelo
datos <- datos %>% rename(SOILTYPE="ï..SOILTYPE")

# Cambiar formato de la fecha
datos$Fecha<-as.Date(paste(datos$Fecha, '-01', sep=""))

# Quitar los datos en el tiempo de calentamiento (primer año - 1956)
datos <- datos[datos$Fecha>="1957-01-01",]

# Gráfica para ver la tendencia temporal
ggplot(data=datos, aes(x=Fecha, y=pods))+
  labs(title="Datos con clima aleatorio")+
  scale_color_brewer(palette='Dark2')+
  geom_point(size=0.8, alpha=0.3)

# Crear columna con número del periodo
datos<-datos %>% separate(Fecha, c("Anio", "Mes", NA))
datos$periodo <- (as.numeric(datos$Anio)-1957)*12 + as.numeric(datos$Mes)

# Calcular tendencia
source("resultado_constante.R")

trend <- tendencia()
b_1 <- trend["tendencia"]

# Realizar ajuste
datos$pods_ajustados <- datos$pods-b_1*sqrt(datos$periodo)

# Gráfica para ver la tendencia temporal despues del ajuste
ggplot(data=datos, aes(x=periodo, y=pods_ajustados))+
  labs(title="Datos ajustados con clima aleatorio")+
  geom_point(size=0.8, alpha=0.3)

# Exportar datos ajustados

write.csv(datos, "datos_ml_ajustados.csv")
