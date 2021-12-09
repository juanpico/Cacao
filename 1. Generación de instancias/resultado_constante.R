# Análisis de cosecha con variables constantes

library(ggplot2)
library(lubridate)
library(tidyverse)

# Directorio
current_working_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_working_dir)

tendencia <- function(){
  # Leer datos
  datos <- read.csv('datos_ml_const.csv', fileEncoding = "latin1")[,-1]
  
  # Gráfica de tendencia
  datos$Fecha<-as.Date(paste(datos$Fecha, '-01', sep=""))
  datos$id<-as.character(datos$id)
  ggplot(data=datos, aes(x=Fecha, y=pods, color=id))+
    geom_line()
  
  # Quitar datos de calentamiento correspondientes al primer año (1956)
  datos <- datos[datos$Fecha>="1957-01-01",]
  ggplot(data=datos, aes(x=Fecha, y=pods, color=id))+
    geom_line()
  
  # Boxplot
  #prueba<-datos[datos$Fecha>"1957-01-01",]
  #prueba$Fecha<-as.character(prueba$Fecha)
  #prueba<-prueba %>% separate(Fecha, c(NA, "Mes", NA))
  #boxplot(pods~Mes, data=prueba)
  
  # Regresión lineal para encontrar la tendencia
  
  # Tomar unicamente datos necesarios
  datos <- datos[, c("Fecha", "pods", "id")]
  
  # Crear columna con número del periodo
  datos<-datos %>% separate(Fecha, c("Anio", "Mes", NA))
  datos$periodo <- (as.numeric(datos$Anio)-1957)*12 + as.numeric(datos$Mes)
  
  # Estimar regresión lineal
  model <- lm(pods~sqrt(periodo), data=datos)
  datos$fitted <- model$fitted.values
  
  # Gráfica regresión
  ggplot()+
    geom_line(data=datos, aes(x=periodo, y=pods, color=id))+
    geom_line(data=datos, aes(x=periodo, y=fitted, color=id))
  
  # Realizar ajuste
  datos$fitted2 <- datos$fitted-model$coefficients[2]*sqrt(datos$periodo)
  datos$y <- datos$pods-model$coefficients[2]*sqrt(datos$periodo)
  
  # Graficar ajuste
  plot <- ggplot()+
    geom_point(data=datos, aes(x=periodo, y=pods, color="Originales"), size=0.8)+
    geom_point(data=datos, aes(x=periodo, y=fitted, color="Regresión"), size=0.8)+
    geom_point(data=datos, aes(x=periodo, y=fitted2, color="Regresión sin tendencia"), size=0.8)+
    geom_point(data=datos, aes(x=periodo, y=y, color="Ajustados"), size=0.8)+
    labs(title="Ajuste de datos mediante regresión con raiz cuadrada del periodo", color="Datos:")+
    scale_color_brewer(palette='Dark2')
  
  # Imprimir plot
  print(plot)
  
  # Calcular MSE y MAPE
  MSE <- (datos$y-mean(datos$y))^2
  MSE <- mean(MSE)
  MAPE <- mean(abs(mean(datos$y)-datos$y)/mean(datos$y))
  
  # Resultado de la función
  resultado <- c(model$coefficients[2], MSE, MAPE)
  names(resultado) <- c("tendencia", "mse", "mape")
  
  return(resultado)
}

  
  
 
  