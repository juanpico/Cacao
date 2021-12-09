# Regresión lineal sobre el efecto de la densidad de plantación en la
# productividad de los cultivos

library(ggplot2)

# Definir directorio
  current_working_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
  setwd(current_working_dir)
  
# Cargar datos
  
  datos <- read.csv("datos_densidad.csv")
  
  # Definir las variables categóricas
  datos$Municipio <- as.factor(datos$Municipio)  
  datos$Conf <- as.factor(datos$Conf)  
  datos$Año <- as.factor(datos$Año)  

# Análisis descriptivo
  
  datos <- datos[datos$Año!='2',] 
  ggplot(data=datos, aes(x=factor_densidad, y=factor_pods))+
    geom_point()
  
  # Año
  ggplot(data=datos, aes(x=factor_densidad, y=factor_pods, color=Año))+
    geom_point()

  # Municipio
  ggplot(data=datos, aes(x=factor_densidad, y=factor_pods, color=Municipio))+
    geom_point()
  
  # Configuración
  ggplot(data=datos, aes(x=factor_densidad, y=factor_pods, color=Conf))+
    geom_point()
  
# Regresión lineal
  
  # Modelo solo considerando el factor densidad
  model1 <- lm(factor_pods~factor_densidad, data=datos) 
  summary(model1) 
  
  ggplot(data=datos[,c('factor_densidad', 'factor_pods')], aes(x=factor_densidad, y=factor_pods))+
    geom_point()+geom_smooth(method='lm')
  
  # Modelo considerando la configuración (suelo y si es potencial)
  
  model2 <- lm(factor_pods~factor_densidad + factor_densidad*Conf, data=datos)
  summary(model2)
  
  ggplot(data=datos[,c('Conf', 'factor_densidad', 'factor_pods')], aes(x=factor_densidad, y=factor_pods, color=Conf))+
    geom_point()+geom_smooth(method='lm', fill=NA)
  
  # Modelo considerando el municipio
  
  model3 <- lm(factor_pods~factor_densidad + factor_densidad*Conf + 
                 factor_densidad*Municipio, data=datos)
  summary(model3) 

  cat('R^2 Ajustado:\n',
  " - Solo con el factor de densidad:",summary(model1)$adj.r.squared,'\n',
  " - Factor de densidad y suelo:",summary(model2)$adj.r.squared,'\n',
  " - Factor de densidad, suelo y municipio:",summary(model3)$adj.r.squared)
  
 model2$coefficients
  