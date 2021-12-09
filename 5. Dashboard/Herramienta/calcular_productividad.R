source("predictor3.R")
library(tidyverse)
calcular_productividad <- function(datos_clima, datos_cultivo, anio){
  
  # Vector de lugares obtenidos
  lugares <- unique(datos_clima$Lugar)
  
  # Lista con los datos de clima de cada lugar
  climas <- list()
  for(l in lugares){
    climas[[l]] <- datos_clima[datos_clima$Lugar==l,] 
  }
  
  # Construir tabla de clima de cada lugar para el modelo predictivo
  for(l in lugares){
    
    # Obtener lags
    climas[[l]] <- climas[[l]] %>%
      mutate(temp_max_lag = dplyr::lag(Temperatura_maxima, n = 1, default = NA)) %>% 
      mutate(temp_min_lag = dplyr::lag(Temperatura_minima, n = 1, default = NA)) %>% 
      mutate(irr_lag = dplyr::lag(Irradiacion, n = 1, default = NA)) %>% 
      mutate(irr_lag2 = dplyr::lag(Irradiacion, n = 2, default = NA)) %>% 
      mutate(vapor_lag = dplyr::lag(Presion_de_vapor, n = 1, default = NA)) %>% 
      mutate(dias_lag = dplyr::lag(Dias_con_lluvia, n = 1, default = NA)) %>% 
      mutate(dias_lag2 = dplyr::lag(Dias_con_lluvia, n = 2, default = NA)) %>% 
      mutate(temp_max_lag2 = dplyr::lag(Temperatura_maxima, n = 2, default = NA)) %>% 
      mutate(temp_min_lag2 = dplyr::lag(Temperatura_minima, n = 2, default = NA)) %>% 
      mutate(Precip_lag = dplyr::lag(Precipitacion, n = 1, default = NA)) %>% 
      mutate(Precip_lag2 = dplyr::lag(Precipitacion, n = 2, default = NA)) %>% 
      mutate(Precip_lag3 = dplyr::lag(Precipitacion, n = 3, default = NA))
    
    # Definir intervalo de tiempo
    anio <- as.character(anio)
    climas[[l]] <- climas[[l]][(climas[[l]]$Fecha>=paste(anio,'-01', sep=""))&(climas[[l]]$Fecha<=paste(anio,'-12', sep="")),] 
    
    # Remover columna con la fecha
    climas[[l]] <- climas[[l]] %>% select(-Fecha)
  }
  
  
  # Calcular cantidad de cultivos
  num_cultivos <- length(unique(datos_cultivo$Cultivo))
  
  # Inicializar tabla con los resultados de cada cultivo
  prods <- data.frame("Cultivo"= paste("Cultivo ",1:num_cultivos, sep=""),
                      "P.Ajustada" =  rep(0, num_cultivos), 
                      "P.Teórica" = rep(0,num_cultivos), 
                      "P.Potencial" = rep(0,num_cultivos), 
                      "P.Limitada" = rep(0,num_cultivos))
  
  # Arreglar suelo
  datos_cultivo <- datos_cultivo %>%  
    mutate(Suelo = factor(case_when(Suelo == 'Limoso'~1,
                                    Suelo == 'Arenoso'~2,
                                    Suelo == 'Arcilloso'~3)))
  
  
  for(i in 1:num_cultivos){
    # Datos de cultivo para el cultivo i
    datos_cultivo_i <- datos_cultivo[datos_cultivo$Cultivo == i,]

    # Identificar lugar del cultivo i
    lugar <- datos_cultivo_i[1, "Lugar"]
    #return(climas[[lugar]])
    prod_i <- predictor(climas[[lugar]], suelo = datos_cultivo_i$Suelo, 
                            variedad = datos_cultivo_i$Variedad,
                            edad_inicial = datos_cultivo_i$Edad, 
                            densidad = datos_cultivo_i$Arboles/datos_cultivo_i$Hectareas,
                            hectareas = datos_cultivo_i$Hectareas,
                            rendimiento = datos_cultivo_i$Rendimiento)
    
    prods[i, 'P.Ajustada'] <- mean(prod_i[[1]])
    prods[i, 'P.Teórica'] <- mean(prod_i[[2]])
    prods[i, 'P.Potencial'] <- sum(prod_i[[3]]*datos_cultivo_i$Hectareas/sum(datos_cultivo_i$Hectareas))
    prods[i, 'P.Limitada'] <- sum(prod_i[[4]]*datos_cultivo_i$Hectareas/sum(datos_cultivo_i$Hectareas))
    
    # Tabla para los boxplots
    if(i == 1){
      boxplots <- data.frame("Teórica" = prod_i[[2]],
                             "Ajustada" = prod_i[[1]])
      boxplots['Cultivo'] <- "Cultivo 1"

    }else{
      temp <- data.frame("Teórica" = prod_i[[2]],
                         "Ajustada" = prod_i[[1]])
      temp['Cultivo'] <- paste("Cultivo ", i, sep="")
      
      boxplots <- rbind(boxplots, temp)
    }
    
  }
  
  prods$Brecha <- 1-(prods[['P.Ajustada']]/prods[['P.Teórica']])
  
  boxplots <- boxplots %>% pivot_longer(c(Teórica, Ajustada),names_to = "tipo_prod", values_to = "semillas")

  return(list(prods, boxplots))
  
}