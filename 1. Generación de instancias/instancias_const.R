k<-1
generacion <- function(k){
  # Generacion de instancias V1.0
  set.seed(2021*k)
  
  # Rangos de las variables predictoras
  
  # Temperatura minima
  temp_min_range <- c(19, 25)
  # Temperatura maxima
  temp_max_range <- c(28, 36)
  # Irradiacion
  irr_range <- c(14, 20)
  # Presion de vapor
  vapor_range <- c(29, 43)
  # Windspeed
  wind_range <- c(-99, -99)
  # Precipitacion
  precip_range <- c(0, 1400)
  # Dias con lluvia
  dias_lluvia_range <- c(0,31)
  
  # Dataframe con rangos
  rangos <- cbind(temp_min_range, temp_max_range, irr_range,
                  vapor_range, wind_range, precip_range, dias_lluvia_range)
  
  # Generacion de instancias climaticas
  
  # Parametros
  anios_range <- c(1956, 2002)
  
  # Generacion de instancias aleatorias
  
  # Inicializacion del dataframe
  variables <- c("Ano", "Mes", "Temperatura_minima", "Temperatura_maxima", "Irradiacion",
                 "Presion_de_vapor", "windspeed", "Precipitacion", "Dias_con_lluvia")
  colnames(rangos) <- variables[3:length(variables)]
  data <- data.frame(matrix(ncol = length(variables), nrow=0,dimnames=list(NULL,variables)))
  
  # Vector de anos
  anios <- anios_range[1]:anios_range[2]
  
  # Llenar con anos y meses
  fila=1
  for(anio in anios){
    for(mes in 1:12){
      data[fila, "Ano"] <- anio
      data[fila, "Mes"] <- mes
      fila=fila+1
    }
  }
  
  
  
  # Datos aleatorios de las variables climaticas
  for(var in variables[3:length(variables)]){
    if(var=="Dias_con_lluvia"){
      
    }else if(var=="Precipitacion"){
      data[,var] <- round(runif(nrow(data), min=rangos[,var][1], max=rangos[,var][2]))[1]
    
    }else{
      data[,var] <- runif(nrow(data), min=rangos[,var][1], max=rangos[,var][2])[1]
    }
    
  }
  
  # Datos aleatorios de los dias con lluvia
  dias_por_mes <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  
  #for(i in 1:nrow(data)){
  #  data[i, "Dias_con_lluvia"] <- round(runif(1, min=0, max=dias_por_mes[data[i, "Mes"]]))
  #}
  x<-13
  for(i in 1:nrow(data)){
    data[i, "Dias_con_lluvia"] <- x
  }
  
  data[, "Temperatura_minima"]<-21.3
  data[, "Temperatura_maxima"]<-30.4
  data[, "Irradiacion"]<-17
  data[, "Presion_de_vapor"]<-34
  data[, "Temperatura_minima"]<-21.3
  data[, "Precipitacion"]<-500
  data[, "Dias_con_lluvia"]<-20
  
  
  # Garantizar que si precipitación o dias con lluvia es 0, ambas deben ser 0
  for(i in 1:nrow(data)){
    if(data[i, "Precipitacion"]==0){
      data[i, "Dias_con_lluvia"]=0
    }
    if(data[i, "Dias_con_lluvia"]==0){
      data[i, "Precipitacion"]=0
    }
    
  }
  
  
  latitud <- 4.161919444
  altitud <- 422
  # Exportar instancia
  file <- paste("C:/case2/weather/monthly/instancia_aleatoria", ".wof", sep="")
  cat(paste("Instancia_",k, '\n',anios_range[1]," ",latitud," ",altitud, '\n', sep = ""), file=file)
  for(i in anios_range[1]:anios_range[2]){
    if(i>anios_range[1]){
      cat(paste('Instancia_',k,'\n',i," ",latitud," ",altitud, '\n', sep = ""), file=file, append=TRUE)
    }
    write.table(data[(1+(i-anios_range[1])*12):(12+(i-anios_range[1])*12),c(-1, -2)], file=file, col.names=FALSE, row.names = FALSE, append=TRUE)
  }
  return(data)
}

