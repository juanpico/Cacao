library(triangle)

generacion <- function(k){
  # Generacion de instancias 
  set.seed(2021*k)
  
  # Dataframe con parámetros de la distribución triangular de cada variable
  rangos <- read.csv("params_triangular.csv")[-1]
  
  ## Generacion de instancias climaticas
  
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
  
  # Llenar con anios y meses
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
      data[,var] <- round(rtriangle(nrow(data), a=rangos[,var][1], b=rangos[,var][2], c=rangos[,var][4]))
    
    }else{
      data[,var] <- rtriangle(nrow(data), a=rangos[,var][1], b=rangos[,var][2], c=rangos[,var][4])
    }
    
  }
  
  # Datos aleatorios de los dias con lluvia
  dias_por_mes <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  
  for(i in 1:nrow(data)){
    if(dias_por_mes[data[i, "Mes"]]==28){
      data[i, "Dias_con_lluvia"] <- round(rtriangle(1, a=rangos[,"Dias_con_lluvia"][1], b=dias_por_mes[data[i, "Mes"]], 
                                                    c=(3*(rangos[,"Dias_con_lluvia"][3]-2) - rangos[,"Dias_con_lluvia"][1] - rangos[,"Dias_con_lluvia"][2])))
    }else{
      data[i, "Dias_con_lluvia"] <- round(rtriangle(1, a=rangos[,"Dias_con_lluvia"][1], b=dias_por_mes[data[i, "Mes"]], 
                                                    c=(3*rangos[,"Dias_con_lluvia"][3] - rangos[,"Dias_con_lluvia"][1] - rangos[,"Dias_con_lluvia"][2])))
    }
    
  }

  
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
  # Exportar instancia en el formato del CASE2
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

