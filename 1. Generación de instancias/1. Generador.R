# Generador de instancias

#librerias
library(tidyverse)

# Vector que indica el mes cada dia
dias_por_mes <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
dias_por_mes_b <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

meses <- c()
for(a in 1956:1984){
  if(a%%4==0){
    for(m in 1:12){
      meses <- c(meses, rep(m, dias_por_mes_b[m]))
    }
  }else{
    for(m in 1:12){
      meses <- c(meses, rep(m, dias_por_mes[m]))
    }
  }
}

# Directorio de la carpeta actual
current_working_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_working_dir)

# Importar función que genera archivo de datos climaticos aleatorios
source('instancias.R')

n = 10
for(i in 1:n){
  
  # Medir tiempo de inicio del algoritmo
  if(i == 1){
    t <- proc.time()
  }
  
  # generar archivo de clima
  setwd(current_working_dir)
  data <- generacion(i)
  
  # Cambiar el wd al directorio del case2
  setwd("C:/case2/program/")
  
  # MANUALMENTE Correr el case2
  
  # Verificar si el CASE2 ya terminó de ejectutar mediante la revisión de los
  # archivos de salida cada 5 segundos.
  error = TRUE
  while(error==TRUE){
    tryCatch(
      expr = {
        # Intentar leer los archivos de salida
        for(l in 0:5){
          ruta <- paste('Run_', l,'.txt',sep="")
          run <- read.table(ruta, header = TRUE, sep=',')
        }
        error=FALSE
      },
      # Si aún no están listos...
      error = function(e){ 
        message('No esta listo, esperando 5 segundos:')
        Sys.sleep(5)
      }
    )
  }
  
  
  # Leer resultados
  setwd(current_working_dir)
  
  # Leer el archivo que indica los parámetros de cada rerun
  reruns <- read.csv("reruns.csv", sep=";")
  reruns$id <- 0:5
  
  # Leer el output del CASE2
  setwd("C:/case2/program/")
  for(j in 0:5){
    # Importar archivos
    ruta <- paste('Run_', j,'.txt',sep="")
    run <- read.table(ruta, header = TRUE, sep=',')
    
    # Calcular cosecha mensual
    run$mes <- meses[1:nrow(run)]
    run$Fecha <- paste(run$YEAR, run$mes, sep="-")
    run <- run %>% group_by(Fecha) %>% summarise(granos = sum(YLDBN),
                                                 pods = sum(YLDPD))
    
    # Pegar datos climáticos
    data$Fecha <- paste(data$Ano, data$Mes, sep="-")
    run <- run %>% left_join(data, by="Fecha")
    # Adjuntar identificador del rerun
    run$id <- j
    
    # Pegar parámetros del rerun
    run <- run %>% left_join(reruns, by="id")
    
    # Adjuntar tabla a tabla de los anteriores reruns
    if(j==0){
      runs <- run
    }else{
      runs<-rbind(runs, run)
    }
  }
  
  # Adjuntar identificador de iteración del algoritmo
  runs$it<-i
  runs <- subset(runs, select=-c(Ano, Mes))
  
  # Unir a la tabla que se tenia anteriormente
  if(i==1){
    datos<-runs
  }else{
    datos<-rbind(datos, runs)
  }
  
  # guardar resultados
  setwd(current_working_dir)
  write.csv(datos, "datos_ml_tr.csv")
  
  # Eliminar archivos de reruns
  setwd("C:/case2/program/")
  for(k in 0:5){
    ruta <- paste('Run_', k,'.txt',sep="")
    unlink(ruta)
  }
  
  # Imprimir la finalización de cada iteración del algoritmo
  print(paste("Iteracion: ", i, " terminada."))
  
  # Imprimir tiempo final de ejecución del algoritmo
  if(i == n){
    print(proc.time()-t)
  }
  
}




