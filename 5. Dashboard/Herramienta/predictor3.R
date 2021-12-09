# Función para predecir productividad

library(randomForest)

# Cargar funciones para la distribución triangular truncada
source("Truncated-Triangular-Simulation/Scripts/Generate-Triangular.R")
source("Truncated-Triangular-Simulation/Scripts/Generate-Truncated-Triangular.R")

# Cargar el modelo 
model <- readRDS("random_forest.rds")

# Cargar parámetros de las variedades
params_IM <- read.csv('params_IM.csv', sep=';')
params_MPA <- read.csv('params_MPA.csv', sep=';')

# Cargar coeficientes para estimar ajuste por densidad de plantación
coef_densidad <- read.csv('coeficientes densidad.csv', sep=';')

# Función para realizar la predicción de la productividad
# Parámetrps:
# - X: Dataframe con las siguientes columnas:
#      -Temperatura_minima [°C]
#      -Temperatura_maxima [°C]
#      -Irradiacion [kJ/m2/dia]
#      -Presion_de_vapor [hPa]
#      -Precipitacion [mm/mes]
#      -Dias_con_lluvia
#      -SOILTYPE (int: 1 - Limoso, 2- Sandy, 3 - Arcilloso)
#      -PRODLEVL (int: 1 - Potencial, 2 - Limitado)
#      -temp_max_lag: Temperatura máxima el mes anterior [°C]
#      -temp_min_lag: Temperatura mínima el mes anterior [°C]
#      -irr_lag: Irradiación el mes anterior [kJ/m2/dia]
#      -irr_lag2: Irradiación dos meses atrás [kJ/m2/dia]
#      -vapor_lag: Presión de vapor un mes atrás [hPa]
#      -dias_lag: Dias con lluvia el mes anterior
#      -dias_lag2: Dias con lluvia dos meses atrás
#      -temp_max_lag2: Temperatura máxima dos meses atrás [°C]
#      -temp_min_lag2: Temperatura mínima dos meses atrás [°C]
#      -Precip_lag: Precipitación el mes anterior [mm/mes]
#      -Precip_lag2: Precipitación dos meses atrás [mm/mes]
#      -Precip_lag3: Precipitación tres meses atrás [mm/mes]
# - variedad: character con el nombre de la variedad
# - densidad: numeric con la densidad del cultivo [arboles/hect]
# - arboles: numeric con la cantidad de arboles
# - edad_inicial: int con edad inicial del arbol
predictor <- function(X, suelo, variedad, edad_inicial, densidad, hectareas, rendimiento){

  # Verificar que los parámetros tengan la misma longitud (1 por lote)
  longitud = FALSE
  if(length(suelo)==length(variedad)){
    if(length(variedad)==length(edad_inicial)){
      if(length(edad_inicial)==length(densidad)){
        if(length(densidad)==length(rendimiento)){
            longitud = TRUE
        }
      }
    }
  }
  
  if(longitud == FALSE){
    print('La longitud de los parámetros no es consistente. Debe haber un valor de cada parámetro especificado para cada lote.')
    return(NULL)
  }

  # Realizar estimación de productividad de mazorcas para cada lote
  pods_lotes_reales <- c()
  pods_lotes_potenciales <- c()
  for(i in 1:length(suelo)){
    
    # Crear X para la producción potencial y para la producción real
    X_real = X
    X_potencial = X
    
    # Configurar las variables categóricas como factores de forma que sean 
    # consistentes con los datos de entrenamientO
    
    X_real$SOILTYPE <- factor(suelo[i], levels=c("1", "2", "3"))
    X_real$PRODLEVL <- factor(2, levels=c("1", "2"))
    
    X_potencial$SOILTYPE <- factor(suelo[i], levels=c("1", "2", "3"))
    X_potencial$PRODLEVL <- factor(1, levels=c("1", "2"))
    
    # Realizar predicción potencial de mazorcas
    pred_pods_pot <- predict(model, X_potencial)
    
    # Realizar predicción real de mazorcas
    pred_pods_real <- predict(model, X_real)
    
    # Ajustar para la densidad de plantación
    
    ## Ajuste de densidad para la producción potencial
    factor_pods_pot <- coef_densidad[4, "Intercepto"] + (densidad[i]/1000)*coef_densidad[4, "Pendiente"]
    pred_pods_pot <- pred_pods_pot*factor_pods_pot
    
    ## Ajuste de densidad para la producción real
    factor_pods_real <- coef_densidad[suelo[i], "Intercepto"] + (densidad[i]/1000)*coef_densidad[suelo[i], "Pendiente"]
    pred_pods_real <- pred_pods_real*factor_pods_real 
    
    # Ajustes para la predicción real
    
    ## Realizar ajuste de productividad por edad
    prod_edad <- c(0, 0.06, 0.2, 0.85, 0.89, 0.92, 0.96, 1, 1, 1, 1, 1,
                   0.96, 0.91, 0.87, 0.83, 0.79, 0.74, 0.7, 0.65)
    
    ## Revisar el año de cada mes para determinar la edad del arbol y su ajuste
    ## correspondiente
    for(p in 1:length(pred_pods_real)){
      pred_pods_real[p] <- pred_pods_real[p]*prod_edad[edad_inicial[i]+floor((p-1)/12)]
    }
    
    # Calcular kg de mazorcas reales totales
    pred_pods_real <- sum(pred_pods_real)
    
    # Agregar kg mazorcas reales del lote al vector con las mazorcas reales de cada lote
    pods_lotes_reales <- c(pods_lotes_reales, pred_pods_real)
    
    # Calcular kg de mazorcas potenciales totales
    pred_pods_pot <- sum(pred_pods_pot)
   
    # Agregar kg mazorcas del lote al vector con las mazorcas de cada lote
    pods_lotes_potenciales <- c(pods_lotes_potenciales, pred_pods_pot)
    
  }

  # Calcular gap según el CASE2
  gap <- pods_lotes_reales/pods_lotes_potenciales

  # Simulación de Monte Carlo para calcular kg de semilla a partir de número de
  # pods mediante la variable aleatoria IM (indice de mazorca)
  
  # Definir semilla para que el resultado sea replicable
  set.seed(10)
  
  # Crear vector que contiene la productividad de la finca en cada escenario
  simu_final <- c()
  simu_teorica <- c()
  # Crear 3000 escenarios
  n = 3000
  for(i in 1:n){
    
    # Calcular semillas para cada lote
    lotes_teorica <- c()
    for(j in 1:length(suelo)){
      
      # Definir la distribución triangular para el IM del lote j
      l = params_IM[params_IM$Variedad==variedad[j], 'Min']
      u = params_IM[params_IM$Variedad==variedad[j], 'Max']
      m = params_IM[params_IM$Variedad==variedad[j], 'Moda']
      my.tri.dist_IM <- generate.triangular(L = l, U = u, M = m)
      
      # Definir la distribución triangular truncada del IM según el rendimiento del lote
      if(rendimiento[j]=='Bajo'){
        # Para lotes con rendimiento bajo, se llevan la parte superior de la distribución triangular
        my.trun.tri.dist_IM <-
          generate.truncated.triangular(a = params_IM[params_IM$Variedad==variedad[j], 'Límite'],
                                        b = params_IM[params_IM$Variedad==variedad[j], 'Max'],
                                        orig.tri.dist = my.tri.dist_IM)
      }else if(rendimiento[j]=='Alto'){
        # Para lotes con rendimiento alto, se llevan la parte inferior de la distribución triangular
        my.trun.tri.dist_IM <-
          generate.truncated.triangular(a = params_IM[params_IM$Variedad==variedad[j], 'Min'],
                                        b = params_IM[params_IM$Variedad==variedad[j], 'Límite'],
                                        orig.tri.dist = my.tri.dist_IM)
      }
      
      # Generar número aleatorio entre 0 y 1
      r <- runif(1)
      
      # Generar IM aleatorio
      IM <- my.trun.tri.dist_IM$inverse.cdf(r)
      
      # Definir la distribución triangular para el MPA del lote j
      l = params_MPA[params_MPA$Variedad==variedad[j], 'Min']
      u = params_MPA[params_MPA$Variedad==variedad[j], 'Max']
      m = params_MPA[params_MPA$Variedad==variedad[j], 'Moda']
      my.tri.dist_MPA <- generate.triangular(L = l, U = u, M = m)
      
      # Definir la distribución triangular truncada del MPA según el rendimiento del lote
      if(rendimiento[j]=='Bajo'){
        # Para lotes con rendimiento bajo, se llevan la parte superior de la distribución triangular
        my.trun.tri.dist_MPA <-
          generate.truncated.triangular(a = params_MPA[params_MPA$Variedad==variedad[j], 'Min'],
                                        b = params_MPA[params_MPA$Variedad==variedad[j], 'Límite'],
                                        orig.tri.dist = my.tri.dist_MPA)
      }else if(rendimiento[j]=='Alto'){
        # Para lotes con rendimiento alto, se llevan la parte inferior de la distribución triangular
        my.trun.tri.dist_MPA <-
          generate.truncated.triangular(a = params_MPA[params_MPA$Variedad==variedad[j], 'Límite'],
                                        b = params_MPA[params_MPA$Variedad==variedad[j], 'Max'],
                                        orig.tri.dist = my.tri.dist_MPA)
      }
      
      # Generar número aleatorio entre 0 y 1
      r <- runif(1)
      
      # Generar MPA aleatorio
      MPA <- my.trun.tri.dist_MPA$inverse.cdf(r)
      
      # Calcular productividad teórica para el lote j
      prod_teorica <- MPA/ IM         # semillas secas por arbol
      prod_teorica <- prod_teorica*densidad[j]  # semillas secas por hectarea
      
      # Agregar al vector que contiene las semillas de cada lote
      lotes_teorica <- c(lotes_teorica, prod_teorica)
    }
    
    # Realizar ajuste del CASE2 con el gap de cada lote
    prod_final <- lotes_teorica*gap
    
    # Calcular productividad teórica total para la finca en el escentario i
    
    lotes_teorica <- sum(lotes_teorica*(hectareas/sum(hectareas)))
    
    # Calcular productividad final total para la finca en el escenario i
    prod_final <- sum(prod_final*(hectareas/sum(hectareas)))
    
    # Agregar producción teórica al vector que contiene las semillas totales de cada escenario
    simu_teorica <- c(simu_teorica, lotes_teorica)
    
    # Agregar producción final al vector que contiene las semillas totales de cada escenario
    simu_final <- c(simu_final, prod_final)
  }
  
  # Retornar:
  # 1. Simulación de la producción real (teórica con simulación + ajuste CASE2)
  # 2. Gap para cada lote
  # 3. Pods potenciales
  return(list(simu_final, simu_teorica, pods_lotes_potenciales, pods_lotes_reales, gap))
}



