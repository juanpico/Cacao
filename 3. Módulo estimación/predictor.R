# Función para predecir productividad

# Directorio
#current_working_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
#setwd(current_working_dir)

library(randomForest)

# Cargar el modelo 
model <- readRDS("../2. Modelo predictivo/random_forest_tr.rds")

# Cargar parámetros de las variedades
params <- read.csv('params_variedades.csv', sep=';')

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
predictor <- function(X, suelo, variedad, edad_inicial, densidad, prod_levl = 2){
  
  # Crear X para la producción potencial y para la producción real
  X_real = X
  X_potencial = X
  
  # Configurar las variables categóricas como factores de forma que sean 
  # consistentes con los datos de entrenamientO
  
  X_real$SOILTYPE <- factor(suelo, levels=c("1", "2", "3"))
  X_real$PRODLEVL <- factor(2, levels=c("1", "2"))
  
  X_potencial$SOILTYPE <- factor(suelo, levels=c("1", "2", "3"))
  X_potencial$PRODLEVL <- factor(1, levels=c("1", "2"))

  # Realizar predicción potencial de mazorcas
  pred_pods_pot <- predict(model, X_potencial)
  
  # Realizar predicción real de mazorcas
  pred_pods_real <- predict(model, X_real)

  # Ajustar para la densidad de plantación
  
  ## Ajuste de densidad para la producción potencial
  factor_pods_pot <- coef_densidad[4, "Intercepto"] + (densidad/1000)*coef_densidad[4, "Pendiente"]
  pred_pods_pot <- pred_pods_pot*factor_pods_pot
  
  ## Ajuste de densidad para la producción real
  factor_pods_real <- coef_densidad[suelo, "Intercepto"] + (densidad/1000)*coef_densidad[suelo, "Pendiente"]
  pred_pods_real <- pred_pods_real*factor_pods_real 
  
  # Realizar ajuste de fermentación
  pred_beans_real <- pred_pods_real*params[params$Variedad==variedad,'f_beans']*params[params$Variedad==variedad,'Factor_fermentación']
  pred_beans_pot <- pred_pods_pot*params[params$Variedad==variedad,'f_beans']*params[params$Variedad==variedad,'Factor_fermentación']
  
  # Realizar ajuste de productividad por edad
  prod_edad <- c(0, 0.06, 0.2, 0.85, 0.89, 0.92, 0.96, 1, 1, 1, 1, 1,
                 0.96, 0.91, 0.87, 0.83, 0.79, 0.74, 0.7, 0.65)
  
  # Revisar el año de cada mes para determinar la edad del arbol y su ajuste
  # correspondiente
  for(p in 1:length(pred_beans_real)){
    pred_beans_real[p] <- pred_beans_real[p]*prod_edad[edad_inicial+floor((p-1)/12)]
  }

  return(list(pred_beans_real, pred_pods_real/(params[params$Variedad==variedad, 'Peso']/1000), pred_beans_pot, pred_pods_pot/(params[params$Variedad==variedad, 'Peso']/1000)))  # Retornar predicción
}
