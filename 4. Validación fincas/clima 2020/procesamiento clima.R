# Procesamiento del clima para el 2020

# Directorio
current_working_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_working_dir)

# Cargar datos de temperatura
  temperatura <- read.csv("datos_clima.csv")[-1]
  temperatura <- temperatura[-ncol(temperatura)]    # Quitar columna windspeed
  
# Cargar datos de precipitacion
  precipitacion <- read.csv("datos_precip.csv")[-1]

# Cambiar nombre de columnas
  temperatura <- temperatura %>% rename(c('Temperatura_minima'=Temperatura.mínima.diaria_mean,
                                          'Temperatura_maxima'=Temperatura.máxima.diaria_mean,
                                          'Presion_de_vapor'=presion_vapor_prom))
  precipitacion <- precipitacion %>% rename(c('Precipitacion'=precipitacion,
                                              Dias_con_lluvia=dias_lluvia))

# Crear tablas de clima para los 3 municipios

  # Acacias
  clima_acacias <- temperatura %>% left_join(precipitacion[precipitacion$Municipio=='Acacías',], by='Fecha')
  clima_acacias <- clima_acacias %>% select(-Municipio)
  summary(clima_acacias)
  
  # Villavicencio
  clima_villavicencio <- temperatura %>% left_join(precipitacion[precipitacion$Municipio=='Villavicencio',], by='Fecha')
  clima_villavicencio <- clima_villavicencio %>% select(-Municipio)
  summary(clima_villavicencio)
  
  # Cubarral
  clima_cubarral <- temperatura %>% left_join(precipitacion[precipitacion$Municipio=='Cubarral',], by='Fecha')
  clima_cubarral <- clima_cubarral %>% select(-Municipio)
  summary(clima_cubarral)
  
# Exportar tablas de clima
  
  write.csv(clima_acacias, "clima_acacias.csv")              # clima acacias
  write.csv(clima_cubarral, 'clima_cubarral.csv')            # clima cubarral
  write.csv(clima_villavicencio, 'clima_villavicencio.csv')  # clima villavicencio

  
    