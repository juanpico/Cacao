# Limpieza de datos
library(dplyr)
library(tidyr)

# Leer datos de temparatura minima diaria y temperatura máxima diaria
temperatura <- read.csv("datos clima/temp_min_max.csv")

# Seleccionar variables relevantes
temperatura <- select(temperatura, c('CodigoEstacion', 'NombreEstacion','Latitud', 'Longitud',
                                     'Altitud', 'DescripcionSerie', 'Fecha',
                                     'Valor'))

# Establecer una columna para el minimo y otra para el máximo
temperatura <- spread(temperatura, DescripcionSerie, Valor)

# Convertir la columna Fecha en objeto tipo fecha
temperatura$Fecha <- as.Date(temperatura$Fecha, format='%Y-%m-%d')

# Revisar nulos
summary(temperatura)
temperatura[is.na(temperatura$`Temperatura mínima diaria`),]

# 9 nulos para temperatura máxima diaria. 4 nulos son en los 4 dias disponibles
# para el 2021.
# 5 nulos para temperatura mínima diaria.
# Se decide tomar los datos hasta el 31 de diciembre del 2020

temperatura <- temperatura[temperatura$Fecha<"2021-01-01",]

# Llenar temperatura máxima nula con el promedio del valor anterior y el valor
# siguiente.

for(i in 1:nrow(temperatura)){
  # llenar nulos de la temperatura máxima diaria
  if(is.na(temperatura$`Temperatura máxima diaria`[i])){
    temperatura$`Temperatura máxima diaria`[i] <- mean(c(temperatura$`Temperatura máxima diaria`[i-1], temperatura$`Temperatura máxima diaria`[i+1] ))
  }
  # llenar nulos de la temperatura mínima diaria
  if(is.na(temperatura$`Temperatura mínima diaria`[i])){
    temperatura$`Temperatura mínima diaria`[i] <- mean(c(temperatura$`Temperatura mínima diaria`[i-1], temperatura$`Temperatura mínima diaria`[i+1] ))
  }
}

# Cambiar formato de fecha a año-mes
temperatura$Fecha <- format(temperatura$Fecha, '%Y-%m')

# Promediar por mes
temperatura <- temperatura %>%
  group_by(Fecha) %>%
  summarise_at(vars(c(CodigoEstacion, Latitud, Longitud, Altitud, 
                      `Temperatura máxima diaria`, `Temperatura mínima diaria`)), 
               list(mean=mean))


# Leer datos de precipitacion
precipitacion <- read.csv("datos clima/precipitacion.csv")


# Seleccionar variables relevantes
precipitacion <- select(precipitacion, c('Municipio', 'DescripcionSerie', 'Fecha',
                                     'Valor'))


# Convertir la columna Fecha en objeto tipo fecha
precipitacion$Fecha <- as.Date(precipitacion$Fecha, format='%Y-%m-%d')

# Revisar por nulos
summary(precipitacion) # No hay datos nulos

# Determinar dias con lluvia
precipitacion$Lluvia <- precipitacion$Valor>=0.1

# Convertir fecha a tipo año-mes
precipitacion$Fecha <- format(precipitacion$Fecha, '%Y-%m')

# Sumar precipitación mensual y contar dias con lluvia
precipitacion <- precipitacion %>%
  group_by(Fecha, Municipio) %>%
  summarise_at(vars(c(Valor, Lluvia)), 
               list(sum=sum))

# Renombrar columnas
precipitacion <- precipitacion %>% rename(precipitacion=Valor_sum, dias_lluvia=Lluvia_sum)

# Leer datos de temperatura promedio
rangos <- c('00_04', '05_09', '10_14', '15_19', '20_21')
temp_prom <- data.frame()
for(i in rangos){
  file <- read.csv(paste("datos clima/temp_", i, ".csv",sep=""))
  temp_prom <- data.frame(rbind(temp_prom, file))
}

# Seleccionar variables relevantes
temp_prom <- select(temp_prom, c('DescripcionSerie', 'Fecha',
                                         'Valor'))

# Arreglar columna de fecha
temp_prom <- separate(data = temp_prom, col = Fecha, into = c("Fecha", "Hora"), sep = " ")

# Revisar nulos
summary(temp_prom) # no hay nulos

# Convertir la columna Fecha en objeto tipo fecha
temp_prom$Fecha <- as.Date(temp_prom$Fecha, format='%Y-%m-%d')

temp_prom2 <- temp_prom %>% count(Fecha)
temp_prom3 <- temp_prom2 %>% count(n) %>% rename(Cantidad_de_horas=n, Frecuencia=nn)


# Filtrar para tener 3 horas al dia
temp_prom <- subset(temp_prom, temp_prom$Hora %in% c('07:00', '13:00', '18:00', '19:00'))

temp_prom2 <- temp_prom %>% count(Fecha)
temp_prom3 <- temp_prom2 %>% count(n) %>% rename(Cantidad_de_horas=n, Frecuencia=nn)

fechas <- temp_prom2[temp_prom2$n==4,'Fecha']

temp_prom <- subset(temp_prom, (temp_prom$Hora!='19:00')|!(temp_prom$Fecha %in% fechas))

# Quitar los dias en los que se tienen menos de 3 horas al dia
fechas <- temp_prom2[temp_prom2$n<3,'Fecha']

temp_prom <- subset(temp_prom, !(temp_prom$Fecha %in% fechas))

# Promediar la temperatura diaria
temp_prom <- temp_prom %>%
  group_by(Fecha) %>%
  summarise_at(vars(c(Valor)), 
               list(temp_promedio=mean))

# Calcular presión de vapor
vapor <- temp_prom
vapor$presion_vapor <- 6.11 * (10**((7.5*vapor$temp_promedio)/(237.3+vapor$temp_promedio)))

# Pasar fecha a año-mes
vapor$Fecha <- format(vapor$Fecha, '%Y-%m')

# Promediar la presión de vapor diaria
vapor <- vapor %>%
  group_by(Fecha) %>%
  summarise_at(vars(c(presion_vapor)), 
               list(presion_vapor_prom=mean))

# Irradiación
rad <- read.csv('datos clima/irradiacion_prom.csv', sep = ';', dec=',')
rad$Mes <- 1:12

Fecha <- format(seq(as.Date("2000-01-01"), by = "months", length.out = 252),'%Y-%m')
radiacion <- data.frame(Fecha)
radiacion$Mes <- 1:12

radiacion <- radiacion %>% left_join(rad, by='Mes')

radiacion <- radiacion %>% select(c(Fecha, Irradiación_2)) %>% rename(Irradiacion=Irradiación_2)

# Unir tablas
windspeed <- data.frame(Fecha=c('1990-01-01'), windspeed=c(0))

temperatura <- temperatura %>% select(Fecha, `Temperatura mínima diaria_mean`, `Temperatura máxima diaria_mean`)
data <- radiacion %>% left_join(temperatura, by='Fecha') %>% 
  left_join(vapor, by='Fecha') %>% left_join(windspeed, by='Fecha') 


summary(data)

# Exportar datos como csv

write.csv(data, 'datos clima/datos_clima.csv')
write.csv(precipitacion, 'datos clima/datos_precip.csv')



