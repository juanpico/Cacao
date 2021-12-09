# Predicción productividad en fincas
library(tidyverse)

# Importar función para realizar predicción de productividad ------------------
  
  # Definir el directorio del modulo de estimación
  setwd("/Users/juancamilopico/OneDrive - Universidad de los Andes/Uniandes/PG2/Modulo estimación")
  # Cargar el script con la función
  source("../Modulo estimación/predictor3.R")
  
# Definir directorio
  current_working_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
  setwd(current_working_dir)
  
# Preparar variables meteorológicas para predecir
  
  clima_acacias <- read.csv("clima 2020/clima_acacias.csv")[-1]              # Clima acacías
  clima_villavicencio <- read.csv("clima 2020/clima_villavicencio.csv")[-1]  # Clima villavicencio
  clima_cubarral <- read.csv("clima 2020/clima_cubarral.csv")[-1]       # Clima cubarral

  # Riego
  
  #clima_acacias[clima_acacias$Fecha=='2020-01', "Precipitacion"] <- clima_acacias[clima_acacias$Fecha=='2020-01', "Precipitacion"]+37.5
  #clima_acacias[clima_acacias$Fecha=='2020-02', "Precipitacion"] <- clima_acacias[clima_acacias$Fecha=='2020-02', "Precipitacion"]+37.5
  
  # Añadir lags

  clima_acacias <- clima_acacias %>%
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
  
  clima_villavicencio <- clima_villavicencio %>%
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
  
  clima_cubarral <- clima_cubarral %>%
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
  
  # Elegir la el horizonte de tiempo correspondiente (2020)
  
  clima_acacias <- clima_acacias[(clima_acacias$Fecha>='2020-01')&(clima_acacias$Fecha<='2020-12'),] 
  clima_villavicencio <- clima_villavicencio[(clima_villavicencio$Fecha>='2019-01')&(clima_villavicencio$Fecha<='2019-12'),] 
  clima_cubarral <- clima_cubarral[(clima_cubarral$Fecha>='2020-01')&(clima_cubarral$Fecha<='2020-12'),] 
  
  # Remover columna con la fecha
  clima_acacias <- clima_acacias %>% select(-Fecha)
  clima_villavicencio <- clima_villavicencio %>% select(-Fecha)
  clima_cubarral <- clima_cubarral %>% select(-Fecha)
  
# Predecir productividad para las fincas
  
  # Inicializar tabla de productividad
  prods <- data.frame("Nombre"= c("William", "Fabio", "Silvia", "Larry", "Jorge", "Maria T", 'Maria C'),
                      "P_ajustada" =  rep(0, 7), "P_teórica" = rep(0,7), "P_potencial" = rep(0,7), "P_real" = rep(0,7))
  prods <- prods %>% column_to_rownames('Nombre')
  
  # William (Acacías) --------------------------------------------------
  prod_william <- predictor(X = clima_acacias, suelo = 3, variedad='FEAR5',
                            densidad = 360/0.441, hectareas = 0.441, 
                            edad_inicial = 7, rendimiento='Bajo')
  
  prods['William', 'P_ajustada'] <- mean(prod_william[[1]])
  prods['William', 'P_teórica'] <- mean(prod_william[[2]])
  prods['William', 'P_potencial'] <- prod_william[[3]]
  prods['William', 'P_real'] <- prod_william[[4]]
  
  # Fabio Acosta (Acacías) --------------------------------------------------
  
  #plantas por lote
  plantas_fabio <- c(966,198,192,88,
                     5,1,2,2)
  # Hectarea por lote
  hectareas_fabio <- c(1.18335,0.24255,0.2352,0.1078,
                       0.006125,0.001225,0.00245,0.00245)
  # variedad por lote
  variedades_fabio <- c("CCN51",'FEAR5','ICS1','FSA13',
                        'FTA2','Desconocida','Desconocida','FSA12')
  # edad inicial por lote
  edades_fabio <- c(6, 4, 4, 4, 
                    5, 3, 6, 4)
  
  # Calcular la productividad de la finca
  prod_fabio <- predictor(clima_acacias, suelo = rep(3, length(variedades_fabio)), 
                variedad = variedades_fabio,
                edad_inicial = edades_fabio, 
                densidad = plantas_fabio/hectareas_fabio,
                hectareas = hectareas_fabio,
                rendimiento = rep('Alto', length(variedades_fabio)))
  
  prods['Fabio', 'P_ajustada'] <- mean(prod_fabio[[1]])
  prods['Fabio', 'P_teórica'] <- mean(prod_fabio[[2]])
  prods['Fabio', 'P_potencial'] <- sum(prod_fabio[[3]]*hectareas_fabio/sum(hectareas_fabio))
  prods['Fabio', 'P_real'] <- sum(prod_fabio[[4]]*hectareas_fabio/sum(hectareas_fabio))
  
  # Silvia (Acacías) --------------------------------------------------------
  
  # plantas por lote
  plantas_silvia <- c(2200, 800, 1200, 1000)
  # hectaras por lote
  hectareas_silvia <- c(1.98, 0.72, 1.08, 0.9)
  # variedad por lote
  variedades_silvia <- c('FSA12', 'TSH565', 'FSV 41', 'FSV 41')
  # edad inicial por lote
  edades_silvia <- c(18, 8, 5, 3)
  # tipo de suelo por lote
  suelos_silvia <- c(1, 2, 1, 1)
  
  # Calcular la productividad por hectarea
  prod_silvia <- predictor(clima_acacias, suelo = suelos_silvia, 
                           variedad = variedades_silvia,
                           edad_inicial = edades_silvia, 
                           densidad = plantas_silvia/hectareas_silvia,
                           hectareas = hectareas_silvia,
                           rendimiento = rep('Alto', length(variedades_silvia)))
  
  prods['Silvia', 'P_ajustada'] <- mean(prod_silvia[[1]])
  prods['Silvia', 'P_teórica'] <- mean(prod_silvia[[2]])
  prods['Silvia', 'P_potencial'] <- sum(prod_silvia[[3]]*hectareas_silvia/sum(hectareas_silvia))
  prods['Silvia', 'P_real'] <- sum(prod_silvia[[4]]*hectareas_silvia/sum(hectareas_silvia))
  
  
  # Larry (Acacías)  --------------------------------------------------------
  
  # plantas por lote
  plantas_larry <- c(3624, 925, 672, 1158, 1016)
  # hectareas por lote
  hectareas_larry <- c(3.2616, 0.8325, 0.6048, 1.0422, 0.9144)
  # variedad por lote
  variedades_larry <- c('FEAR5', 'FEAR5', 'FTA2', 'TSH565', 'FEAR5')
  # edad inicial por lote
  edades_larry <- c(4, 5, 5, 5, 5)
  
  # Calcular la productividad por hectarea
  prod_larry <- predictor(clima_acacias, suelo = rep(3, length(variedades_larry)), 
                          variedad = variedades_larry,
                          edad_inicial = edades_larry, 
                          densidad = plantas_larry/hectareas_larry,
                          hectareas = hectareas_larry,
                          rendimiento = rep('Bajo', length(variedades_larry)))
  prods['Larry', 'P_ajustada'] <- mean(prod_larry[[1]])
  prods['Larry', 'P_teórica'] <- mean(prod_larry[[2]])
  prods['Larry', 'P_potencial'] <- sum(prod_larry[[3]]*hectareas_larry/sum(hectareas_larry))
  prods['Larry', 'P_real'] <- sum(prod_larry[[4]]*hectareas_larry/sum(hectareas_larry))
  
  # Jorge Martinez (Acacías)  -------------------------------------------------
  
  # plantas por lote
  plantas_jorge <- c(1100, 1100, 1100, 1100)
  # hectareas por lote
  hectareas_jorge <- c(0.99, 0.99, 0.99, 0.99)
  # variedad por lote
  variedades_jorge <- c('FSA12', 'CCN51', 'FEAR5', 'FTA2')
  # edad inicial por lote
  edades_jorge <- c(9, 9, 7, 4)
  # tipo de suelo por lote
  suelos_jorge <- c(2, 2, 2, 2)
  
  # Calcular la productividad por hectarea
  prod_jorge <- predictor(clima_acacias, suelo = suelos_jorge, 
                           variedad = variedades_jorge,
                           edad_inicial = edades_jorge, 
                           densidad = plantas_jorge/hectareas_jorge,
                          hectareas = hectareas_jorge,
                          rendimiento = rep('Bajo', length(variedades_jorge)))
  prods['Jorge', 'P_ajustada'] <- mean(prod_jorge[[1]])
  prods['Jorge', 'P_teórica'] <- mean(prod_jorge[[2]])
  prods['Jorge', 'P_potencial'] <- sum(prod_jorge[[3]]*hectareas_jorge/sum(hectareas_jorge))
  prods['Jorge', 'P_real'] <- sum(prod_jorge[[4]]*hectareas_jorge/sum(hectareas_jorge))
  
  # Maria Trinidad (Acacías)  --------------------------------------------------------
  
  # plantas por lote
  plantas_mariat <- c(243, 152, 20, 12, 28)
  # hectareas por lote
  hectareas_mariat <- c(0.297675, 0.1862, 0.0245, 0.0147, 0.0343)
  # variedad por lote
  variedades_mariat <- c('FTA2', 'FEAR5', 'CCN51', 'TSH565', 'FSA11')
  
  # Calcular la productividad por hectarea
  prod_mariat <- predictor(clima_acacias, suelo = rep(3, length(variedades_mariat)), 
                           variedad = variedades_mariat,
                           edad_inicial = rep(7, length(variedades_mariat)), 
                           densidad = plantas_mariat/hectareas_mariat,
                           hectareas = hectareas_mariat,
                           rendimiento = rep('Bajo', length(variedades_mariat)))
  
  prods['Maria T', 'P_ajustada'] <- mean(prod_mariat[[1]])
  prods['Maria T', 'P_teórica'] <- mean(prod_mariat[[2]])
  prods['Maria T', 'P_potencial'] <- sum(prod_mariat[[3]]*hectareas_mariat/sum(hectareas_mariat))
  prods['Maria T', 'P_real'] <- sum(prod_mariat[[4]]*hectareas_mariat/sum(hectareas_mariat))
  
  # Maria del Carmen Umaña (Cubarral)  --------------------------------------------------------
  
  # plantas por lote
  plantas_mc <- c(140, 500, 220, 36, 63)
  # hectareas por lote
  hectareas_mc <- c(0.126,0.45,0.198, 0.0324, 0.0567)
  # variedad por lote
  variedades_mc <- c('TSH565', 'ICS95', 'CCN51', 'ICS1', 'ICS1')
  # edad inicial por lote
  edades_mc <- c(8, 8, 8, 8, 8)  
  # Calcular la productividad por hectarea
  prod_mc <- predictor(clima_cubarral, suelo = rep(2, length(variedades_mc)), 
                       variedad = variedades_mc,
                       edad_inicial = edades_mc, 
                       densidad = plantas_mc/hectareas_mc,
                       hectareas = hectareas_mc,
                       rendimiento = rep('Bajo', length(variedades_mc)))
  
  prods['Maria C', 'P_ajustada'] <- mean(prod_mc[[1]])
  prods['Maria C', 'P_teórica'] <- mean(prod_mc[[2]])
  prods['Maria C', 'P_potencial'] <- sum(prod_mc[[3]]*hectareas_mc/sum(hectareas_mc))
  prods['Maria C', 'P_real'] <- sum(prod_mc[[4]]*hectareas_mc/sum(hectareas_mc))

  

# Gráficas ----------------------------------------------------------------

boxplots <- data.frame("Teórica" = prod_william[[2]])
boxplots['Persona'] <- "Cultivo 7"
boxplots['Estimada'] <- prod_william[[1]]

boxplots_2 <- data.frame("Estimada" = prod_fabio[[1]])
boxplots_2['Persona'] <- "Cultivo 1"
boxplots_2['Teórica'] <- prod_fabio[[2]]

boxplots_3 <- data.frame("Estimada" = prod_silvia[[1]])
boxplots_3['Persona'] <- "Cultivo 2"
boxplots_3['Teórica'] <- prod_silvia[[2]]

#boxplots_3.5 <- data.frame("semillas" = prod_silvia[[1]])
#boxplots_3.5['Persona'] <- "Silvia y José"
#boxplots_3.5['tipo_prod'] <- "Con riego"


boxplots_4 <- data.frame("Estimada" = prod_larry[[1]])
boxplots_4['Persona'] <- "Cultivo 3"
boxplots_4['Teórica'] <- prod_larry[[2]]

boxplots_5 <- data.frame("Estimada" = prod_jorge[[1]])
boxplots_5['Persona'] <- "Cultivo 5"
boxplots_5['Teórica'] <- prod_jorge[[2]]

boxplots_6 <- data.frame("Estimada" = prod_mariat[[1]])
boxplots_6['Persona'] <- "Cultivo 4"
boxplots_6['Teórica'] <- prod_mariat[[2]]

boxplots_7 <- data.frame("Estimada" = prod_mc[[1]])
boxplots_7['Persona'] <- "Cultivo 6"
boxplots_7['Teórica'] <- prod_mc[[2]]

boxplots <- rbind(boxplots, boxplots_2, boxplots_3, boxplots_4, boxplots_5,
                  boxplots_6, boxplots_7) 


boxplots <- boxplots %>% pivot_longer(c(Teórica, Estimada),names_to = "tipo_prod", values_to = "semillas")


reportados <- data.frame(Persona = unique(boxplots$Persona))
reportados$semillas <- c(682,1640,	1500,	1000,	800,	857,	756)
reportados$tipo_prod <- "Reportada"

sel_order <- 
  reportados %>% 
  filter(tipo_prod == 'Reportada') %>% 
  arrange(desc(semillas)) %>% 
  mutate(Persona = factor(Persona))

boxplots <- rbind(reportados, boxplots)

boxplots$Persona <- factor(boxplots$Persona, levels = sel_order$Persona, ordered = TRUE)
boxplots$tipo_prod <- factor(boxplots$tipo_prod, levels = c("Teórica", "Estimada", "Reportada"))

colors <- c("Teórica" = "#a6cee3", "Estimada" = "#1f78b4", "Reportada" = '#b2de8a')  
ggplot(boxplots[boxplots$tipo_prod!="Reportada",], aes(x=Persona, y=semillas))+
  geom_boxplot(aes(color=tipo_prod),alpha=0.3, 
               position=position_dodge(width=0),
               outlier.colour = NA)+
  geom_point(data=boxplots[boxplots$tipo_prod=="Reportada",],
             aes(x=Persona, y=semillas, color="Reportada"))+
  scale_color_manual(values=colors)+
  scale_y_continuous(labels = function(x) format(x, big.mark = "."))+
  labs(title="Simulación de productividad ajustada por limitaciones de los cultivos", y="Semillas [kg/hect]", x=NULL,
       colour="Producción")+
  scale_x_discrete(guide = guide_axis(n.dodge=2))


