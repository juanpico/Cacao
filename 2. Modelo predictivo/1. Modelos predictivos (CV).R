# PREDICCIÓN DE COSECHA (PODS)

library(ggplot2)
library(tidyverse)
library(caret)

# Directorio
current_working_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_working_dir)


# Preprocesamiento de los datos -------------------------------------------

# Leer datos
datos <- read.csv("datos_ml_ajustados_tr.csv")[, -1]

# Ordenar datos
datos <- arrange(datos, it, id, Anio, Mes)
datos <- datos %>%
  group_by(it, id) %>%
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
  mutate(Precip_lag3 = dplyr::lag(Precipitacion, n = 3, default = NA)) %>% 
  mutate(Precip_lag4 = dplyr::lag(Precipitacion, n = 4, default = NA)) %>%
  mutate(Precip_lag5 = dplyr::lag(Precipitacion, n = 5, default = NA)) %>%
  mutate(Precip_lag6 = dplyr::lag(Precipitacion, n = 6, default = NA))
  

# Quitar cosechas negativas
datos <- datos[datos$pods_ajustados>=0,]

# Quitar lags de precipitación
datos <- subset(datos, select=-c(Precip_lag4, Precip_lag5, Precip_lag6))

# Quitar filas con nulos
datos <- na.omit(datos)

# Guardar fechas
Fechas <- datos[,c("Anio", "Mes", "it", "id")]

# Quitar columnas que no se deben utilizar para el modelo predictivo
datos <- subset(datos, select=-c(Anio, Mes, granos, pods, windspeed, id, it, periodo))

# Volver factores las columnas categóricas
datos$SOILTYPE <- as.factor(datos$SOILTYPE)
datos$PRODLEVL <- as.factor(datos$PRODLEVL)

# Resumen
summary(datos)

# Calcular error irreducible
#source("resultado_constante.R")
#error <- as.numeric(tendencia()["mse"])
#mape <- as.numeric(tendencia()["mape"])

# Crear vector con tiempos de ejecución

modelos <- c("Regresión lineal","Reg lineal inter", "Elastic Net", "KNN", "PCAR",
             "PLS", "Random Forest", "Boosted trees", "NN")

tiempos_train <- rep(0, length(modelos))
names(tiempos_train) <- modelos
tiempos_test <- rep(0, length(modelos))
names(tiempos_test) <- modelos

# Crear vector con mse en test de cada modelo
mse_modelos <- matrix(0, nrow=length(modelos), ncol=2, dimnames=list(modelos, c("mse", "mape")))

# Regresión lineal --------------------------------------------------------

# Crear función para poder calcular el MAPE en los folds
f1 <- function(data, lev = NULL, model = NULL) {
  f1_val <- mean(abs(data$pred-data$obs)/data$obs)
  c(mape = f1_val, defaultSummary(data, lev, model))
}

ctrl=trainControl(method="cv", number=10, summaryFunction = f1)
reg_lineal <- train(pods_ajustados~., data=datos, method='lm',
                    trControl=ctrl, metric="RMSE")
# IC RMSE
qt(0.975, 9)*sd(reg_lineal$resample$RMSE)/3


# Regresión lineal + interacciones ----------------------------------------

feats <- colnames(datos[,-9])
# Concatenate strings
f <- paste(feats,collapse=' + ')
f <- paste('pods_ajustados ~',f)
f <- paste(f, '+ PRODLEVL*Precipitacion + PRODLEVL*Precip_lag + PRODLEVL*Dias_con_lluvia')
# Convert to formula
f <- as.formula(f)
f

reg_lineal_inter <- train(f, data=datos, method='lm',
                    trControl=ctrl, metric="RMSE")
# IC RMSE
qt(0.975, 9)*sd(reg_lineal_inter$resample$RMSE)/3

# Elastic net -------------------------------------------------------------
grid=expand.grid(.alpha=seq(0,1,0.1), .lambda=seq(0,20,1))
elastic_net <- train(pods_ajustados~., data=datos, method='glmnet',
                          trControl=ctrl, metric="RMSE",
                     tuneGrid=grid)

mean(elastic_net$resample$RMSE)
elastic_net$bestTune
# IC RMSE
qt(0.975, 9)*sd(elastic_net$resample$RMSE)/3

mean(elastic_net$resample$mape)

# KNN ---------------------------------------------------------------------
grid=expand.grid(.k=seq(1,40,2))
mod_knn <- train(pods_ajustados~., data=datos, method='knn',
                     trControl=ctrl, metric="RMSE",
                     tuneGrid=grid)

mean(mod_knn$resample$RMSE)
# IC RMSE
qt(0.975, 9)*sd(mod_knn$resample$RMSE)/3

mean(mod_knn$resample$mape)


# PCAR --------------------------------------------------------------------

grid=expand.grid(.ncomp=seq(1,(dim(datos)[2]-1)))
mod_pcar <- train(pods_ajustados~., data=datos, method='pcr',
                 trControl=ctrl, metric="RMSE",
                 tuneGrid=grid)

mean(mod_pcar$resample$RMSE)
# IC RMSE
qt(0.975, 9)*sd(mod_pcar$resample$RMSE)/3

mean(mod_pcar$resample$mape)


# PLS ---------------------------------------------------------------------
grid=expand.grid(.ncomp=seq(1,(dim(datos)[2]-1)))
mod_plsr <- train(pods_ajustados~., data=datos, method='pls',
                  trControl=ctrl, metric="RMSE",
                  tuneGrid=grid)

mean(mod_plsr$resample$RMSE)
# IC RMSE
qt(0.975, 9)*sd(mod_plsr$resample$RMSE)/3

mean(mod_plsr$resample$mape)



# Random Forest -----------------------------------------------------------

# RF con cross-validation
grid <- expand.grid(.mtry=c(6))
rf_cv2 <- train(pods_ajustados~., data=datos, method='rf',
               trControl=ctrl, metric="RMSE",
               ntree=200, tuneGrid=grid)

mean(rf_cv2$resample$RMSE)
# IC RMSE
qt(0.975, 9)*sd(rf_cv2$resample$RMSE)/3
mean(rf_cv2$resample$mape)

# Entrenar RF con partición 80/20
library(randomForest)
set.seed(1)

# Dividir en train y test
set.seed(20)
dt = sort(sample(nrow(datos), nrow(datos)*.8))
train<-datos[dt,]
test<-datos[-dt,]
Fechas_test <- Fechas[-dt, ]


# Modelo en train
set.seed(2)
rf_train =randomForest(pods_ajustados~.,train, ntree=200, mtry=6) # Entrenar
pred_rf=predict(rf_train,test) # Predecir

# Calcular error
mse_rf <- mean((pred_rf-test$pods_ajustados)**2)
mape_rf <- mean(abs(pred_rf-test$pods_ajustados)/test$pods_ajustados)

pred_rf <- data.frame(pred_rf)
datos_test <- cbind(Fechas_test, test, pred_rf)

# Gráfica de dispersión pred vs obs mensual
plot <- ggplot(datos_test, aes(x=pods_ajustados, y=pred_rf))+
  geom_point(alpha=0.5)+
  scale_color_brewer(palette='Dark2')+
  labs(title="Comparación de predicciones mensuales", y="Mazorcas RF [kg/mes]", x="Mazorcas SUCROS-Cocoa [kg/mes]")+
  geom_abline(slope=1)
plot

# Agrupar por año
datos_test = datos_test %>% group_by(Anio, it, id) %>% summarise(prediccion = sum(pred_rf), real=sum(pods_ajustados))

# Gráfica de dispersión pred vs obs anual
plot <- ggplot(datos_test, aes(x=real, y=prediccion))+
  geom_point(alpha=0.5)+
  scale_color_brewer(palette='Dark2')+
  labs(title="Comparación de predicciones anuales", y="Mazorcas RF [kg/año]", x="Mazorcas SUCROS-Cocoa [kg/año]")+
  geom_abline(slope=1)
plot
mse_rf_anual <- mean((datos_test$real-datos_test$prediccion)**2)
mape_rf_anual <- mean(abs(datos_test$real-datos_test$prediccion)/datos_test$real)
mean(abs(datos_test$real-datos_test$prediccion))

# Entrenar modelo final de RF con todos los datos -------------------------

set.seed(1)
rf=randomForest(pods_ajustados~.,datos, ntree=200, mtry=6) # Entrenar

# Guardar modelo RF con todos los datos
saveRDS(rf, "random_forest_tr.rds")

# Mirar importancias
imp_rf=importance(rf)
imp_rf
varImpPlot(rf, main="Importancia de las variables en el RF")

# Calcular importancia relativa
imp_rf2 <- imp_rf
imp_rf2[,1] <- imp_rf[,1]/colSums(imp_rf)[1]
imp_rf2[,2] <- imp_rf[,2]/colSums(imp_rf)[2]

# Agrupar importancias por variables similares
imp_rf3 <- c()
imp_rf3["Precipitacion"] <- imp_rf2["Precipitacion", 1] + imp_rf2["Precip_lag", 1]+
  imp_rf2["Precip_lag2", 1]+imp_rf2["Precip_lag3", 1]
imp_rf3["Temperatura máxima"] <- imp_rf2["Temperatura_maxima", 1] + imp_rf2["temp_max_lag", 1]+
imp_rf2["temp_max_lag2", 1]
imp_rf3["Temperatura mínima"] <- imp_rf2["Temperatura_minima", 1] + imp_rf2["temp_min_lag", 1]+
imp_rf2["temp_min_lag2", 1]
imp_rf3["Irradiación"] <- imp_rf2["Irradiacion", 1] +imp_rf2["irr_lag", 1]+
  imp_rf2["irr_lag2", 1]
imp_rf3["Presión de vapor"] <- imp_rf2["Presion_de_vapor", 1] +imp_rf2["vapor_lag", 1]
imp_rf3["Dias con lluvia"] <- imp_rf2["Dias_con_lluvia", 1] +imp_rf2["dias_lag", 1]+
  imp_rf2["dias_lag2", 1]
imp_rf3["Suelo"] <- imp_rf2["SOILTYPE", 1] 
imp_rf3["Producción"] <- imp_rf2["PRODLEVL", 1] 


# Boosting ----------------------------------------------------------------
library(gbm)
set.seed(4)
boost=gbm(pods_ajustados~.,data=train,
             n.trees=1000,interaction.depth=2,shrinkage=0.1)
summary(boost)

t0 <- proc.time()
lambdas <- seq(0.01, 0.3, 0.05)

mse_boost <- 10000000
mseBOOST <- c()
for(i in lambdas){
  boost=gbm(pods_ajustados~.,data=train,
            n.trees=1000,interaction.depth=4,shrinkage=i)
  num <- as.numeric(gbm.perf(boost,oobag.curve=F,overlay=F, plot.it = F))
  t1 <- proc.time()
  pred_boo=predict(boost,test,n.trees=num)
  t2 <- proc.time() - t1
  mse_i=mean((y_test-pred_boo)^2)
  mseBOOST <- c(mseBOOST, mse_i)
  if(mse_i<mse_boost){
    mse_boost=mse_i
    arboles_boost=num
    shrink=i
    tiempos_test["Boosted trees"] <- t2[3]
  }
}
t <- proc.time() - t0
tiempos_train["Boosted trees"] <- t[3]
plot(lambdas,mseBOOST, type='l')
pred_boo=predict(boost,test,n.trees=arboles_boost, shrinkage=shrink)

mse_boost <- mean((pred_boo-test$pods_ajustados)**2)
mape_boost <- mean(abs(pred_boo-test$pods_ajustados)/test$pods_ajustados)

mse_modelos["Boosted trees", "mse"] <- mse_boost
mse_modelos["Boosted trees", "mape"] <- mape_boost

# Caso de estudio ---------------------------------------------------------

clima_villavicencio <- read.csv("../0. Datos IDEAM/Cubarral/datos_clima_cubarral.csv")[-1]

# Preparar datos de clima

# Cambiar nombres de columnas
clima_villavicencio <- clima_villavicencio %>% 
  rename(Temperatura_minima = Temperatura.mínima.diaria_mean,
         Temperatura_maxima = Temperatura.máxima.diaria_mean,
         Presion_de_vapor = presion_vapor_prom,
         Precipitacion = precipitacion,
         Dias_con_lluvia = dias_lluvia)

# Replicar filas para tomar en cuenta tipo de suelo y de producción
clima_villavicencio <-  clima_villavicencio %>%  uncount(2) %>%
  group_by(Fecha) %>%
  mutate(PRODLEVL = row_number())

clima_villavicencio$n <- 1:nrow(clima_villavicencio)

clima_villavicencio <-  clima_villavicencio %>%  uncount(3) %>%
  group_by(n) %>%
  mutate(SOILTYPE = row_number())

clima_villavicencio <-  clima_villavicencio %>%
  group_by(Fecha) %>%
  mutate(id = row_number())

# Separar fecha en año y mes
clima_villavicencio<-clima_villavicencio %>% separate(Fecha, c("Anio", "Mes"))

# Ordenar datos
clima_villavicencio <- arrange(clima_villavicencio, id, Anio, Mes)
clima_villavicencio <- clima_villavicencio %>%
  group_by(id) %>%
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

# Quitar filas con nulos
clima_villavicencio <- na.omit(clima_villavicencio)

# Guardar fechas
Fechas_vill <- clima_villavicencio[,c("Anio", "Mes", "id")]

# Quitar columnas que no se deben utilizar para el modelo predictivo
clima_villavicencio <- subset(clima_villavicencio, select=-c(Anio, Mes, windspeed, id, n))

# Agregar columna de predicción 
clima_villavicencio$pods_ajustados <- 1

# Volver factores las columnas categóricas
clima_villavicencio$SOILTYPE <- as.factor(clima_villavicencio$SOILTYPE)
clima_villavicencio$PRODLEVL <- as.factor(clima_villavicencio$PRODLEVL)

# Predecir
pred_vill <- predict(rf, clima_villavicencio)

# Agrupar por año 
pred_vill <- data.frame(pred_vill)
resultado_vill <- cbind(Fechas_vill, clima_villavicencio, pred_vill)

resultado_vill = resultado_vill %>% group_by(Anio, PRODLEVL, SOILTYPE, id) %>% summarise(prediccion = sum(pred_vill))
resultado_vill <- resultado_vill %>% ungroup()
resultado_vill$id <- as.factor(resultado_vill$id)
resultado_vill <- resultado_vill %>% rename(Rerun = id)

# Cambiar nombres de suelos y tipo de producción
resultado_vill <- resultado_vill %>%
  mutate(SOILTYPE = case_when(SOILTYPE == 1 ~ 'Loamy',
                                     SOILTYPE == 2 ~ 'Sandy',
                                     SOILTYPE == 3 ~ 'Clayey'))

resultado_vill <- resultado_vill %>%
  mutate(PRODLEVL = case_when(PRODLEVL == 1 ~ 'Potencial',
                                     PRODLEVL == 2 ~ 'Limitado'))

resultado_vill$Anio <- as.integer(resultado_vill$Anio)
resultado_vill <- data.frame(resultado_vill)
# Preparar datos con los resultados del CASE2
case_villavicencio <- read.csv("../Resultados CASE2/2015-2019/V4/resultadosV4.csv")[-1]
case_villavicencio <- case_villavicencio[case_villavicencio$Municipio=='Acacias' & case_villavicencio$semilla=="promedio" & case_villavicencio$Fermentación==5,]
case_villavicencio <- subset(case_villavicencio, select=c(YEAR, Suelo, Producción, pods_anual))

case_villavicencio <- case_villavicencio %>% rename(Anio = YEAR,
                                                    PRODLEVL = Producción,
                                                    SOILTYPE = Suelo)

case_villavicencio$SOILTYPE <- as.character(case_villavicencio$SOILTYPE)
case_villavicencio$PRODLEVL <- as.character(case_villavicencio$PRODLEVL)

# Unir predicción con resultados del CASE2
resultado_vill <- resultado_vill %>% left_join(case_villavicencio, by = c("Anio", "PRODLEVL", "SOILTYPE"))

# Métricas villavicencio
mse_rf_vill <- mean((resultado_vill[resultado_vill$Anio>2015,]$pods_anual-resultado_vill[resultado_vill$Anio>2015,]$prediccion)**2, na.rm=TRUE)
mape_rf_vill <- mean(abs(resultado_vill[resultado_vill$Anio>2015,]$pods_anual-resultado_vill[resultado_vill$Anio>2015,]$prediccion)/resultado_vill[resultado_vill$Anio>2015,]$pods_anual, na.rm=TRUE)

resultado_vill <- resultado_vill %>% rename("Mazorcas estimadas" = prediccion, "Mazorcas reales" = pods_anual)
resultado_vill <- resultado_vill %>% pivot_longer(c("Mazorcas estimadas", "Mazorcas reales"), names_to = "clase", values_to = "y")

resultado_vill <- resultado_vill %>% 
  mutate(Rerun = case_when(Rerun == 2 ~ "Potencial",
                           Rerun == 4 ~ "Limoso",
                           Rerun == 5~ "Arenoso",
                           Rerun == 6 ~ "Arcilloso"))
resultado_vill <- resultado_vill %>% rename(Configuración = Rerun)
# Gráficar 
ggplot()+
  geom_line(data=resultado_vill[resultado_vill$Configuración %in% c("Potencial", "Limoso", "Arenoso", "Arcilloso") & resultado_vill$Anio>2015,], aes(x=Anio, y=y, color=Configuración, linetype=clase))+
  scale_color_brewer(palette='Dark2')+
  labs(title="Cosecha anual en Cubarral", y="Mazorcas (kg/año)", x="Año")+
  ylim(6000, 13000)

  
