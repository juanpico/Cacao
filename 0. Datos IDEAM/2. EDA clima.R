library(tidyverse)
library(ggplot2)
# Análisis descriptivo de las variables meteorológicas

# Datos precipitación
  precipitacion <- read.csv('datos clima/datos_precip.csv')[,-1]
  
# Datos temperatura e irradiación
  temperatura <- read.csv('datos clima/datos_clima.csv')[,-1]

# Fenómeno del niño y la niña
  oni <- read.csv('datos clima/ONI.csv', sep=';')
  oni <- pivot_longer(oni, c(Enero, Febrero, Marzo, Abril, Mayo, Junio, Julio, Agosto, Septiembre, Octubre, Noviembre, Diciembre),
                      names_to = "Mes") %>% rename(Fenómeno=value)
  oni$Fecha <- seq(as.Date("2000-01-01"), by = "months", length.out = nrow(oni))
  
  temperatura <- cbind(temperatura, data.frame(oni$Fenómeno)) %>% rename(Fenómeno=oni.Fenómeno)
  precipitacion <- precipitacion %>% left_join(oni[,c('Fecha', 'Fenómeno')], by='Fecha')
  
# cambiar nombre de columans
  temperatura <- temperatura %>% rename(c('Temperatura_mínima'=Temperatura.mínima.diaria_mean,
                                    'Temperatura_máxima'=Temperatura.máxima.diaria_mean))
  temperatura$Fecha <- as.Date(paste(temperatura$Fecha,'-01', sep=""))
  precipitacion$Fecha <- as.Date(paste(precipitacion$Fecha,'-01', sep=""))
  
### Temperatura mínima
  
  # Lineplot 
  plot <- ggplot(data=temperatura, aes(x=Fecha, y=Temperatura_mínima))+
      geom_line()+
    labs(title='Temperatura mínima mensual', x='Fecha', y='Temperatura (°C)')+
    scale_color_brewer(palette='Dark2')+
    scale_x_date(date_breaks="years", date_labels="%Y", date_minor_breaks = "1 month",
                 limits = as.Date(c('2009-01-01','2020-12-31')))+
    theme(axis.text.x=element_text(angle=60, hjust=1)) 
  plot
  ggsave("Plots/temp_min_line_corta.png", plot, width=16, height=9, units="cm", dpi=200)
  
  # Lineplot niño y niña
  plot <- ggplot(data=temperatura, aes(x=Fecha, y=Temperatura_mínima, color=Fenómeno))+
    geom_path(aes(group=1))+
    labs(title='Temperatura mínima mensual', x='Fecha', y='Temperatura (°C)')+
    scale_color_brewer(palette='Dark2')+
    scale_x_date(date_breaks="years", date_labels="%Y", date_minor_breaks = "1 month")+
    theme(axis.text.x=element_text(angle=60, hjust=1)) 
  plot
  ggsave("Plots/temp_min_line_niños.png", plot, width=30, height=9, units="cm", dpi=200)

  # Histograma
  plot2 <- ggplot(data=temperatura, aes(x=Temperatura_mínima))+
    geom_histogram(alpha=0.5, fill="#1B9E77")+
    labs(title='Temperatura mínima mensual', x='Temperatura (°C)')+
    scale_color_brewer(palette='Dark2')+
    geom_vline(aes(xintercept=mean(Temperatura_mínima, na.rm=TRUE)), color="#7570B3",
               linetype='dashed', size=1)
  plot2
  ggsave("Plots/temp_min_hist.png", plot2, width=30, height=9, units="cm", dpi=200)
  
### Temperatura máxima
  
  # Lineplot 
  plot <- ggplot(data=temperatura, aes(x=Fecha, y=Temperatura_máxima))+
    geom_line()+
    labs(title='Temperatura máxima mensual', x='Fecha', y='Temperatura (°C)')+
    scale_color_brewer(palette='Dark2')+
    scale_x_date(date_breaks="years", date_labels="%Y", date_minor_breaks = "1 month")+
    theme(axis.text.x=element_text(angle=60, hjust=1)) 
  plot
  ggsave("Plots/temp_max_line.png", plot, width=30, height=9, units="cm", dpi=200)
  
  # Lineplot niño y niña
  plot <- ggplot(data=temperatura, aes(x=Fecha, y=Temperatura_máxima, color=Fenómeno))+
    geom_path(aes(group=1))+
    labs(title='Temperatura máxima mensual', x='Fecha', y='Temperatura (°C)')+
    scale_color_brewer(palette='Dark2')+
    scale_x_date(date_breaks="years", date_labels="%Y", date_minor_breaks = "1 month")+
    theme(axis.text.x=element_text(angle=60, hjust=1)) 
  plot
  ggsave("Plots/temp_max_line_niños.png", plot, width=30, height=9, units="cm", dpi=200)
  
  # Histograma
  plot2 <- ggplot(data=temperatura, aes(x=Temperatura_máxima))+
    geom_histogram(alpha=0.5, fill="#1B9E77")+
    labs(title='Temperatura máxima mensual', x='Temperatura (°C)')+
    scale_color_brewer(palette='Dark2')+
    geom_vline(aes(xintercept=mean(Temperatura_máxima, na.rm=TRUE)), color="#7570B3",
               linetype='dashed', size=1)
  plot2
  ggsave("Plots/temp_max_hist.png", plot2, width=30, height=9, units="cm", dpi=200)

  
### Irradiación
  
  plot <- ggplot(data=temperatura, aes(x=Fecha, y=Irradiacion))+
    geom_line()+
    geom_point()+
    labs(title='Irradiación mensual promedio', x='Mes', y='Irradiación (kJ/m2/dia)')+
    scale_x_date(date_breaks='months',date_labels = "%m", 
                 limit=c(as.Date("2017-01-01"),as.Date("2017-12-31")))
  plot  
  ggsave("Plots/irrad_line.png", plot, width=16, height=8, units="cm", dpi=200)

### Presión de vapor
  
  # lineplot
  plot <- ggplot(data=temperatura, aes(x=Fecha, y=presion_vapor_prom))+
    geom_line()+
    labs(title='Presión de vapor mensual', x='Fecha', y='Presión de vapor (hPa)')+
    scale_x_date(date_breaks="years", date_labels="%Y", date_minor_breaks = "1 month")+
    theme(axis.text.x=element_text(angle=60, hjust=1)) 
  plot
  ggsave("Plots/vapour_pres_line.png", plot, width=30, height=9, units="cm", dpi=200)
  
  # Histograma
  plot2 <- ggplot(data=temperatura, aes(x=presion_vapor_prom))+
    geom_histogram(alpha=0.5, fill="#1B9E77")+
    labs(title='Presión de vapor mensual', x='Presión de vapor (hPa)')+
    scale_color_brewer(palette='Dark2')+
    geom_vline(aes(xintercept=mean(presion_vapor_prom, na.rm=TRUE)), color="#7570B3",
               linetype='dashed', size=1)
  plot2
  ggsave("Plots/vapour_pres_hist.png", plot2, width=30, height=9, units="cm", dpi=200)

### Precipitación
  
  # lineplot
  plot <- ggplot(data=precipitacion[precipitacion$Municipio=='Cubarral',], aes(x=Fecha, y=precipitacion, color=Municipio))+
    geom_line()+
    labs(title='Precipitación mensual', x='Fecha', y='Precipitación (mm/mes)')+
    scale_color_manual(values=c("#1B9E77","#7570B3" ))+
    #scale_color_manual(values=c("#D95F02","#1B9E77","#7570B3" ))+
    scale_x_date(date_breaks="years", date_labels="%Y", date_minor_breaks = "1 month",
                 limits = as.Date(c('2010-01-01','2020-12-31')))+
    theme(axis.text.x=element_text(angle=60, hjust=1)) 
  plot
  ggsave("Plots/precipitacion_line_cubarral.png", plot, width=25, height=9, units="cm", dpi=200)
  
  # Histograma
  promedios <- precipitacion %>% group_by(Municipio) %>% summarise(prom=mean(precipitacion))
  
  plot2 <- ggplot(data=precipitacion, aes(x=precipitacion))+
    geom_histogram(alpha=0.35, position = 'identity', aes(fill=Municipio, color=Municipio))+
    labs(title='Precipitación mensual', x='Precipitación (mm/mes)')+
    geom_vline(xintercept=promedios$prom,linetype='dashed', size=1, color=c("#D95F02","#1B9E77","#7570B3" ))

  plot2
  ggsave("Plots/precipitacion_hist.png", plot2, width=32, height=9, units="cm", dpi=200)
  
  # Boxplot por mes
  datos <- precipitacion
  datos$Fecha<-as.character(datos$Fecha)
  datos<-datos %>% separate(Fecha, c(NA, "Mes", NA))
  
  plot3 <- ggplot(datos[datos$Municipio=='Cubarral',], aes(x=Mes, y=precipitacion, color=Municipio))+
    geom_boxplot()+
    labs(title='Precipitación por mes', y='Precipitación (mm/mes)')+
    scale_color_manual(values=c("#1B9E77","#7570B3" ))
  plot3
  ggsave("Plots/precipitacion_box_cubarral.png", plot3, width=25, height=9, units="cm", dpi=200)
  
  # Lineplot niño y niña
  datos <- precipitacion[precipitacion$Municipio=='Acacías',]
  datos$Fecha <- as.numeric(format(datos$Fecha, '%Y'))
  años <- datos %>% group_by(Fecha) %>% summarise(cantidad=n())
  años <- años[años$cantidad==12,]$Fecha

  datos <- datos[(datos$Fecha %in% años),]
  
  mode <- function(codes){
    names(which.max(table(codes)))
  }

  
  datos <- datos %>% group_by(Fecha) %>% summarise(precipitacion=sum(precipitacion),
                                                   dias_lluvia=sum(dias_lluvia),
                                                   Fenómeno=mode(Fenómeno))
  
  plot <- ggplot(data=datos, aes(x=Fecha, y=precipitacion, color=Fenómeno))+
    geom_path(aes(group=1))+
    geom_point()+
    labs(title='Precipitación anual en Acacias', x='Fecha', y='Precipitación (mm/año)')+
    scale_color_brewer(palette='Dark2')+
    theme(axis.text.x=element_text(angle=60, hjust=1)) 
  plot
  ggsave("Plots/precipitacion_line_acacias_niños.png", plot, width=18, height=9, units="cm", dpi=200)
  
### Dias con lluvia
  
  # lineplot
  plot <- ggplot(data=precipitacion, aes(x=Fecha, y=dias_lluvia, color=Municipio))+
    geom_line()+
    labs(title='Dias de lluvia al mes', x='Fecha', y='Dias de lluvia')+
    scale_color_manual(values=c("#D95F02","#1B9E77","#7570B3" ))+
    scale_x_date(date_breaks="years", date_labels="%Y", date_minor_breaks = "1 month")+
    theme(axis.text.x=element_text(angle=60, hjust=1)) 
  plot
  ggsave("Plots/dias_lluvia_line.png", plot, width=32, height=9, units="cm", dpi=200)
  
  # Histograma
  promedios <- precipitacion %>% group_by(Municipio) %>% summarise(prom=mean(dias_lluvia))
  
  plot2 <- ggplot(data=precipitacion, aes(x=dias_lluvia))+
    geom_histogram(alpha=0.35, position = 'identity', aes(fill=Municipio, color=Municipio))+
    labs(title='Dias de lluvia al mes', x='Dias de lluvia')+
    geom_vline(xintercept=promedios$prom,linetype='dashed', size=1, color=c("#D95F02","#1B9E77","#7570B3" ))
  
  plot2
  ggsave("Plots/dias_lluvia_hist.png", plot2, width=32, height=9, units="cm", dpi=200)
  
  # Lineplot niño y niña
  plot <- ggplot(data=datos, aes(x=Fecha, y=dias_lluvia, color=Fenómeno))+
    geom_path(aes(group=1))+
    geom_point()+
    labs(title='Dias con lluvia al año en Acacías', x='Fecha', y='Dias con lluvia')+
    scale_color_brewer(palette='Dark2')+
    theme(axis.text.x=element_text(angle=60, hjust=1)) 
  plot
  ggsave("Plots/dias_lluvia_line_acacias_niños.png", plot, width=18, height=9, units="cm", dpi=200)
  

  