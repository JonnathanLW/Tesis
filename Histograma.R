# Cargar paquetes necesarios
library(ggplot2)
library(readr)
datos = read.csv("C:/Users/marco/OneDrive - ucuenca.edu.ec/Datos_estaciones/Relleno/Resultados.csv")
#Crear histograma para datos
ggplot(datos, aes(x = datos$Caudal_obs)) +
  geom_histogram(fill = "blue", alpha = 0.5) +
  ggtitle("Histograma de Datos Observados") +
  xlab("Valor") +
  ylab("Frecuencia")
#AÑADIR HISTOGRA PARA DATOS PREDICHOS
ggplot(datos, aes(x = datos$Caudal_pred)) +
  geom_histogram(fill = "orange", alpha = 0.5) +
  ggtitle("Histograma de Datos Predichos") +
  xlab("Valor") +
  ylab("Frecuencia")
#COMBINAR HISTOGRAMAS
library(gridExtra)
grid.arrange(datos$Caudal_obs, datos$Caudal_pred, ncol = 2)
#COMPARAR HISTOGRAMAS SUPERPUESTOS
ggplot() +
  geom_histogram(data = datos, aes(x = datos$Caudal_obs), fill = "blue", alpha = 0.5) +
  geom_histogram(data = datos, aes(x = datos$Caudal_pred), fill = "orange", alpha = 0.5) +
  ggtitle("Comparación de Datos Observados y Predichos") +
  xlab("Valor") +
  ylab("Frecuencia")
#Añadir leyenda
ggplot() +
  geom_histogram(data = datos, aes(x = datos$Caudal_obs, color = "Observado"), fill = "blue", alpha = 0.5) +
  geom_histogram(data = datos, aes(x = datos$Caudal_pred, color = "Predicho"), fill = "orange", alpha = 0.5) +
  ggtitle("Comparación de Datos Observados y Predichos") +
  xlab("Valor") +
  ylab("Frecuencia") +
  scale_color_manual(values = c("blue", "orange"), labels = c("Observado", "Predicho"))

#Relaizar grafica de densidad
ggplot(datos, aes(x = datos$Caudal_obs)) +
  geom_density(fill = "blue", alpha = 0.5) +
  ggtitle("Gráfica de Densidad de Datos Observados") +
  xlab("Valor") +
  ylab("Densidad")
#AÑADIR GRAFICA DE DENSIDAD PARA DATOS PREDICHOS
ggplot(datos, aes(x = datos$Caudal_pred)) +
  geom_density(fill = "orange", alpha = 0.5) +
  ggtitle("Gráfica de Densidad de Datos Predichos") +
  xlab("Valor") +
  ylab("Densidad")
#COMBINAR GRAFICAS DE DENSIDAD
grid.arrange(p1, p2, ncol = 2)
#COMPARAR GRAFICAS DE DENSIDAD SUPERPUESTAS
ggplot() +
  geom_density(data = datos, aes(x = datos$Caudal_obs), fill = "blue", alpha = 0.5) +
  geom_density(data = datos, aes(x = datos$Caudal_pred), fill = "orange", alpha = 0.5) +
  ggtitle("Comparación de Datos Observados y Predichos con Curva de Densidad") +
  xlab("Valor") +
  ylab("Densidad")
#COMPARAR GRAFICAS DE DENSIDAD SUPERPUESTAS CON CURVA NORMAL
ggplot() +
  geom_density(data = datos, aes(x = datos$Caudal_obs), fill = "blue", alpha = 0.5) +
  geom_density(data = datos, aes(x = datos$Caudal_pred), fill = "orange", alpha = 0.5) +
  stat_function(fun = dnorm, args = list(mean = mean(datos$Caudal_obs), sd = sd(datos$Caudal_obs)), color = "blue") +
  stat_function(fun = dnorm, args = list(mean = mean(datos$Caudal_pred), sd = sd(datos$Caudal_pred)), color = "orange") +
  ggtitle("Comparación de Datos Observados y Predichos con Curva Normal") +
  xlab("Valor") +
  ylab("Densidad")
  
#Añadir leyenda
ggplot() +
  geom_density(data = datos, aes(x = datos$Caudal_obs, color = "Observado"), fill = "blue", alpha = 0.5) +
  geom_density(data = datos, aes(x = datos$Caudal_pred, color = "Predicho"), fill = "orange", alpha = 0.5) +
  ggtitle("Comparación de Datos Observados y Predichos con Curva de Densidad y Normal") +
  xlab("Valor") +
  ylab("Densidad") +
  scale_color_manual(values = c("blue", "orange"), labels = c("Observado", "Predicho"))
#Añadir leyenda con etiquetas
ggplot() +
  geom_density(data = datos, aes(x = datos$Caudal_obs, color = "Observado"), fill = "blue", alpha = 0.5) +
  geom_density(data = datos, aes(x = datos$Caudal_pred, color = "Predicho"), fill = "orange", alpha = 0.5) +
  ggtitle("Comparación de Datos Observados y Predichos con Curva de Densidad y Normal") +
  xlab("Valor") +
  ylab("Densidad") +
  scale_color_manual(values = c("blue", "orange"), labels = c("Observado", "Predicho")) +
  theme(legend.position = "bottom")



