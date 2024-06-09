#VERSION 2.1.0
#---------------------------------------------------------

# Cargar librerías necesarias
library(dplyr)
library(zoo)
library(ggplot2)
# Función para identificar eventos de sequía según Yevjevich con umbral y duración mínima
identifica_sequia_yevjevich <- function(indice, umbral_sequia, minimo_duracion) {
  eventos <- vector("list", length = 0)
  evento_actual <- c()
  
  for (i in seq_along(indice)) {
    if (indice[i] < umbral_sequia) {
      evento_actual <- c(evento_actual, indice[i])
    } else {
      if (length(evento_actual) >= minimo_duracion) {
        eventos <- c(eventos, list(evento_actual))
      }
      evento_actual <- c()
    }
  }
  
  if (length(evento_actual) >= minimo_duracion) {
    eventos <- c(eventos, list(evento_actual))
  }
  
  return(eventos)
}

# Cargar tus datos de SSI y SPI
SPI <- read.csv("C:/Users/marco/OneDrive - ucuenca.edu.ec/Datos_estaciones/Satelite-Prec/Actualizdo/C.SPI(400).csv")
SSI <- read.csv("C:/Users/marco/OneDrive - ucuenca.edu.ec/Datos_estaciones/Satelite-Prec/Actualizdo/SSI.csv")

# Unir los datos
datos <- merge(SPI, SSI, by = "date")
#seleccionar columna de date, spi,ssi
datos <- datos[,c(1,2,4)]
# Renombrar columnas
#colnames(datos) <- c("fecha", "spi", "ClasificacionSPI","ssi")
colnames(datos) <- c("fecha", "spi","ssi")
# Seleccionar las columnas de interés (fecha, spi, ssi)
#datos <- datos[, c(1, 2, 4)]

# Formato de fecha
datos$fecha <- as.Date(datos$fecha, format = "%Y-%m-%d")

# Definir umbral de sequía y duración mínima
umbral_sequia <- 0  # Umbral de sequía (Teoria de las corridas de Yevjevich)
minimo_duracion <- 1  # Meses

# Identificar eventos de sequía según SSI y SPI
sequias_ssi <- identifica_sequia_yevjevich(datos$ssi, umbral_sequia, minimo_duracion)
sequias_spi <- identifica_sequia_yevjevich(datos$spi, umbral_sequia, minimo_duracion)

# Caracterizar las sequías
caracteristicas_ssi <- lapply(sequias_ssi, function(evento) {
  data.frame(
    inicio = datos$fecha[min(which(datos$ssi %in% evento))],
    fin = datos$fecha[max(which(datos$ssi %in% evento))],
    duracion = length(evento)
  )
})

caracteristicas_spi <- lapply(sequias_spi, function(evento) {
  data.frame(
    inicio = datos$fecha[min(which(datos$spi %in% evento))],
    fin = datos$fecha[max(which(datos$spi %in% evento))],
    duracion = length(evento)
  )
})

# Generar reporte de sequías caracterizadas
reporte_ssi <- do.call(rbind, caracteristicas_ssi)
reporte_spi <- do.call(rbind, caracteristicas_spi)

cat("Reporte de sequías según SSI:\n")
print(reporte_ssi)

cat("\nReporte de sequías según SPI:\n")
print(reporte_spi)

# Graficar índices y eventos de sequía con leyenda
ggplot() +
  geom_line(data = datos, aes(x = fecha, y = ssi, color = "Línea SSI")) +
  geom_rect(data = reporte_ssi, aes(xmin = inicio, xmax = fin, ymin = -Inf, ymax = umbral_sequia, fill = "Evento SSI"), alpha = 0.3) +
  geom_line(data = datos, aes(x = fecha, y = spi, color = "Línea SPI")) +
  geom_rect(data = reporte_spi, aes(xmin = inicio, xmax = fin, ymin = -Inf, ymax = umbral_sequia, fill = "Evento SPI"), alpha = 0.3) +
  xlab("Fecha") +
  ylab("Índice de Sequía") +
  scale_x_date(date_labels = "%Y", date_breaks = "year", limits = c(as.Date("2014-07-01"), NA)) +
  ggtitle("Eventos de Sequía según SSI y SPI") +
  geom_hline(yintercept = umbral_sequia, linetype = "dashed", color = "gray", lty = 2) +
  scale_color_manual(values = c("Línea SSI" = "blue", "Línea SPI" = "red")) +
  scale_fill_manual(values = c("Evento SSI" = "blue", "Evento SPI" = "red")) +
  guides(color = guide_legend(override.aes = list(linetype = c(1, 1))),
         fill = guide_legend(override.aes = list(alpha = 1))) +
  theme_bw()

#VERSION 2-------------
# # Función para identificar eventos de sequía según Yevjevich con umbral y duración mínima
# identifica_sequia_yevjevich <- function(indice, umbral_sequia, minimo_duracion) {
#   eventos <- vector("list", length = 0)
#   evento_actual <- c()
#   
#   for (i in seq_along(indice)) {
#     if (indice[i] < umbral_sequia) {
#       evento_actual <- c(evento_actual, indice[i])
#     } else {
#       if (length(evento_actual) >= minimo_duracion) {
#         eventos <- c(eventos, list(evento_actual))
#       }
#       evento_actual <- c()
#     }
#   }
#   
#   if (length(evento_actual) >= minimo_duracion) {
#     eventos <- c(eventos, list(evento_actual))
#   }
#   
#   return(eventos)
# }
# 
# 
# # Cargar tus datos de SSI y SPI
# SPI <- read.csv("C:/Users/marco/OneDrive - ucuenca.edu.ec/Datos_estaciones/Satelite-Prec/C.SPI.csv")
# SSI <- read.csv("C:/Users/marco/OneDrive - ucuenca.edu.ec/Datos_estaciones/Satelite-Prec/C.SSI(600).csv")
# #Unir los datos
# datos <- merge(SPI, SSI, by = "date")
# #renombrar columnas
# colnames(datos) <- c("fecha", "spi", "ClasificacionSPI","ssi","ClasificacionSSI")
# #seleccionar las columnas de interes(fecha, spi, ssi)
# datos <- datos[,c(1,2,4)]
# #formato de fecha
# datos$fecha <- as.Date(datos$fecha, format = "%Y-%m-%d")
# 
# 
# # Definir umbral de sequía y duración mínima
# umbral_sequia <- 0  # Umbral de sequía (Teoria de las corridas de Yevjevich)
# minimo_duracion <- 1  # Meses
# 
# # Identificar eventos de sequía según SSI y SPI
# sequias_ssi <- identifica_sequia_yevjevich(datos$ssi, umbral_sequia, minimo_duracion)
# sequias_spi <- identifica_sequia_yevjevich(datos$spi, umbral_sequia, minimo_duracion)
# 
# # Caracterizar las sequías
# caracteristicas_ssi <- lapply(sequias_ssi, function(evento) {
#   data.frame(inicio = min(which(datos$ssi %in% evento)),
#              fin = max(which(datos$ssi %in% evento)),
#              duracion = length(evento))
# })
# 
# caracteristicas_spi <- lapply(sequias_spi, function(evento) {
#   data.frame(inicio = min(which(datos$spi %in% evento)),
#              fin = max(which(datos$spi %in% evento)),
#              duracion = length(evento))
# })
# 
# # Generar reporte de sequías caracterizadas
# reporte_ssi <- do.call(rbind, caracteristicas_ssi)
# reporte_spi <- do.call(rbind, caracteristicas_spi)
# 
# print("Reporte de sequías según SSI:")
# print(reporte_ssi)
# 
# print("Reporte de sequías según SPI:")
# print(reporte_spi)

# # Graficar índices y eventos de sequía con leyenda
# ggplot() +
#   geom_line(data = datos, aes(x = fecha, y = ssi, color = "Línea SSI")) +
#   geom_rect(data = reporte_ssi, aes(xmin = datos$fecha[inicio], xmax = datos$fecha[fin], ymin = -Inf, ymax = umbral_sequia, fill = "Evento SSI"), alpha = 0.3) +
#   geom_line(data = datos, aes(x = fecha, y = spi, color = "Línea SPI")) +
#   geom_rect(data = reporte_spi, aes(xmin = datos$fecha[inicio], xmax = datos$fecha[fin], ymin = -Inf, ymax = umbral_sequia, fill = "Evento SPI"), alpha = 0.3) +
#   xlab("Fecha") +
#   ylab("Índice de Sequía") +
#   scale_x_date(date_labels = "%Y", date_breaks = "year", limits = c(as.Date("2014-07-01"), NA)) +
#   ggtitle("Eventos de Sequía según SSI y SPI") +
#   geom_hline(yintercept = umbral_sequia, linetype = "dashed", color = "gray", lty = 2) +
#   scale_color_manual(values = c("Línea SSI" = "blue", "Línea SPI" = "red")) +
#   scale_fill_manual(values = c("Evento SSI" = "blue", "Evento SPI" = "red")) +
#   guides(color = guide_legend(override.aes = list(linetype = c(1, 1))),
#          fill = guide_legend(override.aes = list(alpha = 1))) +
#   theme_bw()

#FIN VERSION 2

#RELACION SPI/SSI------------------------
# Calcular la correlación entre SPI y SSI
correlacion_spi_ssi <- cor(datos$spi, datos$ssi, use = "complete.obs")
print(paste("Correlación entre SPI y SSI:", correlacion_spi_ssi))
#CALCULO DE LA VARIANZA DE SPI Y SSI
# Calcular la varianza de SPI y SSI
varianza_spi <- var(datos$spi, na.rm = TRUE)
varianza_ssi <- var(datos$ssi, na.rm = TRUE)
print(paste("Varianza de SPI:", varianza_spi))
print(paste("Varianza de SSI:", varianza_ssi))

# Función para calcular la relación SPI/SSI
calcula_relacion_spi_ssi <- function(spi, ssi) {
  relacion <- spi / ssi
  relacion[spi < 0 & ssi < 0] <- abs(spi[spi < 0 & ssi < 0]) / abs(ssi[spi < 0 & ssi < 0])
  relacion[is.na(relacion)] <- 0  # Reemplazar NAs por 0
  return(relacion)
}

# Calcular la relación SPI/SSI
datos$relacion_spi_ssi <- calcula_relacion_spi_ssi(datos$spi, datos$ssi)

# ESTANDARIZACIÓN DE LA RELACIÓN SPI/SSI
datos$relacion_spi_ssi <- scale(datos$relacion_spi_ssi)

# Identificar eventos de sequía según la relación SPI/SSI
sequias_relacion <- identifica_sequia_yevjevich(datos$relacion_spi_ssi, umbral_sequia, minimo_duracion)
caracteristicas_relacion <- lapply(sequias_relacion, function(evento) {
  data.frame(inicio = min(which(datos$relacion_spi_ssi %in% evento)),
             fin = max(which(datos$relacion_spi_ssi %in% evento)),
             duracion = length(evento))
})
reporte_relacion <- do.call(rbind, caracteristicas_relacion)
print("Reporte de sequías según SPI/SSI:")
print(reporte_relacion)

# Graficar relación SPI/SSI y eventos de sequía
ggplot() +
  geom_line(data = datos, aes(x = fecha, y = datos$relacion_spi_ssi, color = "Línea SPI/SSI")) +
  geom_rect(data = reporte_relacion, aes(xmin = datos$fecha[inicio], xmax = datos$fecha[fin], ymin = -Inf, ymax = umbral_sequia, fill = "Evento SPI/SSI"), alpha = 0.2) +
  xlab("Fecha") +
  ylab("Relación SPI/SSI") +
  ggtitle("Eventos de Sequía según SPI/SSI") +
  geom_hline(yintercept = umbral_sequia, linetype = "dashed", color = "gray", lty = 2) +
  scale_color_manual(values = c("Línea SPI/SSI" = "purple")) +
  scale_fill_manual(values = c("Evento SPI/SSI" = "purple")) +
  guides(color = guide_legend(override.aes = list(linetype = 1)),
         fill = guide_legend(override.aes = list(alpha = 1))) +
  theme_bw()
summary(datos$relacion_spi_ssi)
# FIN RELACIÓN SPI/SSI------------------------

# RELACIÓN SPI3 CON SSI3------------------------
# Calcular SPI3 y SSI3
datos_SPI3 <- rollapplyr(datos$spi, width = 3, FUN = mean, fill = NA)
datos_SSI3 <- rollapplyr(datos$ssi, width = 3, FUN = mean, fill = NA)

# Calcular la correlación entre SPI3 y SSI3
correlacion_spi3_ssi3 <- cor(datos_SPI3, datos_SSI3, use = "complete.obs")
print(paste("Correlación entre SPI3 y SSI3:", correlacion_spi3_ssi3))

