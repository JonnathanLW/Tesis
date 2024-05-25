library(missForest)
library(caret)
library(hydroGOF)
# 
# prec = read.csv("C:/Users/Jonna/Desktop/Randon_Forest/Algoritmo RF_2/Downscalling/prec_microcuencas/Micro_Yanuncay.csv", header = TRUE)
# prec = prec[, c("Fecha", "mod_CuantilMapping")]
# names(prec) = c("Fecha", "prec")
# prec$Fecha = as.Date(prec$Fecha, format = "%Y-%m-%d")
# 
# # Cargo prec mas cercana
# prec_hizhil = read.csv("C:/Users/Jonna/Desktop/Randon_Forest/Estaciones_Tierra/Diario/Huizhil.csv", header = TRUE)
# names(prec_hizhil) = c("Fecha", "prec")
# prec_hizhil$Fecha = as.Date(prec_hizhil$Fecha, format = "%Y-%m-%d")

# Cargamos caudales
caudal = read.csv("C:/Users/Jonna/Desktop/Randon_Forest/Caudales/YanuncayAjTarqui.csv", header = TRUE)
caudal$TIMESTAMP = as.Date(caudal$TIMESTAMP, format = "%Y-%m-%d")
names(caudal) = c("Fecha", "nivel")
summary(caudal)
convertir.caudal = function(df){
  names(df) = c("TIMESTAMP", "nivel")
  df$TIMESTAMP = as.Date(df$TIMESTAMP, format = "%Y-%m-%d")
  df$nivel = as.numeric(df$nivel)
  
  df$Caudal = NA
  
  # Aplico las fórmulas de caudal según el nivel
  for (i in seq_along(df$nivel)) {
    nivel = df$nivel[i]
    if (!is.na(nivel)) {
      if (nivel < 95) {
        df$Caudal[i] = 0.0116 * (nivel^1.086)
      } else {
        df$Caudal[i] = 0.00000145966 * ((nivel - 20)^3.3435)
      }
    }
  }
  
  # selecciono las columnas que me interesan
  # df = df[,c("TIMESTAMP", "Caudal")]
  # voy filtrando por fechas de interés
  fecha.min = as.Date("2014-05-14")
  fecha.max = as.Date("2023-06-06")
  df = df[(df$TIMESTAMP >= fecha.min & df$TIMESTAMP <= fecha.max),]
  return(df)
}

caudal = convertir.caudal(caudal)
caudal = caudal[, c("TIMESTAMP", "Caudal")]
names(caudal) = c("Fecha", "caudal")
summary(caudal)

caudal.max = 100
indices.c = which(caudal$caudal > caudal.max)

# coloco Na en los valores mayores a 100
caudal$caudal[indices.c] = NA
summary(caudal)


# Aplicacion de randon Forest --------------------------------------------------
set.seed(123) # reproducibilidad
df = caudal
df$year = as.numeric(format(df$Fecha, "%Y"))
df$month = as.numeric(format(df$Fecha, "%m"))
df$day = as.numeric(format(df$Fecha, "%d"))
df = df[,(-1)]

# creo un 20% de datos faltantes, pero conociendo los indices donde serán creados

indices_Na = sample(1:nrow(df), nrow(df)*0.20)
df$caudal[indices_Na] = NA

modelo.rf = missForest(df, 
                       ntree = 500,
                       sqrt(ncol(df) - 1),
                       xtrue = NA)

datos_imputados = modelo.rf$ximp
obs = caudal$caudal[indices_Na]
pred = datos_imputados$caudal[indices_Na]
gof(pred, obs)
