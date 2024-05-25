library(missForest)
library(dplyr)
library(tidyr)
library(svDialogs)
library(caret)
library(hydroGOF)
library(randomForest)
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

# df = merge(prec, caudal, by = "Fecha", all = TRUE)
# names(df) = c("Fecha", "prec_sat", "caudal")
# df = merge(df, prec_hizhil, by = "Fecha", all = TRUE)
# names(df) = c("Fecha", "prec_sat", "caudal", "prec_tier")
# 
# #data = na.omit(df)
# data = data[,c("caudal", "prec_sat",  "prec_tier")]
# data$Tlag1sat = c(NA, data$prec_sat[1:(nrow(data)-1)])
# data$Tlag2sat = c(NA, NA, data$prec_sat[1:(nrow(data)-2)])
# data$Tlag3sat = c(NA, NA, NA, data$prec_sat[1:(nrow(data)-3)])
# data$Tlag1tier = c(NA, data$prec_tier[1:(nrow(data)-1)])
# data$Tlag2tier = c(NA, NA, data$prec_tier[1:(nrow(data)-2)])
# data$Tlag3tier = c(NA, NA, NA, data$prec_tier[1:(nrow(data)-3)])




# Aplicacion de randon Forest ---------------------------------------------
set.seed(123) # reproducibilidad
# trainIndex = createDataPartition(data$caudal, p = 0.8, list = FALSE)
# data_train = data[trainIndex,]
# data_test = data[-trainIndex,]

df = caudal
df$year = as.numeric(format(df$Fecha, "%Y"))
df$month = as.numeric(format(df$Fecha, "%m"))
df$day = as.numeric(format(df$Fecha, "%d"))
df = df[,(-1)]

modelo.rf = missForest(df, 
                       ntree = 500,
                       sqrt(ncol(df) - 1),
                       xtrue = NA)




datos_imputados = modelo.rf$ximp
obs = df$caudal
pred = datos_imputados$caudal
gof(pred, obs)
# df_eval = cbind(obs, pred)
# df_eval = na.omit(df_eval)
# 
# datos_imputados$caudal[is.na(datos_imputados$caudal)] <- modelo.rf$ximp[is.na(datos_imputados$caudal), 1]
# índices_no_na <- which(!is.na(data_test$caudal))
# obs = data_test[índices_no_na, "caudal"]
# pred = modelo.rf$ximp[índices_no_na, "caudal"]
# gof(pred, obs)
