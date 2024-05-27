library(missForest)
library(caret)
library(hydroGOF)
library(ggplot2)
library(mice)

################################################################################
# Funciones necesarias ---------------------------------------------------------
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

  # # selecciono las columnas que me interesan
  # df = df[,c("TIMESTAMP", "nivel")]
  df = df[,c("TIMESTAMP", "Caudal")]
  names(df) = c("Fecha", "caudal")
  # voy filtrando por fechas de interés
  # fecha.min = as.Date("2014-05-14")
  # fecha.max = as.Date("2023-06-06")
  # df = df[(df$TIMESTAMP >= fecha.min & df$TIMESTAMP <= fecha.max),]
  return(df)
}
convertir.prec = function(df){
  names(df) = c("TIMESTAMP", "prec")
  df$TIMESTAMP = as.Date(df$TIMESTAMP, format = "%Y-%m-%d")
  df$prec = as.numeric(df$prec)
  names(df) = c("Fecha", "prec")
  return(df)
}
T.lag = function(x, n){
  c(rep(NA, n), x)[1:length(x)]
}
################################################################################'
# Carga de datos ---------------------------------------------------------------
# Caudales
caudal = read.csv("C:/Users/Jonna/Desktop/Nueva carpeta/Yanuncay_diario.csv", header = TRUE)
caudal = convertir.caudal(caudal)
summary(caudal)

# selecciono todas las filas q sean menores a 300
ind.1 = which(caudal$caudal > 300)
caudal$caudal[ind.1] = NA
summary(caudal)
# precipitaciones
# prec = read.csv("C:/Users/Jonna/Desktop/Nueva carpeta/Huizhil.csv", header = TRUE)
prec = read.csv("C:/Users/Jonna/Desktop/Randon_Forest/Estaciones_Tierra/Diario/CebollarPTAPM.csv", header = TRUE)
prec_h = read.csv("C:/Users/Jonna/Desktop/Randon_Forest/Estaciones_Tierra/Diario/Huizhil.csv", header = TRUE)
prec_y = read.csv("C:/Users/Jonna/Desktop/Randon_Forest/Estaciones_Tierra/Diario/YanuncayPucan.csv", header = TRUE)

prec = convertir.prec(prec)
prec = prec[,c("Fecha", "prec")]
prec_h = convertir.prec(prec_h)
prec_y = convertir.prec(prec_y)

summary(prec)
summary(prec_h)
summary(prec_y)

ind.2 = which(prec_y$prec > 300)
prec_y$prec[ind.2] = NA

################################################################################
# Preprocesamiento de datos ----------------------------------------------------
df = merge(prec, prec_h, by = "Fecha", all = TRUE)
names(df) = c("Fecha", "prec", "prec_h")
df = merge(df, prec_y, by = "Fecha", all = TRUE)
names(df) = c("Fecha", "prec", "prec_h", "prec_y")
df = merge(df, caudal, by = "Fecha", all = TRUE)
names(df) = c("Fecha", "prec", "prec_h", "prec_y", "caudal")
#df = merge(prec, caudal, by = "Fecha", all = TRUE)
cor(df[,-1], use = "complete.obs") # Analizo la correlacion entre prec y caudal

# agrego Tlag para ver la correlación en los diferentes desfases
df$Tplag_1 = T.lag(df$prec, 1)
df$Tplag_2 = T.lag(df$prec, 2)
df$Tplag_3 = T.lag(df$prec, 3)

df$Tplag_h_1 = T.lag(df$prec_h, 1)
df$Tplag_h_2 = T.lag(df$prec_h, 2)
df$Tplag_h_3 = T.lag(df$prec_h, 3)

df$Tplag_y_1 = T.lag(df$prec_y, 1)
df$Tplag_y_2 = T.lag(df$prec_y, 2)
df$Tplag_y_3 = T.lag(df$prec_y, 3)


cor(df[,-1], use = "complete.obs")

df_train = df[,c("Fecha", "Tplag_h_1", "Tplag_1", "Tplag_y_1", "prec_h", "caudal")]

# Extraigo la primer y ultima fecha donde caudal tenga el primer dato
# df_train = df_train[!is.na(df_train$caudal),]
first.date = which(!is.na(df_train$caudal))[1]
df.min = df_train$Fecha[first.date]
fin.date = tail(which(!is.na(df_train$caudal)), n = 1)
df.max = df_train$Fecha[fin.date]

seq = seq(df.min, df.max, by = "day")
Fecha = data.frame(Fecha = seq)
df_train = merge(Fecha, df_train, by = "Fecha")
# filtro los datos
# df_train = df_train[(df_train$Fecha >= df.min & df_train$Fecha <= df.max),]
################################################################################
# Aplicación de randon Forest --------------------------------------------------

set.seed(123) # reproducibilidad
df_train$Year = as.numeric(format(df_train$Fecha, "%Y"))
df_train$Month = as.numeric(format(df_train$Fecha, "%m"))
df_train$Day = as.numeric(format(df_train$Fecha, "%d"))

df_train = df_train[,-1]

# creo un 20% de datos faltantes, pero conociendo los indices donde serán creados
indices_Na = sample(1:nrow(df_train), nrow(df_train)*0.20)
obs = df_train$caudal[indices_Na]
df_train$caudal[indices_Na] = NA

modelo.rf = missForest(df_train, 
                       ntree = 500,
                       sqrt(ncol(df) - 1),
                       xtrue = NA)


# Validación del modelo --------------------------------------------------------
datos_imputados = modelo.rf$ximp # ectraigo datos imputados
modelo.rf$OOBerror # error de imputación

# test de HydroGOF
pred = datos_imputados$caudal[indices_Na]
gof(pred, obs)

# ploteo de resultados
plot(density(obs, na.rm = TRUE), col = "black", lwd = 2, main = "Densidad de caudales", xlab = "Caudal", ylab = "Densidad")
lines(density(pred), col = "red", lwd = 2)
legend("topright", c("Caudal observado", "caudal imputado"), fill = c("black", "red"))


# Algoritmo Mice ---------------------------------------------------------------

mice.model = mice(df_train, m = 5, method = "rf", seed = 123)
data.mice = complete(mice.model)
pred.2 = data.mice$caudal[indices_Na]
gof(pred.2, obs)

# ploteo de resultados
plot(density(obs, na.rm = TRUE), col = "black", lwd = 2, main = "Densidad de caudales", xlab = "Caudal", ylab = "Densidad")
lines(density(pred.2), col = "red", lwd = 2)
legend("topright", c("Caudal observado", "caudal imputado"), fill = c("black", "red"))

