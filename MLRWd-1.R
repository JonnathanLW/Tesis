library(caret)
library(hydroGOF)

Estaciones = read.csv("C:/Users/Jonna/Desktop/Randon_Forest/AlgoritmoRF/YanunTomebPpts.csv")
Estaciones$TIMESTAMP = as.Date(Estaciones$TIMESTAMP, format = "%Y-%m-%d")
summary(Estaciones)

metadatos.estaciones = read.csv("C:/Users/Jonna/Desktop/Randon_Forest/AlgoritmoRF/YanunTomebPPgis.csv")

# Crear una matriz vacía para almacenar las distancias euclidianas
distancias_km = matrix(NA, nrow = nrow(metadatos.estaciones), ncol = nrow(metadatos.estaciones))
# Calcular la distancia euclidiana entre cada par de estaciones
for (i in 1:nrow(metadatos.estaciones)) {
  for (j in 1:nrow(metadatos.estaciones)) {
    dist_lon_km <- 111.111 * cos(mean(metadatos.estaciones$lat[c(i, j)]) * pi / 180)
    dist_lat_km <- 111.111
    distancias_km[i, j] = sqrt((metadatos.estaciones$lon[i] - metadatos.estaciones$lon[j])^2 * dist_lon_km^2 +
                                  (metadatos.estaciones$lat[i] - metadatos.estaciones$lat[j])^2 * dist_lat_km^2)
  }
}

# Convertir la matriz de distancias euclidianas en un data frame para mayor claridad
distancias_km_df = as.data.frame(distancias_km)
rownames(distancias_km_df) = metadatos.estaciones$Nombre
colnames(distancias_km_df) = metadatos.estaciones$Nombre

distancias.cebollar = distancias_km_df[, "CebollarPTAPM"]
distancias.cebolalr = data.frame(distancias.cebollar)
rownames(distancias.cebolalr) = rownames(distancias_km_df)
colnames(distancias.cebolalr) = "distancia"
distancias.cebolalr$peso = 1/distancias.cebolalr$distancia

# Aplico el modelo -------------------------------------------------------------
set.seed(123)
df = na.omit(Estaciones)
indices = sample(1:nrow(df), nrow(df)*0.8)
data.train = df[indices,]
length(data.train)
data.test = df[-indices,]

pesos = rep(1/3.674189, nrow(data.train))
length(pesos)
control = trainControl(method = "cv", number = 5)
model.c = train(CebollarPTAPM ~ Totoracocha , data = data.train, method = "lm", trControl = control)
model.c$resample

predicciones = predict(model.c$finalModel, data.test)
predicciones = as.data.frame(predicciones)
TIMESTAMP = data.test$TIMESTAMP
dataf = cbind(TIMESTAMP, predicciones)
data.v = merge(dataf, df, by = "TIMESTAMP")
data.v = data.v[,c("CebollarPTAPM", "predicciones")]

gof(data.v$predicciones, data.v$CebollarPTAPM)


# indices donde exista NA en estaciones 
datos.Na = is.na(Estaciones$CebollarPTAPM)
datos.relleno = Estaciones[datos.Na,]
valores.pred = predict(model.c$finalModel, datos.relleno)
valores.pred

# Rellenar los datos faltantes
Estaciones$CebollarPTAPM[datos.Na] = valores.pred
