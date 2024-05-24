prec = read.csv("C:/Users/Jonna/Desktop/Randon_Forest/Algoritmo RF_2/Downscalling/prec_microcuencas/Micro_Yanuncay.csv", header = TRUE)
prec = prec[, c("Fecha", "mod_CuantilMapping")]
names(prec) = c("Fecha", "prec")
prec$Fecha = as.Date(prec$Fecha, format = "%Y-%m-%d")

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

df = merge(prec, caudal, by = "Fecha", all = TRUE)
df = df[,c("caudal", "prec")]
data = na.omit(df)

# Función para desplazar la serie temporal
shift <- function(x, n) {
  c(rep(NA, n), x)[1:length(x)]
}

# Definir el rango de desfase (por ejemplo, de 0 a 10 días)
tlag_range = 0:10

# Crear un dataframe para almacenar las correlaciones
correlations = data.frame(Tlag = tlag_range, Correlation = NA)

# Calcular la correlación para cada desfase
for (tlag in tlag_range) {
  df$Precipitation_Tlag = shift(df$prec, tlag)
  correlations$Correlation[correlations$Tlag == tlag] = cor(df$caudal, df$Precipitation_Tlag, use = "complete.obs")
}

# Visualizar la correlación en función del Tlag
library(ggplot2)

ggplot(correlations, aes(x = Tlag, y = Correlation)) +
  geom_line() +
  geom_point() +
  labs(title = "Correlación entre Caudal y Precipitación con Desfase",
       x = "Desfase (días)",
       y = "Correlación") +
  theme_minimal()
