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
  
  # df$Caudal = NA
  # 
  # # Aplico las fórmulas de caudal según el nivel
  # for (i in seq_along(df$nivel)) {
  #   nivel = df$nivel[i]
  #   if (!is.na(nivel)) {
  #     if (nivel < 95) {
  #       df$Caudal[i] = 0.0116 * (nivel^1.086)
  #     } else {
  #       df$Caudal[i] = 0.00000145966 * ((nivel - 20)^3.3435)
  #     }
  #   }
  # }
  
  # # selecciono las columnas que me interesan
  df = df[,c("TIMESTAMP", "nivel")]
  # df = df[,c("TIMESTAMP", "Caudal")]
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
corelacion = function(df.1, df.2, res){
  if (res == "y"){
    df.1 = convertir.prec(df.1)
    df.2 = convertir.caudal(df.2)
    df = merge(df.1, df.2, by = "Fecha", all = TRUE)
    names(df) = c("Fecha", "prec", "caudal")
    Fechas = data.frame(df$Fecha)
    Tlag_1 = T.lag(df$prec, 1)
    Tlag_2 = T.lag(df$prec, 2)
    Tlag_3 = T.lag(df$prec, 3)
    
    Tlag_1 = cbind(Fechas, Tlag_1)
    names(Tlag_1) = c("Fecha", "Tlag_1")
    Tlag_2 = cbind(Fechas, Tlag_2)
    names(Tlag_2) = c("Fecha", "Tlag_2")
    Tlag_3 = cbind(Fechas, Tlag_3)
    names(Tlag_3) = c("Fecha", "Tlag_3")
    
    anal.cor = function(f.2){
      df.c = merge(df.2, f.2, by = "Fecha", all = TRUE)
      correlac = cor(df.c[,-1], use = "complete.obs")
      return(correlac)
    }
    
    cor_1 = anal.cor(df.1)
    cor_2 = anal.cor(Tlag_1)
    cor_3 = anal.cor(Tlag_2)
    cor_4 = anal.cor(Tlag_3)
    
    resultados = data.frame(
      Caud_prec = cor_1[1,2],
      Caud_Tlag_1 = cor_2[1,2],
      Caud_Tlag_2 = cor_3[1,2],
      Caud_Tlag_3 = cor_4[1,2]
    )
    
    resultados_correlacion <<- resultados
    
    
  } else {
    df.1 = convertir.caudal(df.1)
    df.2 = convertir.caudal(df.2)
    df = merge(df.1, df.2, by = "Fecha", all = TRUE)
    names(df) = c("Fecha", "Caudal.orig", "caudal.cercan")
    correlac = cor(df[,-1], use = "complete.obs")
    
    resultados = data.frame(
      Caudalorig_caudal.cercan = correlac[1,2]
    )
    
    resultados_correlacion <<-  resultados
  }
}

# Nota: Si se analiza correlación entre prec y caudal df.1 = prec, df.2 = caudal
# Nota: Si se analiza correlación entre caudal y caudal df.1 = caudal original, df.2 = caudal cercano

################################################################################

caudal = read.csv("C:/Users/Jonna/Desktop/Nueva carpeta/Yanuncay_diario.csv", header = TRUE)
caudal = convertir.caudal(caudal)
summary(caudal)

# selecciono todas las filas q sean menores a 300
ind.1 = which(caudal$caudal > 400)
caudal$caudal[ind.1] = NA
summary(caudal)


# precipitaciones
# prec = read.csv("C:/Users/Jonna/Desktop/Nueva carpeta/Huizhil.csv", header = TRUE)
prec = read.csv("C:/Users/Jonna/Desktop/Nueva carpeta/Cebollar.csv", header = TRUE)
prec_h = read.csv("C:/Users/Jonna/Desktop/Randon_Forest/Estaciones_Tierra/Diario/Huizhil.csv", header = TRUE)
prec_y = read.csv("C:/Users/Jonna/Desktop/Nueva carpeta/YanuncayPucan.csv", header = TRUE)

correlacion = corelacion(prec_h, caudal, "y")
