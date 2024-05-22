df = read.csv("C:/Users/Jonna/Desktop/Randon_Forest/Caudales/YanuncayAjTarqui.csv", header = TRUE, sep = ",")
df = df[, c(1, 2)]

analisis.caudales - function(df){
  names(df) = c("TIMESTAMP", "Nivel")
  df$TIMESTAMP = as.POSIXct(df$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  df$Nivel = as.numeric(df$Nivel)
  
  df.diario = split(df$Nivel, cut(df$TIMESTAMP, "day"))
  
  df.final = list()
  
  for (i in 1:length(df.diario)){
    q1 = quantile(df.diario[[i]], 0.25)
    q3 = quantile(df.diario[[i]], 0.75)
    iqr = q3 - q1
    limite_inferior = q1 - 1.5 * iqr
    limite_superior = q3 + 1.5 * iqr
    # guardamos en la lista df.final solo los datos que esten dentro de los limites
    df.final[[i]] = df.diario[[i]][df.diario[[i]] >= limite_inferior & df.diario[[i]] <= limite_superior]
  }
  
  length(df.final[[5]])
}

df = ordenar