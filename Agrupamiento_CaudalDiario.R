library(dplyr)
library(tidyr)
library(svDialogs)

df = read.csv("C:/Users/Jonna/Desktop/Randon_Forest/Caudales/YanuncayAjTarqui.csv", header = TRUE, sep = ",")
df = df[, c(1, 2)]

analisis.caudales = function(df){
  names(df) = c("TIMESTAMP", "Nivel")
  df$TIMESTAMP = as.POSIXct(df$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  df$Nivel = as.numeric(df$Nivel)
  date.min = min(df$TIMESTAMP)
  date.max = max(df$TIMESTAMP)
  seq.date = seq(date.min, date.max, by = "5 min")
  level = length(seq.date)
  df.join = data.frame(TIMESTAMP = seq.date, Nivel = NA)
  
  df.1 <- left_join(df.join, df, by = "TIMESTAMP")
  df.1$Nivel = ifelse(is.na(df.1$Nivel.y), df.1$Nivel.x, df.1$Nivel.y)
  df.1 = df.1[, c(1, 4)]
  
  df.diario = split(df.1$Nivel, cut(df.1$TIMESTAMP, "day"))
  
  df.final = list()
  
  for (i in 1:length(df.diario)){
    q1 = quantile(df.diario[[i]], 0.25, na.rm = TRUE)
    q3 = quantile(df.diario[[i]], 0.75, na.rm = TRUE)
    iqr = q3 - q1
    limite_inferior = q1 - 1.5 * iqr
    limite_superior = q3 + 1.5 * iqr
    # guardamos en la lista df.final solo los datos que estén dentro de los limites
    df.final[[i]] = df.diario[[i]][df.diario[[i]] >= limite_inferior & df.diario[[i]] <= limite_superior]
  }
  
  for (i in 1:length(df.final)){
    longitud = length(df.final[[i]])
    if (any(longitud) > 288){
      dlg_message("La cantidad de datos es mayor a 288")
      stop()
    }
  }
  
  l.max = as.numeric(round(288 * 0.10,0))
  
  promedios = list()
  Nas = list()
  Nas.atip = list()
  for (i in 1:length(df.final)){
    Nas[[i]] = sum(is.na(df.final[[i]]))
    if (Nas[[i]] > l.max){
      promedios[[i]] = NA
    } else {
      promedios[[i]] = mean(df.final[[i]], na.rm = TRUE)
    }
    Nas.atip[[i]] = sum(is.na(df.final[[i]]))
  }
  
  fechas.1 = as.Date(df$TIMESTAMP, format = "%Y-%m-%d")
  fechas.1 = unique(fechas.1)
  fecha.minima = min(fechas.1)
  fecha.maxima = max(fechas.1)
  seq.diario = seq(fecha.minima, fecha.maxima, by = "day")
  df.final_diario = data.frame(TIMESTAMP = seq.diario, Nivel = unlist(promedios))
  
  # Generamos reporte ----------------------------------------------------------
  
  Nas.org = list()
  for (i in 1:length(df.diario)){
    Nas.org[[i]] = sum(is.na(df.diario[[i]]))
  }
  
  Atipicos = list()
  Nas.puros = list()
  
  for (i in 1:length(Nas.org)){
    Nas.puros[[i]] = Nas.org[[i]]
    for (j in 1:length(Nas)){
      Atipicos[[j]] = ifelse(Nas[[i]]  != Nas.org[[i]], Nas[[j]] - Nas.org[[i]], 0)
    }
  }
  
  reporte = data.frame(Fecha = seq.diario, Nas.puros = unlist(Nas.puros), Atipicos = unlist(Atipicos))
  reporte.informe = data.frame(
    Fecha.inicio = fecha.minima,
    Fecha.fin = fecha.maxima,
    Nas.total = sum(is.na(df.1$Nivel)),
    Nas.puros = sum(reporte$Nas.puros),
    Nas.atipicos = sum(reporte$Atipicos)
  )
  
  reporte.informe <<- reporte.informe
  return(df.final_diario)
}

YanuncayAjTarqui = analisis.caudales(df)

  
  
  
