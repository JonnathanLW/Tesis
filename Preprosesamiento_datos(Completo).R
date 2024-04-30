########################### Pre procesamiento de datos ##########################
# Autor: Jonnathan Landi
# Fecha creación: 2024-02-05
# ------------------------------------------------------------------------------
# Fecha ultima modificación: 2024-04-29 (año-mes-día)
# Autor de ultima modificación: Jonnathan Landi
# Versión: 2.6.0
# ------------------------------------------------------------------------------
# ESTADO: En desarrollo
###########(En desarrollo, En revisión, Refactorización, Terminado) ############
# ------------------------------------------------------------------------------
################################################################################
# Librerías necesarias ---------------------------------------------------------
library(data.table)
library(dplyr)
library(svDialogs)
library(nortest)
library(outliers)
library(tidyr)
library(lubridate)
library(tools)
library(gridExtra)
library(ggplot2)
# ------------------------------------------------------------------------------
directory = "C:/Users/Jonna/Desktop/Downscalling/Estaciones/"
name.estacion = "Ventanas.csv"
nombre.estat = "Ventanas"
# ------------------------------------------------------------------------------
data = fread(paste(directory, name.estacion, sep = "")) 

# ------------------------------------------------------------------------------
####################### Funciones utilizadas (Terminado ) ######################
control.general = function(df) {
  
  # Buscar fechas repetidas
  indices.duplicados = duplicated(df$TIMESTAMP) | duplicated(df$TIMESTAMP, fromLast = TRUE)
  
  if (any(indices.duplicados)) {
    dlg_message("Se encontraron fechas duplicadas en el archivo de datos. Se procederá a eliminarlas.")
    datos.duplicados  = df[indices.duplicados, ]
    datos.duplicados <<- datos.duplicados
    
    # eliminar filas con datos duplicados
    df = df[!indices.duplicados, ]
  }
  
  
  # Agregar secuencia completa a los datos cada 5 minutos
  fecha.minima = min(df$TIMESTAMP)
  fecha.maxima = max(df$TIMESTAMP)
  seq.fechas = seq(fecha.minima, fecha.maxima, by = "5 min")
  
  if (length(seq.fechas) != length(df$TIMESTAMP)) {
    dlg_message("Se encontraron fechas faltantes en el archivo de datos. Se procederá a completarlas con la secuencia completa.")
  }
  
  df = merge(df, data.frame(TIMESTAMP = seq.fechas), by = "TIMESTAMP", all = TRUE) # quitar all = True para evitar luego ver si hay secuencia cada 5 min
  
  # Agregar comparación para ver si los datos están cada 5 minutos.
  # Beta de verificar que los datos estén en secuencia cada 5 minutos ------------
  # Calculo la diferencia entre marcas de tiempo consecutivas
  diferencia.minutos = diff(df$TIMESTAMP, units = "mins")
  
  #verifico si tosas las diferencias son iguales a 5 minutos
  if (all(diferencia.minutos == 5)) {
    dlg_message("Los datos están en secuencia cada 5 minutos")
  } else {
    dlg_message("Los datos no están en secuencia cada 5 minutos, se procedera a elimiar los valores intermedios")
    df = merge(df, data.frame(TIMESTAMP = seq.fechas), by = "TIMESTAMP")
  }
  
  df = df[order(df$TIMESTAMP),]
  # . -----------------------------------------------------------------------
  return(df)
}

control.rangoFijo = function(df) {
  # ----------------------------------------------------------------------------
  # Limites duros segun etapa
  # precipitacion
  L.S.prec = 10 #mm
  L.I.prec = 0 #mm
  
  # Temperaturas
  L.S.temp = 45 #°C
  L.I.temp = -20 #°C
  
  # Humedad relativa
  L.S.hum = 100 #%
  L.I.hum = 0 #%
  
  # Velocidad del viento
  L.S.viento = 30 #km/h
  L.I.viento = 0 #km/h
  
  # direccion del viento
  L.S.dir = 360 #°
  L.I.dir = 0 #°
  
  # Radiación solar
  L.S.rad = 1500 #W/m2
  L.I.rad = 0 #W/m2
  
  # Evaporación
  L.S.eva = 8 #mm
  L.I.eva = 0 #mm
  # ----------------------------------------------------------------------------
 
  ind.L.S.precep = which(df$Lluvia_Tot > L.S.prec)
  ind.L.I.precep = which(df$Lluvia_Tot < L.I.prec)
  
  ind.L.S.TempMin = which(df$TempAire_Min > L.S.temp)
  ind.L.I.TempMin = which(df$TempAire_Min < L.I.temp)
  
  ind.L.S.TempMax = which(df$TempAire_Max > L.S.temp)
  ind.L.I.TempMax = which(df$TempAire_Max < L.I.temp)
  
  ind.L.I.TempAvg = which(df$TempAire_Avg < L.I.temp)
  ind.L.S.TempAvg = which(df$TempAire_Avg > L.S.temp)
  
  ind.L.S.HumMax = which(df$HumAire_Max > L.S.hum)
  ind.L.I.HumMax = which(df$HumAire_Max < L.I.hum)
  
  ind.L.S.HumMin = which(df$HumAire_Min > L.S.hum)
  ind.L.I.HumMin = which(df$HumAire_Min < L.I.hum)
  
  ind.Ls.HumAvg = which(df$HumAire_Avg > L.S.hum)
  ind.L.I.HumAvg = which(df$HumAire_Avg < L.I.hum)
  
  ind.L.S.VV_Max = which(df$VV_Max > L.S.viento)
  ind.L.I.VV_Max = which(df$VV_Max < L.I.viento)
  
  ind.L.S.VV_Avg = which(df$VV_Avg > L.S.viento)
  ind.L.I.VV_Avg = which(df$VV_Avg < L.I.viento)
  
  # ind.L.S.VV_Min = which(df$VV_Min > L.S.viento)
  # ind.L.I.VV_Min = which(df$VV_Min < L.I.viento)
  
  ind.L.S.dir = which(df$DireccionViento > L.S.dir)
  ind.L.I.dir = which(df$DireccionViento < L.I.dir)
  
  ind.L.S.RS_W_Ma = which(df$RS_W_Max > L.S.rad)
  ind.L.I.RS_W_Ma = which(df$RS_W_Max < L.I.rad)
  
  ind.L.S.RS_W_Avg = which(df$RS_W_Avg > L.S.rad)
  ind.L.I.RS_W_Avg = which(df$RS_W_Avg < L.I.rad)
  
  ind.L.S.ETgrass = which(df$ETgrass > L.S.eva)
  ind.L.I.ETgrass = which(df$ETgrass < L.I.eva)
  
  # ----------------------------------------------------------------------------
  # Elimino todos los valores que sean mayores a los limites superiores e inferiores
  # ----------------------------------------------------------------------------
  df$Lluvia_Tot[ind.L.S.precep] = NA
  df$Lluvia_Tot[ind.L.I.precep] = NA
  df$TempAire_Min[ind.L.S.TempMin] = NA
  df$TempAire_Min[ind.L.I.TempMin] = NA
  df$TempAire_Max[ind.L.S.TempMax] = NA
  df$TempAire_Max[ind.L.I.TempMax] = NA
  df$TempAire_Avg[ind.L.S.TempAvg] = NA
  df$TempAire_Avg[ind.L.I.TempAvg] = NA
  df$HumAire_Max[ind.L.S.HumMax] = NA
  df$HumAire_Max[ind.L.I.HumMax] = NA
  df$HumAire_Min[ind.L.S.HumMin] = NA
  df$HumAire_Min[ind.L.I.HumMin] = NA
  df$HumAire_Avg[ind.Ls.HumAvg] = NA
  df$HumAire_Avg[ind.L.I.HumAvg] = NA
  df$VV_Max[ind.L.S.VV_Max] = NA
  df$VV_Max[ind.L.I.VV_Max] = NA
  df$VV_Avg[ind.L.S.VV_Avg] = NA
  df$VV_Avg[ind.L.I.VV_Avg] = NA
  # df$VV_Min[ind.L.S.VV_Min] = NA
  # df$VV_Min[ind.L.I.VV_Min] = NA
  df$DireccionViento[ind.L.S.dir] = NA
  df$DireccionViento[ind.L.I.dir] = NA
  df$RS_W_Max[ind.L.S.RS_W_Ma] = NA
  df$RS_W_Max[ind.L.I.RS_W_Ma] = NA
  df$RS_W_Avg[ind.L.S.RS_W_Avg] = NA
  df$RS_W_Avg[ind.L.I.RS_W_Avg] = NA
  df$ETgrass[ind.L.S.ETgrass] = NA
  df$ETgrass[ind.L.I.ETgrass] = NA
  return(df)
}


datos.faltantes.horario = function(df){
  #  df = data  # Eliminar esto al finalizar el desarrollo
  umbral.min = 10 # examinar esto
  umbral.max = 15
  
  fecha.minima = min(df$TIMESTAMP)
  fecha.maxima = max(df$TIMESTAMP)
  indices = which(is.na(df$prec))
  fechas = df$TIMESTAMP[indices]
  fechas = data.frame(fechas)
  
  fechas.1 = as.Date(fechas$fechas, format = "%Y-%m-%d")
  fechas.1 = unique(fechas.1)
  fechas.1 = data.frame(fechas.1)
  
  resultados = lapply(1:nrow(fechas.1), function(i) {
    Li = as.POSIXct(paste0(fechas.1[i, ], " 00:00:00"), tz = "UTC")
    Ls = as.POSIXct(paste0(fechas.1[i, ], " 23:55:00"), tz = "UTC")
    horas = seq(Li, Ls, by = "hour")
    num_fechas.horaria = sapply(horas, function(hora) {
      fechas.filtradas = fechas %>%
        filter(fechas >= hora & fechas < hora + hours(1))
      
      return(nrow(fechas.filtradas))
      
    })
    return(num_fechas.horaria)
  })
  
  horas.seq = lapply(1:nrow(fechas.1), function(i) {
    Li = as.POSIXct(paste0(fechas.1[i, ], " 00:00:00"), tz = "UTC")
    Ls = as.POSIXct(paste0(fechas.1[i, ], " 23:55:00"), tz = "UTC")
    horas = seq(Li, Ls, by = "hour")
    return(horas)
  })
  
  horas.seq = do.call(rbind, lapply(horas.seq , function(x) data.frame(fecha = x)))
  Nas = data.frame(fecha = horas.seq$fecha, num.fechas = unlist(resultados))
  names(Nas) = c("fecha", "Na")
  
  # Error aqui, al hacer secuencia horaria no estoy teniendo que si inicio a las 4:40 la secuencia se hara de esa forma y no de forma correcta
  fechas.comparativa = seq(as.POSIXct(fecha.minima), as.POSIXct(fecha.maxima), by = "hour")
  fechas.comparativa = trunc(fechas.comparativa, "hour")
  fechas.comparativa = data.frame(fechas.comparativa)
  names(fechas.comparativa) = c("fecha")
  
  # posible error aquí 
  conteo = merge(fechas.comparativa, Nas, by = "fecha", all = TRUE)
  conteo = data.frame(conteo)
  conteo$Na[is.na(conteo$Na)] = 0
  conteo$porcentaje = round((conteo$Na / 12) * 100,2)
  names(conteo) = c("fecha", "Na", "% Datos_faltantes")
  summary.Nas <<- conteo
  
  mayor.umbral = conteo %>% filter(conteo$`% Datos_faltantes` > umbral.min)
  
  faltantes = mayor.umbral
  names(faltantes)[1] = "TIMESTAMP"
  
  df.1 = df %>% mutate(TIMESTAMP = as.POSIXct(TIMESTAMP)) %>%
    mutate(TIMESTAMP = floor_date(TIMESTAMP, "hour")) %>%
    anti_join(faltantes, by = "TIMESTAMP") %>%
    group_by(TIMESTAMP) %>%
    summarise(prec = sum(prec, na.rm = TRUE))
  
  fechas.completas = fechas.comparativa
  names(fechas.completas) = c("TIMESTAMP")
  df = merge(fechas.completas, df.1, by = "TIMESTAMP", all = TRUE)
  df = df[order(df$TIMESTAMP),]
  
  # En pruebas --------------------------------------------------------------
  # Descripción: Incorporar gráfico para ver la distribución de los datos que no fueron agrupados
  # de manera horaria, verificar como están distribuios y considerar agruparlos si el umbral 
  # es mayor al 15 %
  indices.vac = which(is.na(df$prec))
  fechas.vac = df$TIMESTAMP[indices.vac]
  fechas.vac = data.frame(fechas.vac)
  names(fechas.vac) = c("TIMESTAMP")
  
  data.rev = merge(fechas.vac, faltantes, by = "TIMESTAMP")
  
  indices.mayor15 = which(data.rev$`% Datos_faltantes` <= umbral.max)
  if (any(indices.mayor15)) {
    dlg_message("Se encontraron fechas con un porcentaje de datos faltantes menor al umbral máximo (15), revise los datos faltantes")
    stop("Se encontraron fechas con un porcentaje de datos faltantes menor al umbral máximo (15), revise los datos faltantes")
  }
  
  # Reporte -----------------------------------------------------------------
  
  # nombre.estat = sub("_min5.csv", "", name.estacion)
  faltantes = sum(is.na(df$prec))
  total = nrow(df)
  
  reporte.horario = data.frame(
    Estacion = nombre.estat,
    periodo_estudio = paste(fecha.minima, "al", fecha.maxima),
    Numero_años = as.numeric(year(fecha.maxima) - year(fecha.minima)),
    Datos_registrados = total,
    Datos_ausentes = faltantes,
    porcentaje_completos = round(100 - ((faltantes / total) * 100),2),
    porcentaje_ausentes = round((faltantes / total) * 100,2)
  )  
  
  reporte.horario <<- reporte.horario
  dlg_message("Se ha generado un reporte de datos faltantes. Verificar el objeto 'reporte.horario' ")
  
  # Guardo los datos horarios
  if (!dir.exists(paste0(directory, "Datos_horarios"))) {
    dir.create(paste0(directory, "Datos_horarios"))
  }
  
  write.csv(df, paste0(directory, "Datos_horarios/", nombre.estat, ".csv"), row.names = FALSE)
  return(df)
}

datos.faltantes.diario = function(df) {
  
  #  df = data  # Eliminar esto al final
  fecha.minima = min(df$TIMESTAMP)
  fecha.maxima = max(df$TIMESTAMP)
  umbral.min = 10 
  umbral.max = 15
  indices = which(is.na(df$prec))
  fechas = df$TIMESTAMP[indices]
  fechas = data.frame(fechas)
  
  fechas.1 = as.Date(fechas$fechas, format = "%Y-%m-%d")
  fechas.1 = unique(fechas.1)
  fechas.1 = data.frame(fechas.1)
  
  resultados = lapply(1:nrow(fechas.1), function(i) {
    Li = as.POSIXct(paste0(fechas.1[i, ], " 00:00:00"), tz = "UTC")
    Ls = as.POSIXct(paste0(fechas.1[i, ], " 23:55:00"), tz = "UTC")
    
    fechas.filtradas = fechas %>%
      filter(fechas >= Li & fechas <= Ls)
    # Contar el número de fechas filtradas
    num.fechas = nrow(fechas.filtradas)
    
    return(num.fechas)
  })
  
  
  Nas = data.frame(fecha = fechas.1$fechas.1, num.fechas = unlist(resultados))
  names(Nas) = c("fecha", "Na")
  
  
  fechas.comparativa = seq(as.Date(fecha.minima), as.Date(fecha.maxima), by = "1 day")
  fechas.comparativa = data.frame(fechas.comparativa)
  names(fechas.comparativa) = c("fecha")
  
  conteo = merge(fechas.comparativa, Nas, by = "fecha", all = TRUE)
  conteo = data.frame(conteo)
  conteo$Na[is.na(conteo$Na)] = 0
  conteo$porcentaje = round((conteo$Na / 24) * 100,1)
  names(conteo) = c("fecha", "Na", "% Datos_faltantes")
  
  Total.diario <<- conteo
  
  mayor.umbral = conteo %>% filter(conteo$`% Datos_faltantes` > umbral.min)
  faltantes = nrow(mayor.umbral)
  total = nrow(conteo)
  
  # construcción de tabla de reporte
  fecha.i = as.Date(fecha.minima)
  fecha.f = as.Date(fecha.maxima)
  # nombre.estat = sub("_min5.csv", "", name.estacion)
  # nombre.estat = sub("Datos_horarios/", "", nombre.estat)
  
  reporte = data.frame(
    Estacion = nombre.estat,
    periodo_estudio = paste(fecha.i, "al", fecha.f),
    Numero_años = as.numeric(year(fecha.f) - year(fecha.i)),
    Datos_registrados = total,
    Datos_ausentes = faltantes,
    porcentaje_completos = round(100 - ((faltantes / total) * 100),2),
    porcentaje_ausentes = round((faltantes / total) * 100,2)
  )
  
  
  reporte.diario <<- reporte
  dlg_message("Se ha generado un reporte de datos faltantes. Verificar el objeto 'reporte.diario' ")
  
  # Agrupación de datos de forma diaria --------------------------------------
  
  faltantes = mayor.umbral
  names(faltantes)[1] = "TIMESTAMP"
  
  df.1 = df %>% mutate(TIMESTAMP = as.POSIXct(TIMESTAMP)) %>%
    mutate(TIMESTAMP = floor_date(TIMESTAMP, "day")) %>%
    anti_join(faltantes, by = "TIMESTAMP") %>%
    group_by(TIMESTAMP) %>%
    summarise(prec = sum(prec, na.rm = TRUE))
  
  fechas.completas = fechas.comparativa
  names(fechas.completas) = c("TIMESTAMP")
  
  df = left_join(fechas.completas, df.1, by = "TIMESTAMP") %>%
    distinct() %>%
    arrange(TIMESTAMP)
  
  # Gráfico los resultados ---------------------------------------------------
  
  # divido mi df POR AÑOS
  año = year(df$TIMESTAMP)
  año = unique(año)
  
  par(mfrow = c(3, 4))
  graficos = list()
  
  # Creo carpera para guardar los gráficos
  if (!dir.exists(paste0(directory, "/G_agrupacionDiaria"))) {
    dir.create(paste0(directory, "/G_agrupacionDiaria"))
  }
  
  for (i in 1:length(año)) {
    df.año = df %>% filter(year(TIMESTAMP) == año[i])
    p.1 = ggplot(df.año, aes(x = TIMESTAMP, y = prec)) +
      geom_line(color = "blue") +
      labs(title = paste("Precipitación en el año", año[i]), x = "Fecha", y = "Precipitación (mm)") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    graficos[[i]] = p.1
  }
  
  p.f = grid.arrange(grobs = graficos)
  ggsave(paste0(directory, "G_agrupacionDiaria/", nombre.estat, "_diario.png"),
         plot = p.f, width = 12, height = 8, units = "in", dpi = 300, type = "cairo")
  
  
  # Guardo los datos diarios
  if (!dir.exists(paste0(directory, "/Datos_diarios"))) {
    dir.create(paste0(directory, "/Datos_diarios"))
  }
  
  write.csv(df, paste0(directory, "Datos_diarios/", nombre.estat, ".csv"), row.names = FALSE)
  
  return(df)
  
}