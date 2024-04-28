########################### Pre procesamiento de datos ##########################

# Autor: Jonnathan Landi es el mejor xdexdexdeeee 

# Autor: Marco Mogro1.1

# Fecha creación: 2024-02-05
# ------------------------------------------------------------------------------
# Fecha ultima modificación: 2024-02-09 (año-mes-día)
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
directory = "C:/Users/Jonna/OneDrive - ucuenca.edu.ec/Universidad/Tesis/Scripts R/Preprocesamiento de datos/Dependencias/Estaciones meteorologicas/"
name.estacion = "Datos procesados/Crudo/SayausiPTARM.csv"
# nombre.estat = sub("Datos_horarios/", "", name.estacion)
nombre.estat = "SayausiPTARM"
# ------------------------------------------------------------------------------
data = fread(paste(directory, name.estacion, sep = "")) 

# ------------------------------------------------------------------------------
####################### Funciones utilizadas (Terminado ) ######################
control.general = function(df) {
  # df = data # eliminar esto al final
  df = df %>% select(c("TIMESTAMP", "Lluvia_Tot")) %>%
    mutate(TIMESTAMP = as.POSIXct(TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")) %>%
    mutate(prec = as.numeric(Lluvia_Tot)) %>% select(c("TIMESTAMP", "prec"))
  
  
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
  #df = data # Eliminar esto al final
  setwd(directory)
  Limite.superior = 10 #mm
  Limite.inferior = 0 #mm
  
  indices.inferior = which(df$prec < Limite.inferior)
  indices.superior = which(df$prec > Limite.superior)
  
  atipipico.maximo = 30
  indice.atipico = which(df$prec > atipipico.maximo)
  if (any(indice.atipico)) {
    dlg_message("Se encontraron datos atípicos superiores a 30 mm. Se procederá a eliminarlos.")
    # eliminar filas con datos atípicos
    datos.umbral30= df[indice.atipico, ]
    Atipico.Sup30mm <<- datos.umbral30
    df$prec[indice.atipico] = NA
  }
  
  # divido mi df POR AÑOS
  año = year(df$TIMESTAMP)
  año = unique(año)
  
  par(mfrow = c(3, 4))
  graficos = list()
  
  # Creo carpera para guardar los gráficos
  if (!dir.exists(paste0(directory, "G_rangoFijoCrudo"))) {
    dir.create(paste0(directory, "G_rangoFijoCrudo"))
  }
  
  for (i in 1:length(año)) {
    df.año = df %>% filter(year(TIMESTAMP) == año[i])
    p.1 = ggplot(df.año, aes(x = TIMESTAMP, y = prec)) +
      geom_line(color = "blue") +
      labs(title = paste("Precipitación en el año", año[i]), x = "Fecha", y = "Precipitación (mm)") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    graficos[[i]] = p.1
  }
  
  # guardo mi gráfico en la carpeta
  # nombre.estat = sub("_min5.csv", "", name.estacion)
  p.f = grid.arrange(grobs = graficos)
  ggsave(paste0(directory, "G_rangoFijoCrudo/", nombre.estat, "_rangoFijo.png"),
         plot = p.f, width = 12, height = 8, units = "in", dpi = 300, type = "cairo")
  
  # . -----------------------------------------------------------------------
  
  if (any(indices.inferior)) {
    dlg_message("Se encontraron valores inferiores al limite inferior. Se procederá a eliminarlos.")
    datos.inferior = df[indices.inferior, ]
    datos.LimiteInf <<- datos.inferior
    
    # eliminar filas con datos fuera del rango
    df$prec[indices.inferior] = NA #Pruebas realizadas y funciona correctamente
    
  } else {
    dlg_message("No se encontraron valores inferiores al limite inferior.")
  }
  
  if (any(indices.superior)) {
    dlg_message(paste("Se encontraron valores superiores al limite superior. Antes de eliminarlos reviselos los graficos generados en", paste0(directory, "G_rangoFijoCrudo/", nombre.estat, "_rangoFijo.png")))
    datos.superior = df[indices.superior, ]
    datos.LimiteSup <<- datos.superior
    View(datos.superior)
    
    res = dlg_message("Desea eliminar los datos superiores al limite superior?", type = "yesno")$res
    if (res == "yes") {
      df$prec[indices.superior] = NA
    } else {
      df = df
    }
    
  } else {
    dlg_message("No se encontraron valores superiores al limite superior.")
  }
  
  
  # 
  # if (any(indices.superior)) {
  #   dlg_message("Se encontraron valores superiores al limite superior. Se procederá a verificar los datos.")
  #   datos.superior = df[indices.superior, ]
  #   datos.LimiteSup <<- datos.superior
  # 
  #   k = as.numeric(dlg_input("Ingrese el número de estaciones cercanas para la comprobación de los datos superiores al limite superior")$res)
  #   if (k == 1) {
  #     dlg_message("Se cargará una estación cercana para la comprobación de los datos superiores al limite superior")
  #     estacion.1 = dlg_open("Seleccione el archivo de datos de la estación cercana")$res
  #     name.1 = basename(estacion.1)
  #     estacion.1 = read.table(estacion.1, header = TRUE, sep = ",")
  #     estacion.1  = control.general(estacion.1)
  #     estacion =  which(estacion.1$TIMESTAMP %in% datos.superior$TIMESTAMP)
  #     estacion.1 = estacion.1[estacion, ]
  #     
  #     name.1 = sub("_min5.csv", "", name.1)
  #     name.o = sub("_min5.csv", "", name.estacion)
  #     
  #     comparacion = merge(datos.superior, estacion.1, by = "TIMESTAMP", all = TRUE)
  #     names(comparacion) = c("TIMESTAMP", paste("prec_", name.o, sep = ""), paste("prec_", name.1, sep = ""))
  #     comparacion.estaciones <<- comparacion
  #     View(comparacion)
  #     
  #     res = dlg_message("Desea eliminar los datos superiores al limite superior?", type = "yesno")$res
  #     if (res == "yes") {
  #       df$prec[indices.superior] = NA
  #     } else {
  #       df = df
  #     }
  # 
  #   } else if (k == 2) {
  #     dlg_message("Se cargarán dos estaciones cercanas para la comprobación de los datos superiores al limite superior")
  #     estacion.1 = dlg_open("Seleccione el archivo de datos de la primera estación cercana")$res
  #     name.1 = basename(estacion.1)
  #     estacion.2 = dlg_open("Seleccione el archivo de datos de la segunda estación cercana")$res
  #     name.2 = basename(estacion.2)
  #     estacion.1 = read.table(estacion.1, header = TRUE, sep = ",")
  #     estacion.2 = read.table(estacion.2, header = TRUE, sep = ",")
  #     estacion.1 = control.general(estacion.1)
  #     estacion.2 = control.general(estacion.2)
  #     
  #     estacion =  which(estacion.1$TIMESTAMP %in% datos.superior$TIMESTAMP)
  #     estacion.1 = estacion.1[estacion, ]
  #     
  #     estacion2 =  which(estacion.2$TIMESTAMP %in% datos.superior$TIMESTAMP)
  #     estacion.2 = estacion.2[estacion2, ]
  #     
  #     name.1 = sub("_min5.csv", "", name.1)
  #     name.2 = sub("_min5.csv", "", name.2)
  #     name.o = sub("_min5.csv", "", name.estacion)
  #     
  #     comparacion = merge(datos.superior, estacion.1, by = "TIMESTAMP", all = TRUE)
  #     comparacion = merge(comparacion, estacion.2, by = "TIMESTAMP", all = TRUE)
  #     names(comparacion) = c("TIMESTAMP", paste("prec_", name.o, sep = ""), paste("prec_", name.1, sep = ""), paste("prec_", name.2, sep = ""))
  #     comparacion.estaciones <<- comparacion
  #     View(comparacion)
  
  #     res = dlg_message("Desea eliminar los datos superiores al limite superior?", type = "yesno")$res
  #     if (res == "yes") {
  #       df$prec[indices.superior] = NA
  #     } else {
  #       df = df
  #     }
  #     
  #   } else {
  #     dlg_message("El script no soporta mas de dos estaciones cercanas")
  #     stop("Se admite unicamente uno o dos estaciones cercanas")
  #   }
  # } else {
  #   dlg_message("No se encontraron valores superiores al limite superior.")
  # }
  # directorio.save = paste0(directory, "Datos.1/")
  # write.csv(df, paste0(directorio.save, name.estacion), row.names = FALSE)
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


datos.faltantes.mensual = function(df) {
  #df = data  # Eliminar esto al final
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
  
  fechas.unique = format(fechas.1, "%Y-%m")
  fechas.unique = data.frame(fechas.unique)
  fechas.unique = unique(fechas.unique)
  
  fechas.mensual = lapply(seq_along(fechas.unique), function(i) {
    as.Date(paste0(fechas.unique[[i]], "-01"), format = "%Y-%m-%d")
  })
  
  fechas.mensual = data.frame(fecha = do.call("c", fechas.mensual))
  
  resultados = lapply(1:nrow(fechas.mensual), function(i) {
    Li = as.Date(fechas.mensual[i, "fecha"])
    # Obtener el último día del mes
    Ls = ceiling_date(Li, "month") - days(1)
    
    
    fechas.filtradas = fechas %>%
      filter(fechas >= Li & fechas <= Ls)
    # Contar el número de fechas filtradas
    num.fechas = nrow(fechas.filtradas)
    
    return(num.fechas)
  })
  
  fecha.fin = fechas.mensual
  fecha.fin$fecha = fecha.fin$fecha + months(1) - days(1)
  Nas = data.frame(fecha = fecha.fin$fecha, num.fechas = unlist(resultados))
  names(Nas) = c("fecha", "Na")
  
  
  fechas.comparativa = seq(as.Date(format(fecha.minima, "%Y-%m-01")), as.Date(format(fecha.maxima, "%Y-%m-01")), by = "1 month") - 1
  fechas.comparativa = data.frame(fechas.comparativa)
  fechas.comparativa = fechas.comparativa[-1,]
  fechas.comparativa = data.frame(fechas.comparativa)
  names(fechas.comparativa) = c("fecha")
  
  conteo = merge(fechas.comparativa, Nas, by = "fecha", all = TRUE)
  conteo = data.frame(conteo)
  conteo$Na[is.na(conteo$Na)] = 0
  conteo$dias = as.numeric(format(conteo$fecha, "%d"))
  
  # Calcular el porcentaje de datos faltantes considerando el número real de días en cada mes
  conteo = conteo %>%
    mutate(porcentaje = round((Na / dias) * 100, 1))
  
  
  # Renombrar las columnas
  names(conteo) <- c("fecha", "Na", "dias_mes", "% Datos_faltantes")
  
  Total.mensual <<- conteo
  
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
  
  
  reporte.mensual <<- reporte
  dlg_message("Se ha generado un reporte de datos faltantes. Verificar el objeto 'reporte.mensual' ")
  
  # Agrupación de datos de forma diaria --------------------------------------
  
  faltantes = mayor.umbral
  names(faltantes)[1] = "TIMESTAMP"
  
  df.1 = df  %>%
    mutate(TIMESTAMP = ceiling_date(TIMESTAMP, "month") - days(1)) %>%
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
  if (!dir.exists(paste0(directory, "G_agrupacionMensual"))) {
    dir.create(paste0(directory, "G_agrupacionMensual"))
  }
  
  for (i in 1:length(año)) {
    df.año = df %>% filter(year(TIMESTAMP) == año[i])
    p.1 = ggplot(df.año, aes(x = TIMESTAMP, y = prec)) +
      geom_line(color = "blue") +
      labs(title = paste("Precipitación en el año", año[i]), x = "Fecha", y = "Precipitación (mm)") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    graficos[[i]] = p.1
  }
  
  # guardo mi gráfico en la carpeta
  
  p.f = grid.arrange(grobs = graficos)
  ggsave(paste0(directory, "G_agrupacionMensual/", nombre.estat, "_mensual.png"),
         plot = p.f, width = 12, height = 8, units = "in", dpi = 300, type = "cairo")
  
  
  # Guardo los datos diarios
  if (!dir.exists(paste0(directory, "Datos_Mensuales"))) {
    dir.create(paste0(directory, "Datos_Mensuales"))
  }
  
  write.csv(df, paste0(directory, "Datos_Mensuales/", nombre.estat,".csv"), row.names = FALSE)
  
  return(df)
}


####################### Funciones utilizadas (En desarrollo)####################



datos.atipicos = function(df) {
  # Nota: Debo realizar la imputación de datos faltantes antes de realizar el control de datos atípicos
  df = data.diario
  Q.1 = quantile(df$prec, 0.25 )
  Q.3 = quantile(df$prec, 0.75)
  IQR = Q.3 - Q.1
  atipico.inferior = Q.1 - (1.5 * IQR)
  atipico.superior = Q.3 + (1.5 * IQR)
  # indices de datos atipicos
  indices.atipicos = which(df$prec < atipico.inferior | df$prec > atipico.superior)
  datos.atipicos = df[indices.atipicos, ]
  datos.atipicos <<- datos.atipicos
}

# ------------------------------------------------------------------------------
####################### Ejecución de control de datos ##########################
data = control.general(data)  #Control de datos generales
data = control.rangoFijo(data) # Control de rango fijo 

data = datos.faltantes.horario(data) # Control de datos faltantes horarios
data = datos.faltantes.diario(data) # Control de datos faltantes diarios
data = datos.faltantes.mensual(data) # Control de datos faltantes mensuales


umbral = 5 # % umbral máximo para datos faltantes por día
registro.faltantes = datos.faltantes(data) # Control de datos faltantes
data.diario = agrupacion.diario(data) # Agrupación de datos diarios
data.diario = relleno.datos(data.diario) # Relleno de datos faltantes
data.atipicos = datos.atipicos(data.diario) # Control de datos atípicos
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
####################### Detección de errores ###################################
# 1. Análisis de Normalidad
normalidad = function(df) {
  #  options(digits = 20)
  # prueba de liliefors
  test.lillie = lillie.test(df$prec)
  # Prueba de Anderson-Darling
  test.anderson = ad.test(df$prec)
  
  p.value.lillie = test.lillie$p.value
  p.value.ad = ad.test(df$prec)$p.value
  
  if (p.value.lillie < 0.05 & p.value.ad < 0.05) {
    dlg_message("El test de lillie y Anderson-Darling rechazan la hipotesis nula, los datos no siguen una distribución normal")
  } else if (p.value.lillie < 0.05 & p.value.ad > 0.05) {
    dlg_message("El test de lillie rechaza la hipotesis nula, El test de Anderson-Darling no rechaza la hipotesis nula")
  } else if (p.value.lillie > 0.05 & p.value.ad < 0.05) {
    dlg_message("El test de lillie no rechaza la hipotesis nula, El test de Anderson-Darling rechaza la hipotesis nula")
  } else {
    dlg_message("Los dos test aplicados aceptan Ho. Los datos siguen una distribución normal")
  }
  
  print(test.lillie)
  print(test.anderson)
  
}

test.diario = normalidad(data.mensual)
# ------------------------------------------------------------------------------
# 2. Análisis de datos atípicos
plot(data.mensual$prec)
boxplot(data.mensual$prec, main = "Boxplot de precipitacion diaria", ylab = "Precipitacion (mm)", col = "lightblue", border = "blue")
# Filtrar valores no atípicos
data_sin_outliers <- subset(data.diario, prec < quantile(data.diario$prec, 0.95))

# Ver resumen después de eliminar los outliers
summary(data_sin_outliers)

