# Librerías necesarias ---------------------------------------------------------
library(dplyr)
library(ggplot2)
# Funciones complementarias ----------------------------------------------------
proces = function(df, name) {
  names(df) = c("date", name)
  df$date = as.Date(df$date, format = "%Y-%m-%d")
  return(df)
}

# Preparación de datos ---------------------------------------------------------
# Cargar de datos de SSI y SPI y merge
SPI = read.csv("C:/Users/Jonna/Desktop/Sequias/SPI.csv")
SSI = read.csv("C:/Users/Jonna/Desktop/Sequias/SSI.csv")
SPI = proces(SPI, "SPI")
# SPI$evento = seq(1, nrow(SPI))
SSI = proces(SSI, "SSI")

# Teoria de las corridas -------------------------------------------------------
teori.run = function(df) {
  df = SPI
  names(df) = c("date", "value")
  dry_thresh = -0.5
  wet_thresh = 0.5
  
  events = list()
  excesos = list()
  event_start = 0
  event_type = 0 # 0 = ninguno, -1 = seco, 1 = húmedo
  
  for (i in 1:nrow(df)) {
    if (df[i, 2] < dry_thresh) {
      df$estado[i] = "Sequia"
    } else if (df[i, 2] > wet_thresh) {
      df$estado[i] = "humedad"
    } else if (df[i, 2] >= dry_thresh & df[i, 2] <= wet_thresh){
      df$estado[i] = "normal"
    }
  }
  
  # Función para identificar las corridas
  identify_runs <- function(data) {
    runs <- rle(as.character(data$estado))
    runs_df <- data.frame(
      estado = runs$values,
      longitud = runs$lengths
    )
    return(runs_df)
  }
  corridas <- identify_runs(df)
  
  # Calcular duracion y severidad
  calculate_duration_severity <- function(data) {
    runs <- rle(as.character(data$estado))
    start_indices <- cumsum(c(1, head(runs$lengths, -1)))
    end_indices <- cumsum(runs$lengths)
    
    estado <- runs$values
    longitud <- runs$lengths
    severidad <- sapply(1:length(estado), function(i) {
      sum(data$SPI[start_indices[i]:end_indices[i]])
    })
    
    result <- data.frame(
      estado = estado,
      duracion = longitud,
      severidad = severidad
    )
    return(result)
  }
  
  # Aplicar la función al data.frame
  result <- calculate_duration_severity(df)
  
  
  
  
  
  
    
    if (event_type == -1) {
        event_end = i - 1
        event_dur = event_end - event_start + 1
        event_mag = sum(abs(df[event_start:event_end]))
        values = df[event_start:event_end]
        event_sev = sum(dry_thresh - (df[event_start:event_end]))
        events[[length(events) + 1]] = list(type = event_type, start = event_start, end = event_end, duration = event_dur, 
                                            values = values,
                                            magnitude = event_mag, 
                                            severity = event_sev)
        event_type = 0

      } else if (event_type == 1) {
        event_end = i - 1
        event_dur = event_end - event_start + 1
        event_mag = sum(abs(df[event_start:event_end]))
        values = df[event_start:event_end]
        event_sev = sum((df[event_start:event_end]) - dry_thresh)

        events[[length(events) + 1]] = list(type = event_type, start = event_start, end = event_end, duration = event_dur, 
                                            values = values,
                                            magnitude = event_mag, 
                                            severity = event_sev)
        event_type = 0
    } 
  }
  # 
  # sep.eventos = function(eventos, n){
  #   df = lapply(eventos, function(evento) {
  #     if (evento$type == n) {
  #       return(evento)
  #     } else {
  #       return(NULL)
  #     }
  #   })
  #   
  #   df = Filter(Negate(is.null), df)
  #   return(df)
  # }
  # 
  # sequias = sep.eventos(events, -1)
  # fechas = list()
  # vis.list = list()
  # for (i in 1:(length(sequias)-1)) {
  #   estart.date = sequias[[i]]$end
  #   end.date = sequias[[i+1]]$start
  #   vf = 0
  #   values = df[estart.date:end.date]
  #   for (j in 1:length(values)) {
  #     if (values[j] > dry_thresh) {
  #       vi = values[j] - dry_thresh
  #       vf = vf + vi  # Sumar el volumen de exceso al total
  #     
  #     }
  #   }
  #   vis.list[[length(vis.list) + 1]] = list(type = -1, start = estart.date, end = end.date, vf = vf)
  # }
  # # identificacion de eventos húmedos
  # humedos = sep.eventos(events, 1)
  # vis.humedos = list()
  # for (i in 1:(length(humedos)-1)) {
  #   estart.date = humedos[[i]]$end
  #   end.date = humedos[[i+1]]$start
  #   vf = 0
  #   values = df[estart.date:end.date]
  #   for (j in 1:length(values)) {
  #     if (values[j] > wet_thresh) {
  #       vi =  values[j] - wet_thresh
  #       vf = vf + vi  # Sumar el volumen de exceso al total
  #       vf.2 = sum((df[event_start:event_end]) - wet_thresh)
  #     }
  #   }
  #   vis.humedos[[length(vis.humedos) + 1]] = list(type = 1, start = estart.date, end = end.date, ev.humedos = vf, vf.2 = vf.2)
  # }
  
  return(list(events = events))
}


sep.eventos = function(eventos, n){
  df = lapply(eventos, function(evento) {
    if (evento$type == n) {
      return(evento)
    } else {
      return(NULL)
    }
  })
  
  df = Filter(Negate(is.null), df)
  return(df)
}

events.SPI = teori.run(SPI$SPI)
SPI.sequia = sep.eventos(events.SPI$events, -1)
SPI.humeda = sep.eventos(events.SPI$events, 1)
SPI.sequia = list(eventos_sequia = SPI.sequia, vol_excess = events.SPI$vi)
#-------------------------------------------------------------------------------
events.SSI = teori.run(SSI$SSI)
SSI.sequia = sep.eventos(events.SSI$events, -1)
SSI.humeda = sep.eventos(events.SSI$events, 1)
SSI.sequia = list(eventos_sequia = SSI.sequia, vol_excess = events.SSI$vi)
#-------------------------------------------------------------------------------
# Pooling ----------------------------------------------------------------------
# Función para agrupar eventos adyacentes
pooling = function(evento, tc, pc, vi){
  eventos_agrupados = list()
  # Verifica si la lista 'evento' está vacía
  if (length(evento) == 0) {
    stop("La lista de eventos está vacía")
  }
  i = 1
  while (i < length(evento)) {
    evento.actual = evento[[i]]
    evento.siguiente = evento[[i + 1]]
    t.i = evento.siguiente$start - evento.actual$end - 1
    v = vi[[i]]$vf
    s = evento.actual$severity
    p.i = v/s
    
    if (t.i <= tc && p.i <= pc) {
      evento.agrupado = list(type = evento.actual$type,
                             start = evento.actual$start,
                             end = evento.siguiente$end,
                             duration = evento.actual$duration + evento.siguiente$duration + t.i,
                             severity = evento.actual$severity + evento.siguiente$severity - v)
      eventos_agrupados[[length(eventos_agrupados) + 1]] = evento.agrupado
      i = i + 2
    } else {
      eventos_agrupados[[length(eventos_agrupados) + 1]] = evento.actual
      i = i + 1
    }
    
    if (i == length(evento)) {
      eventos_agrupados[[length(eventos_agrupados) + 1]] = evento[[i]]
    }

  }
  
  return(eventos_agrupados)
}


pooling.SPI = pooling(evento = SPI.sequia$eventos_sequia, tc = 5, pc = 0.4, vi = SPI.sequia$vol_excess)
pooling.SSI = pooling(evento = SSI.sequia$eventos_sequia, tc = 2, pc = 0.4, vi = SSI.sequia$vol_excess)

#-------------------------------------------------------------------------------
# Exclusión de eventos de sequía cortos/menores --------------------------------
# Función para excluir eventos de sequía cortos/menores
exclusion = function(evento, rd, rs){
  # evento = pooling.SPI
  eventos_filtrados = list()
  eventos_excluding = list()
  duracion_media = mean(sapply(evento, function(x) x$duration))
  severity_medio = mean(sapply(evento, function(x) x$severity))
  
  # modificar el y por el or
  for (i in 1:length(evento)) {
    if (evento[[i]]$duration < rd * duracion_media | evento[[i]]$severity < rs * severity_medio) {
      
      eventos_filtrados[[length(eventos_filtrados) + 1]] = evento[[i]]
      
    } else {
      eventos_excluding[[length(eventos_excluding) + 1]] = evento[[i]]
    }
  }
  return(eventos_excluding)
}
exclusion.SPI = exclusion(evento = pooling.SPI, rd = 0.3, rs = 0.3)
exclusion.SSI = exclusion(evento = pooling.SSI, rd = 0.3, rs = 0.3)
#-------------------------------------------------------------------------------
# intervalos de activación
trigger = function(evento, tp) {
  eventos_trigger = list()
  for (i in 1:(length(evento) -1)) {
    if(i == 1) {
      sequia = evento[[i]]
      start = sequia$start
      end = sequia$end
    } else {
      sequia = evento[[i]]
      start = sequia$start
      end = sequia$end
      
      sequia_1 = evento[[i-1]]
      start_1 = sequia_1$start
      end_1 = sequia_1$end
      
      tsi = start
      tei = end
      tei_1 = end_1
      compro = tsi - tei_1
      if (tsi - tei_1 >= tp) {
        i.act = tsi - tp
        trigger_interval = c(i.act, tei)
      } else {
        trigger_interval = c(tei_1, tei)
      }
      
      eventos_trigger[[length(eventos_trigger) + 1]] = list(type = sequia$type, 
                                                            start = start, 
                                                            end = end, 
                                                            compr = compro,
                                                            trigger_interval = trigger_interval)
      
    }
  }
  return(eventos_trigger)
}
trigger = trigger(evento = exclusion.SSI, tp = 3)

# Cálculo de la tasa de activación (Tr) ----------------------------------------
match_droughts = function(meteo_events, trigger_intervals, wet_events) {
  # trigger_intervals = trigger
  for (i in 1:length(trigger_intervals)) {
    i = 5
    start.interval = trigger_intervals[[i]]$trigger_interval[1]
    end.interval = trigger_intervals[[i]]$trigger_interval[2]
    interval = start.interval:end.interval
    met.events = meteo_events[sapply(meteo_events, function(x) any(x$start %in% interval & x$end %in% interval))]
    wet.events = wet_events[sapply(wet_events, function(x) any(x$start %in% interval & x$end %in% interval))]
    
    duration = sum(sapply(met.events, function(x) x$duration))
    severity = sum(sapply(met.events, function(x) x$severity)) - sum(sapply(wet.events, function(x) x$vf))
    
    matched_events[[i]] = list(duration = duration, severity = severity)
  }
}

# Emparejar eventos utilizando los intervalos de disparo calculados
matched_events = match_droughts(exclusion.SPI, trigger, SPI.sequia$vol_excess)


matching <- function(exclusion.SSI, exclusion.SPI, SPI.sequia) {
  
  matched_events <- list()
  
  for (i in seq_along(exclusion.SSI)) {
    hyd_event <- exclusion.SSI[[i]]
    start_interval <- max(1, hyd_event$start - tp)
    end_interval <- hyd_event$end
    
    if (i > 1) {
      prev_end <- exclusion.SSI[[i - 1]]$end
      if (start_interval < prev_end) {
        start_interval <- prev_end
      }
    }
    
    interval <- start_interval:end_interval
    
    met_events <- exclusion.SPI[sapply(exclusion.SPI, function(x) any(x$start %in% interval & x$end %in% interval))]
    wet_events <- SPI.sequia$vol_excess[sapply(SPI.sequia$vol_excess, function(x) any(x$start %in% interval & x$end %in% interval))]
    
    duration <- sum(sapply(met_events, function(x) x$duration))
    severity <- sum(sapply(met_events, function(x) x$severity)) - sum(sapply(wet_events, function(x) x$vf))
    
    matched_events[[i]] <- list(duration = duration, severity = severity)
  }
  
  return(matched_events)
}

# Aplicar la función de matching
matched_events <- matching(exclusion.SSI, exclusion.SPI, SPI.sequia)










#-------------------------------------------------------------------------------
# calculo de Tr
Tr = (length(matched_events) / length(exclusion.SPI)) * 100
Tr
#-------------------------------------------------------------------------------
severidad.maches = data.frame()
for (i in 1:length(matched_events)) {
  severidad.maches = rbind(severidad.maches, data.frame(matched_events[[i]]$severity))
}
names(severidad.maches) = c("Severidad")
write.csv(severidad.maches, "C:/Users/Jonna/Desktop/Sequias/Funcion_copula/severitymatch.csv")

severidad.SPI = data.frame()
for (i in 1:length(exclusion.SPI)) {
  severidad.SPI = rbind(severidad.SPI, data.frame(exclusion.SPI[[i]]$severity))
}
names(severidad.SPI) = c("Severidad")
write.csv(severidad.SPI, "C:/Users/Jonna/Desktop/Sequias/Funcion_copula/severitySPI.csv")

severidad.SSI = data.frame()
for (i in 1:length(exclusion.SSI)) {
  severidad.SSI = rbind(severidad.SSI, data.frame(exclusion.SSI[[i]]$severity))
}
names(severidad.SSI) = c("Severidad")
write.csv(severidad.SSI, "C:/Users/Jonna/Desktop/Sequias/Funcion_copula/severitySSI.csv")
