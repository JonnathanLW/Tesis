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
SPI$evento = seq(1, nrow(SPI))
SSI = proces(SSI, "SSI")

# Teoria de las corridas -------------------------------------------------------
teori.run = function(df) {
  dry_thresh = -0.5
  wet_thresh = 0.5
  
  events = list()
  event_start = 0
  event_type = 0 # 0 = ninguno, -1 = seco, 1 = húmedo
  
  for (i in 1:length(df)) {
    if (df[i] < dry_thresh) {
      if (event_type != -1) {
        event_start = i
        event_type = -1
      }
      
    } else if (df[i] > wet_thresh) {
      if (event_type != 1) {
        event_start = i
        event_type = 1
      }
    } else {
      if (event_type != 0) {
        event_end = i - 1
        event_dur = event_end - event_start + 1
        event_mag = sum(abs(df[event_start:event_end]))
        event_sev = event_mag / event_dur
        vi = sum(df[event_start:event_end] - dry_thresh)
        vd = sum(dry_thresh - df[event_start:event_end])
        
        if (event_type == -1) {
          event_sev = -event_sev
        }
        events[[length(events) + 1]] = list(type = event_type, start = event_start, end = event_end, duration = event_dur, 
                                            magnitude = event_mag, 
                                            severity = event_sev,
                                            vi = vi,
                                            vd = vd)
        event_type = 0
      }
    }
  }
  
  return(events)
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
SPI.sequia = sep.eventos(events.SPI, -1)
SPI.humeda = sep.eventos(events.SPI, 1)

events.SSI = teori.run(SSI$SSI)
SSI.sequia = sep.eventos(events.SSI, -1)
SSI.humeda = sep.eventos(events.SSI, 1)
#-------------------------------------------------------------------------------
# Pooling ----------------------------------------------------------------------
# Función para agrupar eventos adyacentes
pooling = function(evento, tc, pc){
  eventos_agrupados = list()
  # Verifica si la lista 'evento' está vacía
  if (length(evento) == 0) {
    stop("La lista de eventos está vacía")
    return(eventos_agrupados)
  }
  t.yo = 0
  p.yo = 0
  while (t.yo <= tc || p.yo <= pc) {
    for (i in 1:(length(evento) - 1)) {
      # Verifica que el índice esté dentro del rango válido
      if (i >= 1 && i <= length(evento) - 1) {
        evento.actual = evento[[i]]
        evento.siguiente = evento[[i + 1]]
        t.yo = evento.siguiente$start - evento.actual$end - 1
        v = evento.actual$vi + evento.siguiente$vi
        s = evento.actual$vi
        p.yo = v/s
        if (t.yo <= tc && p.yo <= pc) {
          evento.agrupado = list(type = evento.actual$type,
                                 start = evento.actual$start,
                                 end = evento.siguiente$end,
                                 duration = evento.actual$duration + evento.siguiente$duration + t.yo,
                                 severity = evento.actual$severity + evento.siguiente$severity + p.yo)
          eventos_agrupados[[length(eventos_agrupados) + 1]] = evento.agrupado
        } else {
          if (i <= 10) {
            eventos_agrupados[[length(eventos_agrupados) + 1]] = evento.actual
          } else {
            eventos_agrupados[[length(eventos_agrupados) + 1]] = evento.actual
            eventos_agrupados[[length(eventos_agrupados) + 1]] = evento.siguiente
          }
        }
      } 
    }
  }
  return(eventos_agrupados)
}

pooling.SPI = pooling(evento = SPI.sequia, tc = 5, pc = 0.4)
pooling.SSI = pooling(evento = SSI.sequia, tc = 2, pc = 0.4)
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
    if (evento[[i]]$duration < rd * duracion_media & evento[[i]]$severity < rs * severity_medio) {
      
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
trigger = function(evento, tp) {
  eventos_trigger = list()
  for (i in 1:length(evento)) {
    sequia = evento[[i]]
    start = sequia$start
    end = sequia$end
    
    # Cálculo del intervalo de activación
    tsi = start
    tei = end
    tei_1 = end - 1
    
    if (tsi - tei_1 >= tp) {
      i.act = tsi - tp
      trigger_interval = c(i.act, tei)
    } else {
      trigger_interval = c(tei_1, tei)
    }
    eventos_trigger[[length(eventos_trigger) + 1]] = list(type = sequia$type, 
                                                          start = start, 
                                                          end = end, 
                                                          trigger_interval = trigger_interval)
  }
  return(eventos_trigger)
}
trigger = trigger(evento = exclusion.SSI, tp = 3)

match_droughts = function(meteo_events, trigger_intervals, wet_events) {
  matches = list()
  for (meteo_event in meteo_events) {
    for (trigger_event in trigger_intervals) {
      if (meteo_event$start %in% trigger_event$trigger_interval ||
          meteo_event$end %in% trigger_event$trigger_interval) {
        # Calcular duración (d)
        d = meteo_event$duration
        
        # Calcular severidad (s)
        s = meteo_event$severity - sum(sapply(wet_events, function(event) {
          if (event$start %in% trigger_event$trigger_interval || event$end %in% trigger_event$trigger_interval) {
            return(event$severity)
          } else {
            return(0)
          }
        }))
        matches = append(matches, list(list(meteo_event = meteo_event, trigger_event = trigger_event, duration = d, severity = s)))
      }
    }
  }
  return(matches)
}

# Emparejar eventos utilizando los intervalos de disparo calculados
matched_events = match_droughts(exclusion.SPI, trigger, SSI.humeda)
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
