# Librerías necesarias ---------------------------------------------------------
library(dplyr)
library(ggplot2)
library(fitdistrplus)
library(MASS)
library(evir)
library(extRemes)
library(gamlss)
library(evd)
library(bbmle)
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
SPI$estado = ifelse(SPI$SPI < -0.5, "Sequia", ifelse(SPI$SPI > 0.5, "Humedo", "Normal"))
SSI = proces(SSI, "SSI")
SSI$evento = seq(1, nrow(SSI))
SSI$estado = ifelse(SSI$SSI < -0.5, "Sequia", ifelse(SSI$SSI > 0.5, "Humedo", "Normal"))

# Teoría de las corridas para sequías ------------------------------------------
teori.run <- function(df) {
  dry_thresh = -0.5
  sequias = list()
  event_start = NULL
  event_type = 0 # 0 = ninguno, -1 = seco, 1 = húmedo
  
  for (i in 1:length(df)) {
    if (df[i] < dry_thresh) {
      if (event_type != -1) {
        if (!is.null(event_start)) {
          event_end = i - 1
          event_dur = event_end - event_start + 1
          event_mag = sum(abs(df[event_start:event_end]))
          values = df[event_start:event_end]
        
          event_sev =  sum(dry_thresh - (df[event_start:event_end]))
          
          if (event_type == -1) {
            event_sev = event_sev
          }
          sequias[[length(sequias) + 1]] = list(
            type = event_type, start = event_start, end = event_end,
            duration = event_dur, magnitude = event_mag, severity = event_sev,
            values = values
          )
        }
        event_start = i
        event_type = -1
      }
    } else if (df[i] > dry_thresh) {
      if (event_type != 1) {
        if (!is.null(event_start)) {
          event_end = i - 1
          event_dur = event_end - event_start + 1
          event_mag = sum(abs(df[event_start:event_end]))
          values = df[event_start:event_end]
          
          event_sev =  sum(dry_thresh - (df[event_start:event_end]))
          
          if (event_type == -1) {
            event_sev = event_sev
          }
          sequias[[length(sequias) + 1]] = list(
            type = event_type, start = event_start, end = event_end,
            duration = event_dur, magnitude = event_mag, severity = event_sev,
            values = values
          )
        }
        event_start = i
        event_type = 1
      }
    } else {
      if (event_type != 0 && !is.null(event_start)) {
        event_end = i - 1
        event_dur = event_end - event_start + 1
        event_mag = sum(abs(df[event_start:event_end]))
        values = df[event_start:event_end]
        
        event_sev =  sum(dry_thresh - (df[event_start:event_end]))
        
        if (event_type == -1) {
          event_sev = event_sev
        }
        sequias[[length(sequias) + 1]] = list(
          type = event_type, start = event_start, end = event_end,
          duration = event_dur, magnitude = event_mag, severity = event_sev,
          values = values
        )
        event_type = 0
        event_start = NULL
      }
    }
  }
  
  # Capturar el último evento si el estado no cambió
  if (!is.null(event_start)) {
    event_end = length(df)
    event_dur = event_end - event_start + 1
    event_mag = sum(abs(df[event_start:event_end]))
    values = df[event_start:event_end]
    event_sev = sum(dry_thresh - (df[event_start:event_end]))
    
    if (event_type == -1) {
      event_sev = event_sev
    }
    sequias[[length(sequias) + 1]] = list(
      type = event_type, start = event_start, end = event_end,
      duration = event_dur, magnitude = event_mag, severity = event_sev,
      values = values
    )
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
  
  sequias = sep.eventos(sequias, -1)
  return(sequias)
}
SPI.sequias = teori.run(SPI$SPI)
SSI.sequias = teori.run(SSI$SSI)
# Teoría de las corridas para volumen en exceso --------------------------------
teori.runHumedad = function(df){
    dry_thresh = 0.5
    sequias = list()
    event_start = NULL
    event_type = 0 # 0 = ninguno, -1 = seco, 1 = húmedo
    
    for (i in 1:length(df)) {
      if (df[i] < dry_thresh) {
        if (event_type != -1) {
          if (!is.null(event_start)) {
            event_end = i - 1
            event_dur = event_end - event_start + 1
            event_mag = sum(abs(df[event_start:event_end]))
            values = df[event_start:event_end]
            event_sev = sum((df[event_start:event_end]) - dry_thresh)
            
            if (event_type == -1) {
              event_sev = event_sev
            }
            sequias[[length(sequias) + 1]] = list(
              type = event_type, start = event_start, end = event_end,
              duration = event_dur, magnitude = event_mag, severity = event_sev,
              values = values
            )
          }
          event_start = i
          event_type = -1
        }
      } else if (df[i] > dry_thresh) {
        if (event_type != 1) {
          if (!is.null(event_start)) {
            event_end = i - 1
            event_dur = event_end - event_start + 1
            event_mag = sum(abs(df[event_start:event_end]))
            values = df[event_start:event_end]
            event_sev =  sum((df[event_start:event_end]) - dry_thresh)
            
            if (event_type == -1) {
              event_sev = event_sev
            }
            sequias[[length(sequias) + 1]] = list(
              type = event_type, start = event_start, end = event_end,
              duration = event_dur, magnitude = event_mag, severity = event_sev,
              values = values
            )
          }
          event_start = i
          event_type = 1
        }
      } else {
        if (event_type != 0 && !is.null(event_start)) {
          event_end = i - 1
          event_dur = event_end - event_start + 1
          event_mag = sum(abs(df[event_start:event_end]))
          values = df[event_start:event_end]
          event_sev = sum((df[event_start:event_end]) - dry_thresh)
          
          if (event_type == -1) {
            event_sev = event_sev
          }
          sequias[[length(sequias) + 1]] = list(
            type = event_type, start = event_start, end = event_end,
            duration = event_dur, magnitude = event_mag, severity = event_sev,
            values = values
          )
          event_type = 0
          event_start = NULL
        }
      }
    }
    
    # Capturar el último evento si el estado no cambió
    if (!is.null(event_start)) {
      event_end = length(df)
      event_dur = event_end - event_start + 1
      event_mag = sum(abs(df[event_start:event_end]))
      values = df[event_start:event_end]
      event_sev = sum((df[event_start:event_end]) - dry_thresh)
      
      if (event_type == -1) {
        event_sev = event_sev
      }
      sequias[[length(sequias) + 1]] = list(
        type = event_type, start = event_start, end = event_end,
        duration = event_dur, magnitude = event_mag, severity = event_sev,
        values = values
      )
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
    sequias = sep.eventos(sequias, 1)
    return(sequias)
}
SPI.humedad = teori.runHumedad(SPI$SPI)
SSI.humedad = teori.runHumedad(SSI$SSI)
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
    v = vi[[i]]$severity
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

pooling.SPI = pooling(evento = SPI.sequias, tc = 5, pc = 0.4, vi = SPI.humedad)
pooling.SSI = pooling(evento = SSI.sequias, tc = 2, pc = 0.4, vi = SSI.humedad)
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
    
    if (i + 1 == length(evento)) {
      sequia = evento[[i + 1]]
      start = sequia$start
      end = sequia$end
      
      sequia_1 = evento[[i]]
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
#-------------------------------------------------------------------------------
matching <- function(exclusion.SPI, trigger, SPI.humedad) {
  matched_events <- list()
  for (i in 1:(length(trigger)-1)) {
    start_interval <- trigger[[i]]$trigger_interval[1]
    end_interval <- trigger[[i]]$trigger_interval[2]
    interval = start_interval:end_interval
    met_events <- exclusion.SPI[sapply(exclusion.SPI, function(x) any(x$start %in% interval & x$end %in% interval))]
    
    met_duration = ifelse(length(met_events) != 0, sum(sapply(met_events, function(x) x$duration)), 0)
    met_sever = ifelse(length(met_events) != 0, sum(sapply(met_events, function(x) x$severity)), 0)
    met_info <- lapply(met_events, function(x) list(start = x$start, end = x$end, severity = x$severity))
    wet_events <- SPI.humedad[sapply(SPI.humedad, function(x) any(x$start %in% interval & x$end %in% interval))]
    wet_sever = ifelse(length(wet_events) != 0, sum(sapply(wet_events, function(x) x$severity)), 0)
    hum_info <- lapply(wet_events, function(x) list(start = x$start, end = x$end, severity = x$severity))
    duration <- met_duration
    severity <- met_sever - wet_sever
    matched_events[[i]] <- list(met_info = met_info, hum_info = hum_info, 
                                match_severity = severity, match_duration = duration)
    
    if (i + 1 == (length(trigger))) {
      start_interval <- trigger[[i + 1]]$trigger_interval[1]
      end_interval <- trigger[[i + 1]]$trigger_interval[2]
      
      interval = start_interval:end_interval
      met_events <- exclusion.SPI[sapply(exclusion.SPI, function(x) any(x$start %in% interval & x$end %in% interval))]
      met_duration = ifelse(length(met_events) != 0, sum(sapply(met_events, function(x) x$duration)), 0)
      met_sever = ifelse(length(met_events) != 0, sum(sapply(met_events, function(x) x$severity)), 0)
      met_info <- lapply(met_events, function(x) list(start = x$start, end = x$end, severity = x$severity))
      wet_events <- SPI.humedad[sapply(SPI.humedad, function(x) any(x$start %in% interval & x$end %in% interval))]
      wet_sever = ifelse(length(wet_events) != 0, sum(sapply(wet_events, function(x) x$severity)), 0)
      hum_info <- lapply(wet_events, function(x) list(start = x$start, end = x$end, severity = x$severity))
      duration <- met_duration
      severity <- met_sever - wet_sever
      matched_events[[i]] <- list(met_info = met_info, hum_info = hum_info, 
                                  match_severity = severity, match_duration = duration)
    }
  }
  
  # eliminar de matched severity cuando matched_events[[i]]$match_severity = 0
  matched_events <- Filter(function(x) !is.null(x$match_severity) && !isTRUE(all.equal(x$match_severity, 0)), matched_events)
  
  
  return(matched_events)
}
# Aplicar la función de matching
matched_events <- matching(exclusion.SPI, trigger, SPI.humedad)

#-------------------------------------------------------------------------------
############################### Graficos #######################################
# crea histogramas en funcion de la severidad de SPI.humedad, en el eje y va la severidad y eje x va desde cuando inicia hasta cuando termina, el ancho del hisograma seria la duracion del evento
# Función para graficar los eventos de sequía
Extraer.eventos = function(eventos) {
  starts <- sapply(eventos, function(x) x$start)
  ends <- sapply(eventos, function(x) x$end)
  severities <- sapply(eventos, function(x) x$severity)
  
  events_df <- data.frame(
    start = starts,
    end = ends,
    severity = severities
  )
  events_df$start = as.numeric(events_df$start)
  return(events_df)
}
Sequia.InicialSPI = Extraer.eventos(SPI.sequias)
humedad = Extraer.eventos(SPI.humedad)
Sequia.InicialSSI = Extraer.eventos(SSI.sequias)
# Crear el DataFrame combinando los datos de SPI y SSI
Sequia.InicialSPI$type <- "SPI"
Sequia.InicialSSI$type <- "SSI"
humedad$type <- "Humedad"
combined_data <- rbind(Sequia.InicialSPI, Sequia.InicialSSI, humedad)
combined_data$start <- as.numeric(combined_data$start)
combined_data$end <- as.numeric(combined_data$end)
combined_data$severity <- as.numeric(combined_data$severity)
write.csv(combined_data, "C:/Users/Jonna/Desktop/d/combined.csv")

# Crear el gráfico con ggplot2

ggplot(combined_data) +
  geom_rect(aes(xmin = start, xmax = end, ymin = 0, ymax = severity, fill = type), alpha = 1) +
  scale_fill_manual(values = c("blue", "red", "brown")) +
  labs(title = "Eventos iniciales de sequía", x = "Fecha de inicio", y = "Severidad") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, max(combined_data$end, na.rm = TRUE), by = 10)) +
  scale_y_continuous(breaks = seq(0, max(combined_data$severity, na.rm = TRUE), by = 0.5))


# Exclusión de sequías gráfico --------------------------------------------
sequias.excluidas.SPI = Extraer.eventos(exclusion.SPI)
sequias.excluidas.SSI = Extraer.eventos(exclusion.SSI)
sequias.excluidas.SPI$type <- "SPI"
sequias.excluidas.SSI$type <- "SSI"
sequias.excluidasComb = rbind(sequias.excluidas.SPI, sequias.excluidas.SSI)

ggplot(sequias.excluidasComb) +
  geom_rect(aes(xmin = start, xmax = end, ymin = 0, ymax = severity, fill = type), alpha = 0.5) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(title = "Eventos iniciales de sequía", x = "Fecha de inicio", y = "Severidad") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, max(combined_data$end, na.rm = TRUE), by = 10), limits = c(0, max(combined_data$end, na.rm = TRUE))) +
  scale_y_continuous(breaks = seq(0, max(combined_data$severity, na.rm = TRUE), by = 0.5), limits = c(0, max(combined_data$severity, na.rm = TRUE)))

#-------------------------------------------------------------------------------
# calculo de Tr
Tr = (length(matched_events) / length(exclusion.SPI)) * 100
Tr
#-------------------------------------------------------------------------------
################################## Etapa 1 #####################################
# Determinación de las distribuciones marginales de las variables aleatorias 
# duración de la sequía Match junto con la severidad de las sequías meteo e hidro
# duración de las sequías combinadas
drought.duration = function(df){
  duracion.match = list()
  for (i in 1:length(df)){
    duration = df[[i]]$match_duration
    duracion.match[[length(duracion.match) + 1]] = duration
  }
  return(duracion.match)
}
duracion.match = drought.duration(matched_events)
duracion.match = unlist(duracion.match)
# severidad de la sequía meteorológica 
severiti.meteo = function(df) {
  severidad.meteo = list()
  for (i in 1:length(df)){
    severity = df[[i]]$severity
    severidad.meteo[[length(severidad.meteo) + 1]] = severity
  }
  return(severidad.meteo)
}
severidad.meteo = severiti.meteo(exclusion.SPI)
severidad.meteo = unlist(severidad.meteo)

# severidad de la sequía hidrologica 
severiti.hidro = function(df) {
  severidad.hidro = list()
  for (i in 1:length(df)){
    severity = df[[i]]$severity
    severidad.hidro[[length(severidad.hidro) + 1]] = severity
  }
  return(severidad.hidro)
}
severidad.hidro = severiti.hidro(exclusion.SSI)
severidad.hidro = unlist(severidad.hidro)

# Ajuste de la distribución marginal
distribucion.match = function(df) {
  df = as.vector(df)
  # Ajuste de distribuciones
  dist.weibull = fitdist(df, distr = "weibull")
  summary(dist.weibull)
  dist.exp = fitdist(df, distr = "exp")
  summary(dist.exp)
  
  m1 <- mle2(df ~ dgev(loc, exp(logscale), shape), data = data.frame(df), 
             start = list(loc = 0, logscale = 0, shape = 0), method = "Nelder-Mead")
  
  dist.gev = fitdist(df, "gev", 
                     start= with(as.list(coef(m1)), list(loc = loc, scale = exp(logscale), shape = shape)))
  summary(dist.gev)
  
  # Calculo de los AIC 
  aic.weibull = dist.weibull$aic
  aic.exp = dist.exp$aic
  aic.gev = dist.gev$aic
  
  # Kolmogorov smirnov
  ks.weibull = ks.test(df, "pweibull", shape = dist.weibull$estimate[1], scale = dist.weibull$estimate[2])
  ks.exp = ks.test(df, "pexp", rate = dist.exp$estimate[1])
  ks.gev = ks.test(df, "pgev", loc = dist.gev$estimate[1], scale = dist.gev$estimate[2], shape = dist.gev$estimate[3])


  
  resultados = data.frame(
    weibull = c(aic.weibull, ks.weibull$statistic, ks.weibull$p.value),
    exp = c(aic.exp, ks.exp$statistic, ks.exp$p.value),
    gev = c(aic.gev, ks.gev$statistic, ks.gev$p.value))
  rownames(resultados) = c("AIC", "estatics", "p-value KS")
  
  return(resultados)
}

dist.match = distribucion.match(duracion.match)
dist.severidad.meteo = distribucion.match(severidad.meteo)
dist.severidad.hidro = distribucion.match(severidad.hidro)
#-------------------------------------------------------------------------------
