# Librerías necesarias ---------------------------------------------------------
library(dplyr)
library(ggplot2)
# Funciones complementarias ----------------------------------------------------
proces = function(df, name) {
  names(df) = c("date", name)
  df$date = as.Date(df$date, format = "%Y-%m-%d")
  return(df)
}
# Funciones principales --------------------------------------------------------

# Preparación de datos ---------------------------------------------------------
# Cargar de datos de SSI y SPI y merge
SPI = read.csv("C:/Users/Jonna/Desktop/Sequias/SPI.csv")
SSI = read.csv("C:/Users/Jonna/Desktop/Sequias/SSI.csv")
SPI = proces(SPI, "SPI")
SSI = proces(SSI, "SSI")

# Calculo de tc (tiempo critico) -----------------------------------------------
threshold = -0.5

# # Identificar corridas negativas para SPI
# spi_data = SPI %>%
#   mutate(run_spi = ifelse(SPI < threshold, 1, 0)) %>%
#   mutate(run_id_spi = cumsum(c(0, diff(run_spi) != 0)))
# 
# # Identificar corridas negativas para SSI
# ssi_data = SSI %>%
#   mutate(run_ssi = ifelse(SSI < threshold, 1, 0)) %>%
#   mutate(run_id_ssi = cumsum(c(0, diff(run_ssi) != 0)))
# 
# # Calcular la duración crítica para SPI
# spi_negative_runs = spi_data %>%
#   filter(run_spi == 1) %>%
#   group_by(run_id_spi) %>%
#   summarise(duration = n()) 
# 
# t_c_spi = max(spi_negative_runs$duration)
# 
# # Calcular la duración crítica para SRI
# ssi_negative_runs = ssi_data %>%
#   filter(run_ssi == 1) %>%
#   group_by(run_id_ssi) %>%
#   summarise(duration = n())
# 
# t_c_ssi =max(ssi_negative_runs$duration)
# 
# 
# # Caracterizar las sequías
# results.SPI = data.frame()
# for (i in 1:length(spi_negative_runs$run_id_spi)) {
#   start_date = min(spi_data$date[spi_data$run_id_spi == spi_negative_runs$run_id_spi[i]])
#   end_date = max(spi_data$date[spi_data$run_id_spi == spi_negative_runs$run_id_spi[i]])
#   duration = spi_negative_runs$duration[i]
#   magnitude = sum(spi_data$SPI[spi_data$run_id_spi == spi_negative_runs$run_id_spi[i]] - threshold) # verificar calculo de magnitud 
#   intensity = magnitude / duration
#   results.SPI = rbind(results.SPI, data.frame(start_date, end_date, duration, magnitude, intensity))
# }
# 
# results.SSI = data.frame()
# for (i in 1:length(ssi_negative_runs$run_id_ssi)) {
#   start_date = min(ssi_data$date[ssi_data$run_id_ssi == ssi_negative_runs$run_id_ssi[i]])
#   end_date = max(ssi_data$date[ssi_data$run_id_ssi == ssi_negative_runs$run_id_ssi[i]])
#   duration = ssi_negative_runs$duration[i]
#   magnitude = sum(ssi_data$SSI[ssi_data$run_id_ssi == ssi_negative_runs$run_id_ssi[i]] - threshold) # verificar calculo de magnitud 
#   intensity = magnitude / duration
#   results.SSI = rbind(results.SSI, data.frame(start_date, end_date, duration, magnitude, intensity))
# }


# Agrupacion 2 -----------------------------------------------------------------
drought_threshold =-0.5

# Función para identificar eventos de sequía
identify_droughts =function(ssi, threshold) {
  droughts =list()
  in_drought =FALSE
  start_month =0
  deficit_volume =0
  
  for (i in 1:length(ssi)) {
    if (ssi[i] < threshold) {
      if (!in_drought) {
        in_drought =TRUE
        start_month =i
      }
      deficit_volume =deficit_volume + ssi[i]
    } else {
      if (in_drought) {
        in_drought =FALSE
        end_month =i - 1
        duration =end_month - start_month + 1
        excess_volume =sum(ssi[seq(end_month + 1, i - 1)][ssi[seq(end_month + 1, i - 1)] > 0])
        droughts =c(droughts, list(list(start = start_month, end = end_month, duration = duration,
                                          deficit_volume = -deficit_volume, excess_volume = excess_volume)))
        deficit_volume =0
      }
    }
  }
  
  return(droughts)
}


# Identificar eventos de sequía
drought_eventsSPI =identify_droughts(SPI$SPI, drought_threshold)
drought_eventsSSI =identify_droughts(SSI$SSI, drought_threshold)


# Definir funciones para agrupar eventos de sequía
pool_droughts =function(drought_events, pc = 0.4, tc = tc) {
  pooled_droughts =list()
  i =1
  
  while (i < length(drought_events)) {
    current_drought =drought_events[[i]]
    next_drought =drought_events[[i + 1]]
    
    # Calcular intervalo de tiempo y ratio de exceso de volumen
    time_interval =next_drought$start - current_drought$end
    excess_volume_ratio =next_drought$excess_volume / current_drought$deficit_volume
    
    if (time_interval <= tc & excess_volume_ratio <= pc) {
      # Agrupar eventos de sequía
      pooled_duration =current_drought$duration + next_drought$duration + time_interval
      pooled_deficit =current_drought$deficit_volume + next_drought$deficit_volume + next_drought$excess_volume
      pooled_drought =list(start = current_drought$start, end = next_drought$end, duration = pooled_duration, deficit_volume = pooled_deficit)
      pooled_droughts =c(pooled_droughts, list(pooled_drought))
      i =i + 2
    } else {
      pooled_droughts =c(pooled_droughts, list(current_drought))
      i =i + 1
    }
  }
  
  if (i == length(drought_events)) {
    pooled_droughts =c(pooled_droughts, list(drought_events[[i]]))
  }
  
  return(pooled_droughts)
}

pooled_droughtsSPI =pool_droughts(drought_eventsSPI, pc = 0.4, tc = 5) # pooling
pooled_droughtsSSI =pool_droughts(drought_eventsSSI, pc = 0.4, tc = 2) # pooling

# Función para excluir eventos de sequía cortos/menores
exclude_minor_droughts =function(drought_events, rd = 0.3, rs = 0.3) {
  filtered_droughts =list()
  
  mean_duration =mean(sapply(drought_events, function(x) x$duration))
  mean_deficit =mean(sapply(drought_events, function(x) x$deficit_volume))
  
  for (drought in drought_events) {
    if (drought$duration >= rd * mean_duration & drought$deficit_volume >= rs * mean_deficit) {
      filtered_droughts =c(filtered_droughts, list(drought))
    }
  }
  return(filtered_droughts)
}


filtered_droughtsSPI = exclude_minor_droughts(pooled_droughtsSPI, rd = 0.3, rs = 0.3) # excluding
filtered_droughtsSSI = exclude_minor_droughts(pooled_droughtsSSI, rd = 0.3, rs = 0.3) # excluding

met_droughts = filtered_droughtsSPI
hydro_droughts = filtered_droughtsSSI

# Función para emparejar eventos de sequía meteorológica e hidrológica
# Calcular el tiempo de propagación (t_p)
# Aquí puedes utilizar la función ccf() de R para calcular las correlaciones cruzadas
# y encontrar el desfase con la correlación más alta
# Por ejemplo:
correlaciones <- ccf(SPI$SPI, SSI$SSI, lag.max = 12, plot = FALSE)
t_p <- which.max(correlaciones$acf) - 1

# Función para calcular la sequía meteorológica coincidente
calcular_sequia_meteo <- function(trigger_inicio, trigger_fin) {
  # Filtrar eventos de sequía meteorológica dentro del "trigger interval"
  eventos_meteo_filtrados <- met_droughts[sapply(met_droughts, function(x) x$start >= trigger_inicio & x$end <= trigger_fin)]
  
  # Calcular la duración total
  duracion_total <- sum(sapply(eventos_meteo_filtrados, function(x) x$duration))
  
  # Calcular la severidad total
  severidad_total <- sum(sapply(eventos_meteo_filtrados, function(x) x$deficit_volume))
  
  # Restar los volúmenes de los eventos húmedos a la severidad total
  # (Aquí debes tener una lista de eventos húmedos y filtrarlos dentro del "trigger interval")
  # Por ejemplo:
  eventos_humedos <- ... # Lista de eventos húmedos
  eventos_humedos_filtrados <- eventos_humedos[sapply(eventos_humedos, function(x) x$start >= trigger_inicio & x$end <= trigger_fin)]
  severidad_total <- severidad_total - sum(sapply(eventos_humedos_filtrados, function(x) x$volumen))
  
  return(list(duracion = duracion_total, severidad = severidad_total))
}

# Calcular el "trigger interval" y la sequía meteorológica coincidente para cada evento de sequía hidrológica
sequia_meteo_coincidente <- lapply(seq_along(hydro_droughts), function(i) {
  evento_hidro <- hydro_droughts[[i]]
  trigger_inicio <- evento_hidro$start - t_p
  trigger_fin <- evento_hidro$end
  
  # Verificar si el "trigger interval" se superpone con el intervalo del evento anterior
  if (i > 1) {
    evento_hidro_anterior <- hydro_droughts[[i - 1]]
    if (trigger_inicio < evento_hidro_anterior$end) {
      trigger_inicio <- evento_hidro_anterior$end
    }
  }
  
  sequia_meteo <- calcular_sequia_meteo(trigger_inicio, trigger_fin)
  return(sequia_meteo)
})

# Calculo del TR
spi_acum = read.csv("C:/Users/Jonna/Desktop/Sequias/SPI_acumulado(1-12).csv")
merge.SPI_SSI = merge(spi_acum, SSI, by = "date")
cor(merge.SPI_SSI[-1], use = "complete.obs")
ccf(merge.SPI_SSI$SPI.1, merge.SPI_SSI$SSI, lag.max = 12, plot = FALSE)

propagation_time = 3 # meses
