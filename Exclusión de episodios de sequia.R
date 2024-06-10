
# Librerias necesarias ----------------------------------------------------
library(dplyr)
library(drought)

# Funciones complementarias -----------------------------------------------
proces = function(df, name) {
  names(df) = c("date", name)
  df$date = as.Date(df$date, format = "%Y-%m-%d")
  return(df)
}



# Funciones principales ---------------------------------------------------



# Preparacion de datos ----------------------------------------------------
# Cargar de datos de SSI y SPI y merge
SPI = read.csv("C:/Users/Jonna/Desktop/Sequias/SPI.csv")
SSI = read.csv("C:/Users/Jonna/Desktop/Sequias/SSI.csv")
SPI = proces(SPI, "SPI")
SSI = proces(SSI, "SSI")


# Calculo de Tc (tiempo critico) ------------------------------------------
threshold = -0.5

# Identificar corridas negativas para SPI
spi_data = SPI %>%
  mutate(run_spi = ifelse(SPI < threshold, 1, 0)) %>%
  mutate(run_id_spi = cumsum(c(0, diff(run_spi) != 0)))

# Identificar corridas negativas para SSI
ssi_data = SSI %>%
  mutate(run_ssi = ifelse(SSI < threshold, 1, 0)) %>%
  mutate(run_id_ssi = cumsum(c(0, diff(run_ssi) != 0)))

# Calcular la duración crítica para SPI
spi_negative_runs = spi_data %>%
  filter(run_spi == 1) %>%
  group_by(run_id_spi) %>%
  summarise(duration = n()) 

t_c_spi = max(spi_negative_runs$duration)

# Calcular la duración crítica para SRI
ssi_negative_runs = ssi_data %>%
  filter(run_ssi == 1) %>%
  group_by(run_id_ssi) %>%
  summarise(duration = n())

t_c_ssi <- max(ssi_negative_runs$duration)


# Caracterizar las sequías
results.SPI = data.frame()
for (i in 1:length(spi_negative_runs$run_id_spi)) {
  start_date = min(spi_data$date[spi_data$run_id_spi == spi_negative_runs$run_id_spi[i]])
  end_date = max(spi_data$date[spi_data$run_id_spi == spi_negative_runs$run_id_spi[i]])
  duration = spi_negative_runs$duration[i]
  magnitude = sum(spi_data$SPI[spi_data$run_id_spi == spi_negative_runs$run_id_spi[i]] - threshold) # verificar calculo de magnitud 
  intensity = magnitude / duration
  results.SPI = rbind(results.SPI, data.frame(start_date, end_date, duration, magnitude, intensity))
}

results.SSI = data.frame()
for (i in 1:length(ssi_negative_runs$run_id_ssi)) {
  start_date = min(ssi_data$date[ssi_data$run_id_ssi == ssi_negative_runs$run_id_ssi[i]])
  end_date = max(ssi_data$date[ssi_data$run_id_ssi == ssi_negative_runs$run_id_ssi[i]])
  duration = ssi_negative_runs$duration[i]
  magnitude = sum(ssi_data$SSI[ssi_data$run_id_ssi == ssi_negative_runs$run_id_ssi[i]] - threshold) # verificar calculo de magnitud 
  intensity = magnitude / duration
  results.SSI = rbind(results.SSI, data.frame(start_date, end_date, duration, magnitude, intensity))
}


identify_events <- function(index_values, threshold) {
  events <- list()
  current_event <- NULL
  for (i in seq_along(index_values)) {
    if (index_values[i] < threshold) {
      if (is.null(current_event)) {
        current_event <- list(start = i, values = index_values[i])
      } else {
        current_event$values <- c(current_event$values, index_values[i])
      }
    } else {
      if (!is.null(current_event)) {
        current_event$end <- i - 1
        events <- c(events, list(current_event))
        current_event <- NULL
      }
    }
  }
  return(events)
}
spi_events <- identify_events(SPI$SPI, threshold = -0.5)
ssi_events <- identify_events(SSI$SSI, threshold = -0.5)

group_events <- function(events, tc, pc) {
  grouped_events <- list()
  for (i in seq_along(events)) {
    current_event <- events[[i]]
    if (length(grouped_events) == 0) {
      grouped_events <- list(current_event)
    } else {
      previous_event <- grouped_events[[length(grouped_events)]]
      time_gap <- current_event$start - previous_event$end
      volume_ratio <- sum(current_event$values) / sum(previous_event$values)
      if (time_gap < tc && volume_ratio < pc) {
        merged_event <- list(
          start = previous_event$start,
          end = current_event$end,
          values = c(previous_event$values, current_event$values)
        )
        grouped_events[[length(grouped_events)]] <- merged_event
      } else {
        grouped_events <- c(grouped_events, list(current_event))
      }
    }
  }
  return(grouped_events)
}




tc <- 5  # Duración crítica en meses
pc <- 0.5 # Relación crítica de excesos/déficits

# Agrupar eventos SPI
spi_grouped <- group_events(spi_events, tc, pc)

# Agrupar eventos SRI
sri_grouped <- group_events(ssi_events, tc, pc)



spi_durations <- sapply(spi_grouped, function(event) event$end - event$start + 1)

# Calcular severidad de eventos agrupados SRI
sri_severities <- sapply(sri_grouped, function(event) sum(event$values))



















# Identificar eventos en SRI
sri_events <- identify_events(sri_values, threshold = -0.5)

# Identificar eventos en SPI
spi_events <- identify_events(spi_values, threshold = -0.5)

# Identificar eventos en SRI
sri_events <- identify_events(sri_values, threshold = -0.5)
















# Definir umbrales críticos
tc_spi <- -0.5 # Umbral crítico para SPI
tc_sri <- 0.5 # Umbral crítico para SRI

# Inicializar variables para almacenar eventos combinados
eventos_combinados <- list()

data = merge(SPI, SSI, by = "date", all = TRUE)
# Iterar sobre los pares de eventos adyacentes
for (i in 1:(nrow(data) - 1)) {
  # Obtener los datos de SPI y SRI de los eventos i e i+1
  spi_actual <- data$SPI[i]
  spi_siguiente <- data$SPI[i + 1]
  sri_actual <- data$SRI[i]
  sri_siguiente <- data$SRI[i + 1]
  
  # Verificar los criterios de agrupación
  if (spi_actual < tc_spi && spi_siguiente < tc_spi && sri_actual < tc_sri && sri_siguiente < tc_sri) {
    # Agrupar los eventos
    evento_combinado <- list(
      SPI = spi_actual + spi_siguiente,
      SRI = sri_actual + sri_siguiente
    )
    
    # Almacenar el evento combinado
    eventos_combinados[[length(eventos_combinados) + 1]] <- evento_combinado
  } else {
    # Almacenar los eventos individuales si no se cumplen los criterios
    eventos_combinados[[length(eventos_combinados) + 1]] <- list(SPI = spi_actual, SRI = sri_actual)
    eventos_combinados[[length(eventos_combinados) + 1]] <- list(SPI = spi_siguiente, SRI = sri_siguiente)
  }
}

# Mostrar los resultados
print(eventos_combinados)















# Calcular el valor crítico para SPI
spi_data <- spi_data %>%
  mutate(excess_volume = ifelse(SPI < threshold, threshold - SPI, 0))

# Agrupar por run_id_spi y calcular el volumen total de cada corrida
spi_volumes <- spi_data %>%
  filter(run_spi == 1) %>%
  group_by(run_id_spi) %>%
  summarise(total_volume = sum(excess_volume))

# Relación del exceso de volumen (se puede definir un método específico según el contexto)
p_c_spi <- max(spi_volumes$total_volume / lag(spi_volumes$total_volume, default = first(spi_volumes$total_volume)))

# Calcular el valor crítico para SRI
ssi_data <- ssi_data %>%
  mutate(excess_volume = ifelse(SSI < threshold, threshold - SSI, 0))

# Agrupar por run_id_sri y calcular el volumen total de cada corrida
ssi_volumes <- ssi_data %>%
  filter(run_ssi == 1) %>%
  group_by(run_id_ssi) %>%
  summarise(total_volume = sum(excess_volume))

# Relación del exceso de volumen (se puede definir un método específico según el contexto)
p_c_ssi <- max(ssi_volumes$total_volume / lag(ssi_volumes$total_volume, default = first(ssi_volumes$total_volume)))

# Agrupar eventos de sequía para SPI
spi_data <- spi_data %>%
  mutate(pooling = ifelse(run_spi == 1 & lag(run_spi, default = 0) == 1 & (date - lag(date, default = first(date))) <= t_c_spi & (excess_volume / lag(excess_volume, default = first(excess_volume))) <= p_c_spi, 1, 0))

spi_data <- spi_data %>%
  mutate(run_id_pooled = cumsum(c(0, diff(pooling) != 0)))

# Agrupar eventos de sequía para SRI
ssi_data <- ssi_data %>%
  mutate(pooling = ifelse(run_ssi == 1 & lag(run_ssi, default = 0) == 1 & (date - lag(date, default = first(date))) <= t_c_ssi & (excess_volume / lag(excess_volume, default = first(excess_volume))) <= p_c_ssi, 1, 0))

ssi_data <- ssi_data %>%
  mutate(run_id_pooled = cumsum(c(0, diff(pooling) != 0)))

# Duración y severidad de los eventos agrupados para SPI
spi_grouped <- spi_data %>%
  filter(run_spi == 1) %>%
  group_by(run_id_pooled) %>%
  summarise(total_duration = n(), total_severity = sum(excess_volume))

print(spi_grouped)

# Duración y severidad de los eventos agrupados para SRI
ssi_grouped <- ssi_data %>%
  filter(run_ssi == 1) %>%
  group_by(run_id_pooled) %>%
  summarise(total_duration = n(), total_severity = sum(excess_volume))

print(ssi_grouped)

# Unir los datos de SPI y SRI por fecha
combined_data <- spi_data %>%
  left_join(ssi_data, by = "date", suffix = c("_spi", "_sri"))

# Identificar eventos de sequía coincidentes
coincident_events <- combined_data %>%
  filter(run_spi == 1 & run_ssi == 1) %>%
  group_by(run_id_pooled_spi, run_id_pooled_sri) %>%
  summarise(
    start_date = min(date),
    end_date = max(date),
    duration_spi = n(),
    severity_spi = sum(excess_volume_spi),
    duration_sri = n(),
    severity_sri = sum(excess_volume_sri)
  )

print(coincident_events)
