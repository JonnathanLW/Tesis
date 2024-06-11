# Librerías necesarias ---------------------------------------------------------
library(dplyr)
library(ggplot2)

# Cargar datos de caudal diario
datos = read.csv("C:/Users/Jonna/Desktop/Sequias/caudal_SSI.csv", header = TRUE)
datos = datos[,-1]
names(datos) = c("Fecha", "caudal")
datos$Fecha = as.Date(datos$Fecha, format = "%Y-%m-%d")
caudal = datos$caudal

# Definir umbral de flujo
umbral = quantile(caudal, 0.7)

# serie temporal de caudal 
ggplot(datos, aes(x = Fecha, y = caudal)) +
  geom_line() +
  geom_hline(yintercept = umbral, linetype = "dashed", color = "red") +
  labs(title = "Serie temporal de caudal",
       x = "Fecha",
       y = "Caudal") +
  theme_minimal()

# Función para identificar períodos de sequía
identificar_sequias = function(caudal, umbral) {
  sequias = list()
  secuencia = FALSE
  inicio = 0
  fin = 0
  duracion = 0
  deficit = 0
  for (i in 1:length(caudal)) {
    if (caudal[i] < umbral) {
      if (!secuencia) {
        secuencia = TRUE
        inicio = i
      }
      duracion = duracion + 1
      deficit = deficit + (umbral - caudal[i])
    } else {
      if (secuencia) {
        secuencia = FALSE
        fin = i - 1
        # Guardar los detalles de la sequía en la lista
        sequias[[length(sequias) + 1]] = list(
          inicio = inicio,
          fin = fin,
          duracion = duracion,
          deficit = deficit
        )
        duracion = 0
        deficit = 0
      }
    }
  }
  return(sequias)
}

# Identificar períodos de sequía
sequias = identificar_sequias(caudal, umbral)

# Función para agrupar sequías dependientes (método IC)
agrupar_sequias = function(sequias, t_c, p_c) {
  sequias_agrupadas = list()
  i = 1
  
  while (i <= length(sequias)) {
    inicio = sequias[[i]]$inicio
    fin = sequias[[i]]$fin
    duracion = sequias[[i]]$duracion
    deficit = sequias[[i]]$deficit
    j = i + 1
    
    while (j <= length(sequias)) {
      tiempo_entre = sequias[[j]]$inicio - fin
      volumen_entre = sum(caudal[(fin + 1):sequias[[j]]$inicio] - umbral)
      
      if (tiempo_entre <= t_c && volumen_entre / deficit <= p_c) {
        fin = sequias[[j]]$fin
        duracion = duracion + sequias[[j]]$duracion + tiempo_entre
        deficit = deficit + sequias[[j]]$deficit - volumen_entre
        j = j + 1
      } else {
        break
      }
    }
    
    sequias_agrupadas[[length(sequias_agrupadas) + 1]] = list(
      inicio = inicio,
      fin = fin,
      duracion = duracion,
      deficit = deficit
    )
    
    
    i = j
  }
  
  return(sequias_agrupadas)
}

# Crear una lista vacía para almacenar los resultados
resultados = list()

# Bucle para diferentes valores de t_c y p_c
t.c = seq(1, 10, by = 1)
p.c = seq(0.1, 0.9, by = 0.1)

for (i in 1:length(t.c)){
  for (j in 1:length(p.c)){
    sequias_agrupadas = agrupar_sequias(sequias, t.c[i], p.c[j])
    
    # Calcular estadísticas de las sequías agrupadas
    duraciones = vapply(sequias_agrupadas, function(x) x$duracion, numeric(1))
    deficits = vapply(sequias_agrupadas, function(x) x$deficit, numeric(1))
    
    media_duracion = mean(duraciones)
    media_deficit = mean(deficits)
    
    # Almacenar los resultados en la lista
    resultado = list(
      t_c = t.c[i],
      p_c = p.c[j],
      media_duracion = media_duracion,
      media_deficit = media_deficit
    )
    
    # Añadir el resultado a la lista
    resultados[[length(resultados) + 1]] = resultado
  }
}

# graficar los resultados
resultados_df <- do.call(rbind, lapply(resultados, as.data.frame))

# Gráfico de media_duracion vs p_c
ggplot(resultados_df, aes(x = p_c, y = media_duracion, color = factor(t_c))) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(0.1, 0.9, by = 0.1)) +
  labs(title = "Gráfico de media_duracion vs p_c",
       x = "p_c",
       y = "media_duracion",
       color = "t_c") +
  theme_minimal()

# Gráfico de media_deficit vs p_c
ggplot(resultados_df, aes(x = p_c, y = media_deficit, color = factor(t_c))) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(0.1, 0.9, by = 0.1)) +
  labs(title = "Gráfico de media_deficit vs p_c",
       x = "p_c",
       y = "media_deficit",
       color = "t_c") +
  theme_minimal()
