######################################## SPI ###################################
# Autor: Jonnathan Landi
# Fecha creación: 2024-01-31
# Fecha ultima modificación: 2024-02-04 (año-mes-día)
# Versión: 1.9.3
# ------------------------------------------------------------------------------
# ESTADO: En desarrollo 
###########(En desarrollo, En revisión, Refactorización, Terminado) ############
# ------------------------------------------------------------------------------
################################################################################

# Librerías requeridas ---------------------------------------------------------
library(data.table)
library(fitdistrplus)
library(dplyr)
library(tidyr)
#library(knitr)
library(ggplot2)
#library(actuar)
library(MASS)
library(svDialogs)

# preparación de datos ---------------------------------------------------------
directory = "C:/Users/Jonna/Desktop/Nueva carpeta"
data = read.table(file.path(directory, "Yanuncay (800 NTREE).csv"), 
                  header = TRUE, sep = ",")
data = data[,-1]
names(data) = c("TIMESTAMP", "Lluvia_Tot")
# Temporal recortar columna prec. estos datos ya deben estar listos desde el análisis
# de calidad de datos 
#data = data[,c("TIMESTAMP", "Lluvia_Tot")]
data = data.frame(data)

# Agrupamiento de "Lluvia_Tot" por mes
data = data %>% 
  mutate(date = as.POSIXct(TIMESTAMP, format = "%Y-%m-%d")) %>%
  group_by(year(date), month(date)) %>%
  summarise(prec = sum(Lluvia_Tot, na.rm = TRUE))
names(data) = c("año", "mes", "prec")

date = paste(data$año, data$mes, "01", sep = "-")

data.mensual = cbind(date, data$prec)
data.mensual = data.frame(data.mensual)
data.mensual$date = as.Date(data.mensual$date, format = "%Y-%m-%d")
data.mensual[,2] = as.numeric(data.mensual[,2])
names(data.mensual) = c("date", "pp")
# ------------------------------------------------------------------------------
############################ Funciones para calculos ###########################
best.dist = function(){
  
  data.mensual = as.vector(data.mensual$pp)
  #Distribución Exponencial
  ajuste.exponencial = fitdistr(data.mensual, "exponential")
  
  # Distribución de probabilidad gamma
  
  ajuste.gamma  = fitdist(data.mensual$pp, "gamma")
  
  # Ajuste de la distribución Log-Normal
  ajuste.LogNorm = fitdistr(data.mensual$pp, "log-normal") 
  
  
  # Ajuste de la distribución Weibull
  # ajuste.weibull = fitdistr(data.mensual$pp, "weibull") 
  ajuste.weibull = fitdist(data.mensual$pp, "weibull")
  
  
  # Ajuste de la distribución Normal
  ajuste.normal = fitdistr(data.mensual$pp, "normal")
  
  # ajuste de distribución logística
  ajuste.logistica = fitdistr(data.mensual$pp, "logistic")

  
  # Test de kolmogorov
  ks.exponencial = ks.test(data.mensual$pp, "pexp", ajuste.exponencial$estimate[1]) # exponencial
  ks.gamma = ks.test(data.mensual$pp, "pgamma", ajuste.gamma$estimate[1], ajuste.gamma$estimate[2]) # gamma
  ks.LogNorm = ks.test(data.mensual$pp, "plnorm", ajuste.LogNorm$estimate[1], ajuste.LogNorm$estimate[2]) # Log-Normal
  ks.weibull = ks.test(data.mensual$pp, "pweibull", ajuste.weibull$estimate[1], ajuste.weibull$estimate[2]) # Weibull
  ks.normal = ks.test(data.mensual$pp, "pnorm", ajuste.normal$estimate[1], ajuste.normal$estimate[2]) # Normal
  ks.logistica = ks.test(data.mensual$pp, "plogis", ajuste.logistica$estimate[1], ajuste.logistica$estimate[2]) # Logistica
  
  # Criterio de akaike
  aic.exponencial = AIC(ajuste.exponencial)
  aic.gamma = AIC(ajuste.gamma)
  aic.LogNorm = AIC(ajuste.LogNorm)
  aic.weibull = AIC(ajuste.weibull)
  aic.normal = AIC(ajuste.normal)
  aic.logistica = AIC(ajuste.logistica)
  
  # Almacenamiento de los parámetros en un dataframe
  parametros = data.frame(
    statics = c(ks.exponencial$statistic, ks.gamma$statistic, ks.LogNorm$statistic, ks.weibull$statistic, ks.normal$statistic, ks.logistica$statistic),
    p.value = c(ks.exponencial$p.value, ks.gamma$p.value, ks.LogNorm$p.value, ks.weibull$p.value, ks.normal$p.value, ks.logistica$p.value),
    aic = c(aic.exponencial, aic.gamma, aic.LogNorm, aic.weibull, aic.normal, aic.logistica)
  )
  
  names(parametros) = c("Estadístico", "p-valor", "AIC")
  rownames(parametros) = c("Exponencial", "Gamma", "Log-Normal", "Weibull", "Normal", "Logistica")
  
  parametros <<- parametros
  
  # Selección de la mejor distribución
  best.KS = parametros[which.min(parametros$`p-valor`),]
  best.AIC = parametros[which.min(parametros$AIC),]
  dlg_message(paste("La mejor distribución según el test de Kolmogorov-Smirnov es: ", rownames(best.KS), "(p-value:", best.KS$`p-valor`,")"), type = c("ok"))
  dlg_message(paste("La mejor distribución según el criterio de Akaike es: ", rownames(best.AIC), "(AIC:", best.AIC$AIC,")"), type = c("ok"))
  

  # # Distribuciones de probabilidad ajustadas
  # SPI.exp = qnorm(pexp(data.mensual$pp, ajuste.exponencial$estimate[1]), mean = 0, sd = 1) # exponencial
  # SPI.gamma = qnorm(pgamma(data.mensual$pp, ajuste.gamma$estimate[1], ajuste.gamma$estimate[2]), mean = 0, sd = 1) # gamma
  # SPI.LogNorm = qnorm(plnorm(data.mensual$pp, ajuste.LogNorm$estimate[1], ajuste.LogNorm$estimate[2]), mean = 0, sd = 1) # Log-Normal
  # SPI.weibull = qnorm(pweibull(data.mensual$pp, ajuste.weibull$estimate[1], ajuste.weibull$estimate[2]), mean = 0, sd = 1) # Weibull
  # SPI.normal = qnorm(pnorm(data.mensual$pp, ajuste.normal$estimate[1], ajuste.normal$estimate[2]), mean = 0, sd = 1) # Normal
  # SPI.logistica = qnorm(plogis(data.mensual$pp, ajuste.logistica$estimate[1], ajuste.logistica$estimate[2]), mean = 0, sd = 1) # Logistica
  # 
  # # grafica comparativa
  # SPI.serie = data.frame(cbind(SPI.exp, SPI.gamma, SPI.LogNorm, SPI.weibull, SPI.normal, SPI.logistica))
  # SPI.serie = cbind(data.mensual$date, SPI.serie)
  # names(SPI.serie) = c("date", "Exponencial", "Gamma", "Log-Normal", "Weibull", "Normal", "Logistica")
  # 
  # # Grafica de las series
  # SPI.serie = data.table(SPI.serie)
  # SPI.serie = melt(SPI.serie, id.vars = "date")
  # ggplot(SPI.serie, aes(x = date, y = value, color = variable)) + geom_line() + theme_minimal() + labs(title = "SPI", x = "Fecha", y = "SPI")
  
  return(list(ajuste.exponencial = ajuste.exponencial, ajuste.gamma = ajuste.gamma, ajuste.LogNorm = ajuste.LogNorm, ajuste.weibull = ajuste.weibull, ajuste.normal = ajuste.normal, ajuste.logistica = ajuste.logistica))
  
}

SPI = function(ds){
  if(ds == "Exponencial"){
    SPI = qnorm(pexp(data.mensual$pp, distribucion$ajuste.exponencial$estimate[1]), mean = 0, sd = 1)
  } else if(ds == "Gamma"){
    SPI = qnorm(pgamma(data.mensual$pp, distribucion$ajuste.gamma$estimate[1], distribucion$ajuste.gamma$estimate[2]), mean = 0, sd = 1)
  } else if(ds == "Log-Normal"){
    SPI = qnorm(plnorm(data.mensual$pp, distribucion$ajuste.LogNorm$estimate[1], distribucion$ajuste.LogNorm$estimate[2]), mean = 0, sd = 1)
  } else if(ds == "Weibull"){
    SPI = qnorm(pweibull(data.mensual$pp, distribucion$ajuste.weibull$estimate[1], distribucion$ajuste.weibull$estimate[2]), mean = 0, sd = 1)
  } else if(ds == "Normal"){
    SPI = qnorm(pnorm(data.mensual$pp, distribucion$ajuste.normal$estimate[1], distribucion$ajuste.normal$estimate[2]), mean = 0, sd = 1)
  } else if(ds == "Logistica"){
    SPI = qnorm(plogis(data.mensual$pp, distribucion$ajuste.logistica$estimate[1], distribucion$ajuste.logistica$estimate[2]), mean = 0, sd = 1)
  }
  return(SPI)
}

# ------------------------------------------------------------------------------
######################### Llamado a funciones y Gráficos #######################
distribucion = best.dist()
SPI = SPI("Weibull")

# 1.0 Gráfica de las series SPI
SPI.serie = cbind(as.character(data.mensual$date), SPI)
SPI.serie = data.frame(SPI.serie)
names(SPI.serie) = c("date", "SPI")
SPI.serie$date = as.Date(SPI.serie$date, format = "%Y-%m-%d")
SPI.serie$SPI = as.numeric(SPI.serie$SPI)
SPI.serie = data.frame(SPI.serie)

# 1.1 Gráfica de las series
lmts = data.frame(
  categoria = c("Extremadamente humedo", "Muy humedo", "Moderadamente humedo", "Normal", 
  "Moderadamente seco", "Severamente seco", "Extremadamente seco"),
  limites = c(2, 1.5, 1, -0.99, -1, -1.5, -2)
)

pl = 
  ggplot() +
  geom_line(data = SPI.serie, aes(x = date, y = SPI), 
            size = 1, alpha = 1) +
  geom_hline(data = lmts, aes(yintercept = 2, color = "Extremadamente humedo"),
             linetype = "dashed", size = 1, alpha = 0.7) +
  geom_hline(data = lmts, aes(yintercept = 1.5, color = "Muy humedo"),
             linetype = "dashed", size = 1) +
  geom_hline(data = lmts, aes(yintercept = 1, color = "Moderadamente humedo"),
             linetype = "dashed", size = 1) +
  geom_hline(data = lmts, aes(yintercept = -0.99,color = "Normal"),
             linetype = "dashed", size = 1) +
  geom_hline(data = lmts, aes(yintercept = -1, color = "Moderadamente seco"),
             linetype = "dashed", size = 1) +
  geom_hline(data = lmts, aes(yintercept = -1.5, color = "Severamente seco"),
             linetype = "dashed", size = 1) +
  
  geom_hline(data = lmts, aes(yintercept = -2,color = "Extremadamente seco"),
             linetype = "dashed", size = 1) +
  
    
  labs(x = "Fechas", y = "SPI",
       title = "SPI") +
  theme_minimal() +
  scale_color_manual(values = c("Extremadamente humedo" = "red", "Muy humedo" = "mediumblue", 
                                "Moderadamente humedo" = "#743414", "Normal" = "green",
                                "Moderadamente seco" = "#78288c", "Severamente seco" = "#f7a40c",
                                "Extremadamente seco" = "#ceab86"),
                     name = "Categorias") +
  
  guides(color = guide_legend(
    override.aes = list(linetype = c("solid"), lwd = 4))) +
  theme(
    axis.title = element_text(size = 20, face = "bold"),
    axis.text = element_text(size = 18, hjust = 0.9),   # Tamaño de la letra en los ejes
    legend.title = element_text(
      colour = "brown",
      face = "bold",
      size = 12),
    legend.position = "bottom",
    legend.justification = c(0.5, 0.5),
    legend.background = element_rect(fill = "white", colour = "black", linewidth = 1),
    plot.title = element_text(
      hjust = 0.5,
      size = rel(2), lineheight = .9,
      face = "bold", colour = "brown")) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  
  scale_y_continuous(breaks = seq(-3, 2, 0.5)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  
  geom_rect(aes(xmin =  as.Date(-Inf), xmax = as.Date(Inf), ymin = -Inf, ymax = Inf),
            color = "black", fill = NA, size = 1.5) 
print(pl)

# ------------------------------------------------------------------------------
######################### Clasificacion del SPI ###############################
# Clasificación del SPI
limites = c(-100, -2, -1.5, -1, 1, 1.5, 2, 100)

# Etiqueta las categorías con las interpretaciones
categorias = c("extremadamente seco", "severamente seco", "moderadamente seco", 
                "normal", "moderadamente húmedo", 
                "muy húmedo", "extremadamente húmedo")

C.SPI = cut(SPI.serie$SPI, breaks = limites, labels = categorias)
C.SPI = cbind(SPI.serie, C.SPI)
names(C.SPI) = c("date", "SPI", "Clasificación")

# porcentaje de cada categoria
porcentaje = table(C.SPI$Clasificación) / length(C.SPI$Clasificación) * 100
porcentaje = round(porcentaje, 2)
porcentaje = data.frame(porcentaje)
names(porcentaje) = c("Categoria", "%")

# ------------------------------------------------------------------------------
