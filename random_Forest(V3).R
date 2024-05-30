################################################################################
# Información del algoritmo ----------------------------------------------------
# versión: 1.0
# Notas:
# - Se utiliza cross - validation para analisar sobreajuste
# -
# -

################################################################################
# Librerías necesarias ---------------------------------------------------------
library(caret)
library(hydroGOF)
library(ggplot2)
library(randomForest)

################################################################################
# Funciones necesarias ---------------------------------------------------------
convertir.nivel = function(df){
  names(df) = c("TIMESTAMP", "nivel")
  df$TIMESTAMP = as.Date(df$TIMESTAMP, format = "%Y-%m-%d")
  df$nivel = as.numeric(df$nivel)
  # # selecciono las columnas que me interesan
  df = df[,c("TIMESTAMP", "nivel")]
  names(df) = c("Fecha", "caudal")
  return(df)
}
# Preparación de datos ---------------------------------------------------------
caudal = read.csv("C:/Users/Jonna/Desktop/Nueva carpeta/Yanuncay_diario.csv", header = TRUE)
caudal = convertir.nivel(caudal)
summary(caudal)

# selecciono todas las filas q sean menores a 300
ind.1 = which(caudal$caudal > 400)
caudal$caudal[ind.1] = NA
summary(caudal)

df_train = caudal
df_train$Year = as.numeric(format(df_train$Fecha, "%Y"))
df_train$Month = as.numeric(format(df_train$Fecha, "%m"))
df_train$Day = as.numeric(format(df_train$Fecha, "%d"))

df_train = df_train[,c("Year", "Month", "Day", "caudal")]
head(df_train,4)

# divido mi set de datos
index.NA = which(is.na(df_train$caudal))
df_complete = df_train[-index.NA,]
df_missing = df_train[index.NA,]

set.seed(1111)
train.index = sample(seq_len(nrow(df_complete)), size = 0.8 * nrow(df_complete), replace = FALSE)

data.train = df_complete[train.index,] # para entrenar mi modelo 
data.test = df_complete[-train.index,] # para evaluar el modelo final


# Busqueda de hyperparametros  -------------------------------------------------
# NUMERO DE ARBOLES 
set.seed(1)
# train.control = trainControl(method = "cv", number = 10) # 10 pliegues
# ntree = seq(100, 1000, by = 100)
# mtry_values = c(1, 2, 3)
# tuneGrid = expand.grid(mtry = mtry_values)
# 
# set.seed(123)
# results = data.frame(ntree = integer(), mtry = integer(), RMSE = numeric(), MAE = numeric(), Rsquared = numeric())
# for (i in 1:length(ntree)) {
#   for (j in 1:length(mtry_values)) {
#     model.ntree = train(caudal ~ ., 
#                         data = data.train, 
#                         method = "rf", 
#                         ntree = ntree[i],
#                         tuneGrid = data.frame(mtry = mtry_values[j]),
#                         trControl = train.control)
#     
#     # Obtener los valores de las métricas
#     RMSE_value = model.ntree$results$RMSE
#     MAE_value = model.ntree$results$MAE
#     Rsquared_value = model.ntree$results$Rsquared
#     
#     # Agregar los resultados al data frame
#     results = rbind(results, 
#                     data.frame(ntree = ntree[i], mtry = mtry_values[j], RMSE = RMSE_value, MAE = MAE_value, Rsquared = Rsquared_value))
#   }
# }
# 
# ggplot(data = results, aes(x = factor(ntree), y = MAE, color = factor(mtry), group = factor(mtry))) +
#   geom_line(size = 1) +  # Todas las líneas serán sólidas
#   geom_point(aes(shape = factor(mtry)), size = 3) +
#   labs(title = "MAE vs ntree and mtry",
#        x = "ntree",
#        y = "MAE",
#        color = "mtry",
#        shape = "mtry") +
#   scale_color_manual(values = c("black", "red", "blue", "green", "purple")) +
#   scale_shape_manual(values = c(15, 16, 17, 18, 19)) +
#   theme_bw() +
#   theme(
#     axis.title = element_text(size = 20, face = "bold"),
#     axis.text = element_text(size = 18, hjust = 0.9),   # Tamaño de la letra en los ejes
#     legend.title = element_text(
#       colour = "brown",
#       face = "bold",
#       size = 12),
#     legend.position = "right",
#     legend.text = element_text(size = 12),
#     legend.background = element_rect(fill = "white", colour = "black", linewidth = 1),
#     plot.title = element_text(
#       hjust = 0.5,
#       size = rel(2), lineheight = .9,
#       face = "bold", colour = "brown")) +
#   theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
#   geom_rect(aes(xmin = -Inf, xmax =  Inf, ymin = -Inf, ymax = Inf),
#             color = "black", fill = NA, size = 1.5)
# 
# # guardo mi grafico
# directory = "C:/Users/Jonna/Desktop/Randon_Forest/RandomForest_Caudal/hiperparametros/"
# ggsave(paste0(directory, "_MAE.png"),
#        width = 12, height = 8, units = "in", dpi = 300, type = "cairo")
#   


# RandomForest + validación cruzada --------------------------------------------
set.seed(12)
tuneGrid = expand.grid(mtry = c(3))
train.control = trainControl(method = "cv", number = 10) # 10 pliegues

# entreno mi modelo 
set.seed(1235)
model.rf = train(caudal ~ ., 
                 data = data.train, 
                 method = "rf", 
                 ntree = 600,
                 replace = TRUE,
                 tuneGrid = tuneGrid,
                 trControl = train.control)

print(model.rf)


# Hago predicciones en el set de evaluación del modelo final
predicciones.prueba = predict(model.rf, newdata = data.test)

# Calcular métricas de rendimiento en el set de evaluación del modelo final
test.results.prueba = postResample(predicciones.prueba, data.test$caudal)

# Obtener las estadísticas de cada fold
folds.results = model.rf$resample

# comparo métricas
analisis.sobreajuste = data.frame(
  training.folds = c(mean(folds.results$RMSE), mean(folds.results$Rsquared), mean(folds.results$MAE)),
  set.evaluación = c(test.results.prueba[1], test.results.prueba[2], test.results.prueba[3])
)

analisis.sobreajuste

# prediccion final de caudales vacios 
predicciones.NA = predict(model.rf, newdata = df_missing)
df_missing$caudal = predicciones.NA

# set rellenado
df.rellenado = rbind(df_complete, df_missing)
df.rellenado$Fecha = as.Date(paste(df.rellenado$Year, df.rellenado$Month, df.rellenado$Day, sep = "-"))
df.rellenado = df.rellenado[,c("Fecha", "caudal")]
df.rellenado = df.rellenado[order(df.rellenado$Fecha),]
names(df.rellenado) = c("Fecha", "caudal_rellenado")

write.csv(df.rellenado, "C:/Users/Jonna/Desktop/Nueva carpeta/Yanuncay (600 NTREE).csv")
# set original
df.original = df_train
df.original$Fecha = as.Date(paste(df.original$Year, df.original$Month, df.original$Day, sep = "-"))
df.original = df.original[,c("Fecha", "caudal")]
df.original = df.original[order(df.original$Fecha),]
names(df.original) = c("Fecha", "caudal_original")

# gráfico

pl = 
  ggplot() +
  geom_line(data = df.rellenado, aes(x = Fecha, y = caudal_rellenado, color = "Nivel del agua rellenado"), 
            size = 1, alpha = 1) +
  geom_line(data = df.original, aes(x = Fecha, y = caudal_original, color = "Nivel del agua original"), 
            size = 1, alpha = 1) +

  labs(x = "Fechas", y = "Nivel del agua (cm)",
       title = "Niveles del agua rellenados vs Niveles de agua original") +
  theme_minimal() +
  scale_color_manual(values = c("Nivel del agua rellenado" = "blue", "Nivel del agua original" = "red"),
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
  
  scale_y_continuous(breaks = seq(0, 300, 50)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  
  geom_rect(aes(xmin =  as.Date(-Inf), xmax = as.Date(Inf), ymin = -Inf, ymax = Inf),
            color = "black", fill = NA, size = 1.5) 
print(pl)
directory = "C:/Users/Jonna/Desktop/Randon_Forest/RandomForest_Caudal/hiperparametros/"
ggsave(paste0(directory, "_imputado.png"),
       plot = pl,
       width = 12, height = 8, units = "in", dpi = 300, type = "cairo")
