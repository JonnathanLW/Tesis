############################ Procesamiento Caudales ############################
#Versión: 1.0.0
# ------------------------------------------------------------------------------
# Librerías necesarias ---------------------------------------------------------
library(caret)
library(e1071)
# ------------------------------------------------------------------------------
directory = "C:/Users/Jonna/Desktop/Randon_Forest/Caudales/"
BermejosAjYanuncay = read.csv(paste0(directory, "BermejosAjYanuncay.csv"), header = TRUE, sep = ",")
SoldadosAjYanuncay = read.csv(paste0(directory, "SOLDADOSAJYANUNCAY.csv"), header = TRUE, sep = ",")
TarquiAjYanuncay = read.csv(paste0(directory, "TarquiAjYanuncayL.csv"), header = TRUE, sep = ",")
YanuncayAjTarqui = read.csv(paste0(directory, "YanuncayAjTarqui.csv"), header = TRUE, sep = ",")
#YanuncayPucan = read.csv(paste0(directory, "YanuncayPucan1.csv"), header = TRUE, sep = ",")
# ------------------------------------------------------------------------------
preparar.datos = function(df){
  names(df) = c("TIMESTAMP", "nivel")
  df$TIMESTAMP = as.Date(df$TIMESTAMP, format = "%Y-%m-%d")
  df$nivel = as.numeric(df$nivel)
  
  # # convierto caudales
  # for (i in 1:(nrow(df))){
  #   if (i < 95){
  #     df$Caudal[i] = 0.0116 * df$nivel[i] ** 1.086
  #   } else {
  #     df$Caudal[i] =  0.00000145966 * ((df$nivel[i]-20)**3.3435)
  #   }
  # }
  # 
  # # selecciono las columnas que me interesan
  # df = df[,c("TIMESTAMP", "Caudal")]
  # # voy filtrando por fechas de interés
  # fecha.min = as.Date("2014-05-14")
  # fecha.max = as.Date("2023-06-06")
  # df = df[(df$TIMESTAMP >= fecha.min & df$TIMESTAMP <= fecha.max),]
  return(df)
}

BermejosAjYanuncay = preparar.datos(BermejosAjYanuncay)
SoldadosAjYanuncay = preparar.datos(SoldadosAjYanuncay)
TarquiAjYanuncay = preparar.datos(TarquiAjYanuncay)
YanuncayAjTarqui = preparar.datos(YanuncayAjTarqui)
YanuncayPucan = preparar.datos(YanuncayPucan)
# ------------------------------------------------------------------------------

df = merge(BermejosAjYanuncay, SoldadosAjYanuncay, by = "TIMESTAMP", all = TRUE)
names(df) = c("TIMESTAMP", "BermejosAjYanuncay", "SoldadosAjYanuncay")
df = merge(df, TarquiAjYanuncay, by = "TIMESTAMP", all = TRUE)
names(df) = c("TIMESTAMP", "BermejosAjYanuncay", "SoldadosAjYanuncay", "TarquiAjYanuncay")
df = merge(df, YanuncayAjTarqui, by = "TIMESTAMP", all = TRUE)
names(df) = c("TIMESTAMP", "BermejosAjYanuncay", "SoldadosAjYanuncay", "TarquiAjYanuncay", "YanuncayAjTarqui")
df = merge(df, YanuncayPucan, by = "TIMESTAMP", all = TRUE)
names(df) = c("TIMESTAMP", "BermejosAjYanuncay", "SoldadosAjYanuncay", "TarquiAjYanuncay", "YanuncayAjTarqui", "YanuncayPucan")

cor(df[,2:5], use = "complete.obs")
summary(df)

# Cross validation -------------------------------------------------------------
set.seed(123)
data = df
data = na.omit(data)
train.control = trainControl(method = "cv", number = 5)
model = train(YanuncayAjTarqui ~  BermejosAjYanuncay +  SoldadosAjYanuncay , data = data, method = "lm", trControl = train.control)
model$resample
coeficientes = coef(model$finalModel)
predicciones = predict(model, df)
predicciones = as.data.frame(predicciones)

yanuncay = df[,c("TIMESTAMP", "YanuncayAjTarqui")]

indices_na  = which(is.na(yanuncay$YanuncayAjTarqui))
yanuncay$YanuncayAjTarqui[indices_na] = predicciones$predicciones[indices_na]

# remplazar los nas de yanuncay por las predicciones
yanuncay$YanuncayAjTarqui[inindices] = predicciones[inindices]

# extraer todos los indices de predicciones independientemente si non na o no 



# unir datos entre yanuncay y predicciones mediante los indices
#yanuncay = yanuncay[indices.predicciones,]

yanuncay = as.data.frame(yanuncay)

# best.model = model$finalModel
# # data.final = df$YanuncayAjTarqui
# # data.final = data.frame(data.final)
# predicciones = predict(best.model, df)
predicciones = as.data.frame(predicciones)
summary(predicciones)
