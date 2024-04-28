# Liberias necesarias ----------------------------------------------------------
library(dplyr)
library(hydroGOF)
library(caret)
# ------------------------------------------------------------------------------
dia.juliano = function(df) {
  # Igualo los años para que todos tengan 365 dias
  df_29.i = which(format(df$Fecha, "%m-%d") == "02-29")
  df_29 = df[df_29.i,]
  
  # Elimino los años bisiestos
  df = df[-df_29.i,]
  rownames(df) = NULL
  df$anio = as.numeric(format(df$Fecha, "%Y"))
  
  # Subdivido mi set de datos
  df_temp1 = df[!df$anio %in% c(2016, 2020, 2024),]
  df_temp2 = df[df$anio %in% c(2016, 2020, 2024),] # incluyen datos de 29 de febrero
  
  # Calculo el día juliano para aquellos años no bisiestos
  df_temp1$dia_juliano = as.numeric(format(df_temp1$Fecha, "%j"))
  
  # Calculo el día juliano para aquellos años bisiestos
  df_temp2$dia_juliano = seq_len(nrow(df_temp2)) %% 365
  df_temp2$dia_juliano[df_temp2$dia_juliano == 0] = 365
  
  # Uno los dos dataframes
  df = rbind(df_temp1, df_temp2)
  # ordeno por fecha
  df = df[order(df$Fecha),]
}
promedio.diaJuliano = function(df) {
  df = df %>% select(dia_juliano, promedio)
  model = as.matrix(df)
  prom.day = array(0,dim=c(1,365))
  
  for (i in 1:365){
    prom.day[i]=mean(model[model[,1]==i,2],na.rm=TRUE)
  }
  return(prom.day)
}
factores.correcion = function(prom.obs,prom.sat, name ){
  fact_correc = array(0,dim=c(1,365))
  for (i in 1:365){
    fact_correc[i]= prom.obs[i]/prom.sat[i]
  }
  fact_correc = t(fact_correc)
  seq = seq(1,365,1)
  fact_correc = cbind(seq, fact_correc)
  fact_correc = as.data.frame(fact_correc)
  names(fact_correc) = c("dia_juliano", "factor_correcion")
  directory.save_fc = "C:/Users/Jonna/OneDrive - ucuenca.edu.ec/Universidad/Tesis/Datos Satelitales/Factores de corrección"
 # write.csv(fact_correc, paste(directory.save_fc, "/", name, ".csv", sep = ""))
  return(fact_correc)
}
datos.corregidos = function(data_crudo, fact_correc, name.g){
  data_crudo = data_crudo %>% select(Fecha, dia_juliano, prec) # nomenclatura 
  data.original = data_crudo
  for (i in 1:365){
    data_crudo[data_crudo[,2]==i,3] = data_crudo[data_crudo[,2]==i,3]* fact_correc[fact_correc[,1]==i,2]
  }
  names(data_crudo) = c("Fecha", "dia_juliano", "prec_corregida")
  df = merge(data.original, data_crudo, by = c("Fecha"), all = TRUE)
  df = df %>% select(Fecha, prec, prec_corregida)
  direc.save = "C:/Users/Jonna/OneDrive - ucuenca.edu.ec/Universidad/Tesis/Datos Satelitales/Datos_Corregidos"
 # write.csv(df, paste(direc.save, "/", name.g, ".csv", sep = ""))
  return(df)
}

# ------------------------------------------------------------------------------
# Cargar datos Observados ------------------------------------------------------
directory = "C:/Users/Jonna/OneDrive - ucuenca.edu.ec/Universidad/Tesis/Scripts R/Preprocesamiento de datos/Dependencias/Estaciones meteorologicas/Datos procesados/Diario"
data.Ventanas = read.csv(paste(directory, "/Ventanas.csv", sep = ""))
data.Chaucha = read.csv(paste(directory, "/Chaucha.csv", sep = ""))
data.Izcairrumi = read.csv(paste(directory, "/Izcairrumi.csv", sep = ""))
data.MamamagM = read.csv(paste(directory, "/MamamagM.csv", sep = ""))
# Cargar datos Satelitales -----------------------------------------------------
dir.satelital = "C:/Users/Jonna/OneDrive - ucuenca.edu.ec/Universidad/Tesis/Datos Satelitales/Datos_extraidos/precipitacion_crudo"
micro.Bermejos = read.csv(paste(dir.satelital, "/Bermejos_crudo.csv", sep = ""))
micro.Soldados = read.csv(paste(dir.satelital, "/Soldados_crudo.csv", sep = ""))
micro.Galgan = read.csv(paste(dir.satelital, "/Galgan_crudo.csv", sep = ""))
micro.Quinsacocha = read.csv(paste(dir.satelital, "/Quinsacocha_crudo.csv", sep = ""))
micro.Yanuncay = read.csv(paste(dir.satelital, "/Yanuncay_crudo.csv", sep = ""))


rename = function(data) {
  names(data) = c("Fecha", "prec")
  data$Fecha = as.Date(data$Fecha, format = "%Y-%m-%d")
  return(data)
}

# ------------------------------------------------------------------------------
######################## Microcuencas Bermejos - Soldados ######################
# ------------------------------------------------------------------------------
# Preparación de los factores de corrección de datos observados ----------------
data.Ventanas = rename(data.Ventanas)
data.Izcairrumi = rename(data.Izcairrumi)
data.MamamagM = rename(data.MamamagM)
# Promedio de las estaciones Ventanas e Izcairrumi (Observadas)
data.B_S = merge(data.Ventanas, data.Izcairrumi, by = "Fecha", all = TRUE)
names(data.B_S) = c("Fecha", "Ventanas", "Izcairrumi")
data.B_S = merge(data.B_S, data.MamamagM, by = "Fecha", all = TRUE)
names(data.B_S) = c("Fecha", "Ventanas", "Izcairrumi", "MamamagM")
data.B_S$promedio = apply(data.B_S[,2:4], 1, mean, na.rm = TRUE)
data.B_S = data.B_S[,c(1,5)]
#obtener la fecha minima donde la columna promedio no sea NA
min_date = min(data.B_S$Fecha[!is.na(data.B_S$promedio)])
data.B_S = data.B_S[data.B_S$Fecha >= min_date,]
fecha.min = min(data.B_S$Fecha)
fecha.max = max(data.B_S$Fecha)
summary(data.B_S)


# Preparo datos -----------------------------------------------------------
#Promedio de las estaciones ventanas e Izcairrumi (Satelitales)
data.Bermejos = rename(micro.Bermejos)
data.Bermejos = data.Bermejos[data.Bermejos$Fecha >= fecha.min & data.Bermejos$Fecha <=fecha.max,]
data.Bermejos = dia.juliano(data.Bermejos)

# promedio de los dias julianos 
names(data.Bermejos) = c("Fecha", "promedio", "anio", "dia_juliano")
promedio.Bermejos = promedio.diaJuliano(data.Bermejos)

# Corrección de los datos satelitales de Bermejos
data.crudoBermejos = rename(micro.Bermejos)
data.crudoBermejos = dia.juliano(data.crudoBermejos)
######################## Validación cruzada con 5 folds ########################
# 80% de los datos para entrenamiento y 20% para validación
set.seed(123)
data.clean = na.omit(data.B_S)
summary(data.clean)
indices = sample(1:nrow(data.clean), 0.8 * nrow(data.clean))
data.train = data.clean[indices,]
data.test = data.clean[-indices,]

folds = createFolds(data.train$promedio, k = 5, list = TRUE, returnTrain = TRUE)
resultados = list()
for (i in 1:5){
  train = data.train[folds[[i]],]
  test = data.train[-folds[[i]],]
  
  train = rename(train)
  train = dia.juliano(train)
  names(train) = c("Fecha", "promedio", "anio", "dia_juliano")
  promedio_train = promedio.diaJuliano(train)

  factores = factores.correcion(promedio_train, promedio.Bermejos, paste("FC_Bermejos_Soldados_", i, sep = ""))
  data_corregida <- datos.corregidos(data.crudoBermejos, factores, paste("Bermejos_corregido_", i, sep = ""))
  
  test = rename(test)
  test = dia.juliano(test)
  names(test) = c("Fecha", "promedio", "anio", "dia_juliano")
  promedio_test = promedio.diaJuliano(test)

  resultados[[i]] = list(train = train, test = test, factores = factores, data_corregida = data_corregida)
}

fold <- resultados[[1]]

# Aplicar los factores de corrección al test_data
data_crudo_test <- rename(micro.Bermejos) # Cambiar a los datos crudos correspondientes al test_data
data_crudo_test <- dia.juliano(data_crudo_test)
data_corregida_test <- datos.corregidos(data_crudo_test, fold$factores, "Bermejos_corregido_test")

# Comparar la precipitación corregida con la observada en test_data
resultados_test <- merge(fold$test, data_corregida_test, by = "Fecha", all = TRUE)
gof(resultados_test$prec, resultados_test$prec_corregida)

