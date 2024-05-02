# Liberias necesarias ----------------------------------------------------------
library(dplyr)
library(hydroGOF)
library(caret)

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
######################### Funciones auxiliares #################################
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
    prom.day[i]=median(model[model[,1]==i,2],na.rm=TRUE)
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
rename = function(data) {
  names(data) = c("Fecha", "prec")
  data$Fecha = as.Date(data$Fecha, format = "%Y-%m-%d")
  return(data)
}

######################### Validación cruzada con k folds #######################
n = 5 # número de folds
validacion.cruzada = function(data.obs, name.micro, name.fc, promedio.micro){
  # # ----- Debug
  # data.obs = data.B_S
  # name.micro = "Bermejos"
  # name.fc = "Bermejos-soldados"
  # promedio.micro = promedio.Bermejos
  # data.sat = micro.Bermejos
  # # ----- Debug
  # 80% de los datos para entrenamiento y 20% para validación
  set.seed(123)
  data.clean = na.omit(data.obs)
  indices = sample(1:nrow(data.clean), 0.8 * nrow(data.clean))
  data.train = data.clean[indices,]
  data.test = data.clean[-indices,]
  
  folds = createFolds(data.train$promedio, k = n, list = TRUE, returnTrain = TRUE)
  resultados = list()
  for (i in 1:n){
    train = data.train[folds[[i]],]
    test = data.train[-folds[[i]],]
    
    train = rename(train)
    train = dia.juliano(train)
    names(train) = c("Fecha", "promedio", "anio", "dia_juliano")
    promedio_train = promedio.diaJuliano(train)
    
    factores = factores.correcion(promedio_train, promedio.micro, paste("FC_",name.fc, i, sep = ""))
    data_corregida = datos.corregidos(data.crudoBermejos, factores, paste(name.micro, "_", i, sep = ""))
    
    obs_test = test$promedio
    pred_test = data_corregida$prec_corregida[match(test$Fecha, data_corregida$Fecha)]
    
    rmse = sqrt(mean((obs_test - pred_test)^2))
    mae = mean(abs(obs_test - pred_test))
    r = cor(obs_test, pred_test)
    pbias = 100 * sum(pred_test - obs_test) / sum(obs_test)
    
    resultados[[i]] = list(data.test = data.test, train = train, test = test, factores = factores,
                           data_corregida = data_corregida, rmse = rmse, mae = mae,
                           r = r, pbias = pbias)
  }
  
  # Imprimir MAE y RMSE de cada fold
  estadisticos_lista = list()
  
  for (i in 1:n) {
    estadisticos <- data.frame(
      Fold = i,
      MAE = resultados[[i]]$mae,
      RMSE = resultados[[i]]$rmse,
      r = resultados[[i]]$r,
      pbias = resultados[[i]]$pbias
    )
    estadisticos_lista[[i]] = estadisticos
  }
  estadisticos_final = do.call(rbind, estadisticos_lista)
  estadisticos <<- estadisticos_final
  
  return(resultados)
  
}
evaluacion.final = function(rest.validation, best.factor, data.crudo) {

  #-----
  # Evaluación del modelo con datos nunca vistos ----------------------------
  mejores_factores = rest.validation[[best.factor]]$factores
  
  # Aplicar los factores de corrección a los datos crudos para obtener los datos corregidos
  data.test = rest.validation[[best.factor]]$data.test
  data.test = rename(data.test)
  # data.test = dia.juliano(data.test) # bug encoentrado 
  data_corregida_test = datos.corregidos(data.crudo, mejores_factores, "Test")
  
  # Calcular métricas de error en el conjunto de prueba
  obs_test = data.test$prec
  pred_test = data_corregida_test$prec_corregida[match(data.test$Fecha, data_corregida_test$Fecha)]
  
  rmse_test = sqrt(mean((obs_test - pred_test), na.rm = TRUE)^2)
  mae_test = mean(abs(obs_test - pred_test), na.rm = TRUE)
  r_test = cor(obs_test, pred_test, use = "complete.obs")
  pbias_test = 100 * sum((pred_test - obs_test),na.rm = TRUE)  / sum(obs_test, na.rm = TRUE)
  
  evaluacion.final = data.frame(
    MAE = mae_test,
    RMSE = rmse_test,
    r = r_test,
    pbias = pbias_test
  )

  return(evaluacion.final)
  
}
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
######################## Carga de datos obs y satelitales ######################
# Cargar datos Observados ------------------------------------------------------
directory = "C:/Users/Jonna/OneDrive - ucuenca.edu.ec/Universidad/Tesis/Scripts R/Preprocesamiento de datos/Dependencias/Estaciones meteorologicas/Datos procesados/Diario"
data.Ventanas = read.csv(paste(directory, "/Ventanas.csv", sep = ""))
data.Chaucha = read.csv(paste(directory, "/Chaucha.csv", sep = ""))
data.Izcairrumi = read.csv(paste(directory, "/Izcairrumi.csv", sep = ""))
data.MamamagM = read.csv(paste(directory, "/MamamagM.csv", sep = ""))
data.Llaviucu = read.csv(paste(directory, "/Llaviucu.csv", sep = ""))
data.CebollarPTAPM = read.csv(paste(directory, "/CebollarPTAPM.csv", sep = ""))
# Cargar datos Satelitales -----------------------------------------------------
dir.satelital = "C:/Users/Jonna/OneDrive - ucuenca.edu.ec/Universidad/Tesis/Datos Satelitales/Datos_extraidos/precipitacion_crudo"
micro.Bermejos = read.csv(paste(dir.satelital, "/Bermejos_crudo.csv", sep = ""))
micro.Soldados = read.csv(paste(dir.satelital, "/Soldados_crudo.csv", sep = ""))
micro.Galgan = read.csv(paste(dir.satelital, "/Galgan_crudo.csv", sep = ""))
micro.Quinsacocha = read.csv(paste(dir.satelital, "/Quinsacocha_crudo.csv", sep = ""))
micro.Yanuncay = read.csv(paste(dir.satelital, "/Yanuncay_crudo.csv", sep = ""))
# ------------------------------------------------------------------------------

################################################################################
################################################################################
########################### Cálculos para las micro cuencas #####################

# -------------------------------Bermejos Soldados -----------------------------
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
#obtener la fecha mínima donde la columna promedio no sea NA
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

# Validación cruzada ------------------------------------------------------
cross.validationBERMEJOS = validacion.cruzada(data.B_S, "Bermejos", "Bermejos-soldados", promedio.Bermejos)
eval.BermF = evaluacion.final(cross.validationBERMEJOS, 3, data.crudoBermejos)

################################################################################
# --------------------------------- Yanuncay -----------------------------------
################################################################################
# Preparación de los factores de corrección de datos observados ----------------
data.MamamagM = rename(data.MamamagM)
data.Llaviucu = rename(data.Llaviucu)
data.CebollarPTAPM = rename(data.CebollarPTAPM)
# Promedio de las estaciones Ventanas e Izcairrumi (Observadas)
data.B_S = merge(data.MamamagM, data.Llaviucu, by = "Fecha", all = TRUE)
names(data.B_S) = c("Fecha", "MamamagM", "Llaviucu")
data.B_S = merge(data.B_S, data.CebollarPTAPM, by = "Fecha", all = TRUE)
names(data.B_S) = c("Fecha", "MamamagM", "Llaviucu", "CebollarPTAPM")
data.B_S$promedio = apply(data.B_S[,2:4], 1, mean, na.rm = TRUE)
data.B_S = data.B_S[,c(1,5)]
#obtener la fecha mínima donde la columna promedio no sea NA
min_date = min(data.B_S$Fecha[!is.na(data.B_S$promedio)])
data.B_S = data.B_S[data.B_S$Fecha >= min_date,]
fecha.min = min(data.B_S$Fecha)
fecha.max = max(data.B_S$Fecha)
summary(data.B_S)
# Preparo datos -----------------------------------------------------------
#Promedio de las estaciones ventanas e Izcairrumi (Satelitales)
data.Yanuncay = rename(micro.Yanuncay)
data.Yanuncay = data.Yanuncay[data.Yanuncay$Fecha >= fecha.min & data.Yanuncay$Fecha <=fecha.max,]
data.Yanuncay = dia.juliano(data.Yanuncay)

# promedio de los dias julianos 
names(data.Yanuncay) = c("Fecha", "promedio", "anio", "dia_juliano")
promedio.Yanuncay = promedio.diaJuliano(data.Yanuncay)

# Corrección de los datos satelitales de Bermejos
data.crudoYanuncay = rename(micro.Yanuncay)
data.crudoYanuncay = dia.juliano(data.crudoYanuncay)

# Validación cruzada ------------------------------------------------------
cross.validationYANUNCAY = validacion.cruzada(data.B_S, "Yanuncay", "Yanuncay", promedio.Yanuncay)
modelo.f = evaluacion.final(cross.validationYANUNCAY, 5, data.crudoYanuncay)

################################################################################
# --------------------------------- Galgan -----------------------------------
################################################################################
data.Ventanas = rename(data.Ventanas)
data.Izcairrumi = rename(data.Izcairrumi)
data.Chaucha = rename(data.Chaucha)
data.MamamagM = rename(data.MamamagM)
data.Llaviucu = rename(data.Llaviucu)
data.CebollarPTAPM = rename(data.CebollarPTAPM)
# Promedio de las estaciones Ventanas e Izcairrumi (Observadas)
data.B_S = merge(data.Ventanas, data.Izcairrumi, by = "Fecha", all = TRUE)
names(data.B_S) = c("Fecha", "Ventanas", "Izcairrumi")
data.B_S = merge(data.B_S, data.Chaucha, by = "Fecha", all = TRUE)
names(data.B_S) = c("Fecha", "Ventanas", "Izcairrumi", "Chaucha")
data.B_S = merge(data.B_S, data.MamamagM, by = "Fecha", all = TRUE)
names(data.B_S) = c("Fecha", "Ventanas", "Izcairrumi", "Chaucha", "MamamagM")
data.B_S = merge(data.B_S, data.Llaviucu, by = "Fecha", all = TRUE)
names(data.B_S) = c("Fecha", "Ventanas", "Izcairrumi", "Chaucha", "MamamagM", "Llaviucu")
data.B_S = merge(data.B_S, data.CebollarPTAPM, by = "Fecha", all = TRUE)
names(data.B_S) = c("Fecha", "Ventanas", "Izcairrumi", "Chaucha", "MamamagM", "Llaviucu", "CebollarPTAPM")
data.B_S$promedio = apply(data.B_S[,2:7], 1, mean, na.rm = TRUE)
data.B_S = data.B_S[,c(1,8)]

#obtener la fecha mínima donde la columna promedio no sea NA
min_date = min(data.B_S$Fecha[!is.na(data.B_S$promedio)])
data.B_S = data.B_S[data.B_S$Fecha >= min_date,]
fecha.min = min(data.B_S$Fecha)
fecha.max = max(data.B_S$Fecha)
summary(data.B_S)
# Preparo datos -----------------------------------------------------------
#Promedio de las estaciones ventanas e Izcairrumi (Satelitales)
data.Galgan = rename(micro.Galgan)
data.Galgan = data.Galgan[data.Galgan$Fecha >= fecha.min & data.Galgan$Fecha <=fecha.max,]
data.Galgan = dia.juliano(data.Galgan)


# promedio de los dias julianos 
names(data.Galgan) = c("Fecha", "promedio", "anio", "dia_juliano")
promedio.Galgan = promedio.diaJuliano(data.Galgan)

# Corrección de los datos satelitales de Galgan
data.crudoGalgan = rename(micro.Galgan)
data.crudoGalgan = dia.juliano(data.crudoGalgan)

# Validación cruzada ------------------------------------------------------
# Validación cruzada ------------------------------------------------------
cross.validationGALGAN = validacion.cruzada(data.B_S, "Galgan", "Galgan", promedio.Galgan)
modelo.GalgaF = evaluacion.final(cross.validationGALGAN, 5, data.crudoGalgan)

