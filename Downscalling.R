# Liberias necesarias ----------------------------------------------------------
library(dplyr)
library(hydroGOF)
library(caret)
library(qmap)

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
######################### Funciones auxiliares #################################
dia.juliano = function(df) {
  Fecha.1 = as.Date("2016-02-29")
  Fecha.2 = as.Date("2020-02-29")
  Fecha.3 = as.Date("2024-02-29")
  df_julianos = as.Date(c(Fecha.1, Fecha.2, Fecha.3))
  fechas.presentes = intersect(df_julianos, df$Fecha)
  
  # Igualo los años para que todos tengan 365 dias
  df_29.i = which(format(df$Fecha, "%m-%d") == "02-29")
  df_29 = df[df_29.i,]
  
  f = function(n){
    df = df[-df_29.i,]
    rownames(df) = NULL
    df$anio = as.numeric(format(df$Fecha, "%Y"))
    df_temp1 = df[!df$anio %in% c(n),]
    df_temp2 = df[df$anio %in% c(n),] # incluyen datos de 29 de febrero
    
    # Calculo el día juliano para aquellos años no bisiestos
    df_temp1$dia_juliano = as.numeric(format(df_temp1$Fecha, "%j"))
    
    # Calculo el día juliano para aquellos años bisiestos
    df_temp2$dia_juliano = seq_len(nrow(df_temp2)) %% 365
    df_temp2$dia_juliano[df_temp2$dia_juliano == 0] = 365
    
    # Uno los dos dataframes
    df = rbind(df_temp1, df_temp2)
    # ordeno por fecha
    df = df[order(df$Fecha),]
    # Acción si solo Fecha.1 está en df_compara
    return(df)
  }
  f.2 = function(n1, n2){
    df = df[-df_29.i,]
    rownames(df) = NULL
    df$anio = as.numeric(format(df$Fecha, "%Y"))
    df_temp1 = df[!df$anio %in% c(n1, n2),]
    df_temp2 = df[df$anio %in% c(n1, n2),] # incluyen datos de 29 de febrero
    # Calculo el día juliano para aquellos años no bisiestos
    df_temp1$dia_juliano = as.numeric(format(df_temp1$Fecha, "%j"))
    # Calculo el día juliano para aquellos años bisiestos
    df_temp2$dia_juliano = seq_len(nrow(df_temp2)) %% 365
    df_temp2$dia_juliano[df_temp2$dia_juliano == 0] = 365
    
    # Uno los dos dataframes
    df = rbind(df_temp1, df_temp2)
    # ordeno por fecha
    df = df[order(df$Fecha),]
    # Acción si solo Fecha.1 está en df_compara
    return(df)
  }
  f.3 = function(n1, n2, n3){
    df = df[-df_29.i,]
    rownames(df) = NULL
    df$anio = as.numeric(format(df$Fecha, "%Y"))
    df_temp1 = df[!df$anio %in% c(n1, n2, n3),]
    df_temp2 = df[df$anio %in% c(n1, n2, n3),] # incluyen datos de 29 de febrero
    # Calculo el día juliano para aquellos años no bisiestos
    df_temp1$dia_juliano = as.numeric(format(df_temp1$Fecha, "%j"))
    # Calculo el día juliano para aquellos años bisiestos
    df_temp2$dia_juliano = seq_len(nrow(df_temp2)) %% 365
    df_temp2$dia_juliano[df_temp2$dia_juliano == 0] = 365
    
    # Uno los dos dataframes
    df = rbind(df_temp1, df_temp2)
    # ordeno por fecha
    df = df[order(df$Fecha),]
    # Acción si solo Fecha.1 está en df_compara
    return(df)
  }
  
  if (nrow(df_29) > 1) {
    if(length(fechas.presentes) == 1 && as.Date("2016-02-29") %in% df$Fecha) {
      df = f(2016)
    } else if(length(fechas.presentes) == 1 && as.Date("2020-02-29") %in% df$Fecha) {
      df = f(2020)
    } else if(length(fechas.presentes) == 1 && as.Date("2024-02-29") %in% df$Fecha) {
      df = f(2024)
    } else if(length(fechas.presentes) == 2 && as.Date("2016-02-29") %in% df$Fecha && as.Date("2020-02-29") %in% df$Fecha) {
      df = f.2(2016, 2020)
      # Acción si Fecha.1 y Fecha.2 están juntas en df_compara
    } else if(length(fechas.presentes) == 2 && as.Date("2016-02-29") %in% df$Fecha && as.Date("2024-02-29") %in% df$Fecha) {
      df = f.2(2016, 2024)
      # Acción si Fecha.1 y Fecha.3 están juntas en df_compara
    } else if(length(fechas.presentes) == 2 && as.Date("2020-02-29") %in% df$Fecha && as.Date("2024-02-29") %in% df$Fecha) {
      df = f.2(2020, 2024)
      # Acción si Fecha.2 y Fecha.3 están juntas en df_compara
    } else if (length(fechas.presentes) == 3) {
      # Acción si las tres fechas están juntas en df_compara
      df = f.3(2016, 2020, 2024)
    } else {
      dlg_message("Verifique el script posibles errores encoentrados")
      stop("Error en el script")
    }
  }
  
  df$anio = as.numeric(format(df$Fecha, "%Y"))
  df$dia_juliano = as.numeric(format(df$Fecha, "%j"))
  df = df[order(df$Fecha),]
  }

promedio.diaJuliano = function(df) {
  df = data.frame(df)
  df = df %>% dplyr::select(dia_juliano, promedio)
  model = as.matrix(df)
  prom.day = array(0,dim=c(1,365))
  
  for (i in 1:365){
    prom.day[i]=median(model[model[,1]==i,2],na.rm=TRUE) # cambiar por median = mediana, mean = promedio
  }
  return(prom.day)
}

factores.correcion = function(prom.obs,prom.sat, name){
  
  fact_correc = array(0,dim=c(1,365))
  last_value = NA
  for (i in 1:365) {
    if (prom.sat[i] == 0) {
      if (!is.na(last_value)) {
        fact_correc[i] = last_value  # Usar el último valor calculado si prom.sat[i] es cero
      } else {
        fact_correc[i] = NA  # Si no hay un último valor, asignar NA
      }
    } else {
      fact_correc[i] = prom.obs[i] / prom.sat[i]
      last_value = fact_correc[i]  # Actualizar el último valor calculado
    }
  }
  
  fact_correc = t(fact_correc)
  seq = seq(1,365,1)
  fact_correc = cbind(seq, fact_correc)
  fact_correc = as.data.frame(fact_correc)
  
  names(fact_correc) = c("dia_juliano", "factor_correcion")
  # directory.save_fc = "C:/Users/Jonna/OneDrive - ucuenca.edu.ec/Universidad/Tesis/Datos Satelitales/Factores de corrección"
 # write.csv(fact_correc, paste(directory.save_fc, "/", name, ".csv", sep = ""))
  return(fact_correc)
}

datos.corregidos = function(data_crudo, fact_correc, name.g){
  
  data_crudo = data_crudo %>% dplyr::select(Fecha, dia_juliano, prec) # nomenclatura 
  data.original = data_crudo
  for (i in 1:365){
    data_crudo[data_crudo[,2]==i,3] = data_crudo[data_crudo[,2]==i,3]* fact_correc[fact_correc[,1]==i,2]
  }
  
  names(data_crudo) = c("Fecha", "dia_juliano", "prec_corregida")
  df = merge(data.original, data_crudo, by = c("Fecha"), all = TRUE)
  df = df %>% dplyr::select(Fecha, prec, prec_corregida)
  # direc.save = "C:/Users/Jonna/OneDrive - ucuenca.edu.ec/Universidad/Tesis/Datos Satelitales/Datos_Corregidos"
 # write.csv(df, paste(direc.save, "/", name.g, ".csv", sep = ""))
  return(df)
}

rename = function(data) {
  names(data) = c("Fecha", "prec")
  data$Fecha = as.Date(data$Fecha, format = "%Y-%m-%d")
  return(data)
}

######################### Validación cruzada con k folds #######################
n = 10 # número de folds
validacion.cruzada = function(data.obs, name.micro, name.fc, promedio.micro, data.crudo){
  set.seed(123)

  # data.obs = data.B_S
  # name.micro = "Bermejos"
  # name.fc = "Bermejos"
  # promedio.micro = promedio.Bermejos
  # data.crudo = data.crudoBermejos

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
    # any(is.na(promedio.micro)) # eliminar

    # Temportal

    factores = factores.correcion(promedio_train, promedio.micro, paste("FC_",name.fc, i, sep = ""))
    # any(is.na(factores)) # eliminar
    
    data_corregida = datos.corregidos(data.crudo, factores, paste(name.micro, "_", i, sep = ""))
    
    
    obs_test = test$promedio
    pred_test = data_corregida$prec_corregida[match(test$Fecha, data_corregida$Fecha)]
    valid_values = complete.cases(obs_test, pred_test)
    obs_testV = obs_test[valid_values]
    pred_testV = pred_test[valid_values]
    
    test = gof(pred_testV, obs_testV)
    test = data.frame(test)
    names(test) = "valor"
    # Crear un nuevo marco de datos con los nombres de las filas como una columna
    test.f = cbind(Estadistico = rownames(test), test)
    rownames(test.f)  = NULL
    
    me = mean(obs_testV - pred_testV)
    rmse = sqrt(mean(obs_testV - pred_testV)^2)
    mae = mean(abs(obs_testV - pred_testV))
    r = cor(obs_testV, pred_testV)
    pbias = 100 * sum(pred_testV - obs_testV) / sum(obs_testV)

    resultados[[i]] = list(data.test = data.test, train = train, test = test, factores = factores,
                           data_corregida = data_corregida, rmse = rmse, mae = mae,
                           r = r, pbias = pbias, test = test.f)
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
  
  gof_lista = list()
  
  # Calcular gof para cada iteración y almacenar los resultados en la lista
  for (i in 1:n) {
    test = resultados[[i]]$test
    gof_lista[[paste("Fold", i)]] = test
  }
  
  # Combinar los resultados en un único marco de datos
  gof_df = do.call(cbind, gof_lista)
  gof_df = as.data.frame(gof_df)
  colnames(gof_df) = paste("Fold", 1:n, sep = "_")
  gof_df <<- gof_df
  
  return(resultados)
  
}

evaluacion.final = function(rest.validation, best.factor, data.crudo) {
  
  # rest.validation = cross.validationBERMEJOS
  # best.factor = 1
  # data.crudo = data.crudoBermejos

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
  
  # data_corregido.final = datos.corregidos(data.crudo, factores, paste(name.micro, "_", i, sep = ""))
  
  valid_values = complete.cases(obs_test, pred_test)
  obs_testVV = obs_test[valid_values]
  pred_testVV = pred_test[valid_values]
  
  test = gof(pred_testVV, obs_testVV)
  test = data.frame(test)
  names(test) = "valor"
  # Crear un nuevo marco de datos con los nombres de las filas como una columna
  test.f = cbind(Estadistico = rownames(test), test)
  rownames(test.f)  = NULL

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
  
  GOF_final <<- test.f

  return(data_corregida_test)
  
}

###################### Corrección del sesgo (Mapeo de cuantiles) ###############
mapeo.cuantil_Bias = function(data.obser, data.satelit){
  
  names(data.obser) = c("Fecha", "prec")
  names(data.satelit) = c("Fecha", "prec")
  
  mapeo = ecdf(data.obser$prec)
  mapeo.cuantil = quantile(data.satelit$prec, probs = mapeo(data.satelit$prec))
  data.satelit$prec_cuantil =  mapeo.cuantil
  
  data.merge = merge(data.obser, data.satelit, by = "Fecha")
  names(data.merge) = c("Fecha", "prec", "prec_sat", "prec_cuantil")
  estadistico.cuant = gof(data.merge$prec_cuantil, data.merge$prec)
  estadisticos.sat = gof(data.merge$prec_sat, data.merge$prec)
  
  estadistico.cuant = data.frame(estadistico.cuant)
  estadistico.sat = data.frame(estadisticos.sat)
  
  names = rownames(estadistico.cuant)
  names = data.frame(names)
  
  estadisticos.f = cbind(names, estadistico.cuant[,1], estadistico.sat[,1])
  names(estadisticos.f) = c("Estadistico", "prec_Cuantil", "prec_Satelital")
  # Corrijo Bias de los datos satelitales y prec)cuanti
  bias.ind = which(estadisticos.f$Estadistico == "PBIAS %")
  values.bias = estadisticos.f[bias.ind,]
  
  bias.cuantil = 1 - (values.bias$prec_Cuantil / 100)
  bias.sat = 1 - (values.bias$prec_Satelital / 100)
  
  data.merge$prec_CuantilBias = data.merge$prec_cuantil * bias.cuantil
  data.merge$prec_SatBias = data.merge$prec_sat * bias.sat
  
  estadistico.cuantB = gof(data.merge$prec_CuantilBias, data.merge$prec)
  estadisticos.satB = gof(data.merge$prec_SatBias, data.merge$prec)
  
  estadistico.cuantB = data.frame(estadistico.cuantB)
  estadisticos.satB = data.frame(estadisticos.satB)
  
  estadisticos.f = cbind(estadisticos.f , estadistico.cuantB[,1], estadisticos.satB[,1])
  names(estadisticos.f) = c("Estadistico", "prec_Cuantil", "prec_Satelital", "prec_CuantilBias", "prec_SatBias")
  Estadisticos_CB <<- estadisticos.f
  return(data.merge)
}
# ------------------------------------------------------------------------------
################################################################################
################################################################################
################### Cálculos para las micro cuencas Bermejos ###################
# ------------------------------------------------------------------------------
######################## Carga de datos obs y satelitales ######################
# Cargar datos Observados ------------------------------------------------------
directory = "C:/Users/Jonna/Desktop/Randon_Forest/Estaciones_Tierra/Diario"
data.Ventanas = read.csv(paste(directory, "/Ventanas.csv", sep = ""))
data.Izcairrumi = read.csv(paste(directory, "/Izhcayrrumi.csv", sep = ""))

# Cargar datos Satelitales -----------------------------------------------------
dir.satelital = "C:/Users/Jonna/Desktop/Randon_Forest/AlgoritmoFC"
micro.Bermejos = read.csv(paste(dir.satelital, "/Bermejos.csv", sep = "")) # Probar con MSWEP puro y el MSWEP RANDON FOREST
micro.Bermejos$TIMESTAMP = as.Date(micro.Bermejos$TIMESTAMP, format = "%Y-%m-%d")

datemin.sat = min(micro.Bermejos$TIMESTAMP)
datemax.sat = max(micro.Bermejos$TIMESTAMP)

# ------------------------------------------------------------------------------
# -------------------------------Bermejos Soldados -----------------------------
# Preparación de los factores de corrección de datos observados ----------------
data.Ventanas = rename(data.Ventanas)
data.Izcairrumi = rename(data.Izcairrumi)
#data.SoldadosPTARM = rename(data.SoldadosPTARM)


#data.MamamagM = rename(data.MamamagM)
# Promedio de las estaciones Ventanas e Izcairrumi (Observadas)
data.B_S = merge(data.Ventanas, data.Izcairrumi, by = "Fecha", all = TRUE)
names(data.B_S) = c("Fecha", "Ventanas", "Izcairrumi")

# data.B_S = merge(data.B_S, data.SoldadosPTARM, by = "Fecha", all = TRUE)
# names(data.B_S) = c("Fecha", "Ventanas", "Izcairrumi", "SoldadosPTARM")
# data.B_S = merge(data.B_S, data.MamamagM, by = "Fecha", all = TRUE)
# names(data.B_S) = c("Fecha", "Ventanas", "Izcairrumi", "MamamagM")
data.B_S$promedio = apply(data.B_S[,2:3], 1, mean, na.rm = TRUE)
data.B_S = data.B_S[,c(1,4)]
#obtener la fecha mínima donde la columna promedio no sea NA
min_date = min(data.B_S$Fecha[!is.na(data.B_S$promedio)])
data.B_S = data.B_S[data.B_S$Fecha >= min_date,]
# fecha.min = min(data.B_S$Fecha)
# fecha.max = max(data.B_S$Fecha)
summary(data.B_S)
data.B_S = data.B_S[data.B_S$Fecha >= datemin.sat & data.B_S$Fecha <=datemax.sat,]
# Preparo datos -----------------------------------------------------------
#Promedio de las estaciones ventanas e Izcairrumi (Satelitales)
data.Bermejos = rename(micro.Bermejos)
# data.Bermejos = data.Bermejos[data.Bermejos$Fecha >= fecha.min & data.Bermejos$Fecha <=fecha.max,]
data.Bermejos = dia.juliano(data.Bermejos)

# promedio de los dias julianos 
names(data.Bermejos) = c("Fecha", "promedio", "anio", "dia_juliano")
promedio.Bermejos = promedio.diaJuliano(data.Bermejos)

# Corrección de los datos satelitales de Bermejos
data.crudoBermejos = rename(micro.Bermejos)
data.crudoBermejos = dia.juliano(data.crudoBermejos)

# Validación cruzada ------------------------------------------------------
cross.validationBERMEJOS = validacion.cruzada(data.B_S, "Bermejos", "Bermejos", promedio.Bermejos, data.crudoBermejos)

#Datos RF-Merge (Validación con datos no vistos)
eval.BermF = evaluacion.final(cross.validationBERMEJOS, 1, data.crudoBermejos) # el numero es el fold que se quiere evaluar

# correccion por mapeo de cuantiles y bias
data.sate = eval.BermF[,c("Fecha", "prec_corregida")]
CuantBiasBermejos = mapeo.cuantil_Bias(data.B_S, data.sate) # con los factores de corrección 

CuantBiasBermejos.1 = mapeo.cuantil_Bias(data.B_S, micro.Bermejos) # sin los factores de correccion





# ------------------------------------------------------------------------------
################################################################################
################################################################################
################### Cálculos para las micro cuencas Galgan ###################
# ------------------------------------------------------------------------------
######################## Carga de datos obs y satelitales ######################
# Cargar datos Observados ------------------------------------------------------
directory = "C:/Users/Jonna/Desktop/Randon_Forest/Estaciones_Tierra/Diario"
data.Cancan = read.csv(paste(directory, "/Cancan.csv", sep = ""))
data.Cancan$TIMESTAMP = as.Date(data.Cancan$TIMESTAMP, format = "%Y-%m-%d")
min.cancan = min(data.Cancan$TIMESTAMP)
max.cancan = max(data.Cancan$TIMESTAMP)

# Cargar datos Satelitales -----------------------------------------------------
dir.satelital = "C:/Users/Jonna/Desktop/Randon_Forest/AlgoritmoFC"
micro.Bermejos = read.csv(paste(dir.satelital, "/Galgan.csv", sep = ""))
micro.Bermejos$TIMESTAMP = as.Date(micro.Bermejos$TIMESTAMP, format = "%Y-%m-%d")
datemin.sat = min(micro.Bermejos$TIMESTAMP)
datemax.sat = max(micro.Bermejos$TIMESTAMP)

micro.Bermejos = micro.Bermejos[micro.Bermejos$TIMESTAMP >= min.cancan & micro.Bermejos$TIMESTAMP <= max.cancan,]

# ------------------------------------------------------------------------------
# -------------------------------Bermejos Soldados -----------------------------
# Preparación de los factores de corrección de datos observados ----------------
data.Ventanas = rename(data.Ventanas)
data.Izcairrumi = rename(data.Izcairrumi)
#data.SoldadosPTARM = rename(data.SoldadosPTARM)


#data.MamamagM = rename(data.MamamagM)
# Promedio de las estaciones Ventanas e Izcairrumi (Observadas)
data.B_S = merge(data.Ventanas, data.Izcairrumi, by = "Fecha", all = TRUE)
names(data.B_S) = c("Fecha", "Ventanas", "Izcairrumi")
# data.B_S = merge(data.B_S, data.SoldadosPTARM, by = "Fecha", all = TRUE)
# names(data.B_S) = c("Fecha", "Ventanas", "Izcairrumi", "SoldadosPTARM")
# data.B_S = merge(data.B_S, data.MamamagM, by = "Fecha", all = TRUE)
# names(data.B_S) = c("Fecha", "Ventanas", "Izcairrumi", "MamamagM")
data.B_S$promedio = apply(data.B_S[,2:3], 1, mean, na.rm = TRUE)
data.B_S = data.B_S[,c(1,4)]
#obtener la fecha mínima donde la columna promedio no sea NA
min_date = min(data.B_S$Fecha[!is.na(data.B_S$promedio)])
data.B_S = data.B_S[data.B_S$Fecha >= min_date,]
fecha.min = min(data.B_S$Fecha)
fecha.max = max(data.B_S$Fecha)
summary(data.B_S)
data.B_S = data.B_S[data.B_S$Fecha >= datemin.sat & data.B_S$Fecha <=datemax.sat,]
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
cross.validationBERMEJOS = validacion.cruzada(data.B_S, "Bermejos", "Bermejos", promedio.Bermejos, data.crudoBermejos)
eval.BermF = evaluacion.final(cross.validationBERMEJOS, 1, data.crudoBermejos)

# correccion por mapeo de cuantiles y bias
data.sate = eval.BermF[,c("Fecha", "prec_corregida")]
CuantBiasBermejos = mapeo.cuantil_Bias(data.B_S, data.sate) # con los factores de correccion 
CuantBiasBermejos.1 = mapeo.cuantil_Bias(data.B_S, micro.Bermejos) # sin los factores de correccion