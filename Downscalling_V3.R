################################# Downscalling #################################
# ------------------------------------------------------------------------------
# Fecha ultima modificación: 2024-02-09 (año-mes-día)
# Versión: 3.1.0
# ------------------------------------------------------------------------------
################################################################################
# Librerías necesarias ---------------------------------------------------------
library(dplyr)
library(hydroGOF)
library(caret)
library(svDialogs)
library(qmap)
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
################################################################################
################################################################################
################################################################################
######################### Funciones auxiliares ################################

factor.correccion = function(df.obs, df.sat, n){
  rename = function(data) {
    names(data) = c("Fecha", "prec")
    data$Fecha = as.Date(data$Fecha, format = "%Y-%m-%d")
    return(data)
  }
  
  df.obs = rename(df.obs)
  df.sat = rename(df.sat)
  dia.juliano = function(df) {
    # df = df.obs
    df = data.frame(df)
    rownames(df) = NULL
    
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
      } else if(length(fechas.presentes) == 2 && as.Date("2016-02-29") %in% df$Fecha && as.Date("2024-02-29") %in% df$Fecha) {
        df = f.2(2016, 2024)
      } else if(length(fechas.presentes) == 2 && as.Date("2020-02-29") %in% df$Fecha && as.Date("2024-02-29") %in% df$Fecha) {
        df = f.2(2020, 2024)
      } else if (length(fechas.presentes) == 3) {
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
  
  df.obs_jul = dia.juliano(df.obs)
  df.sat_jul = dia.juliano(df.sat)
  
  promedio.diaJuliano = function(df) {
    df = data.frame(df)
    df = df %>% dplyr::select(dia_juliano, prec)
    model = as.matrix(df)
    prom.day = array(0,dim=c(1,365))
    
    for (i in 1:365){
      prom.day[i]=mean(model[model[,1]==i,2],na.rm=TRUE) # cambiar por median = mediana, mean = promedio
    }
    return(prom.day)
  }
  
  df.obs_prom = promedio.diaJuliano(df.obs_jul)
  df.sat_prom = promedio.diaJuliano(df.sat_jul)
  
  factores.correcion = function(prom.obs,prom.sat){
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
  # 
  # df.fc = factores.correcion(df.obs_prom, df.sat_prom)
  
  datos.corregidos = function(data_crudo, fact_correc){
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
  
  #############
  data.obs = df.obs
  promedio.micro = df.sat_prom
  data.crudo = df.sat_jul
  #############
  
  validacion.cruzada = function(data.obs, promedio.micro, data.crudo){
    set.seed(123)
    
    data.clean = na.omit(data.obs)
    indices = sample(1:nrow(data.clean), 0.8 * nrow(data.clean))
    data.train = data.clean[indices,] # entrenamiento
    data.test = data.clean[-indices,] # validacion 
    
    folds = createFolds(data.train$prec, k = n, list = TRUE, returnTrain = TRUE)
    resultados = list()
    for (i in 1:n){
      train = data.train[folds[[i]],]
      test = data.train[-folds[[i]],]
      
      # train = rename(train)
      train = dia.juliano(train)
      promedio_train = promedio.diaJuliano(train)

      factores = factores.correcion(promedio_train, promedio.micro)

      data_corregida = datos.corregidos(data.crudo, factores)
      
      # validación del modelo
      obs_test = test$prec
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
      
      resultados[[i]] = list(data.test = data.test, train = train, test = test, factores = factores,
                             data_corregida = data_corregida, test.f = test.f)
    }
    
    gof_lista = list()
    for (i in 1:n) {
      test = resultados[[i]]$test
      gof_lista[[paste("Fold", i)]] = test
    }
    
    # Combinar los resultados en un único marco de datos
    gof_df = do.call(cbind, gof_lista)
    gof_df = as.data.frame(gof_df)
    colnames(gof_df) = paste("Fold", 1:n, sep = "_")
    
    gof_df$promedio = rowMeans(gof_df[,1:n])
    
    gof_cross_validation <<- gof_df
    
    # Evaluamos de manera interna 80$ de entrenamiento y 20% de validación
    data.train = dia.juliano(data.train)
    promedio_train = promedio.diaJuliano(data.train)
    factores.finales = factores.correcion(promedio_train, promedio.micro)
    data_corregida.f = datos.corregidos(data.crudo, factores.finales)
    
    obs_test = data.test$prec
    pred_test.f = data_corregida.f$prec_corregida[match(data.test$Fecha, data_corregida.f$Fecha)]
    valid_values.f = complete.cases(obs_test, pred_test.f)
    obs_testV.f = obs_test[valid_values.f]
    pred_testV.f = pred_test.f[valid_values.f]
    
    test.f = gof(pred_testV.f, obs_testV.f)
    test.f = data.frame(test.f)
    names(test.f) = "valor"
    
    # Crear un nuevo marco de datos con los nombres de las filas como una columna
    sobreajuste = cbind(Estadistico = rownames(test.f), test.f, Est.folds = gof_df$promedio)
    names(sobreajuste) = c("Estadistico", "Eval.Final", "Est.folds")
    rownames(sobreajuste)  = NULL
    
    Estad.SobreajusteFC <<- sobreajuste
    
    return(list(resultados = resultados, test.f = test.f, sobreajuste = sobreajuste))
  
  }
  
  validacion.cruzada(data.obs, promedio.micro, data.crudo)
  
  # Evaluamos de manera interna 80$ de entrenamiento y 20% de validación
  
  data.obsf = data.obs
  data.obsf = dia.juliano(data.obsf)
  promedio.dataobsf = promedio.diaJuliano(data.obsf)
  
  factores.finalesF = factores.correcion(promedio.dataobsf, promedio.micro)
  data_corregida.f = datos.corregidos(data.crudo, factores.finalesF)
  names(data_corregida.f) = c("Fecha", "prec_sat", "prec_correg")
  
  
  return(data_corregida.f)

}

MC.bIAS = function(data.obser, data.satelit, n){

  mapeo.cuantil = function(data.obser, data.satelit, n){
    
    names(data.obser) = c("Fecha", "prec")
    names(data.satelit) = c("Fecha", "prec")
    
    # MAPEO DE CUANTILES CON CROSS VALIDATION
    set.seed(123)
    datos = merge(data.obser, data.satelit, by = "Fecha", all = TRUE)
    names(datos) = c("Fecha", "obs", "mod")
    train = na.omit(datos)
    
    indices = sample(1:nrow(train), 0.8 * nrow(train))
    data.train = train[indices,]
    data.test = train[-indices,]
    
    folds = createFolds(data.train$obs, k = n, list = TRUE, returnTrain = TRUE)
    resultados = list()
    for (i in 1:n){
      train = data.train[folds[[i]],]
      test = data.train[-folds[[i]],]
      Fecha = test$Fecha
      Fecha = as.Date(Fecha, format = "%Y-%m-%d")
      Fecha = data.frame(Fecha)
      
      qm_fit = fitQmap(train$obs, train$mod, method = "QUANT")
      qm1 = doQmap(train$mod, qm_fit)
      qm2 = doQmap(test$mod, qm_fit)
      
      obs_test = test$obs
      pred_test = qm2
      data.merge = cbind(Fecha, obs_test, pred_test)
      
      test = gof(pred_test, obs_test)
      test = data.frame(test)
      names(test) = "valor"
      # Crear un nuevo marco de datos con los nombres de las filas como una columna
      test.f = cbind(Estadistico = rownames(test), test)
      rownames(test.f)  = NULL
      resultados[[i]] = list(data.test = data.test, train = train, test = test, test.f = test.f, qm_fit = qm_fit)
    }
    
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
    gof_df$promedio = rowMeans(gof_df[,1:n])
    gof_CuantilMapping <<- gof_df
  
    # Valido el modelo con el 20% de los datos
    data.train$Fecha = as.Date(data.train$Fecha, format = "%Y-%m-%d")
    data.test$Fecha = as.Date(data.test$Fecha, format = "%Y-%m-%d")
    Fecha = as.data.frame(data.test$Fecha)
    
    qm_fit = fitQmap(data.train$obs, data.train$mod, method = "QUANT")
    qm1 = doQmap(data.train$mod, qm_fit)
    qm2 = doQmap(data.test$mod, qm_fit)
    
    obs_test.f = data.test$obs
    pred_test.f = qm2
    
    data.merge.f = cbind(Fecha, obs_test.f, pred_test.f)
    
    test.val = gof(pred_test.f, obs_test.f)
    test.val = data.frame(test)
    names(test) = "valor"
    
    # Crear un nuevo marco de datos con los nombres de las filas como una columna
    test.val = cbind(Estadistico = rownames(test.val), test.val)
    
    Estad.Sobreajuste = cbind(Estadistico = rownames(test.val), Est.Vald =  test.val$valor, Est.folds = gof_df$promedio)
    Estad.Sobreajuste = as.data.frame(Estad.Sobreajuste)
    Estad.SobreajusteQM <<- Estad.Sobreajuste
   
    return(list(resultados = resultados, test.f = gof_df))
 
    # validacion del modelo con el Test Data
  }
  
  mapeo.cuantil(data.obser, data.satelit, n)
  
  names(data.obser) = c("Fecha", "prec")
  names(data.satelit) = c("Fecha", "prec")
  
  data.obser$Fecha = as.Date(data.obser$Fecha, format = "%Y-%m-%d")
  data.satelit$Fecha = as.Date(data.satelit$Fecha, format = "%Y-%m-%d")
  
  qm_fitF = fitQmap(data.obser$prec, data.satelit$prec, method = "QUANT")
  qm2F = doQmap(data.satelit$prec, qm_fitF)
  
  data.merge.F = cbind(data.obser, data.satelit$prec, qm2F)
  names(data.merge.F) = c("Fecha", "prec_obser", "prec_Sat", "prec_QM")
  
  return(data.merge.F)
}

# ------------------------------------------------------------------------------
rename = function(data) {
  names(data) = c("Fecha", "prec")
  data$Fecha = as.Date(data$Fecha, format = "%Y-%m-%d")
  return(data)
}
# ------------------------------------------------------------------------------
# Cargar datos Satelitales -----------------------------------------------------
dir.satelital = "C:/Users/Jonna/Desktop/Randon_Forest/Algoritmo RF_4/Data_test"
micro.Bermejos = read.csv(paste(dir.satelital, "/Prec_CuencaYanuncay.csv", sep = "")) # Probar con MSWEP puro y el MSWEP RANDON FOREST
micro.Bermejos = rename(micro.Bermejos)
fecha.min = min(micro.Bermejos$Fecha)
fecha.max = max(micro.Bermejos$Fecha)
df.sat = micro.Bermejos
# Cargar datos Observados -------------------------------------------------------
directory = "C:/Users/Jonna/Desktop/Randon_Forest/Algoritmo RF_4/Data_test"
data.B_S = read.csv(paste(directory, "/Prec_EstacionesTierraCuencaYanuncay.csv", sep = ""))
data.B_S = rename(data.B_S)
df.obs = data.B_S

# Downscalling ---------------------------------------------------------------
rm(data.B_S, micro.Bermejos)

fc_biascrud = MC.bIAS(df.obs, df.sat, 10)  # con los RF-MEP puro

resulados.1 = data.frame(
  O_Sp = gof(fc_biascrud$prec_Sat, fc_biascrud$prec_obser), # Set de datos original vs Satelital puro
  O_Qm = gof(fc_biascrud$prec_QM, fc_biascrud$prec_obser) # Set de datos original vs Cuantil Mapping 
)

# Metodo 2
fc  = factor.correccion(df.obs, df.sat, 10)  # con los RF-MEP
fc.2 = fc[, c("Fecha", "prec_correg")]
df.obs.2 = which(format(df.obs$Fecha, "%m-%d") == "02-29")
df.obs.1 = df.obs[-df.obs.2,]
fc_biasfc = MC.bIAS(df.obs.1, fc.2, 10)  # mapeo de cuantiles a los datos quue estan con fc

resulados.2 = data.frame(
  O_Sp = gof(fc_biasfc$prec_Sat, fc_biasfc$prec_obser), # Set de datos original vs Satelital + fc
  O_Qm = gof(fc_biasfc$prec_QM, fc_biasfc$prec_obser) # Set de datos original vs Cuantil Mapping (Satelital + fc)
)

resultados.final = data.frame(
  Estadistico = rownames(resulados.1),
  SatPuro_Obs = resulados.1$O_Sp,
  QMSatPuro_Obs = resulados.1$O_Qm,
  SatFC_Obs = resulados.2$O_Sp,
  QMSatFC_Obs = resulados.2$O_Qm
)

