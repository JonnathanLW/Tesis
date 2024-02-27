########################### Relleno de datos faltantes #########################
# Autor: Jonnathan Landi
# Fecha creación: 2024-02-10
# ------------------------------------------------------------------------------
# Fecha ultima modificación: 2024-02-27 (año-mes-día)
# Autor de ultima modificación: Jonnathan Landi
# Versión: 1.0.0
# ------------------------------------------------------------------------------
# ESTADO: En desarrollo 
###########(En desarrollo, En revisión, Refactorización, Terminado) ############
# ------------------------------------------------------------------------------
################################################################################
# Librerías necesarias ---------------------------------------------------------
library(data.table)
library(dplyr)
library(stats)
library(corrplot)
library(svDialogs)
library(GGally)
# ------------------------------------------------------------------------------
directoty = "C:/Users/Jonna/OneDrive - ucuenca.edu.ec/Universidad/Tesis/Scripts R/Preprocesamiento de datos/Dependencias"
# ------------------------------------------------------------------------------
################################ Funciones #####################################
analisis.correlacion = function(){
  n = as.numeric(dlgInput("Ingrese el número de estaciones ")$res)
  
  data.names = character(n)
  files = character(n)
  names.estat = character(n)
  names.encab = character(n)
  df = data.frame()
  # Solicitar la selección de archivo para cada nombre de dato
  for (i in 1:n){
    file = dlg_open(paste("Estación", i))$res
    files[i] = file
  }
  # Cargar los datos
  for (i in 1:n){
    data.names[i] = paste("data.", i, sep = "")
    names.estat[i] = basename(files[i])
    names.encab[i] = substr(names.estat[i], 1, 5)
    data.temp = fread(files[i])
    if (i ==1) {
      df = data.temp
    } else {
      df = merge(df, data.temp, by = "TIMESTAMP")
    }
    
  }
  names(df)[2:(n+1)] = names.encab[1:n]
  return(df)
}

# ------------------------------------------------------------------------------
################################################################################
data = analisis.correlacion()
TIMESTAMP = data$TIMESTAMP
data = data[,-1]
matriz.cor = cor(data, use = "pairwise.complete.obs") # ignoro filas vacías en la correlación
corrplot(matriz.cor, method = "number")
pairs(data)
ggpairs(data)
# ------------------------------------------------------------------------------
dev.off()


