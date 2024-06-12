################################ Función cópula ################################
# Autor: Jonnathan Landi
# Fecha creación: 2024-02-20
# ------------------------------------------------------------------------------
# Fecha ultima modificación: 2024-02-21 (año-mes-día)
# Autor de ultima modificación: Jonnathan Landi
# Versión: 1.0.0
# ------------------------------------------------------------------------------
# ESTADO: En desarrollo 
###########(En desarrollo, En revisión, Refactorización, Terminado) ############
# ------------------------------------------------------------------------------
################################################################################
# Librerías necesarias ---------------------------------------------------------
library(data.table)
#library(quantmod)
#library(tseries)
library(copula)
# ------------------------------------------------------------------------------
directory = "C:/Users/jlandi/Documents/Proyectos/2024-02-20_Copulas"
SPI = read.table(paste0(directory, "/SPI.csv"), header = TRUE, sep = ",")
SSI = read.table(paste0(directory, "/SSI.csv"), header = TRUE, sep = ",")
SPI = rnorm(100, mean = 0, sd = 1) # serie para pruebas (Eliminar al final)
SSI = rnorm(100, mean = 0, sd = 1) # serie para pruebas (Eliminar al final)
data = cbind(SPI, SSI)      # serie para pruebas (Eliminar al final)
data = data.frame(data)     # serie para pruebas (Eliminar al final)
# ------------------------------------------------------------------------------
################################### Cópulas ####################################
funcion.copula = function(){
 # data = merge(SPI, SSI, by = "FECHA")
  n = length(data$SPI)
  columnas = 2 # debido a que mi copula es bi-variada
  
  x = matrix(c(data$SPI, data$SSI), n, columnas)
  
  u = pobs(x)
  
  # criterio AIC Y BIC
  criterios = function(Log_likel){
    AIC = -2 * Log_likel + 2 * 1 #-2*log-likelihood + k*npar
    BIC = -2 * Log_likel + log(n) * 1 #-2*log-likelihood + k*npar
    crit = data.frame(AIC = AIC, BIC = BIC)
    return(crit)
  }
  
  # Cópulas elípticas ----------------------------------------------------------
  # Copula de Gaussiana o Normal
  Gauss = fitCopula(normalCopula(dim = 2, dispstr = "un"), u, method = "ml", traceOpt = TRUE) # ml= verisimilitud
  Log.Gauss = Gauss@loglik
  C.Gauss = criterios(Log.Gauss)
  
  # Copula t de Student
  tStudent = fitCopula(tCopula(dim=2, dispstr="un"), u, method="ml")
  Log.tStudent = tStudent@loglik
  C.tStudent = criterios(Log.tStudent)
  tStudent@estimate
  # ----------------------------------------------------------------------------
  # Familia de copulas arquimidianas -------------------------------------------
  # copula Ali–Mikhail–Haq
  Ali = fitCopula(amhCopula(dim = 2), u, method = "ml")
  Log.Ali = Ali@loglik
  C.Ali = criterios(Log.Ali)
  
  # Copula de Clayton  
  Clayton = fitCopula(claytonCopula(dim = 2), u, method = "ml")
  Log.Clayton = Clayton@loglik
  C.Clayton = criterios(Log.Clayton)

  # Copula de Frank
  Frank = fitCopula(frankCopula(dim = 2), u, method = "ml")
  Log.Frank = Frank@loglik
  C.Frank = criterios(Log.Frank)
  
  # Copula de Gumbel
  Gumble = fitCopula(gumbelCopula(), u, method = "ml")
  Log.Gumble = Gumble@loglik
  C.Gumble = criterios(Log.Gumble)
  
  # Copula de Jose
  Jose = fitCopula(joeCopula(dim = 2), u, method = "ml")
  Log.Jose = Jose@loglik
  C.Jose = criterios(Log.Jose)
  # ----------------------------------------------------------------------------
  
  # Data frame para guardar resultados
  resultados = data.frame(
    Estimador = c(Gauss@estimate, tStudent@estimate[2], Ali@estimate, Clayton@estimate, Frank@estimate, Gumble@estimate, Jose@estimate),
    Error.estand = c(Gauss@var.est, NA, Ali@var.est, Clayton@var.est, Frank@var.est, Gumble@var.est, Jose@var.est),
    AIC = c(C.Gauss$AIC, C.tStudent$AIC, C.Ali$AIC, C.Clayton$AIC, C.Frank$AIC, C.Gumble$AIC, C.Jose$AIC),
    BIC = c(C.Gauss$BIC, C.tStudent$BIC, C.Ali$BIC, C.Clayton$BIC, C.Frank$BIC, C.Gumble$BIC, C.Jose$BIC),
    row.names = c("Gaussiana", "tStudent", "Ali–Mikhail–Haq", "Clayton", "Frank", "Gumble", "Jose")
    )
  
  menor.AIC = min(resultados$AIC)
  menor.BIC = min(resultados$BIC)
  print(paste("De acuerdo con el analisis, el valor mas bajo de AIC presenta la copula de:", rownames(resultados)[which(resultados$AIC == menor.AIC)]))
  print(paste("De acuerdo con el analisis, el valor mas bajo de BIC presenta la copula de:", rownames(resultados)[which(resultados$BIC == menor.BIC)]))
 
  return(resultados)
}
# ------------------------------------------------------------------------------
result.copulas = funcion.copula()
