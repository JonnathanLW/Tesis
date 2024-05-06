###IMPLEMENTACION RFMERGE EN ECUADOR
###TESIS EXTREMOS CLIM?TICOS

##CARGAR LIBRERIAS

library(rgdal)
library(raster)
library(ncdf4)
library(RFmerge)
library(zoo)
library(sf)
library(rasterVis) #multitemporal visualization 
library(latticeExtra) #multitemporal visualization

#setwd("D:/TESIS/RFMEP/RF_Final")
#load("DATOSRF.RData")

#CARGAR ?REA DE ESTUDIO
area<-readOGR("C:/Users/Jonna/Desktop/Cuenca_Yanuncay/Cuenca_Yanuncay.shp")
area<-spTransform(area, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

#CARGAR DATOS MSWEP
setwd("C:/Users/Jonna/OneDrive - ucuenca.edu.ec/Universidad/Tesis/Scripts R/Randon_Forest")
MSWEP<-brick("YanuncaySatResampling.nc")
nlayers(MSWEP)
MSWEP<-MSWEP[[232:3488]]
nlayers(MSWEP)
MSWEP<-raster::crop(MSWEP, area)
MSWEP<-raster::mask(MSWEP, area)
plot(MSWEP[[9]])
plot(area, add=TRUE)
#spplot(MSWEP[[1:6]], par.settings = RdBuTheme, margin= FALSE, main="MSWEP") + latticeExtra::layer(sp.polygons(area, col='black'))

#CARGAR DATOS CHIRPS
# setwd("D:/TESIS/RFMEP/RF_Final/input")
# CHIRPS<-brick("CHIRPS_Ecuador_1981_2015_10km.nc")
# CHIRPS<-raster::crop(CHIRPS, area)
# CHIRPS<-raster::mask(CHIRPS, area)
#spplot(CHIRPS[[1:6]], par.settings = RdBuTheme, margin= FALSE, main="CHIRPS") + latticeExtra::layer(sp.polygons(area, col='black'))

#CARGAR DEM ECUADOR
setwd("C:/Users/Jonna/OneDrive - ucuenca.edu.ec/Universidad/Tesis/Scripts R/Randon_Forest")
DEM<-raster("MDT_10K.tif")
DEM<-projectRaster(DEM, crs = "+proj=longlat +datum=WGS84 +no_defs")
DEM<- raster::crop(DEM, area)
DEM<- raster::mask(DEM, area)
plot(DEM)

#DEM<-raster("EcuadorDEM10km.tif")
#DEM<-projectRaster(DEM, crs = "+proj=longlat +datum=WGS84 +no_defs")
#RESAMPLE DEM a 10 km
ref <- MSWEP[[1]]
DEM<- raster::resample(DEM, ref, method = "ngb")
plot(DEM)
#VISUALIZAR DEM
#spplot(DEM, par.settings = infernoTheme, margin= FALSE, main="DEM") + latticeExtra::layer(sp.polygons(area, col='black'))
#CORTAR RESAMPLE DEM CON AREA DE ESTUDIO
#DEM<- raster::crop(DEM, area)
#DEM<- raster::mask(DEM, area)

#GUARDAR DEM A 10KM
#setwd("D:/TESIS/RFMEP/RF_Final/output")
#raster::writeRaster(DEM, filename="DEM_Ecuador_10km.nc", format="CDF", overwrite=TRUE)


#RF-MEP 1:
covariates<-list(mswep=MSWEP)
#RF-MEP 2: 
#covariates<-list(chirps=CHIRPS)
#RF-MEP 3: 
#covariates<-list(mswep=MSWEP, chirps=CHIRPS)
#RF-MEP 4: 
covariates<-list(mswep=MSWEP, dem=DEM)

setwd("C:/Users/Jonna/OneDrive - ucuenca.edu.ec/Universidad/Tesis/Scripts R/Randon_Forest")
EcuadorPPts<-read.csv("YanuncayPpts.csv", sep=",", header=T,blank.lines.skip=F)
#EcuadorPPts<-EcuadorPPts[1:12783, ]
EcuadorPPts<-zoo(EcuadorPPts)
EcuadorPPgis<-read.csv("EcuadorPPgis.csv",sep=",", header=T,blank.lines.skip=F)
# Renombrar las columnas para que coincidan con los nombres esperados por RFmerge
colnames(EcuadorPPgis) <- c("id", "lon", "lat", "Nombre", "Nom_Region", "Elevacion")

# Llamar a RFmerge con los nombres de columnas correctos
rfmep <- RFmerge(x=EcuadorPPts, metadata=EcuadorPPgis, cov=covariates,
                 id="id", lat="lat", lon="lon", mask=Ecuador_st,
                 training=1, write2disk=FALSE)



Ecuador_st<-st_as_sf(area)

rfmep <- RFmerge(x=EcuadorPPts, metadata=EcuadorPPgis, cov=covariates,
                 id="id", lat="lat", lon="lon", mask=Ecuador_st,
                 training=1, write2disk=FALSE)



setwd("C:/Users/Jonna/OneDrive - ucuenca.edu.ec/Universidad/Tesis/Scripts R/Randon_Forest")
raster::writeRaster(rfmep, filename="RFMEP_1C.nc", format="CDF", overwrite=TRUE)
