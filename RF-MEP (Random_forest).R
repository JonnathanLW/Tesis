########################### RF - MEP (Random Forest) ###########################
# Autor: Jonnathan Landi 
# Fecha creación: 2024-05-02

# ------------------------------------------------------------------------------
# Fecha ultima modificación: 2024-05-02 (año-mes-día)
# Autor de ultima modificación: Jonnathan Landi
# Versión: 1.0.0
# ------------------------------------------------------------------------------
# ESTADO: En desarrollo
###########(En desarrollo, En revisión, Refactorización, Terminado) ############
# ------------------------------------------------------------------------------
################################################################################
# Librerías necesarias ---------------------------------------------------------
library(zoo)
library(hydroTSM)
library(hydroGOF)
library(sf)
library(raster)
library(RFmerge)
library(ggplot2)
library(data.table)

# ------------------------------------------------------------------------------
# Cargo datos necesarios
directory = "C:/Users/Jonna/OneDrive - ucuenca.edu.ec/Universidad/Tesis/Scripts R/Randon_Forest"
YanuncayPPts = fread(file.path(directory, "YanuncayPpts.csv"))
summary(YanuncayPPts)
YanuncayPPts = data.frame(YanuncayPPts)
names(YanuncayPPts) = c("TIMESTAMP", "M001", "M002", "M003")
YanuncayPPts = YanuncayPPts[,c("TIMESTAMP", "M001")]
YanuncayPPts = zoo::zoo(YanuncayPPts[, -1], order.by = as.Date(YanuncayPPts$TIMESTAMP))

YanuncayPPgis = fread(file.path(directory, "YanuncayPPgis.csv"))
YanuncayPPgis = data.frame(YanuncayPPgis)
YanuncayPPgis = YanuncayPPgis[1,]
# Área de la cuenca del Yanuncay 
YanuncaySHP = st_read("C:/Users/Jonna/Desktop/Cuenca_Yanuncay/Cuenca_Yanuncay.shp")
res(YanuncaySHP)
#lonLat17s =  CRS("+proj=longlat +datum=WGS84 +no_defs") #
YanuncaySHP = st_transform(YanuncaySHP, crs = 4326)

# preparo mis estaciones
stations = YanuncayPPgis
stations = st_as_sf(stations, coords = c('lon', 'lat'), crs = 4326)

# ploteo el mapa y las estaciones dentro de este
stations_sf <- st_as_sf(stations, coords = c("geometry.x", "geometry.y"), crs = 4326)
ggplot() +
  geom_sf(data = YanuncaySHP) +  # Asumiendo que ValparaisoSHP es tu shapefile base
  geom_sf(data = stations_sf, color = "red", pch = 19) +
  theme_minimal()

# Cargo datos satelitales y DEM
MSWEP = brick(paste0(directory, "/YanuncaySatResampling.nc"))
MSWEP = MSWEP[[232:3488]]

DEM = raster(paste0(directory, "/MDT_10K.tif"))
#crs(DEM) <- "+proj=longlat +datum=WGS84 +no_defs "
res(DEM)

################# Reproyeccion de todo a UTM 17S ###############################

# selecciono las coordenadas de las estaciones
stations.utm = sf::st_transform(stations, crs=32717) # for 'sf' objects
YanuncaySHP.utm = sf::st_transform(YanuncaySHP, crs=32717)

st.coords = st_coordinates(stations.utm)
lon = st.coords[, "X"]
lat = st.coords[, "Y"]


utmz17s.p4s <- CRS("+init=epsg:32717")
MSWEP10KM.utm = projectRaster(from=MSWEP, crs=utmz17s.p4s)
MSWEP10KM.utm  = raster::crop(MSWEP10KM.utm , YanuncaySHP.utm)
MSWEP10KM.utm  = raster::mask(MSWEP10KM.utm , YanuncaySHP.utm)
crs(MSWEP10KM.utm)

YanuncayDEM.utm = projectRaster(from=DEM, crs=utmz17s.p4s)
YanuncayDEM.utm = raster::crop(YanuncayDEM.utm, YanuncaySHP.utm)
YanuncayDEM.utm =  raster::mask(YanuncayDEM.utm, YanuncaySHP.utm)
res(YanuncayDEM.utm)

ref = MSWEP10KM.utm[[1]]
res(ref)
YanuncayDEM.utm = raster::resample(YanuncayDEM.utm, ref, method = "ngb")
res(YanuncayDEM.utm)
plot(YanuncayDEM.utm)


YanuncayPPgis.utm = data.frame(ID=stations.utm[["Code"]], lon=lon, lat=lat)

covariates.utm = list(mswep=MSWEP10KM.utm, 
                       dem=YanuncayDEM.utm)

rfmep = RFmerge(x=YanuncayPPts, metadata=YanuncayPPgis.utm, cov=covariates.utm,
                 id="ID", lat="lat", lon="lon",  mask=YanuncaySHP.utm, 
                 training=0.8, write2disk=FALSE)

# ------------------------------------------------------------------------------
# Guardo el modelo
setwd("C:/Users/Jonna/OneDrive - ucuenca.edu.ec/Universidad/Tesis/Scripts R/Randon_Forest")
save(rfmep, file = "rfmep.RData")
raster::writeRaster(rfmep, filename="RFMEP_1C.nc", format="CDF", overwrite=TRUE)
