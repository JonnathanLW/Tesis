library(rgdal)
library(raster)
library(terra)
library(sf)
library(raster)
library(ncdf4)


# Proceso Imagenes satelitales MSWEP -------------------------------------------
setwd("C:/Users/Jonna/OneDrive - ucuenca.edu.ec/Universidad/Tesis/Datos Satelitales/Img_Sat")
data.file.name= list.files("./",full.names =T,pattern = ".nc" )
data.file.name 

MSWEP = stack(data.file.name)
res(MSWEP)
#CHIRPS = projectRaster(CHIRPS, crs = 4326)
# res(CHIRPS)
# dat.stck

# Cargo el shape de la Area Local
Area_LocalSHP = st_read("C:/Users/Jonna/Desktop/Randon_Forest/Algoritmo RF_2/Shape_YanunTomeb/YanunTomeb_res43.shp")
#Area_LocalSHP = st_transform(Area_LocalSHP, crs = 4326)
plot(Area_LocalSHP)

# recorto en funcion del area local
MSWEP = crop(MSWEP, Area_LocalSHP)
capa.1 = MSWEP[[5]]
plot(capa.1)


# Cargo el DEM de la Area Local
DEM = stack("C:/Users/Jonna/Desktop/Randon_Forest/Algoritmo RF_2/Img_0.1/DEM_4326_.tif")
res(DEM)
DEM = crop(DEM, Area_LocalSHP)
plot(DEM[[1]])
#dat.stck = projectRaster(dat.stck, crs = 4326)

# plot(dat.stck, add = TRUE)


# Recorto las imagenes satelitales en funcion del Area Local
# dat.stck = raster::crop(dat.stck, Area_LocalSHP)
# plot(dat.stck[[1]])
# dat.stck = projectRaster(dat.stck, crs = 4326)
# res(dat.stck)

# CHIRPS = raster::crop(CHIRPS, Area_LocalSHP)
# CHIRPS = projectRaster(CHIRPS, crs = 4326)
# plot(CHIRPS[[1]])

# Resampleo a 0.1 grados
# ref = dat.stck[[1]]
# Area_LocalDEM = raster::resample(DEM, ref, method = "ngb")
# plot(Area_LocalDEM)
# res(Area_LocalDEM)


# Convierto a UTM para verificar 
# utmz17s = CRS("+init=epsg:32717")
# dat.stck.UTM = projectRaster(from = dat.stck, crs = utmz17s)
# res(dat.stck.UTM)
# 
# Area_LocalDEM.UTM = projectRaster(from = Area_LocalDEM, crs = utmz17s)
# res(Area_LocalDEM.UTM)



############ Repeoyecto a 10 km ############
setwd("C:/Users/Jonna/Desktop/Randon_Forest/Algoritmo RF_2/Img_0.1")

res_fin= c(0.09, 0.09)
ref = MSWEP[[1]]
raster_ref = raster(res = res_fin, crs = crs(ref), ext = extent(ref))

# resampleo MSWEP
resampling_MSWEP = resample(MSWEP, y = raster_ref, method = "ngb")
res(resampling_MSWEP)
raster::writeRaster(resampling_MSWEP, filename="MSWEP_resampling_4326.nc", format="CDF", overwrite=TRUE)

# Resampling DEM
Area_LocalDEM_resampling = resample(DEM, y = raster_ref, method = "ngb")
res(Area_LocalDEM_resampling)
plot(Area_LocalDEM_resampling)
raster::writeRaster(Area_LocalDEM_resampling, filename="DEM_resampling_4326.nc", format="CDF", overwrite=TRUE)
# guardo el raster


WMSWEP = resample(dat.stck, y = new_raster, method = "ngb")
res(WMSWEP)
raster::writeRaster(WMSWEP, filename="MSWEP_resampling_4326.nc", format="CDF", overwrite=TRUE)

CHIRPS = resample(CHIRPS, y = new_raster, method = "ngb")
res(CHIRPS)
plot(CHIRPS[[1]])
raster::writeRaster(CHIRPS, filename="CHIRPS_resampling_4326.nc", format="CDF", overwrite=TRUE)

