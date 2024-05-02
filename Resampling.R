library(rgdal)
library(raster)
library(terra)
library(sf)
library(raster)

setwd("C:/Users/Jonna/OneDrive - ucuenca.edu.ec/Universidad/Tesis/Datos Satelitales/Img_Sat")

data.file.name= list.files("./",full.names =T,pattern = ".nc" )
data.file.name 

dat.stck= stack(data.file.name)
crs(dat.stck)= crs("+init=epsg:4326")
dat.stck
plot(dat.stck[[9]])

directory_shape = "C:/Users/Jonna/OneDrive - ucuenca.edu.ec/Universidad/Tesis/Shapes/Cuenca Yanuncay/Cuenca_Yanuncay/Cuenca_Yanuncay.shp"
#directory_shape = "C:/Users/Jonna/OneDrive - ucuenca.edu.ec/Universidad/Tesis/Datos Satelitales/Shape_Base/AREA_LOCAL.shp"
ROI = st_read(directory_shape)
ROI = st_transform(ROI, CRS("+proj=longlat +datum=WGS84"))
plot(ROI)

dat.stck= raster::crop(dat.stck,ROI)
plot(dat.stck[[9]])
nlayers(dat.stck)
res_fin= c(0.0898311174, 0.0898311174)

new_raster= raster(res = res_fin, crs = crs(dat.stck), ext = extent(dat.stck ))

# Realiza el remuestreo utilizando el método de interpolación "bilinear"
resampled_brick= resample(dat.stck, y = new_raster, method = "bilinear")
resampled_brick
nlayers(resampled_brick)
plot(resampled_brick[[9]])

coor <- data.frame(lon=-79.3,lat= -3)
plot(resampled_brick[[9]])+
  points(coor)


test1 <- raster::extract(resampled_brick,coor)
test1 <- as.data.frame(t(test1))
test1$id <- c(1:length(data.file.name))
View(test1)

output_file = "C:/Users/Jonna/OneDrive - ucuenca.edu.ec/Universidad/Tesis/Scripts R/Randon_Forest/YanuncaySatResampling.nc"

# Guardar el SpatRasterBrick en formato NetCDF
writeRaster(resampled_brick, filename = output_file, format = "CDF", overwrite = TRUE)
