# Librerías ---------------------------------------------------------------
library(hydroGOF)
library(raster)
library(sf)
# ------------------------------------------------------------------

modelo.1 = "C:/Users/Jonna/Desktop/Randon_Forest/AlgoritmoRF/Img_0.1/Modelo_01.nc"
Cuenca_Yanuncay = st_read("C:/Users/Jonna/Desktop/Randon_Forest/Shape_Yanuncay/Cuenca_Yanuncay.shp")
Micro_Yanuncay = st_read("C:/Users/Jonna/Desktop/Randon_Forest/Shape_Microcuencas_Yanuncay/Micros_Yanuncay.shp")
stack.modelo.1 = stack(modelo.1)

modelo_crop = crop(stack.modelo.1, Cuenca_Yanuncay)
bordes <- st_boundary(Micro_Yanuncay)

# Crear un nuevo objeto sf con los bordes
bordes_sf <- st_sf(geometry = bordes)
plot(bordes_sf, add = TRUE, col = "red")

# Ploteo para ver como esta la imagen 
plot(modelo_crop[[1]])
plot(bordes_sf, add = TRUE, col = "red")

# extraigo datos del pixel 6 de modelo_crop
data = as.data.frame(extract(modelo_crop, 6))
# Estación en tierra para validar
coor = data.frame(lon=693422,lat= 9677234) #YanuncayPucan