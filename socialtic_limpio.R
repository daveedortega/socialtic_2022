##Social Tic - Manipulation of Spatial Data with R
# Description: This code aims to download the FGJ Victims Database using datos.cdmx.gob.mx, Mexico City's open data portal.
# Using the municipalities provided by IECM 
# ADIP - Subdirección de Datos Abiertos
# David A. Ortega

## Prepare Space----
dev.off()
rm(list=ls())
# install.packages(pacman)
pacman::p_load(st,sf,rgeos,tidyverse,jsonlite,rgdal,ggiraph,plotly,httr,sp,widgetframe)
## Consulta base de datos de víctimas de la fiscalía con la API----

url <- "https://datos.cdmx.gob.mx/api/3/action/"
funcion <- "datastore_search?resource_id="
id_carpetas <- "d543a7b1-f8cb-439f-8a5c-e56c5479eeb5"
limite <- "&limit=32000" # Si no se especifica el número de observaciones se regresan 100 por default
# limita las consultas de json a 32,000 observaciones por consulta la página de datos abiertos

# realizamos consulta -
req <- fromJSON(paste0(url,funcion,id_carpetas,limite))
# Extraemos los datos
datos_fgj <- req$result$records
glimpse(datos_fgj)
# Extraemos el total de observaciones de la base
total_records <- req$result$total
total_records
# obtenemos el total de registros con una consulta por 32,000 observaciones a la vez hasta consumir toda la base
while (nrow(datos_fgj)<total_records){
  ofst <- nrow(datos_fgj)
  ofset <- paste0("&offset=",ofst)
  req_2 <- fromJSON(paste0(url,funcion,id_carpetas,limite,ofset))
  datos_fgj <- rbind(datos_fgj,req_2$result$records)
  print(nrow(datos_fgj))
}

glimpse(datos_fgj)

# seleccionamos y limpiamos los datos --
datos_fgj <- datos_fgj %>% select(Delito,Categoria,Sexo,Edad,Ano_hecho,Mes_hecho,latitud,longitud) #separo lo que quiero
datos_fgj$latitud <- as.numeric(datos_fgj$latitud) #hago números las coordenadas
datos_fgj$longitud <- as.numeric(datos_fgj$longitud) #números de las coordenadas
datos_fgj <- datos_fgj[-which(is.na(datos_fgj$latitud)),] # quitamos los que no tienen latitud
datos_fgj <- datos_fgj[-which(is.na(datos_fgj$longitud)),] # quitamos los que no tienen longitud

glimpse(datos_fgj) 

# Graficar datos ----
#Cagamos mapa de colonias iecm y los puntos escritos ---
mapa_iecd <- read_sf("./input/colonias_iecm.shp") #No lo usamos, queremos polígonos
mapa_iecd2 <- readOGR("input/colonias_iecm.shp")
### problemas con el mapa-
poly_invalidos <- which(st_is_valid(mapa_iecd$geometry)!=T) #hay que hacer que sea una geometría, algunos polígonos son inválidos
mapa_iecd[poly_invalidos,] <- st_make_valid(mapa_iecd[poly_invalidos,])#coercer a hacer validos algunos polígonos

puntos_fgj <- st_as_sf(datos_fgj, coords = c("longitud", "latitud"), 
                   crs = "+proj=longlat +datum=WGS84 +no_defs", agr = "constant") #Convertimos en geometrias los puntos con el crs adecuado

solo_puntos <- st_transform(solo_puntos,st_crs(solo_mapa)) #transformamos a que sea la misma proyección
puntos_fgj$geometry <- solo_puntos

st_crs(puntos_fgj)
st_crs(mapa_iecd)

## Plots ----
#1)
plot_base <- ggplot(data = datos_fgj,aes(longitud,latitud)) # No tiene features to plot, sólo los datos y 
#qué va a ser qué eje
mapa_base <- (ggplot(mapa_iecd2))+
   geom_sf()

plot_base + geom_point(aes(colour = factor(Categoria))) #Plot con puntos por color
  
plot_base + geom_point(aes(colour = factor(Categoria)),size=0.01) #Puntos distribuídos por color 

ggplot(datos_fgj , aes(longitud,latitud),) + stat_binhex(bins = 100)# Agrupar en Hexágonos

#2) puntos sobre mapa
ggplot(mapa_iecd)+
  geom_sf() +
  geom_point(solo_puntos)+
  coord_sf() 

# 3) Puntos sobre el mapa divididos por categoría de delito
ggplot(data = mapa_iecd) +
  geom_sf() +
  geom_sf(data = puntos_fgj,aes(colour=Categoria), size = 0.001)

# Solo puntos 
ggplot(puntos_fgj) +
  geom_sf() +
  coord_sf() 
# Solo mapa
ggplot(mapa_iecd) +
  geom_sf() +
  coord_sf() 

##Queremos revisar si los puntos se encuentran dentro de un polígono con coordenadas-

solo_mapa <- mapa_iecd$geometry
solo_puntos <- puntos_fgj$geometry

puntos_en_mapa <- solo_puntos[solo_mapa,] #dame todos los puntos que se encuentran sobre el mapa

polys <- as_Spatial(mapa_iecd2, "SpatialPolygons") #Lo queremos en formato spatial polygon para over
punts <- as_Spatial(solo_puntos) #Lo queremos ne formato spatial point para over
crime_per_polygon <- over(polys,punts,returnList =F,fn=sum) 

mapa_iecd$cpp <- crime_per_polygon
mapa_iecd$cpp <- ifelse(is.na(mapa_iecd$cpp),0,mapa_iecd$cpp)
mapa_iecd$geometry[which(st_is_valid(mapa_iecd$geometry)==F)] <- st_make_valid(mapa_iecd$geometry[which(st_is_valid(mapa_iecd$geometry)==F)])

plot_final <- ggplot(data=mapa_iecd) +
  geom_sf(aes(fill=cpp)) +
  coord_sf()


plot_final  + scale_fill_gradientn(colours = rev(rainbow(7)),
                                       breaks = c(2, 4, 10, 100, 1000, 10000),
                                       trans = "log10")


