##Social Tic - Manipulation of Spatial Data with R
# Descripción: Este código pretende descargar la base de víctimas de la FGJ y analizar geográficamente los datos
# Con base en los mapas del IECM IECM: https://datos.cdmx.gob.mx/dataset/coloniascdmx 
# ADIP - Subdirección de Datos Abiertos
# David A. Ortega
# 04/03/2021

## Preparamos el Entornio----
dev.off()
rm(list=ls())
# install.packages(pacman) 
# Pacman se utiliza para manejar paquetes de R 
pacman::p_load(sf,st,tidyverse,jsonlite,rgdal,leaflet,raster,htmltools)

## Cargar de mapas a R ----
## Mapa de colonias de la CDMX del IECM que se encuentra en https://datos.cdmx.gob.mx/dataset/coloniascdmx 

mapa_iecd <- readOGR("./input/mgpc_2019.shp") #Cargamos el mapa de colonias del IEECM
class(mapa_iecd) # Objeto de clase espacial y de datos
mapa_iecd@bbox # Podemos accesar a atributos particulares con el @
mapa_iecd %>% head() # Tambien podemos realizar operaciones de data.frame sobre este objeto

#Ver el Mapa
plot(mapa_iecd, col="#f2f2f2", bg="skyblue", border="black" ,lwd=0.5 )
dev.off()

## Carga de la Base de Datos----

## Una variable de interés con datos geográficos: 
# https://datos.cdmx.gob.mx/dataset/victimas-en-carpetas-de-investigacion-fgj 

### Consulta base de datos de víctimas de la fiscalía con la API---

# Documentacion de la API de CKAN: https://docs.ckan.org/en/2.9/api/ 

url <- "https://datos.cdmx.gob.mx/api/3/action/"
funcion <- "datastore_search?resource_id="
id_carpetas <- "d543a7b1-f8cb-439f-8a5c-e56c5479eeb5" #carpetas y viictimas de investigación en la CDMX:
limite <- "&limit=32000" # Si no se especifica el número de observaciones se regresan 100 por default
# limita las consultas de json a 32,000 observaciones por consulta la página de datos abiertos

# Hacemos el request
req <- fromJSON(paste0(url,funcion,id_carpetas,limite))
class(req) # el request es una lista, por lo que debemos buscar lo que pedimos dentro de la lista
glimpse(req) # objetos de la lista con listas y objetos dentro de ellos
req$success # el request fue exitoso

#Extraemos los datos de la consulta
datos_fgj <- req$result$records
glimpse(datos_fgj) #Revisamos qué datos vienen
total_records <- req$result$total #Vemos cuál es el total de los datos
total_records
# obtenemos el total de registros con una consulta por 32,000 observaciones a la vez hasta consumir toda la base
while (nrow(datos_fgj)<total_records){
  ofst <- nrow(datos_fgj)
  ofset <- paste0("&offset=",ofst)
  req_2 <- fromJSON(paste0(url,funcion,id_carpetas,limite,ofset))
  datos_fgj <- rbind(datos_fgj,req_2$result$records)
  print(nrow(datos_fgj))
}
#limpiamos espacio
rm(url,funcion,id_carpetas,limite,ofset,ofst,req,req_2,total_records)
glimpse(datos_fgj)
# Limpiamos los datos----
#Para nuestro análisis queremos únicamente la categoría del delito, el año del hecho y el sexo de la victima
datos_fgj <- datos_fgj %>% tibble() %>%  dplyr::select(Categoria,Sexo,Ano_hecho,latitud,longitud) # elegimos lo que queremos 
glimpse(datos_fgj)
#Problema, necesitamos que las latitudes y longitudes sean numéricos:
datos_fgj$latitud <- as.numeric(datos_fgj$latitud) #hago números las coordenadas
datos_fgj$longitud <- as.numeric(datos_fgj$longitud) #números de las coordenadas
sum(is.na(datos_fgj$latitud))
sum(is.na(datos_fgj$longitud))
datos_fgj <- datos_fgj[-which(is.na(datos_fgj$latitud)),] # quitamos los que no tienen latitud
datos_fgj <- datos_fgj[-which(is.na(datos_fgj$longitud)),] # quitamos los que no tienen longitud

# Vemos los puntos
ggplot(data = datos_fgj,aes(longitud,latitud)) +
  geom_point(size=0.01) # datos normales
dev.off()
# Vamos a ver la distribución de los hechos reportados: 
table(datos_fgj$Ano_hecho)
plot(table(datos_fgj$Ano_hecho))
#Restringir datos a los hechos entre 2019 - 2021:
datos_fgj <- datos_fgj %>% filter(Ano_hecho > 2018) %>% filter(Ano_hecho!= "NA")
plot(table(datos_fgj$Ano_hecho))
## Graficas ----
## Densidad de crimen por puntos ---
toc <- Sys.time()
ggplot(datos_fgj, aes(x = longitud, y = latitud)) + 
  coord_equal() + 
  xlab('Longitud') + 
  ylab('Latitud') + 
  labs(title="Densidad de crimen en la CDMX con datos entre 2019-2021", 
       caption = "Con información de la FGJ en Datos Abiertos: https://datos.cdmx.gob.mx/dataset/victimas-en-carpetas-de-investigacion-fgj")+
  stat_density2d(aes(fill = ..level..), alpha = .5,
                 h = .02, n = 300,
                 geom = "polygon", data = datos_fgj) +
  scale_fill_viridis_c() 
tic <- Sys.time()
print(tic-toc)
dev.off()
## Algunas variables de interés: Desglosemos por sexo ---
ggplot(data = datos_fgj,aes(longitud,latitud)) +
  geom_point(size=0.01,aes(colour=Sexo))+
  labs(title="Carpetas de Investigación por Sexo de la Víctima en la CDMX entre 2016 - 2021",
       caption = "Con información de la FGJ en Datos Abiertos: https://datos.cdmx.gob.mx/dataset/victimas-en-carpetas-de-investigacion-fgj")

#Quitamos el sexo NA ---
datos_sexo <- datos_fgj[!datos_fgj$Sexo=="NA",]
nrow(datos_fgj)-nrow(datos_sexo) # Datos perdidos al quitar NA
ggplot(data = datos_sexo,aes(longitud,latitud)) +
  geom_point(size=0.01,aes(colour=Sexo)) + 
  theme(legend.background = element_rect(fill="lightblue",size=0.5, linetype="solid", colour ="darkblue")) +
  labs(title="Carpetas de Investigación por Sexo de la Víctima en la CDMX entre 2016 - 2021",
       caption = "Con información de la FGJ en Datos Abiertos: https://datos.cdmx.gob.mx/dataset/victimas-en-carpetas-de-investigacion-fgj")

## Otra variable de interés: categoría del delito ---
ggplot(data = datos_fgj,aes(longitud,latitud)) +
  geom_point(size=0.01,aes(colour=Categoria)) + 
  theme(legend.position = "bottom",
        legend.background = element_rect(fill="lightblue",size=0.5, linetype="solid", colour ="darkblue")) +
  guides(colour=guide_legend(ncol=2))+
  labs(title="Carpetas de Investigación por Categoría de Delito en la CDMX entre 2016 - 2021",
       caption = "Con información de la FGJ en Datos Abiertos: https://datos.cdmx.gob.mx/dataset/victimas-en-carpetas-de-investigacion-fgj")
  
# Dificil de interpretar, podemos seccionarlo por categoría del delito ---

ggplot(data = datos_fgj,aes(longitud,latitud)) +
  geom_point(size=0.01,aes(colour=Categoria)) + 
  facet_wrap( ~ Categoria)+
  theme(legend.position="none")+
  labs(title="Carpetas de Investigación por Categoría de Delito en la CDMX entre 2016 - 2021",
       caption = "Con información de la FGJ en Datos Abiertos: https://datos.cdmx.gob.mx/dataset/victimas-en-carpetas-de-investigacion-fgj")
dev.off()

## Intersectamos ambos datos ----

# Clase de ambos objetos es distinta:
class(mapa_iecd)
class(datos_fgj)
# Necesitamos hacer que los datos de latitud y longitud sean coordinadas.-
#Extraemos los puntos y hacemos una matriz de ellos:
puntos_fgj <- matrix(c(datos_fgj$longitud,datos_fgj$latitud),ncol = 2)
puntos_fgj %>% head()

#Ambos deben tener la misma predicción al convertir en objeto espacial
# Si revisamos el diccionario de datos encontrramos la projección utilizada por la FGJ:
# https://datos.cdmx.gob.mx/dataset/victimas-en-carpetas-de-investigacion-fgj   
# https://spatialreference.org/ref/epsg/wgs-84/

puntos_fgj <- SpatialPoints(puntos_fgj,proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"),bbox=NULL)
#proj4string se utiliza para obtener la proyección de un objeto espacial
proj4string(mapa_iecd)
#hacemos que el CRS (Coordinate Reference System) sea el mismo para ambas
puntos_fgj <- spTransform(puntos_fgj,crs(mapa_iecd))
puntos_fgj
mapa_iecd
# Ahora ambos son objetos espaciales en las mismas coordenadas y projecciones
class(puntos_fgj) 
class(mapa_iecd)
st_crs(puntos_fgj)==st_crs(mapa_iecd)
#Agregamos los datos que teníamos al nuevo objeto
datos_fgj_geom <- SpatialPointsDataFrame(puntos_fgj, datos_fgj)
datos_fgj_geom
datos_fgj_geom@data %>% head()

### Vemos el mapa superpuesto
plot(mapa_iecd, bg="lightblue", main="Delitos en la CDMX entre 2016-2020")
points(puntos_fgj,pch=20,col="blue")
dev.off()
## La diferencia principal entre los mapas de antes y ahora, es que ahora ambos objetos son espaciales:
## Por lo que podemos operar sobre ellos
## Hacemos uso de la función over para revisar cuántos puntos hay sobre cada polígono
over(mapa_iecd,puntos_fgj,returnList = FALSE, fn=sum) %>% head()

mapa_iecd@data$delitos_totales <- over(mapa_iecd,puntos_fgj,returnList = FALSE, fn=sum)

## leaflet delitos totales----
mapa_iecd <- spTransform(mapa_iecd, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
## Hacemos las divisiones con los deciles de delitos totales
bins <- quantile(mapa_iecd@data$delitos_totales,probs = seq(.1, 1, by = .1),na.rm=T)
bins <- as.numeric(levels(factor(bins)))
#colores de acuerdo a los cutoffs de los deciles
pal <- colorBin("YlOrRd", domain = mapa_iecd$delitos_totales, bins = bins)

#Creamos leaflet
m <- leaflet(mapa_iecd) %>%
  addTiles()
# Creamos los labels
labels <- sprintf(
  "<strong>%s</strong><br/>%g Delitos totales entre 2019-2021",
  mapa_iecd$NOMUT, mapa_iecd$delitos_totales) %>% lapply(htmltools::HTML)

m %>% addPolygons(data=mapa_iecd, weight = .5,  fillColor = ~pal(delitos_totales), 
                  opacity = 1,
                  color = "black",
                  fillOpacity = 0.7,
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"))
m
##Leaflet - violación----
#creamos el subset de los puntos 
violacion <- subset(datos_fgj_geom, Sexo == "Femenino" & Categoria == "VIOLACIÓN")
violacion <- coordinates(violacion) #extraemos coordenadas
st_crs(violacion)
violacion <- SpatialPoints(violacion,proj4string = CRS("+proj=utm +zone=14 +datum=WGS84 +units=m +no_defs"),bbox=NULL) 
mapa_iecd <- spTransform(mapa_iecd, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
violacion <- spTransform(violacion,crs(mapa_iecd))

#intersectamos
mapa_iecd@data$violacion <- over(mapa_iecd,violacion,returnList = FALSE, fn=sum)
#creamos las paletas de colores y divisiones
bins <- quantile(mapa_iecd$violacion,probs = seq(.1, 1, by = .1),na.rm=T)
bins <- as.numeric(levels(factor(bins)))
pal <- colorBin("YlOrRd", domain = mapa_iecd@data$violacion, bins = bins)
# creamos los labels
labels <- sprintf(
  "<strong>%s</strong><br/>%g Violaciones a Mujeres entre 2019-2021",
  mapa_iecd$NOMUT, mapa_iecd$delitos_totales) %>% lapply(htmltools::HTML)

m <- leaflet(mapa_iecd) %>%
  addTiles()

labels <- sprintf(
  "<strong>%s</strong><br/>%g Delitos totales entre 2019-2021",
  mapa_iecd$NOMUT, mapa_iecd$delitos_totales) %>% lapply(htmltools::HTML)

m %>% addPolygons(data=mapa_iecd, weight = .5,  fillColor = ~pal(violacion), 
              opacity = 1,
              color = "black",
              fillOpacity = 0.7,
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"))

  

