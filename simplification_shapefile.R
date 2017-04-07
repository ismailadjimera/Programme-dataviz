
library(sp)              
library(raster)
library(rgdal)
library(maptools)
library(rgeos)
library(readr)
library(readxl)


## Chargement du fond de carte : contours des IRIS pour toute la France
setwd('C:/Users/HCcenter/Desktop/Elia/4. IRIS/0. Old')

contours_IRIS = readOGR(dsn = "CONTOURS-IRIS.shp",
                         layer = "CONTOURS-IRIS", verbose = FALSE, encoding = 'UTF-8')


## On ne garde que les IRIS de la région Brie-Picardie :

# Ajout d'une variable 'département' au shapefile :
# Le code du département est égal au deux premiers chiffres du Code insee de la commune

contours_IRIS$ID_2 = substr(contours_IRIS$INSEE_COM,1,2)

# Filtre le fond de carte pour ne garder que la région Brie-Picardie :
contours_IRIS =(contours_IRIS[contours_IRIS$ID_2 == 60 | contours_IRIS$ID_2 == 77 | contours_IRIS$ID_2 == 80,])


# #     On simplifie les contours des polygones pour réduire la taille du shapefile :
# 
# contours_IRIS_data = contours_IRIS@data
# 
# contours_IRIS_simplified = gSimplify(contours_IRIS, tol = 1)
# 
# spdf = SpatialPolygonsDataFrame(contours_IRIS_simplified, contours_IRIS_data)
# 
# Sys.getlocale("LC_CTYPE")
# setCPLConfigOption("SHAPE_ENCODING", "UTF-8")
# 
# plot(contours_IRIS_simplified)
# 
# #     On sauvegarde le nouveau shapefile :

writeOGR(spdf, ".", "contours_IRIS_BP", driver="ESRI Shapefile", layer_options= c(encoding= "UTF-8"),
         overwrite_layer=T)

