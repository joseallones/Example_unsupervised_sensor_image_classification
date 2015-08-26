# Example taken from https://metvurst.wordpress.com/2015/05/19/unsupervised-google-maps-image-classification-16/

 
#The first time it is required to install devtools, leaflet, satellite, Rsenal and RColorBrewer:
#install.packages("devtools");
#library(devtools)
#devtools::install_github("rstudio/leaflet")

#install package satellite from a local file zip 
#(downloaded from https://cran.r-project.org/web/packages/satellite/index.html)

#install_github("environmentalinformatics-marburg/Rsenal")

#install.packages("RColorBrewer")


library(devtools)
library(satellite)
library(leaflet)
library(Rsenal)
library(RColorBrewer)
library(lattice) 


lib <- c("Rsenal", "cluster", "rasterVis", "RColorBrewer")
jnk <- sapply(lib, function(x) library(x, character.only = TRUE))

## 3-by-3 focal matrix (incl. center)
mat_w3by3 <- matrix(c(1, 1, 1, 
                      1, 1, 1, 
                      1, 1, 1), nc = 3)

## 5-by-5 focal matrix (excl. center)
mat_w5by5 <- matrix(c(1, 1, 1, 1, 1, 
                      1, 1, 1, 1, 1, 
                      1, 1, 0, 1, 1, 
                      1, 1, 1, 1, 1, 
                      1, 1, 1, 1, 1), nc = 5)


#Rsenal includes a built-in dataset (data(gmap_hel)) that shall serve as a basis for an unsupervised classification approach. 
#The image is originated from Google Maps and has been downloaded via dismo::gmap. 
data(gmap_hel, package = "Rsenal")
gmap_hel  # gmap_hel is A rasterBrick (a multi-layer raster object).
gmap_hel[1,1] #rgb of the first cell
plotRGB(gmap_hel)
#The land surface is dominated by small to medium-sized shrubs (medium brown) with smaller proportions of tall bushes (dark brown) 
#and bare soil (light brown). Also included are shadows (dark brown to black), which are typically located next to tall vegetation.


# A number of artificial layers is calculated from the red, green and blue input bands including:
  #   focal means and standard deviations,
  #   a visible vegetation index and
  #   a shadow mask.

gmap_hel_fcmu <- lapply(1:nlayers(gmap_hel), function(i) {
  focal(gmap_hel[[i]], w = mat_w5by5, fun = mean, na.rm = TRUE, pad = TRUE)
})
gmap_hel_fcmu <- stack(gmap_hel_fcmu)


gmap_hel_fcsd <- lapply(1:nlayers(gmap_hel), function(i) {
  focal(gmap_hel[[i]], w = mat_w5by5, fun = sd, na.rm = TRUE, pad = TRUE)
})
gmap_hel_fcsd <- stack(gmap_hel_fcsd)


gmap_hel_vvi <- vvi(gmap_hel)
gmap_hel_vvi
plot(gmap_hel_vvi)

## shadow detection
gmap_hel_shw <- rgbShadowMask(gmap_hel)

## modal filter
gmap_hel_shw <- focal(gmap_hel_shw, w = mat_w3by3, 
                      fun = modal, na.rm = TRUE, pad = TRUE)

gmap_hel_shw
plot(gmap_hel_shw)

gmap_hel_shw[1:20,1:20]



# Image classification via kmeans()
# 
# The unsupervised image classification is  realized via kmeans clustering. 
# We focus on separating the 3 major land-cover types depicted above, namely
# 
# bare soil (class 'A'),
# small to medium-sized vegetation (class 'B') and
# tall vegetation (class 'C').

## assemble relevant raster data
gmap_hel_all <- stack(gmap_hel, gmap_hel_fcmu, gmap_hel_fcsd, gmap_hel_vvi)

## convert to matrix
mat_hel_all <- as.matrix(gmap_hel_all)

## k-means clustering with 3 target groups
kmn_hel_all <- kmeans(mat_hel_all, centers = 3, iter.max = 100, nstart = 10)



#After inserting the classification results into a separate raster template (rst_tmp), 
#an additional class for shadow (tagged '0') is created through multiplication with 
#the initially created shadow mask.


## raster template
rst_tmp <- gmap_hel[[1]]

## insert values
rst_tmp[] <- kmn_hel_all$cluster

## apply shadow mask
rst_tmp <- rst_tmp * gmap_hel_shw



#Some lines to visualize the final raster. Each color is a class:

rat_tmp <- ratify(rst_tmp)
rat <- rat_tmp@data@attributes[[1]]
rat$Class <- c("S", "C", "A", "B")
rat <- rat[order(rat$Class), , ]
levels(rat_tmp) <- rat

ylgn <- brewer.pal(3, "YlGn")
names(ylgn) <- c("A", "B", "C")
ylgnbl <- c(ylgn, c("S" = "black"))

levelplot(rat_tmp, col.regions = ylgnbl)







#Example of leaflet
# library(leaflet)
# 
# m <- leaflet() %>%
#   addTiles() %>%  # Add default OpenStreetMap map tiles
#   addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
# m  # Print the map