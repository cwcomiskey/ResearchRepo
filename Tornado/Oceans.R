# devtools::install_github("ropenscilabs/rnaturalearth")
library(rnaturalearth)
library(raster)
library(ggplot2)
library(rgeos)
library(rgdal)

# grab whole world of oceans
oceans <- ne_download(scale = 50, type = 'ocean', 
                      category = 'physical')

canada <- ne_countries(scale = 50, type = "countries", 
                       country = "canada")
canada <- crop(canada, extent(-125, -65, 40, 50))

mexico <- ne_countries(scale = 50, type = "countries", country = "mexico")
mexico <- crop(mexico, extent(-125, -65, 24, 35))

# clip to relevant region
oceans_usa <- crop(oceans, extent(-125, -65, 24, 50))


# plot as polygons

ggplot(oceans_usa, aes(long, lat)) +
  geom_polygon(aes(group = group), fill = "blue") +
  geom_polygon(data = oceans_usa_south, aes(group = group), fill = "blue") +
  coord_equal(1.3) +
  geom_polygon(data = canada, aes(group = group), fill = "green") +
  geom_polygon(data = mexico, aes(group = group), fill = "green")
