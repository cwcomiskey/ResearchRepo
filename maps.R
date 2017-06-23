# Maps

# some standard map packages.
install.packages(c("maps", "mapdata"))

# the github version of ggmap, which recently pulled in a small fix I had
# for a bug 
devtools::install_github("dkahle/ggmap")
# Load up a few of the libraries we will use

library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)

usa <- map_data("usa")
dim(usa); head(usa)

ggplot() + 
  geom_polygon(data = usa, 
               aes(x=long, y = lat, group = group), 
               fill = NA, color = "black") + 
  coord_fixed(1.3) +
  geom_point(data = torn, 
             aes(x = long, y = lat), 
             size = 0.1, alpha = 1/5)

