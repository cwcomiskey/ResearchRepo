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
  geom_tile(data = tornado_vr[[8]], 
            aes(px, pz, fill=statistic, 
                width = width, height = height)) +
  scale_fill_distiller(palette = "Spectral") +
  geom_polygon(data = usa, 
               aes(x=long, y = lat, group = group), 
               fill = NA, color = "black") + 
  coord_fixed(1.3) +
  ggtitle("US Tornado Intensity, 1950 - 2007 ") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(  # legend.key.size = unit(2, "cm"),
  #       # legend.text = element_text(size = 30),
  #       # legend.title = element_text(size = 30),
  #       # legend.title.align = 0.25,
         axis.title.x = element_text(size=20),
         axis.title.y = element_text(size=20),
         plot.title = element_text(hjust = 0.5, size = 20))
         # axis.text = element_text(size = 20))

  # geom_point(data = torn, 
  #            aes(x = long, y = lat), 
  #            size = 0.1, alpha = 1/5) +

# ggsave("/Users/ABC/Desktop/ResearchRepo/Images/vr_tornados.jpg", width = 11, height = 7)



ggplot(tornado_vr[[8]], aes(px, pz, fill=statistic)) +
  with(tornado_vr[[8]], geom_tile(width = width, height = height)) +
  coord_fixed(1.3) +
  scale_fill_distiller(palette = "Spectral") 

