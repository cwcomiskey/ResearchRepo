# Maps

# Standard map packages.
install.packages(c("maps", "mapdata"))

# the github version of ggmap
devtools::install_github("dkahle/ggmap")

library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(dplyr)
library(readr)
library(ggplot2)

# Orig. cleaning, saving ====
# torn <- read_csv("~/Desktop/ResearchRepo/Tornado/1950-2012_torn.csv", col_names = FALSE)
# torn <- select(torn, fscale = X11, injury = X12, fatality = X13, loss = X14, stlat = X16, endlat = X18, stlon = X17, endlon = X19)
# torn <- mutate(torn, lat = (stlat + endlat)/2,long = (stlon + endlon)/2) %>% filter(long < -67 & long > -130, lat > 20 & lat < 52)
# torn_vr <- select(torn, res = fscale, x = long, y = lat) %>% filter(res >= 0)
# write.csv(torn, file = "torn.csv")
# write.csv(torn_vr, file = "torn_vr.csv")
# end mess =====


usa <- map_data("usa")

torn_vr <- read.csv("/Users/ABC/Desktop/ResearchRepo/Tornado/torn_vr.csv", row.name = 1)

# source('~/Desktop/ResearchRepo/varyres/R/var_res.R')
# source('~/Desktop/ResearchRepo/varyres/R/mapit.R')
tornado_vr <- varyres(data = torn_vr, cutoff = 50)

mapit(tornado_vr[[7]])

# Vanilla plot =====
vanado <- function(dat){
ggplot2::ggplot(dat, aes(x, y, fill=statistic)) +
  with(dat, geom_tile(width = width, height = height)) +
  coord_fixed(1.3) +
  scale_fill_distiller(palette = "Spectral", guide = FALSE) +
    geom_polygon(data = usa,
                 aes(x=long, y = lat, group = group),
                 fill = "NA", color = "black") +
    geom_polygon(data = oceans_usa,
                 aes(x = long, y = lat, group = group),
                 fill = "blue") +
    geom_polygon(data = canada, 
                 aes(x = long, y = lat, group = group), 
                 fill = "green") +
    geom_polygon(data = mexico, 
                 aes(x = long, y = lat, group = group), 
                 fill = "green") 
}

vanado(tornado_vr[[7]])



# Full plot ===== #
mapnado <- function(dat){
ggplot() +
  geom_tile(data = dat,
            aes(x, y, fill=statistic,
                width = width, height = height)) +
  scale_fill_distiller(palette = "Spectral",
                       guide_legend(title = "F-scale")) +
  geom_polygon(data = usa,
               aes(x=long, y = lat, group = group),
               fill = NA, color = "black") +
  # geom_point(data = torn_vr, aes(x = px, y = pz),
  #            alpha = 1/3, size = 0.5) +
  coord_fixed(1.3) +
  ggtitle("US Tornado Intensity, 1950 - 2007 ") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(legend.key.size = unit(2, "cm"),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30),
        legend.title.align = 0.25,
         axis.title.x = element_text(size=28),
         axis.title.y = element_text(size=28),
         plot.title = element_text(hjust = 0.5,
                                   size = 30),
         axis.text = element_text(size = 20))
}

g1 <- mapnado(tornado_vr[[1]])
g2 <- vanado(tornado_vr[[2]])
g3 <- vanado(tornado_vr[[3]])
g4 <- vanado(tornado_vr[[4]])
g5 <- vanado(tornado_vr[[5]])
g6 <- vanado(tornado_vr[[6]])
g7 <- vanado(tornado_vr[[7]])
g8 <- vanado(tornado_vr[[8]])

g <- gridExtra::grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8,
                             ncol = 2)
# dev.off()
# ggsave("/Users/ABC/Desktop/ResearchRepo/Images/tornado_maps.jpg", g, width = 22, height = 28)

geom_point(data = torn,
           aes(x = long, y = lat),
           size = 0.1, alpha = 1/5)

# ggsave("/Users/ABC/Desktop/ResearchRepo/Images/vr_tornados.jpg", width = 11, height = 7)

ggplot(tornado_vr[[8]], aes(px, pz, fill=statistic)) +
  with(tornado_vr[[8]], geom_tile(width = width, height = height)) +
  coord_fixed(1.3) +
  scale_fill_distiller(palette = "Spectral") +
  geom_polygon(data = usa,
               aes(x=long, y = lat, group = group),
               fill = NA, color = "black")

