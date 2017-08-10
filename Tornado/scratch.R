
ggplot(tornado_vr[[5]], aes(x, y, fill=statistic)) +
  geom_tile(aes(width = width, height = height)) +
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
