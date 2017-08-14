# Sequential Color Scale Work

# ggplot(vr[[4]], aes(x = x, y = y)) +
#   geom_tile(data = vr[[4]], aes(fill = statistic),
#             width = vr[[4]]$width, height = vr[[4]]$height) +
#     coord_equal() +
#     scale_fill_distiller(palette = "YlOrRd", trans = "reverse")
# YlOrRd, Greys

seq_mapit <- function(datter, txt = 10){
ggplot(datter, aes(x, y)) +
  geom_tile(data = datter, aes(fill = statistic),
            width = datter$width, height = datter$height) +
  coord_equal() + # sz_fcn() + 
  # geom_text(data = datter, aes(label = count), size = txt) +
  # labs(title = ttl) +
  scale_fill_distiller(palette = "YlOrRd", trans = "reverse", 
    # guide = guide_legend(title = expression(paste(p[b])))) +
    guide = FALSE) +
  theme(legend.key.size = unit(2, "cm"),
      legend.text = element_text(size = 25),
      legend.title = element_text(size = 25),
      legend.title.align = 0.25,
      title = element_text(size = 25))
}

seq_mapit(vr[[4]]) + nal_fcn() + geom_point(data = vr[[4]], aes(x = x, y = y), size = 5)

# ggsave("/Users/ABC/Desktop/ResearchRepo/Images/knots.jpg", height = 8.5, width = 8.5)
