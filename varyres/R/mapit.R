mapit <- function(ABCE) {
  ggplot2::ggplot(ABCE, aes(px, pz, fill=statistic)) +
  with(ABCE, geom_tile(width = width, height = height)) +
  coord_equal() +
  scale_fill_distiller(palette = "Spectral") +
  geom_text(aes(label = count), size = 3.5)
}

mapit2 <- function(ABCE) {
  kzone <- data.frame(x = c(-0.75, -0.75, 0.75, 0.75, -0.75),
                      y = c(1.5, 3.5, 3.5, 1.5, 1.5))
  ggplot2::ggplot(ABCE, aes(px, pz)) +
    with(ABCE, geom_tile(aes(fill = statistic), width = width, height = height)) +
    coord_equal() +
    scale_fill_distiller(palette = "Spectral",
    guide = guide_legend(title = expression(hat(p)))) +
    geom_path(data = kzone, 
              aes(x = c(-0.75, -0.75, 0.75, 0.75, -0.75), 
                  y = c(1.5, 3.5, 3.5, 1.5, 1.5)), 
              lwd = 1.5, col = "blue", linetype = 2) +
  ggtitle("P(Hit|Swing)") +
  xlab("Feet from \n Middle of Home Plate") +
  ylab("Feet Off Ground") +
  theme(legend.key.size = unit(2, "cm"),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 40),
        legend.title.align = 0.25,
        axis.title.x = element_text(size=28),
        axis.title.y = element_text(size=28),
        title = element_text(size = 28),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 28))
}

# filter(ABCE, Count != "NA", Hitting != "NA") # gray zero boxes

# g <- grid.arrange(G1, G4, G16, G64, G256, G1024, ncol = 3)
# dev.off()
# ggsave("Chapter_VarRes.jpg", g,
#        width = 8.5*3, height = 8.5*2)

# kZone <- data.frame(x = c(-0.75, -0.75, 0.75, 0.75, -0.75),
                     # y = c(1.5, 3.5, 3.5, 1.5, 1.5))

# ggplot(ABC.R, aes(Horizontal, Vertical, fill = Hitting)) +
#   geom_tile() +
#   xlim(-1.5, 1.5) + ylim(1, 4) +
#   scale_fill_distiller(palette = "Spectral",
#               limits = c(0, 0.18),



