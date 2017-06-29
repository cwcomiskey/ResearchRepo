mapit <- function(ABCE = ABCE) {
  ggplot2::ggplot(ABCE, aes(x = x, y = y)) +
  ggplot2::geom_tile(data = ABCE, aes(fill = statistic),
                       width = width, height = height) +
  coord_equal()
}

spec_fcn <- function(g = TRUE){
  list(scale_fill_distiller(palette = "Spectral",
                            # limits = c(0, 0.170),
                guide = if(g)
                guide_legend(title = expression(hat(p)))
                else guide = FALSE)
       )
}

text_fcn <- function(s = 8){
  list(geom_text(aes(label = count), size = s))
}

lab_fcn <- function(s1 = 25, s2 = 30){ # Labels function
  list(
    labs(title = "Empirical Success",
         subtitle = "Johny Peralta",
         x = "Feet from \n Middle of Home Plate",
         y = "Feet Off Ground"),
    theme(legend.key.size = unit(2, "cm"),
          legend.text = element_text(size = s2),
          legend.title = element_text(size = s2),
          legend.title.align = 0.25,
          axis.title.x = element_text(size=s1),
          axis.title.y = element_text(size=s1),
          title = element_text(size = s2),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          axis.text = element_text(size = s1))
  )
}

sz_fcn <- function(width = 1.5){
  kzone <- data.frame(x = c(-0.75, -0.75, 0.75, 0.75, -0.75),
                      y = c(1.5, 3.5, 3.5, 1.5, 1.5))
  list(geom_path(data = kzone,
                 aes(x = c(-0.75, -0.75, 0.75, 0.75, -0.75),
                     y = c(1.5, 3.5, 3.5, 1.5, 1.5)),
                 lwd = width, col = "blue", linetype = 2))
}

# g <- grid.arrange(G1, G4, G16, G64, G256, G1024, ncol = 3)
# dev.off()

# ggsave("/Users/ABC/Desktop/ResearchRepo/Images/Chapter4x4.jpg", width = 8.5, height = 8.5)
