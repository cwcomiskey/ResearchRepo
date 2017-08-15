ggplot(aes(x, y), data = listn[[1]]) +
  geom_tile(aes(fill = listn[[96]]$ub)) +
  coord_equal() +
  scale_fill_distiller(palette = "YlOrRd",
                       trans = "reverse",
                       # name = guide_title,
                       guide = FALSE,
                       limits = c(0.17, 0)) +
  labs(title = "Pointwise Upper Bound") +
  theme(plot.title = element_text(hjust = 0.5),
        title = element_text(size = 32),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  sz_fcn()



ggsave("Upper.pdf", height = 8.5, width = 8.5, path = "/Users/ABC/Desktop/ResearchRepo/Images")


ggplot(aes(x, y), data = listn[[1]]) +
  geom_tile(aes(fill = listn[[6]]$lb)) +
  coord_equal() +
  scale_fill_distiller(palette = "YlOrRd",
                       trans = "reverse",
                       # name = guide_title,
                       guide = FALSE,
                       limits = c(0.17, 0)) +
  labs(title = "Pointwise Lower Bound") +
  theme(plot.title = element_text(hjust = 0.5),
        title = element_text(size = 32),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  sz_fcn()

ggsave("Lower.pdf", height = 8.5, width = 8.5, path = "/Users/ABC/Desktop/ResearchRepo/Images")

