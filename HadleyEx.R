# Another example with with unequal tile sizes

x.cell.boundary <- c(0, 4, 6, 8, 10, 14)
example <- data.frame(
  x = rep(c(2, 5, 7, 9, 12), 2),
  y = factor(rep(c(1,2), each=5)),
  z = rep(1:5, each=2),
  w = rep(diff(x.cell.boundary), 2)
)

qplot(x, y, fill=z, data=example, geom="tile")
qplot(x, y, fill=z, data=example, geom="tile", width=w)

example
#     x y z w
# 1   2 1 1 4
# 2   5 1 1 2
# 3   7 1 2 2
# 4   9 1 2 2
# 5  12 1 3 4
# 6   2 2 3 4
# 7   5 2 4 2
# 8   7 2 4 2
# 9   9 2 5 2
# 10 12 2 5 4

# geom_rect() ===============

df <- data.frame(
  x = sample(10, 20, replace = TRUE),
  y = sample(10, 20, replace = TRUE)
)
ggplot(df, aes(xmin = x, xmax = x + 1, ymin = y, ymax = y + 2)) +
  geom_rect()
