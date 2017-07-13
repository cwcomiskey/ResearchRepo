# Chapter Images, Code

library("ggplot2")
library(fields)
library("reshape2")
library("gridExtra")
library("dplyr")

# hitter <- read.csv("~/Desktop/ResearchRepo/Data/hitter.csv")

# Empirical Mothership plot ==============================

righties <- read.csv("~/Desktop/ResearchRepo/Data/righties.csv")
righties <- filter(righties, pz >= 1 & pz <= 4, px >= -1.5 & px <= 1.5 )

coordsR <- with(righties, cbind.data.frame(px, pz))
# nx = 55, ny = 75
xbc <- seq(-1.5 - 1e-6, 1.5 + (1e-6), , 11)[c(2, 4, 6, 8, 10)]
ybc <- seq(   1 - 1e-6,   4 + (1e-6), , 11)[c(2, 4, 6, 8, 10)]
hitgridR <- with(righties, 
                 as.image(hit, coordsR, grid = list(x = xbc, y = ybc)))
ABC.R <- with(hitgridR,
              cbind(expand.grid(x, y), as.vector(z)))
names(ABC.R) <- c("Horizontal", "Vertical", "Hitting")

# source('~/Desktop/ResearchRepo/varyres/R/mapit.R')

ggplot(ABC.R, aes(Horizontal, Vertical)) +
  geom_tile(data = ABC.R, aes(fill = Hitting)) +
  # xlim(-1.5, 1.5) + ylim(1, 4) +
  spec_fcn() +
  coord_equal() + lab_fcn() + sz_fcn() +
  ggtitle("Right-Handed Hitter \n Empirical Success")

# ggsave("/Users/ABC/Desktop/ResearchRepo/Images/Rudimentary.jpg", height = 8.5, width = 8.5) # righties empirical


# Empirical Mothership plot ==============================

hitter <- read.csv("~/Desktop/ResearchRepo/Data/hitter.csv")

coordsR <- with(hitter, cbind.data.frame(px, pz))
hitgridR <- with(hitter, as.image(hit, coordsR, nx = 20, ny = 20))
ABC.R <- with(hitgridR,
              cbind(expand.grid(x, y), as.vector(z)))
names(ABC.R) <- c("Horizontal", "Vertical", "Hitting")

ggplot(ABC.R, aes(Horizontal, Vertical)) +
  geom_tile(data = ABC.R, aes(fill = Hitting)) +
  xlim(-1.5, 1.5) + ylim(1, 4) +
  spec_fcn() + sz_fcn() +
  coord_equal() + lab_fcn()

# ggsave("Mothership.pdf", height = 8.5, width = 8.5) # righties empirical

# grid.arrange(...) - Plot increasing resolution, Peralta =======

g <- gridExtra::grid.arrange(G1, G4, G16, G64, G256, G1024, ncol = 3)
dev.off()
ggsave("Chapter_VarRes.jpg", g,
       width = 8.5*3, height = 8.5*2)


# Increasing resolution on `batter == 425509' =======

load("~/Desktop/ResearchRepo/varyres/data/hitter.rda")
source('~/Desktop/ResearchRepo/varyres/R/var_res.R')
source('~/Desktop/ResearchRepo/varyres/R/mapit.R')
dat <- varyres(hitter, cutoff = 200)

# Scatter plot
# ggplot(data = hitter, aes(x, y)) + geom_point(alpha = 0.3, size = 0.3) + coord_equal() + labs(title = "Scatter Plot", x = "", y = "") + theme(plot.title = element_text(hjust = 0.5, size = 35)) 

mapit(dat[[5]]) + spec_fcn() + labs(title = "Variable-Resolution Heat Map", x = "", y = "") + theme(plot.title = element_text(hjust = 0.5, size = 35))
# ggsave("/Users/ABC/Desktop/ResearchRepo/Images/density.jpg", height = 8.5, width = 8.5)

B <- mapit(dat[[2]]) + spec_fcn() + text_fcn(10)
C <- mapit(dat[[3]]) + spec_fcn() + text_fcn(10)
D <- mapit(dat[[4]]) + spec_fcn() + text_fcn(8)
E <- mapit(dat[[5]]) + spec_fcn() + text_fcn(6)
F_<- mapit(dat[[6]]) + spec_fcn() + text_fcn(4)



g <- grid.arrange(A, B, C, D, E, F_, ncol = 3)
ggsave("/Users/ABC/Desktop/ResearchRepo/Images/Chapter_VarRes.jpg", g, height = 2*8.5, width = 3*8.5)
dev.off()




# POLAR GLM =======================================
hitter <- read.csv("~/Desktop/Research/Data/hitter.csv")

#    r  <- sqrt( (px+a)^2 + (pz - b)^2)
# theta <- atan2( (pz - b), (px+a) )

# px = r*cos(theta) - a
# pz = r*sin(theta) + b

# About 18 inches off plate; 0.95 + 1.5 ft
# (-2.45, 5.05) ==> (0,0)
# Use (-2, 3.5) - based on loess max (shenanigans)

# Convert to polar, tranlate origin
hitter <- mutate(hitter,
                 r = sqrt( (px + 2)^2 + (pz - 3.5)^2),
                 theta = atan2(pz - 3.5, px + 2))

# glm() fit ==========================
# doh <- glm(hit ~ px*pz + I(px^2)*I(pz^2),
#            family = binomial, data = hitter )

mod.polar <- glm(hit ~ r*theta + I(r^2)*I(theta^2),
                 family = binomial, data = hitter)

# H-L GoF test ===============
# (pg 133 Myers)
# H_0: Well fit
# H_A: Lack of fit

library(ResourceSelection)
hoslem.test(mod.polar$y, fitted(mod.polar)) # p-value = 0.8217
hoslem.test(doh$y, fitted(doh))

hitter <- mutate(hitter, p.hat = predict(mod.polar, newdata = righties, type = "response"))

# Plot ===================

# Points for plotting through the hitting zone
hitzone <- cbind(expand.grid(seq(-1.5, 1.5, length = 50),
                             seq(0, 4, length = 70)))
names(hitzone) <- c("px", "pz")

# Corresponding polar coords
hitzone <- mutate(hitzone,
                  r = sqrt( (px + 2)^2 + (pz - 3.5)^2),
                  theta = atan2(pz - 3.5, px + 2) )

hitzone <- mutate(hitzone, # type = "response" for `p' instead of `logit'
                  p = predict(mod.polar,
                              newdata = hitzone,
                              type = "response"))

# source('~/Desktop/ResearchRepo/varyres/R/mapit.R')

ggplot(aes(px, pz), data = hitzone) +
  geom_tile(data = hitzone, aes(fill = p)) + 
  sz_fcn() +
  coord_equal() + xlim(-1.5, 1.5) + ylim(1, 4) +
  spec_fcn() + lab_fcn() + 
  ggtitle("Polar Covariate GLM \n Success Probability")

# ggsave("Peralta_polar.pdf", height = 8.5, width = 8.5, path = "/Users/ABC/Desktop/Research/Images")

# Line and Bands =====================

set.seed(955)
# Make some noisily increasing data
dat <- data.frame(cond = rep(c("A", "B"), each=10),
                  xvar = 1:20 + rnorm(20,sd=3),
                  yvar = 1:20 + rnorm(20,sd=3))
head(dat)
#>   cond      xvar         yvar
#> 1    A -4.252354  3.473157275
#> 2    A  1.702318  0.005939612
#> 3    A  4.323054 -0.094252427
#> 4    A  1.780628  2.072808278
#> 5    A 11.537348  1.215440358
#> 6    A  6.672130  3.608111411

library(ggplot2)

# Basic scatterplots with regression lines =======

ggplot(dat, aes(x=xvar, y=yvar)) +
  geom_point(size = 3) +
  geom_smooth(method=lm)

ggsave("CIBands.pdf", height = 8.5, width = 8.5, path = "/Users/ABC/Desktop/ResearchRepo/Images")



