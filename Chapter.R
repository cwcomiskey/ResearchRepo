# Chapter Images, Code

library("ggplot2")
library(fields)
library("reshape2")
library("gridExtra")
library("dplyr")
library(varyres)

# hitter <- read.csv("~/Desktop/ResearchRepo/Data/hitter.csv")
load("~/Desktop/ResearchRepo/varyres/data/hitter.rda")
hitter <- hitter

# source('~/Desktop/ResearchRepo/varyres/R/mapit.R')

# Ch4 Plot =======================
vr <- varyres(hitter, cutoff = 200, max = 6)
mapit(vr[[5]]) + 
  geom_point(data = vr[[5]], aes(x = x, y = y), size = 5)
dim(vr[[5]])

ggsave("/Users/ABC/Desktop/ResearchRepo/Images/knots.jpg", height = 8.5, width = 8.5)



# Empirical Mothership plot ==============================

righties <- read.csv("~/Desktop/ResearchRepo/Data/righties.csv")
righties <- filter(righties, pz >= 1 & pz <= 4, px >= -1.5 & px <= 1.5 )

coordsR <- with(righties, cbind.data.frame(px, pz))
# nx = 55, ny = 75
# xbc <- seq(-1.5 - 1e-6, 1.5 + (1e-6), , 11)[c(2, 4, 6, 8, 10)]
# ybc <- seq(   1 - 1e-6,   4 + (1e-6), , 11)[c(2, 4, 6, 8, 10)]
hitgridR <- with(righties,
                 as.image(hit, nx = 33, ny = 19, coordsR))
ABC.R <- with(hitgridR,
              cbind(expand.grid(x, y), as.vector(z)))
names(ABC.R) <- c("Horizontal", "Vertical", "Hitting")

# source('~/Desktop/ResearchRepo/varyres/R/mapit.R')

mapit_seq(ABC.R)

ggplot(ABC.R, aes(Horizontal, Vertical)) +
  geom_tile(data = ABC.R, aes(fill = Hitting)) +
  coord_equal() + lab_fcn() + sz_fcn() +
  scale_fill_distiller(palette = "YlOrRd", trans = "reverse",
                       guide = guide_legend(title = expression(paste(p[b]))))

# ggsave("/Users/ABC/Desktop/ResearchRepo/Images/Mothership.jpg", height = 8.5, width = 8.5) # righties empirical

# Empirical Mothership plot 2 ==============================

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

# You-name-it resolution =======

load("~/Desktop/ResearchRepo/varyres/data/hitter.rda")
source('~/Desktop/ResearchRepo/varyres/R/var_res.R')
source('~/Desktop/ResearchRepo/varyres/R/mapit.R')
dat <- varyres(hitter, cutoff = 200, max = 6)

seq_mapit(dat[[5]], txt = 4) #  + sz_fcn()

# ggsave("/Users/ABC/Desktop/ResearchRepo/Images/Chapter16x16_200.jpg", height = 8.5, width = 8.5)

a <- seq_mapit(dat[[1]], txt = 15, ttl = "(A)")
b <- seq_mapit(dat[[2]], txt = 15, ttl = "(B)")
c <- seq_mapit(dat[[3]], txt = 15, ttl = "(C)")
d <- seq_mapit(dat[[4]], txt = 7, ttl = "(D)")
e <- seq_mapit(dat[[5]], txt = 5, ttl = "(E)")
f <- seq_mapit(dat[[6]], txt = 3, ttl = "(F)")


g <- grid.arrange(a, b, c, d, e, f, ncol = 3)

# ggsave("/Users/ABC/Desktop/ResearchRepo/Images/6_100.jpg", g, height = 2*8.5, width = 3*8.5)

dev.off()

# Scatter plot ============== 
# ggplot(data = hitter, aes(x, y)) + geom_point(alpha = 0.3, size = 0.3) + coord_equal() + labs(title = "Scatter Plot", x = "", y = "") + theme(plot.title = element_text(hjust = 0.5, size = 35))
# Standard error calculations ========== 
d2 <- filter(d, x > -1 & x < 1 & y > 1.5 & y < 3.5)
d2 <- mutate(d2, SE = sqrt((statistic*(1-statistic))/count))

d3 <- filter(d, x < -1.5 | x > 1.5 | y < .75 | y > 4)
d4 <- filter(d3, statistic > 0)
d5 <- mutate(d4, SE = sqrt((statistic*(1-statistic))/count))
summary(d5$SE)



# POLAR Coords =======================================
#    r  <- sqrt( (px+a)^2 + (pz - b)^2)
# theta <- atan2( (pz - b), (px+a) )

# px = r*cos(theta) - a
# pz = r*sin(theta) + b

# About 18 inches off plate; 0.95 + 1.5 ft
# (-2.45, 5.05) ==> (0,0)
# Use (-2, 3.5) - based on loess max (shenanigans)

# Convert to polar, tranlate origin
hitter <- mutate(hitter,
                 r = sqrt((x + 2)^2 + (y - 3.5)^2),
                 theta = atan2(y - 3.5, x + 2))


# glm() fit ==========================
# doh <- glm(hit ~ px*pz + I(px^2)*I(pz^2),
#            family = binomial, data = hitter )

# hitter2 <- sample_n(hitter, 1000)

mod.polar <- glm(res ~ r*theta + I(r^2)*I(theta^2),
                 family = binomial, data = hitter)

# Points for plotting through the hitting zone
hitzone <- with(hitter,
                cbind(expand.grid(
                  x = seq(min(x), max(x), length = 50),
                  y = seq(min(y), max(y), length = 70)))) %>%
  mutate(r = sqrt( (x + 2)^2 + (y - 3.5)^2),
         theta = atan2(y - 3.5, x + 2) )

preds <- predict(mod.polar, newdata = hitzone, se.fit = TRUE) # logit scale

# source('~/Desktop/ResearchRepo/varyres/R/mapit.R')

inv_logit <- function(x){exp(x)/(1+exp(x))}
hitzone <- with(preds,
                mutate(hitzone, logit = fit,
                       SE_logit = se.fit,
                       phat = inv_logit(logit)
                       )
                )

ggplot(aes(x, y), data = hitzone) +
  geom_tile(data = hitzone, aes(fill = phat)) +
  sz_fcn() +
  coord_equal() + # xlim(-1.5, 1.5) + ylim(1, 4) +
  scale_fill_distiller(palette = "YlOrRd", trans = "reverse", 
                       guide = guide_legend(
                         title = expression(paste(p[GLM]))),
                       limits = c(0.155, 0)) +
                       # guide = FALSE)  
  lab_fcn() +
  ggtitle("GLM Success Probability \n Point Estimate")

# ggsave("Perralta_fit.pdf", height = 8.5, width = 8.5, path = "/Users/ABC/Desktop/ResearchRepo/Images")


# H-L GoF test ===============
# (pg 133 Myers)
# H_0: Well fit
# H_A: Lack of fit

library(ResourceSelection)
d <- hoslem.test(mod.polar$y, fitted(mod.polar)) # p-value = 0.8217

# o_0 <- d$observed
# e_0 <- d$expected

o <- as.data.frame(d$observed)
o2 <- cbind.data.frame(o[1:10,], o[11:20,]) 
o2 <- with(o2, data.frame(k = cutyhat, n0 = o2[,3], n1 = o2[,6]))
o2 <- mutate(o2, p_o = n1/(n0+n1))

e <- as.data.frame(d$expected)
e2 <- cbind.data.frame(e[1:10,], e[11:20,])
e2 <- with(e2, data.frame(k = cutyhat, n0 = e2[,3], n1 = e2[,6]))
e2 <- mutate(e2, p_e = n1/(n0+n1))

pdat <- cbind.data.frame(p_o = o2$p_o, p_e = e2$p_e)

# Plot: p_fitted VS. p_observed =========== #
ggplot(data = pdat, aes(x = p_o, y = p_e)) + geom_point(size = 13) + geom_abline(intercept = 0, slope = 1, size = 2) +   
  lab_fcn(s2 = 20) +
  labs(title = "Success Probabilities: Observed vs. Fitted ", 
       x = expression(p[obs]), y = expression(p[GLM]))

ggsave("ppredVSpobs.jpg", height = 8.5, width = 8.5, path = "/Users/ABC/Desktop/ResearchRepo/Images")

# Plot: ``Successes: Observed VS. Expected'' =========== #
dat <- rbind.data.frame(o, e)
ones <- filter(dat, Var2 == "y1" | Var2 == "yhat1")
ones$cutyhat <- rep(1:10, 2)
names(ones)[1:2] <- c("k", "Successes")

ggplot(data = ones, aes(x = k, y = Freq, color = Successes)) + 
  geom_point(size = 10) + lab_fcn(s2=20) +
  ggtitle("Successes: Observed vs Expected") +
  scale_color_discrete(labels=c("Observed", "Expected"))

ggsave("OvE.jpg", height = 8.5, width = 8.5, path = "/Users/ABC/Desktop/ResearchRepo/Images/")


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



