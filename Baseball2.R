# Nychka - fields ======

library(fields)
Votto <- read.csv("~/Desktop/Research/Baseball/Votto.csv")

# Plain Jane
Votto$hit <- as.numeric(Votto$pitch_result == "In play, no out")
g <- cbind.data.frame(Votto$px, Votto$pz)
hitgrid <- as.image(Votto$hit, g, nx = 30, ny = 30)
image.plot(hitgrid)

# "Taming PITCHf/x Data with XML2R and pitchRx" - Sievert ========

install.packages("RSQLite")
install.packages("dplyr")
install.packages("pitchRx")
library("RSQLite")
library("dplyr")
library("pitchRx")

# INTRODUCING XML2R ======

urls <- makeUrls(start = "2011-06-01", end = "2011-06-01") 
sub("http://gd2.mlb.com/components/game/mlb/", "", head(urls)) 

library(XML2R)
files <- paste0(urls, "/inning/inning_all.xml") 
obs <- XML2Obs(files, url.map = TRUE, quiet = TRUE)
table(names(obs))

obs[1]
obs[c(1, 2500)] # element number 1 and element number 2500, of 6723

# Rename (combine top & bottom of innings)
tmp <- re_name(obs, equiv = c("game//inning//top//atbat",
                              "game//inning//bottom//atbat"), diff.name = "inning_side") 

tmp[grep("game//inning//atbat", names(tmp))][1:2] # returns vector of indices, of elements yielded match

tmp <- re_name(tmp, equiv = c("game//inning//top//atbat//runner",
                              "game//inning//bottom//atbat//runner"), diff.name = "inning_side")

tmp <- re_name(tmp, equiv = c("game//inning//top//action",
                              "game//inning//bottom//action"), diff.name = "inning_side")

tmp <- re_name(tmp, equiv = c("game//inning//top//atbat//po",
                              "game//inning//bottom//atbat//po"), diff.name = "inning_side")

obs2 <- re_name(tmp, equiv = c("game//inning//top//atbat//pitch",
                               "game//inning//bottom//atbat//pitch"), diff.name = "inning_side")

table(names(obs2))

# LINKING OBSERVATIONS === === ===

obs2[grep("^game//inning$", names(obs2))][1:3] # first three observations on "game//inning" level:
obswkey <- add_key(obs2, parent = "game//inning", recycle = "num", key.name = "inning") # add key

obs2[grep("^game//inning//atbat$", names(obs2))][1:3] 
obswkey <- add_key(obswkey, parent = "game//inning//atbat", recycle = "num")

obs2[grep("^game//inning$", names(obs2))][1:3] 
obswkey <- add_key(obswkey, parent = "game//inning", recycle = "next")

# COLLAPSING OBSERVATIONS === === ===

table(names(obswkey)) # before...
tables <- collapse_obs(obswkey) # ...and...
table(names(tables)) # ...after

#As mentioned before, we do not need the 'inning' table
table(names(tables)) # before...
tables <- tables[!grepl("^game//inning$", names(tables))]
table(names(tables)) # ...after
table.names <- c("game", "action", "atbat", "pitch", "po", "runner")
tables <- setNames(tables, table.names)
table(names(tables))
head(tables[["runner"]])
head(tables[["atbat"]])

head(tables[["pitch"]])
dim(tables[["pitch"]])
str(tables[["pitch"]])

names(tables)

# COLLECTING GAMEDAY DATA WITH pitchRx ======

library(pitchRx)
files <- c("inning/inning_all.xml", "inning/inning_hit.xml",
           "miniscoreboard.xml", "players.xml")
dat <- scrape(start = "2011-06-01", end = "2011-06-01", suffix = files)
names(dat) # matches Table 1

# STORING AND QUERYING GAMEDAY DATA === ===

??src_sqlite
db <- src_sqlite("GamedayDB.sqlite3", create = FALSE)
# scrape(start = "2011-06-01", end = "2011-06-01", suffix = "inning/inning_all.xml", connect = db$con)
# scrape(start = "2008-01-01", end = "2016-01-01", suffix = "inning/inning_all.xml", connect = db$con)

library(DBI)
dbSendQuery(db$con, "CREATE INDEX pitcher_index ON atbat(pitcher_name)")
dbSendQuery(db$con, "CREATE INDEX type_index ON pitch(pitch_type)")
dbSendQuery(db$con, "CREATE INDEX date_atbat ON atbat(date)")

dbSendQuery(db$con, 'CREATE INDEX pitch_join ON pitch(gameday_link, num)')
dbSendQuery(db$con, 'CREATE INDEX atbat_join ON atbat(gameday_link, num)')

at.bat <- tbl(db, "atbat") %>%
  filter(pitcher_name %in% c("Mariano Rivera", "Phil Hughes")) # representation of data

fbs <- tbl(db, "pitch") %>%
  filter(pitch_type %in% c("FF", "FC")) # representation of data

pitches <- inner_join(fbs, at.bat) %>%
  filter(date >= "2011_01_01" & date <= "2012_01_01") %>%
  collect()

# VISUALIZING PITCHf/x =========

# STRIKE ZONE PLOTS # ================================== # 

pitch <- tbl(db, "pitch") %>%
  filter(des %in% c("Called Strike", "Ball")) %>%
  # Keep pitch location, descriptions
  select(px, pz, des, gameday_link, num) %>%
  # 0-1 indicator of strike/ball
  mutate(strike = as.numeric(des == "Called Strike"))

atbat <- tbl(db, "atbat") %>%
  # Select variables to be used later as covariates in probabilistic models
  select(b_height, p_throws, stand, inning_side, date, gameday_link, num)

decisions <- inner_join(pitch, atbat) %>%
  filter(date <= "2014_01_01") %>%
  collect()

# DENSITY PLOTS # ================================== 

# strikeFX uses the stand variable to calculate strike-zones
# Here is a slick way to create better facet titles without changing data values
relabel <- function(variable, value) {
  value <- sub("^R$", "Right-Handed Batter", value)
  sub("^L$", "Left-Handed Batter", value)
}

strikes <- subset(decisions, strike == 1)
strikeFX(data.frame(strikes), geom = "raster", layer = facet_grid(. ~ stand, labeller = relabel))

strikeFX(data.frame(decisions), geom = "raster", density1 = list(des = "Called Strike"), density2 = list(des = "Called Strike")) + facet_grid(. ~ stand, labeller = relabel)

strikeFX(data.frame(decisions), geom = "raster", density1 = list(des = "Called Strike"), density2 = list(des = "Ball"), layer = facet_grid(. ~ stand, labeller = relabel))

# PROBABILISTIC PLOTS # ================================== 

library(parallel)
cl <- makeCluster(detectCores() - 1)
library(mgcv)
m <- bam(strike ~ interaction(stand, p_throws, inning_side) +
           s(px, pz, by = interaction(stand, p_throws, inning_side)),
         data = decisions, family = binomial(link = 'logit'), cluster = cl)

# Baseball Savant - Votto =========

setwd("~/Desktop/Research/Baseball")
Votto <- read.csv("~/Desktop/Research/Baseball/Votto.csv")

table(Votto$pitch_result)
names(Votto)
(788 + 1735)/(1579 + 87 + 788 + 1735) 
str(Votto$pitch_result)

Votto$Bernoulli <- ifelse(Votto$pitch_result == "In play, no out" |
                            Votto$pitch_result == "In play, out(s)", 1, 0)
mean(Votto$Bernoulli)

# px: the left/right distance, in feet, of the pitch from the middle of the plate as it crossed home plate. The PITCHf/x coordinate system is oriented to the catcher’s/umpire’s perspective, with distances to the right being positive and to the left being negative.

# pz: the height of the pitch in feet as it crossed the front of home plate.

library(ggplot2)
qplot(px, pz, data = Votto)

sum(is.na(Votto$px))
Votto <- Votto[is.na(Votto$pz) == FALSE,]
sum(is.na(Votto$pz))

# Plot of some description =====
kZone <- data.frame(x = c(-0.95, -0.95, 0.95, 0.95, -0.95), 
                    y = c(1.6, 3.5, 3.5, 1.6, 1.6))

ggplot(ABC.R, aes(Horizontal, Vertical, fill = Hitting)) + 
  geom_tile() + coord_equal() + 
  xlim(-1.5, 1.5) + ylim(1, 4) + 
  scale_fill_distiller(palette = "Spectral", 
                       limits = c(.17, 0), trans = "reverse",
                       guide = guide_legend(title = expression(hat(p)[b]))) +
  geom_path(aes(x, y, fill=NULL), data = kZone, 
            lwd = 1.5, col = "blue", linetype = 2) + 
  ggtitle("Empirical \n Success Probability") +
  xlab("Feet from \n Middle of Home Plate") +
  ylab("Feet Off Ground") +
  theme(legend.key.size = unit(2, "cm"), 
        legend.title = element_text(size = 40),
        legend.text = element_text(size = 30),
        legend.title.align = 0.25,
        axis.title.x = element_text(size=28),
        axis.title.y = element_text(size=28),
        title = element_text(size = 28),
        axis.text = element_text(size = 28)) 

ggsave("Mothership.pdf", height = 8.5, width = 8.5)

#   geom_point(aes(Horizontal, Vertical, fill = NULL), 
#              data = max2[-29,], size = 2.5) +
#   # Get rid of pesky outlier max at (1.06, 2.89)
#   geom_smooth(aes(Horizontal, Vertical), data = max2[-29,],
#               method = "loess", se = FALSE, lwd = 1.2,
#               span = 0.8, color = "red") 

# Perspective plot attempt ======

x <- with(ABC.R, unique(Horizontal))
y <- with(ABC.R, unique(Vertical))
z <- with(ABC.R, matrix(Hitting, nrow =  length(x), ncol = length(y)))
z[z > .17] <- NA

persp(x, y, z, phi = 45, theta = -30,
      xlab = "", ylab = "",
      main = "") # phi = aerial

# Beta priors ========
library(reshape2)

x <- seq(0, 1, length = 1000)
y <- dbeta(x, 2, 8)
y2 <- dbeta(x, 10, 40)
y3 <- dbeta(x, 50, 200)
qplot(x = x, y = y, geom = "line", main = "Prior on P(hit|swing)", xlab = "p", ylab = "") + geom_line(size = 1.8) + ylim(0, 16) + theme_grey(base_size = 18)
ggsave("beta1.pdf", height = 7, width = 8.5)

qplot(x = x, y = y2, geom = "line", main = "Prior on P(hit|swing)", xlab = "p", ylab = "") + geom_line(size = 1.8) + ylim(0, 16) + theme_grey(base_size = 18)
ggsave("beta2.pdf", height = 7, width = 8.5)

qplot(x = x, y = y3, geom = "line", main = "Prior on P(hit|swing)", xlab = "p", ylab = "") + geom_line(size = 1.8) + ylim(0, 16) + theme_grey(base_size = 18)
ggsave("beta3.pdf", height = 7, width = 8.5)

# Test for STAN - Votto STAN ====
Votto <- Votto[1:20,]
N <- dim(Votto)[1] 
px <- Votto$px
pz <- Votto$pz
y <- Votto$Bernoulli

Votto_data <- c("N", "y", "px", "pz")

library("rstan", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")

fit1 <- stan(file="Votto.stan",
             data=Votto_data, iter = 500, chains = 2)

fit2 <- stan(fit=fit1, data=Votto_data, iter=500, chains=4)

print(fit1)

# Votto - Sievert's Strike Zone hit desity ======

# http://cpsievert.github.io/pitchRx/
# "Getting to know pitchRx package"

library("pitchRx")
library("viridis")
table(Votto$pitch_result)
hits <- subset(Votto, pitch_result == "In play, no out")
hits$stand <- rep("L", dim(hits)[1]) 
hits$b_height <- rep("6-3", dim(hits)[1])
strikeFX(hits, geom = "tile") + 
  coord_equal() +
  theme_bw() +
  viridis::scale_fill_viridis() 

# Votto - Sievert probabilistic Strike Zone hit desity ======
library(mgcv)
Votto <- read.csv("~/Desktop/Research/Baseball/Votto.csv")

Votto$hit <- as.numeric(Votto$pitch_result == "In play, no out")

Votto$stand <- as.character(c(rep("L", dim(Votto)[1]-1), "R")) # For some reason, the only plots top half if all rows are left-handed hitters (as Votto is here!)

Votto$b_height <- rep("6-3", dim(Votto)[1])
Votto <- Votto[is.na(Votto$px) == FALSE,]
sum(is.na(Votto$px))

m <- bam(hit ~ s(px, pz, by = factor(stand)), data = Votto, family = binomial(link = 'logit'))

mod <- glm(hit ~ abs(px) + pz + px*pz, family = binomial, data = Votto)

strikeFX(Votto, model = mod) + 
  theme_bw() +
  coord_equal() +
  viridis::scale_fill_viridis(name = "Probability of Hit")

# Baseball Savant - Trout =========

table(Trout$pitch_result)
str(Trout$pitch_result)

Trout$Bernoulli <- ifelse(Trout$pitch_result == "In play, no out" |
                            Trout$pitch_result == "In play, out(s)", 1, 0)
mean(Trout$Bernoulli)

# px: the left/right distance, in feet, of the pitch from the middle of the plate as it crossed home plate. The PITCHf/x coordinate system is oriented to the catcher’s/umpire’s perspective, with distances to the right being positive and to the left being negative.

# pz: the height of the pitch in feet as it crossed the front of home plate.

library(ggplot2)
qplot(px, pz, data = Trout)

sum(is.na(Trout$px))
Trout <- Trout[is.na(Trout$pz) == FALSE,]
sum(is.na(Trout$pz))

# Test for STAN
# Trout <- Trout[1:20,]
N <- dim(Trout)[1] 
px <- Trout$px
pz <- Trout$pz
y <- Trout$Bernoulli

Trout_data <- c("N", "y", "px", "pz")

library("rstan", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")

fit1 <- stan(file="Votto.stan",
             data=Trout_data, iter = 500, chains = 2)

fit2 <- stan(fit=fit1, data=Votto_data, iter=500, chains=4)

print(fit1)

# STAN Schools Example ==================================

# class: stanfit
fit1 <- stan(file="schools.stan",
             data=schools_data, iter = 1000, chains = 4)

print (fit1)
plot (fit1)
traceplot(fit1) # all trace plots

print(fit1, pars=c("beta0", "a", "b"), probs=c(0.025, 0.975))
plot(fit1, pars=c("mu")) 

traceplot(fit1, pars=c("mu", "tau")) # trace plots by specified parameters
extract(fit1, pars=c("mu", "tau")) # the samples

# make a list out of sampling data
s <- extract(fit1, pars=c("mu", "tau"), permuted=TRUE) 
names(s) # [1] "mu"  "tau"
dim(s$mu)
# Then usual functions work
hist(s$mu)
hist(s$tau)
mean(s$tau)
sd(s$tau)

s2 <- extract(fit1, pars = "theta", permuted = FALSE)
dim(s2)
dimnames(s2)

adf <- as.data.frame(fit1) # works on "stanfit" object
names(adf)
adf$mu
str(adf)

# GLM =============================
a <- 2 # Shift two feet to the left
b <- -5 # Shift four feet up
r <- sqrt( (px + a)^2 + (pz + b)^2)
theta <- atan2((pz + b), (px + a))

mod <- glm(y ~ r + theta + r*theta, family = binomial)
summary(mod)
mod$coef

p.hat(3, -pi/4)
r <- seq(0, 5, length = 100)
qplot(r, p.hat(r, -pi/4))

# Messing around ===========================


# Rectangular coordinate origin shift
a <- 2 # Shift two feet to the left
b <- -5 # Shift four feet up

# Convert to polar coordinates
# r <- sqrt( (px + a)^2 + (pz + b)^2)
# theta <- atan2((pz + b), (px + a))

r <- abs(sqrt( (px + a)^2 + (pz + b)^2) - 3) # absolute distance
# from sweet spot (shifted origin)
theta <- abs(atan2((pz + b), (px + a)) + pi/4) # absolute degrees 
# away from zero degree sweet spot

# Just Radius Model
mod <- glm(y ~ r, family = binomial)
summary(mod)
# xb <- function(r){mod$coef[1] + mod$coef[2]*r}

# p.hat <- function(r){exp(xb(r))/(1 + exp(xb(r)))}
p.hat(3)
r2 <- seq(0, 5, length = 100)
qplot(r2, p.hat(r2))

# Just angle model
mod <- glm(y ~ theta + I(theta^2), family = binomial)
summary(mod)
# xb <- function(theta){mod$coef[1] + mod$coef[2]*theta + mod$coef[3]*theta^2}

# p.hat <- function(theta){exp(xb(theta))/(1 + exp(xb(theta)))}
xaxis <- seq(0, 5, length = 100)
qplot(xaxis, p.hat(xaxis)) # recall, 0 degrees is horizontal line at shoulder height,
# negative degrees move down into strike zone


# "All aboard" model ===
mod <- glm(y ~ r*theta*I(theta^2), family = binomial)
summary(mod)
# xb <- function(r, theta){mod$coef[1] + mod$coef[2]*r + mod$coef[3]*theta + mod$coef[4]*theta^2 + mod$coef[5]*r*theta + mod$coef[6]*r*theta^2 + mod$coef[7]*theta*theta^2 + mod$coef[8]*r*theta*theta^2}

# p.hat <- function(r, theta){exp(xb(r, theta))/(1 + exp(xb(r, theta)))}
p.hat(0, 0)
axis <- seq(0, 5, length = 100)
qplot(axis, p.hat(axis, 0)) # p vs. r
qplot(axis, p.hat(0, axis), geom = "line") # p vs. theta

# plots ===
x <- seq(-5, 5, length = 1000)
?inv.logit
library(ggplot2)
qplot(x, inv.logit(x), geom = "line")
ggsave("inverselogit.pdf")
# fx <- function(x){x + x^2 + x^3}

qplot(x, fx(x), geom = "line")

# Non-linear least squares ===
?nls # nonlinear logistic regression examples

# Logistic Regression - Challenger Example
library(mcsm)
data(challenger)
?challenger
qplot(challenger$temp,challenger$oring)
str(factor(challenger$oring))
fit <- glm(challenger$oring ~ challenger$temp, family = binomial)
?glm
x <- seq(0, 100, length = 1000)
# pred <- function(x){1/(1 + exp(15.04 - 0.2322*x))}
qplot(x, 1 - pred(x), geom = "line")

# Analyzing Baseball Data in R =================
# Advanced Graphics - ggplot2, pg. 145
ggplot(data = hits, aes(x = px, y = pz)) + geom_point()

load("~/Desktop/Research/Baseball/balls_strikes_count.RData")
library(ggplot2)
names(cabrera)

# First Layer
p0 <- ggplot(data = cabrera, aes(x = hitx, y = hity))
p1 <- p0 + geom_point()
p1

# Grouping Factors
p0 <- ggplot(data = cabrera, aes(x = hitx, y = hity))
p1 <- p0 + geom_point(aes(color = hit_outcome))
p2 <- p1 + coord_equal()
p2

# Multipanel conditioning (faceting)
p3 <- p2 + facet_wrap(~ season)
p3

# Adding Elements
bases <- data.frame(x = c(0, 90/sqrt(2), 0, -90/sqrt(2), 0),
                    y = c(0, 90/sqrt(2), 2*90/sqrt(2), 90/sqrt(2), 0))
p4 <- p3 + geom_path(aes(x = x, y = y), data = bases)
p4
p4 + geom_segment(x = 0, xend = 300, y = 0, yend = 300) +
  geom_segment(x = 0, xend = -300, y = 0, yend = 300)

# Dealing with cluttered charts
F4verl <- subset(verlander, pitch_type == "FF")
F4verl$gameDay <- as.integer(format(F4verl$gamedate, format = "%j"))

topKzone <- 3.5; botKzone <- 1.6 # top/bottom of strike zone
inKzone <- -0.95; outKzone <- 0.95 # inside/outside strike zone
kZone <- data.frame(x = c(inKzone, inKzone, outKzone, outKzone, inKzone),
                    y = c(botKzone, topKzone, topKzone, botKzone, botKzone))

ggplot(F4verl, aes(px, pz)) +
  geom_point() +
  facet_wrap(~ batter_hand) +
  coord_equal() +
  geom_path(aes(x, y), data = kZone, lwd=2, col = "white")

ggplot(F4verl, aes(px, pz)) +
  stat_binhex() +
  facet_wrap(~ batter_hand) +
  coord_equal() +
  geom_path(aes(x, y), data = kZone, lwd=2, col = "white", alpha = 0.3)

# Carson Sievert - pitchRx =============================
# http://cpsievert.github.io/pitchRx/
# "Getting to know pitchRx package"

# Visualizing Pitch Locations ### ### 
install.packages("pitchRx")
library("pitchRx")
install.packages("viridis")
str(pitches)

strikes <- subset(pitches, des == "Called Strike")
strikeFX(strikes, geom = "tile") + 
  facet_grid(pitcher_name ~ stand) +
  coord_equal() +
  theme_bw() +
  viridis::scale_fill_viridis()

# Probabilistic strike-zone densities ### ###

# Models that estimate the event probabilities conditioned on pitch location provide a better inferential tool than density estimation. Here we use the mgcv package to fit a Generalized Additive Model (GAMs) which estimates the probability of a called strike as a function of pitch location and batter stance.
# **(Could easily replace "called strike" with "hit")

install.packages("pitchRx")
library("pitchRx")
install.packages("viridis")
install.packages("mgcv")
library(mgcv)

noswing <- subset(pitches, des %in% c("Ball", "Called Strike"))
noswing$strike <- as.numeric(noswing$des %in% "Called Strike")

m <- bam(strike ~ s(px, pz, by = factor(stand)) +
           factor(stand), data = noswing, 
         family = binomial(link = 'logit'))


x <- list(
  facet_grid(. ~ stand),
  theme_bw(),
  coord_equal(),
  viridis::scale_fill_viridis(name = "Probability of Called Strike")
)

strikeFX(noswing, model = m, layer = x)

# Faraway Nonparametric Example - pg. 212 =========
library("faraway")

# 11.1 Kernel Estimators - pg. 213 
# (1)
data(exa) 
exa
qplot(x, y, data = exa, main="Example A") + geom_line(aes(x, m), data = exa, color = "blue", size = 2)

# (2)
exb <- as.data.frame(exb)
qplot(x, y, data = exb, main = "Example B") + geom_line(aes(x, m), data = exb, color = "blue", size = 1)

# (3)
data(faithful)
qplot(eruptions, waiting, data = faithful, main = "Old Faithful")

faithful$duration <- faithful$eruptions

# Kernel estimators, with Normal kernel, increasing bandwidth
# BW = 0.1
KE <- ksmooth(x = faithful$duration, y = faithful$waiting, kernel = "normal", bandwidth = 0.1) 
qplot(eruptions, waiting, data = faithful, main = "bandwidth = 0.1") + geom_line(aes(KE$x, KE$y), size = 1, colour = "blue")

# BW = 0.5
KE <- ksmooth(faithful$duration, faithful$waiting, "normal", 0.5) 
qplot(eruptions, waiting, data = faithful, main = "bandwidth = 0.5") + geom_line(aes(KE$x, KE$y), size = 1, colour = "blue")

# BW = 2
KE <- ksmooth(faithful$duration, faithful$waiting, "normal", 2) 
qplot(eruptions, waiting, data = faithful, main = "bandwidth = 2") + geom_line(aes(KE$x, KE$y), size = 1, colour = "blue")

# 11.2 Splines - pg. 217

# (1)
SS <- smooth.spline(faithful$duration, faithful$waiting)
SS <- with(SS, cbind.data.frame(x, y))
qplot(eruptions, waiting, data = faithful, main = "Smoothing Spline") + geom_line(aes(x, y), data = SS, size = 1, colour = "blue")

# (2)
SS <- with(exa, smooth.spline(x, y))
SS <- with(SS, cbind.data.frame(x, y))
qplot(x, y, data = exa, main = "Smoothing Spline") + geom_line(aes(x, y), data = SS, size = 1, colour = "blue") + geom_line(aes(x, m), data = exa, size = 1, colour = "red")

# (3)
SS <- with(exb, smooth.spline(x, y))
SS <- with(SS, cbind.data.frame(x, y))
qplot(x, y, data = exb, main = "Smoothing Spline") + geom_line(aes(x, y), data = SS, size = 1, colour = "blue") + geom_line(aes(x, m), data = exb, size = 1, colour = "red")

# fields - Nychka - from fields.pdf =======

# as.image() - Creates image from irregular x,y,z

# convert precip data to 50X50 image ===

library(fields)
look <- as.image(RMprecip$y, x= RMprecip$x, nx=50, ny=50)
image.plot(look)
# number of obs in each cell -- in this case equal to the
# aggregated weights because each obs had equal wieght in the call
image.plot( look$x ,look$y, look$weights, col=terrain.colors(50))
# hot spot is around Denver

# as.surface() - Creates an "surface" object from grid values. ===

# Make a perspective of the surface Z= X**2 -Y**2
# Do this by evaluating quadratic function on a 25 X 25 grid
grid.l <- list(abcissa = seq(-2,2,,15), ordinate = seq(-2,2,,20))
xg <- make.surface.grid(grid.l)
# xg is a 300X2 matrix that has all pairs of X and Y grid values
z <- xg[,1]**2 - xg[,2]**2 # "**" is "^"
# now fold z in the matrix format needed for persp
out.p <- as.surface(xg, z)
persp(out.p)
# also try plot( out.p) to see the default plot for a surface object

# Adjusted Polar Coordinates - Original Dream Model ============ 
head(sort(hitzone$p, decreasing = TRUE))
filter(hitzone, p > .136)

mod.polar2 <- glm(hit ~ I(r - 3.01)*I(theta + 0.51) +
                    I((r - 3.01)^2)*I((theta + 0.51)^2),
                  family = binomial, data = righties)

mod.polar2 <- glm(hit ~ I(abs(r - 3.01))*I(abs(theta + 0.51)),
                  family = binomial, data = righties) # THIS IS COOL!!!

summary(mod.polar2)

test <- data.frame(r = 3.01, theta = -.51)
predict(mod.polar2, newdata = test, type = "response")

hitzone <- mutate(hitzone, p = predict(mod.polar2, newdata = hitzone, type = "response"))

ggplot(aes(px, pz, fill = p), data = hitzone) + 
  geom_tile() +
  geom_path(aes(x, y, fill = NULL), data = kZone, 
            lwd = 1.5, col = "blue", linetype = 2) + 
  coord_equal() + ggtitle("") + 
  scale_fill_distiller(palette = "Spectral", 
                       limits = c(.17, 0), trans="reverse") +
  xlim(-1.5, 1.5) + ylim(1, 4)

ggsave("Mothership_NCP_GLM.pdf", height = 8.5, width = 8.5)

# Lefties - Williams Resolution=========================

lefties <- pitches %>% filter(stand == "L") %>%
  mutate(hit = as.numeric(des == "In play, no out")) %>%
  filter(abs(px) < 4) # 5 way too big values

# write.csv(lefties, file = "lefties.csv")

mean(lefties$hit)

lefties2 <- lefties %>% filter(abs(px) < 14/12, pz < 4.8 & pz > 1.3) 

lefties2 <- mutate(lefties2, px = -px)

length(unique(lefties2$batter))

coordsR <- with(lefties2, cbind.data.frame(px, pz))
hitgridR <- with(lefties2, as.image(hit, coordsR, nx = 9, ny = 13)) 
ABC.R <- with(hitgridR, cbind(expand.grid(x, y), as.vector(z)))
names(ABC.R) <- c("Horizontal", "Vertical", "Hitting")

kZone <- data.frame(x = c(-0.95, -0.95, 0.95, 0.95, -0.95), 
                    y = c(1.6, 4.5, 4.5, 1.6, 1.6))

ggplot(ABC.R, aes(Horizontal, Vertical, fill = Hitting)) + 
  geom_tile() + 
  xlim(-1.05, 1.05) + ylim(1.4, 4.7) + 
  scale_fill_distiller(palette = "Spectral", limits = c(.17, 0)) + 
  geom_path(aes(x, y, fill=NULL), data = kZone, 
            lwd = 1.5, col = "white") + 
  coord_equal() + ggtitle("") + xlab("") + ylab("")

# ggsave("WilliamsZone.jpeg", height = 10.5, width = 8.5)

# Mike Trout =========

op <- par(mfrow = c(2, 2))

MT <- filter(pitches, batter == 545361, abs(px) < 1.5)

MT <- MT %>% 
  mutate(hit = as.numeric(des == "In play, no out")) 

coordsR <- with(righties, cbind.data.frame(px, pz))
hitgridR <- with(righties, as.image(hit, coordsR, nx = 90, ny = 72)) 
hitgridR <- with(hitgridR, cbind(expand.grid(x, y), as.vector(z)))
names(hitgridR) <- c("Horizontal", "Vertical", "Hitting")

kZone <- data.frame(x = c(-0.95, -0.95, 0.95, 0.95, -0.95), 
                    y = c(1.6, 3.5, 3.5, 1.6, 1.6))

ggplot(hitgridR, aes(Horizontal, Vertical, fill = Hitting)) + 
  geom_tile() + 
  xlim(-1.5, 1.5) + ylim(1, 4) + 
  scale_fill_distiller(palette = "Spectral", 
                       trans="reverse", limits = c(.17,0)) + 
  geom_path(aes(x, y, fill=NULL), data = kZone, 
            lwd = 1.5, col = "blue", linetype = 2) + 
  coord_equal() + ggtitle("P(Hit|Swing)") + xlab("px") + ylab("pz")

# Conforto =========

Conforto <- filter(pitches, batter == 624424)
Conforto <- mutate(Conforto, hit = as.numeric(des == "In play, no out")) 
mean(Conforto$hit)
