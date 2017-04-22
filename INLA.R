# R INLA

install.packages("INLA", repos="https://www.math.ntnu.no/inla/R/stable")

# install.packages("INLA", repos="https://www.math.ntnu.no/inla/R/testing")

(inla.upgrade(testing=TRUE))
library(INLA)

# Tokyo Example ==========

data("Tokyo")
Tokyo

# (1) Mesh
knots <- seq(1, 367, length = 25)
mesh <- inla.mesh.1d(knots, interval = c(1, 367), degree = 2,  boundary = "cyclic")
sigma0 <- 1
kappa0 <- 1e-3
tau0 <- 1/(4 * kappa0^3 * sigma0^2)^0.5

# (2) A
A <- inla.spde.make.A(mesh, loc = Tokyo$time)

# (3) SPDE, Matern GMRF model
spde <- inla.spde2.matern(mesh, constr = FALSE, 
                          B.tau = cbind(log(tau0), 1), 
                          B.kappa = cbind(log(kappa0), 0), 
                          theta.prior.prec = 1e-4)


# (4) Organize
time.index <- inla.spde.make.index("time", n.spde = spde$n.spde)
stack <- inla.stack(data = list(y = Tokyo$y, 
                                link = 1, 
                                Ntrials = Tokyo$n), 
                    A = list(A), 
                    effects = list(time.index), 
                    tag = "est") # Organize

# (5) Model statement
formula <- y ~ -1 + f(time, model = spde) # SPDE GMRF model

# (6) INLA
data <- inla.stack.data(stack)
result <- inla(formula, family = "binomial", data = data, Ntrials = data$Ntrials, control.predictor = list(A = inla.stack.A(stack), link = data$link, compute = TRUE))

# Plot
time <- 1:366
index <- inla.stack.index(stack, "est")$data
plot(Tokyo$time, Tokyo$y / Tokyo$n, xlab = "Day", ylab = "Probability")
lines(time, result$summary.fitted.values$mean[index])
lines(time, result$summary.fitted.values$"0.025quant"[index], lty = 2)
lines(time, result$summary.fitted.values$"0.975quant"[index], lty = 2)

# Plot Matern ============================================
library(geoR)
library(ggplot2)

x <- seq(0,3, length = 1000)
Mat <- matern(x, 
              kappa = 1, # k = nu = 1 - by assumption (R-INLA)
              phi = 0.25) # phi = range parameter
dat <- cbind.data.frame(x, Mat)
ggplot(data = dat, aes(x = x, y = Mat)) + geom_line()

?inla

# Hitter Tests - Simulate & SPDE-INLA Fit ======
library(dplyr)

hitter <- read.csv("Data/hitter.csv")
n_sub <- 500 # size of subsample
hitter <- sample_n(hitter, size = n_sub) # Random subsample

hitter <- mutate(hitter, r = sqrt( (px + 2)^2 + (pz - 3.5)^2), 
                 theta = atan2(pz - 3.5, px + 2), 
                 r2 = r^2, theta2 = theta^2,
                 r_theta = r*theta, r2_theta2 = r2*theta2) 

hitter$obs <- 1:dim(hitter)[1]

# Fit glm() 
mod.polar <- glm(hit ~ r*theta + I(r^2)*I(theta^2), 
                 family = binomial, data = hitter)
summary(mod.polar$linear.predictors)
# beta <- coefficients(mod.polar)
# hitter$eta1 <- mod.polar$linear.predictors # = X*beta

library(geoR)
library(ggplot2)

# Matern, cov.spatial(...)
# ρ(h) = (1/(2^(κ-1) * Γ(κ))) * ((h/φ)^κ) * K_{κ}(h/φ)
Mat_GRF <- grf(n_sub, # number of pts simulate
               grid = hitter[,c("px","pz")], # simulate data coords
               cov.model = "matern",
               cov.pars = c(16, 0.25), # sigma^2, phi = range 
               k = 1, # smoothness; (R-INLA: nu = 1)
               nugget = 0) # non-spatial error
               # RF = FALSE) 

# method	= simulation method with options for "cholesky", "svd", "eigen", "RF". Defaults to the Cholesky decomposition.

# RF - logical, with defaults to TRUE, indicating whether the algorithm should try to use the function GaussRF from the package RandomFields in case of method is missing and the number of points is greater than 500.

mean(Mat_GRF$data); sd(Mat_GRF$data)

# Picture
Mat_GRF_df <- as.data.frame(Mat_GRF)
ggplot(data = Mat_GRF_df, 
       aes(x = px, y = pz, color = data)) + 
  geom_point(size = 3) +   
  coord_equal() + 
  scale_color_gradientn(colours = rainbow(5))
       
hitter$Z = Mat_GRF_df[,"data"]
hitter$p = with(hitter, 1/(1 + exp(-Z)))
hitter$hit_sim <- with(hitter, rbinom(n_sub, 100000, p)) # simulated data

# (1) Mesh
points <- as.matrix(select(hitter, px, pz)) # observed locations
mesh <- inla.mesh.2d(loc = points, 
                     cutoff = 0.1, 
                     offset = c(0.3, 0.3), 
                     max.edge = c(0.1, .5)) # Mesh covering

# plot(mesh)
# plot(mesh, rgl = TRUE) 

# (2) A matrix
A <- inla.spde.make.A(mesh, loc = points) 

# (3) SPDE, Matern GMRF model
sigma0 <- 4
size <- min(c(diff(range(mesh$loc[, 1])), 
              diff(range(mesh$loc[, 2]))))
range0 <- 0.25 # INLA & grf(...) -- same range 
kappa0 <- sqrt(8)/range0
tau0 <- 1/(sqrt(4 * pi) * kappa0 * sigma0)
SPDE <- inla.spde2.matern(mesh, 
                          B.tau = cbind(log(tau0), -1, +1),
                          B.kappa = cbind(log(kappa0), 0, -1), 
                          theta.prior.mean = c(0, 0), 
                          theta.prior.prec = c(0.1, 1))

# (4) Organize

# I don't think I need this???
# From users group: If the location points aren't vertices of the mesh, you need to use the inla.stack()

stack <- inla.stack(...) 

# (5) Model Statment

formula <- hit_sim ~ -1 + f(obs, model = SPDE)

# (6) INLA

# data <- inla.stack.data(stack)
result <- inla(formula, 
               family = "binomial", 
               data = hitter,
               Ntrials = rep(100000, n_sub)
               )
summary(result)

# log(sigma) =  log(sigma0) + theta1
# log(rho)   =  log(rho0)   + theta2

# sigma = sigma0*exp(theta1)
# rho = rho0*exp(theta2)

.25*exp(-1.2)
.25*exp(-.37)

# Hitter SPDE-INLA ============================================
library(dplyr)

hitter <- read.csv("Data/hitter.csv")
hitter <- sample_n(hitter, size = 50) # Random subsample

hitter <- mutate(hitter,
                 r = sqrt( (px + 2)^2 + (pz - 3.5)^2), 
                 theta = atan2(pz - 3.5, px + 2),
                 r2 = r^2, theta2 = theta^2,
                 r_theta = r*theta, 
                 r2_theta2 = r2*theta2) 

hitter$obs <- 1:dim(hitter)[1]

points <- as.matrix(select(hitter, px, pz)) # observed locations

# (1) Mesh
mesh <- inla.mesh.2d(loc = points, 
                     cutoff = 0.1, 
                     offset = c(0.3, 0.3), 
                     max.edge = c(0.1, .5)) # Mesh covering

plot(mesh)
plot(mesh, rgl = TRUE) 

# (2) A matrix
A <- inla.spde.make.A(mesh, loc = points) 

# (3) SPDE, Matern GMRF model
sigma0 <- 1
size <- min(c(diff(range(mesh$loc[, 1])), diff(range(mesh$loc[, 2]))))
range0 <- size/5
kappa0 <- sqrt(8)/range0
tau0 <- 1/(sqrt(4 * pi) * kappa0 * sigma0)
SPDE <- inla.spde2.matern(mesh, 
                          B.tau = cbind(log(tau0), -1, +1),
                          B.kappa = cbind(log(kappa0), 0, -1), 
                          theta.prior.mean = c(0, 0), 
                          theta.prior.prec = c(0.1, 1))

# (4) Organize

# I don't think I need this???

stack <- inla.stack(...) 

# (5) Model Statment

formula <- hit ~ r + theta + r2 + theta2 + f(obs, model = spde)

# (6) INLA

data <- inla.stack.data(stack)
result <- inla(formula, 
               family = "binomial", 
               data = hitter)
summary(result)




# INLA Matern ====================

n=100
a=1
b=1
z = rnorm(n)
eta = a + b*z
Ntrials = sample(c(1,5,10,15), size=n, replace=TRUE) 
prob = exp(eta)/(1 + exp(eta))
y = rbinom(n, size=Ntrials, prob = prob)
data = list(y=y,z=z, obs = 1:n)
formula = y ~ 1 + z + f(obs, model = "matern2d", nrow = 10, ncol = 10)
result = inla(formula, family = "binomial", data = data, Ntrials=Ntrials, verbose = FALSE)
summary(result) # It's alive. It's alive!

# Generate Spatial binomial data ================ 

# Locations
coords <- as.matrix(expand.grid(seq(0,100,length.out=8), 
                                seq(0,100,length.out=8)))

n <- nrow(coords) # number of observations

phi <- 3/50 # Exponential lengh-scale
sigma.sq <- 2 # Exponential scale

Eucl <- as.matrix(dist(coords)) # Euclidean distances
R <- sigma.sq*exp(-phi*as.matrix(dist(coords))) # Exponential cov. matrix

library(mgcv) # install.packages("mgcv")
w <- rmvn(1, rep(0,n), R) # MV rnorm() --> r[mv]norm(1, mean, cov mtx)

x <- as.matrix(rep(1,n)) # intercept vector
beta <- 0.1 # coefficient
p <- 1/(1+exp(-(x%*%beta+w))) # "TRUE" p values; inverse logit

weights <- rep(1, n) # number of obse per location vector
weights[coords[,1]>mean(coords[,1])] <- 10 # many obs on right

# simulate "OBSERVED", i=1,...,64 of binomial(weight[i], p[i]), 
y <- rbinom(n, size=weights, prob=p) # observed data

# Example: seeds  ======
data(Seeds)
formula = r ~ x1*x2+f(plate, model="iid")
mod.seeds = inla(formula,data=Seeds,family="binomial",Ntrials=n)
summary(mod.seeds)
