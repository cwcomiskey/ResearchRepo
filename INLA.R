# R INLA

install.packages("INLA", repos="https://www.math.ntnu.no/inla/R/stable")

# install.packages("INLA", repos="https://www.math.ntnu.no/inla/R/testing")

(inla.upgrade(testing=TRUE))

n=100
a=1
b=1
z = rnorm(n)
eta = a + b*z
Ntrials = sample(c(1,5,10,15), size=n, replace=TRUE) 
prob = exp(eta)/(1 + exp(eta))
y = rbinom(n, size=Ntrials, prob = prob)
data = list(y=y,z=z)
formula = y ~ 1+z
result = inla(formula, family = "binomial", data = data, Ntrials=Ntrials, verbose = FALSE)
summary(result)

# Example - Spatial binomial data ================

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

weights <- rep(1, n) # number of observations per location vector
weights[coords[,1]>mean(coords[,1])] <- 10 # lots of obs on right

# simulate "OBSERVED", i=1,...,64 of binomial(weight[i], p[i]), 
y <- rbinom(n, size=weights, prob=p) # observed data

