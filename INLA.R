# R INLA

install.packages("INLA", repos="https://www.math.ntnu.no/inla/R/stable")

# install.packages("INLA", repos="https://www.math.ntnu.no/inla/R/testing")

(inla.upgrade(testing=TRUE))

# Example ===================== =======================

hitter <- read.csv("Data/hitter.csv")
hitter <- sample_n(hitter, size = 500) # Random subsample

# glm() fit ========================== 


# Add covariates to data frame 
hitter <- mutate(hitter, r = sqrt( (px + 2)^2 + (pz - 3.5)^2), 
                 theta = atan2(pz - 3.5, px + 2),
                 r2 = r^2, theta2 = theta^2,
                 r_theta = r*theta, r2_theta2 = r2*theta2) 

mod.polar <- glm(hit ~ r*theta + I(r^2)*I(theta^2), 
                 family = binomial, data = hitter)

beta <- coefficients(mod.polar)
eta <- mod.polar$linear.predictors # = X*beta

X <- select(hitter, r, theta, r2, theta2, r_theta, r2_theta2) # dsgn mtx
X <- cbind(int = rep(1,dim(hitter)[1]), X) # add intercept
eta2 <- as.matrix(X) %*% beta # same as eta


install.packages("geoR")
library(geoR)
?grf
Mat_GRF <- grf(dim(hitter)[1], 
               grid = hitter[,c("px","pz")], 
               cov.pars = c(1, 0.25),
               kappa = 0.5, # my "nu" parameter
               nugget = 0)

Mat_GRF_df <- as.data.frame(Mat_GRF)
ggplot(data = Mat_GRF_df, aes(x = px, y = pz, color = data)) + geom_point(size = 3) +   coord_equal() + 
  scale_color_gradientn(colours = rainbow(5))

n = dim(hitter)[1]
data = with(Mat_GRF_df, list(y = data, obs = rep(1, dim(hitter)[1])))
formula = y ~ f(obs, model = "matern2d", nu = 1, ncol = 2, nrow = dim(hitter)[1])
result = inla(formula, family = "gaussian", data = data, verbose = FALSE)
summary(result) # It's alive. It's alive!

# ============= ========== ==========

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

weights <- rep(1, n) # number of obse per location vector
weights[coords[,1]>mean(coords[,1])] <- 10 # many obs on right

# simulate "OBSERVED", i=1,...,64 of binomial(weight[i], p[i]), 
y <- rbinom(n, size=weights, prob=p) # observed data

# Matern ===================== =======================
# library(geoR)
# Plot Matern
x <- seq(0,3, length = 1000)
Mat <- matern(x, kappa = 1, phi = 0.25) # kappa = nu (smthnss), phi = range
dat <- cbind.data.frame(x, Mat)
ggplot(data = dat, aes(x = x, y = Mat)) + geom_line()

?inla


# Example ======
data(Seeds)
formula = r ~ x1*x2+f(plate, model="iid")
mod.seeds = inla(formula,data=Seeds,family="binomial",Ntrials=n)
summary(mod.seeds)
