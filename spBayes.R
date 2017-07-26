# spBayes

library(ggplot2)
library(dplyr)
library(fields)
library(reshape)
library(gridExtra)
library("spBayes")

?spGLM
load("~/Desktop/ResearchRepo/varyres/data/hitter.rda")

# glm() fit ========================== 
mod.polar <- glm(hit ~ r*theta + I(r^2)*I(theta^2), 
                 family = binomial, data = hitter)

# Add covariates to data frame ====
hitter <- mutate(hitter,
                 r = sqrt( (px + 2)^2 + (pz - 3.5)^2), 
                 theta = atan2(pz - 3.5, px + 2),
                 r2 = r^2, theta2 = theta^2,
                 r_theta = r*theta, 
                 r2_theta2 = r2*theta2) 

hitter <- sample_n(hitter, size = 5000) # Random subsample

# Fit Predictive Process Model with spGLM() =================
hitter <- hitter2
covs <- as.matrix(
  cbind(rep(1, dim(hitter)[1]), 
        select(hitter, r, theta, r2, theta2, r_theta, r2_theta2)
        ))
beta <- as.matrix(coefficients(mod.polar))
XB <- covs %*% beta

# From Andy's spatial example
beta.starting <- coefficients(mod.polar) # just one
beta.tuning <- t(chol(vcov(mod.polar))) 
    # t(chol(Variance(beta.hat))

n.batch <- 250
batch.length <- 150
n.samples <- n.batch*batch.length

knots <- vr[[5]][,c("x","y")]
# ggplot(data = ABCE, aes(x = px, y = pz)) + geom_point() + coord_equal()

m1 <- spGLM(res ~ r + theta + 
              r2 + theta2 + 
              r_theta + r2_theta2, 
             data = hitter, family="binomial", 
             coords=with(hitter, cbind(x, y)), 
            # modified.pp = TRUE,
             knots = as.matrix(knots),
             weights=rep(1, nrow(hitter)), 
             n.samples = n.samples,
             starting=list("beta"=rep(0, 7), 
                           "phi"=0.06,
                           "sigma.sq"=0.5, 
                            "w"=0),
             priors=list("beta.Normal"= 
                           list(beta.starting, rep(3,7)), 
                         "phi.Unif"=c(0.01, 1), 
                         "sigma.sq.IG"=c(3, 1)),
            amcmc=list("n.batch"=n.batch, 
                       "batch.length"=batch.length, 
                       "accept.rate"=0.43),
            tuning=list("beta"= beta.tuning,  
                        "phi"=0.05, 
                        "sigma.sq"=0.5, 
                        "w"=0.5),
             cov.model="exponential", 
             verbose=TRUE, n.report=100)

# run.time = 2.8 mins, n=300, knots = 97, cutoff = 200, 10K samples
# run.time = 3.8 mins, n=500, knots = 97, n_b < 200, 10K samples
# run.time = 6.7 mins, n=1000, knots = 97, n_b < 200, 10K samples
# run.time = 19.5 mins, n = 1000, kn = 97, n_b<200, 30K samples
# 7 mins, n = 1000, knots = 49, 30K samples
# 33 mins, n = 3000, kn = 49, 50K samples
# 54 mins, n = 3000, kn = 49, 80K samples
m1$run.time

round(mod.polar$coefficients, 2)
summary(mod.polar)

samps <- as.data.frame(m1$p.beta.theta.samples)
summary(samps$r)
round(apply(samps, 2, median), 2)
library("reshape2")
samps <-  cbind.data.frame(rep(1:n.samples, 9), melt(samps)) 
samps <- filter(samps, variable != "XB")
names(samps) <- c("iter", "param", "value")
print(summary(window(m1$p.beta.theta.samples, start=1)))

# ggplot(data = samps, aes(x = iter, y = value)) + 
#   geom_line() + 
#   facet_wrap(~param, scales = "free")

# Comparative Plots =========================

# Variable-Resolution =================== #
vr2 <- varyres(hitter[,1:4], cutoff = 200)

g1 <- mapit(vr2[[4]]) + spec_fcn() + lab_fcn()
mapit() + spec_fcn() + lab_fcn() + 
  + xlim(-1.5, 1.5) + ylim(1, 4) +
  ggtitle("Empirical Variable-Resolution")

# GLM ======================= #

# Hitting zone points for plotting
hitzone <- with(hitter, cbind(expand.grid(x = seq(min(x), max(x), length = 50), y = seq(min(y), max(y), length = 70)))) %>% 
  mutate(r = sqrt( (x + 2)^2 + (y - 3.5)^2), theta = atan2(y - 3.5, x + 2) )

preds <- predict(mod.polar, newdata = hitzone, se.fit = TRUE)

# source('~/Desktop/ResearchRepo/varyres/R/mapit.R')

inv_logit <- function(x){exp(x)/(1+exp(x))}
hitzone <- with(preds, mutate(hitzone, logit = fit, SE_logit = se.fit, phat = inv_logit(logit)))

g2 <- ggplot(aes(x, y), data = hitzone) +
  geom_tile(data = hitzone, aes(fill = phat)) + 
  sz_fcn() +
  coord_equal() + xlim(-1.5, 1.5) + ylim(1, 4) +
  spec_fcn() + lab_fcn() + 
  ggtitle("Generalized Linear Model")

# PPM ===================== #

hitzone2 <- with(hitter, cbind(expand.grid(x = seq(min(x), max(x), length = 50), y = seq(min(y), max(y), length = 70)))) %>% 
  mutate(r = sqrt( (x + 2)^2 + (y - 3.5)^2), theta = atan2(y - 3.5, x + 2) )

inv_logit <- function(x){exp(x)/(1+exp(x))}

hitzone2 <- mutate(hitzone2, int = rep(1, dim(hitzone2)[1]), r2 = r^2, theta2 = theta^2, r_theta = r*theta, r2_theta2 = r2*theta2)

PPMcovs <- as.matrix(select(hitzone2, int, r, theta, r2, theta2, r_theta, r2_theta2))
# samps <- as.data.frame(m1$p.beta.theta.samples)
PPMbeta <- as.matrix(apply(samps, 2, median)[1:7])
PPM_XB <- PPMcovs %*% PPMbeta
PPM_phat <- inv_logit(PPM_XB)

g3 <- ggplot(aes(x, y), data = hitzone2) +
  geom_tile(data = hitzone2, aes(fill = PPM_phat)) + 
  sz_fcn() +
  coord_equal() + xlim(-1.5, 1.5) + ylim(1, 4) +
  spec_fcn() + lab_fcn() + 
  ggtitle("Predictive Process Model")

g3

g <- grid.arrange(g1, g2, g3, ncol = 3)
ggsave("/Users/ABC/Desktop/ResearchRepo/Images/VR_GLM_PPM.jpg", g, width = 25.5, height = 8.5)
dev.off()

# Save the n = 1000 Peralta dataset in thesis
write.csv(hitter, "/Users/ABC/Desktop/ResearchRepo/Images/Peralta_n1000.csv")


# Fix beta coefficients===================================

m1 <- spGLM(hit ~ XB - 1, 
            data = hitter, family="binomial", 
            coords=with(hitter, cbind(px, pz)), 
            # modified.pp = TRUE,
            knots = as.matrix(knots),
            weights=rep(1, nrow(hitter)), 
            n.samples = n.samples,
            starting=list(beta = 1,
                          "phi"=0.06,
                          "sigma.sq"=0.5, 
                          "w"=0),
            priors=list(# beta = 1,
                        "phi.Unif"=c(0.01, 1), 
                        "sigma.sq.IG"=c(3, 1)),
            amcmc=list("n.batch"=n.batch, 
                       "batch.length"=batch.length, 
                       "accept.rate"=0.43),
            tuning=list("beta"= 0,  
                        "phi"=0.05, 
                        "sigma.sq"=0.5, 
                        "w"=0.5),
            cov.model="exponential", 
            verbose=TRUE, n.report=100)

m1$run.time
# N = 3000, 37500 iterations, 13 mins

# spBayes Spatial binomial example ================

# Generate binary data ==== #
coords <- as.matrix(expand.grid(seq(0,100,length.out=8), seq(0,100,length.out=8)))
n <- nrow(coords)

phi <- 3/50
sigma.sq <- 2

Eucl <- as.matrix(dist(coords))
R <- sigma.sq*exp(-phi*as.matrix(dist(coords))) # Exponential cov. matrix
# install.packages("mgcv")
library(mgcv)
w <- rmvn(1, rep(0,n), R) # generate random MVN

x <- as.matrix(rep(1,n))
beta <- 0.1
p <- 1/(1+exp(-(x%*%beta+w))) # inverse logit; "true" p values

weights <- rep(1, n)
weights[coords[,1]>mean(coords[,1])] <- 10 # lots of obs on right

# simulate i=1,...,64 binomial(weight[i], p[i]), "true observations"
y <- rbinom(n, size=weights, prob=p) 

# ====================Collect samples ======================== #

# logit(p.hat) = beta, weighted by num. obs; just weighted grand mean
fit <- glm((y/weights)~x-1, weights=weights, family="binomial")
beta.starting <- coefficients(fit) # just one
beta.tuning <- t(chol(vcov(fit))) # t(chol(Variance(beta.hat))

n.batch <- 200
batch.length <- 50
n.samples <- n.batch*batch.length

# ============= Plot gosh darn Inverse-Gamma(2,1) =========== #
    # install.packages("pscl"); library("pscl")
ig.x <- seq(0.0001, 3, length = 100) # x values
ig <- function(x) densigamma(x, alpha = 3, beta = 1) # I-G function
d.ig <- 0 # create vector
for(i in 1:100) d.ig[i] <- ig(ig.x[i]) # fill 'er up
dat <- as.data.frame(matrix(c(ig.x, d.ig), ncol = 2)) # data frame
ggplot(data = dat, aes(x = V1, y = V2)) + geom_line() # plot
# ========================================================== #

# ==================== Fit Model ==================== #

m.1 <- spGLM(y~1, family="binomial", coords=coords, weights=weights, 
             starting=list("beta"=beta.starting, 
                           "phi"=0.06,
                           "sigma.sq"=1, "w"=0),
             tuning=list("beta"=beta.tuning, 
                         "phi"=0.5, 
                         "sigma.sq"=0.5, 
                         "w"=0.5),
             priors=list("beta.Normal"=list(0,10), 
                         "phi.Unif"=c(0.03, 0.3), 
                         "sigma.sq.IG"=c(2, 1)),
             amcmc=list("n.batch"=n.batch, 
                        "batch.length"=batch.length, 
                        "accept.rate"=0.43),
             cov.model="exponential", 
             verbose=TRUE, n.report=10)

names(m.1)

burn.in <- 0.9*n.samples # burn in (to throw away)
sub.samps <- burn.in:n.samples # Only use final 1000

print(summary(window(m.1$p.beta.theta.samples, start=burn.in)))

beta.hat <- m.1$p.beta.theta.samples[sub.samps,"(Intercept)"]
w.hat <- m.1$p.w.samples[,sub.samps]

p.hat <- 1/(1+exp(-(x%*%beta.hat+w.hat))) # draws

# function() - simulate trial for each of the 64 locations
# apply() - do that for each column
y.hat <- apply(p.hat, 2, function(x){rbinom(n, size=weights, prob=p)}) 

y.hat.mu <- apply(y.hat, 1, mean) # mean of each row (64 location means)
y.hat.var <- apply(y.hat, 1, var)

