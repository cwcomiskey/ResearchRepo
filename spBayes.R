# spBayes

library(ggplot2)
library(dplyr)
library(fields)
library(rstan)
library(reshape)

install.packages("spBayes")
library("spBayes")

?spGLM


hitter <- read.csv("Data/hitter.csv")

# Grid Loops =============================================
library(fields)

cutoff<- 350

# Loop Zero =====

GrZero <- summarise(hitter, min.x = min(px), max.x = max(px), # x bounds
                    min.y = min(pz), max.y = max(pz), # y bounds 
                    Hitting = mean(hit), Count = dim(hitter)[1], # stats
                    x = (max(px)+min(px))/2, # x center
                    y = (max(pz)+min(pz))/2) # y center

ABCE <- with(GrZero, cbind.data.frame(x, y, round(Hitting, 2), Count))
names(ABCE) <- c("px", "pz", "Hitting", "Count")

# Loop 1 (4) =================================== 

# Add if(){} statemtent here
# if(count > cutoff){Do everything else!!}

ABCE <-  with(GrZero, mutate(ABCE, width = max.x - min.x, height = max.y - min.y))

# Magic step
gridder <- with(hitter, as.image(hit, cbind.data.frame(px, pz), nx = 2, ny = 2)) 

# Manually add (bounds) and correct (centers)
gridder$xbb <- with(GrZero, seq(min.x, max.x, , 5))[c(1,3,5)] # x box boundaries
gridder$ybb <- with(GrZero, seq(min.y, max.y, , 5))[c(1,3,5)] # y box boundaries 
gridder$x <- with(GrZero, seq(min.x, max.x, , 5))[c(2,4)] # x box centers
gridder$y <- with(GrZero, seq(min.y, max.y, , 5))[c(2,4)] # y box centers

# Create important-stuff matrix: box centers, BA, counts
ABCE <- with(gridder, cbind(expand.grid(x, y), as.vector(z), as.vector(weights)))

names(ABCE) <- c("px", "pz", "Hitting", "Count")

# Loop 2 (16) =================================== 

gridder$bw <- gridder$xbb[2] - gridder$xbb[1] # box width
gridder$bh <- gridder$ybb[2] - gridder$ybb[1] # box height

for(i in 1:2){
  for(j in 1:2){
    if(gridder$weights[i,j] > cutoff){
      
      # Filter original data
      Box_ij <- with(gridder, filter(hitter, 
                                     px >= xbb[i] & px < xbb[i+1], 
                                     pz >= ybb[j] & pz < ybb[j+1]))
      
      # subdivided box centers (sbc)
      sbc.x <- with(gridder, seq(xbb[i], xbb[i+1], , 5)[c(2,4)])
      sbc.y <- with(gridder, seq(ybb[j], ybb[j+1], , 5)[c(2,4)])
      
      gridder_ij <- with(Box_ij, as.image(hit, cbind.data.frame(px, pz), nx = 2, ny =2, grid = list(x = sbc.x, y = sbc.y))) 
      
      ABCE_Box_ij <- with(gridder_ij, cbind(expand.grid(x, y), as.vector(z), as.vector(weights)))
      
      names(ABCE_Box_ij) <- c("px", "pz", "Hitting", "Count")
      
      # ABCE: Remove old box (i,j), add new boxes ======== #
      ABCE <- with(gridder, filter(ABCE, !(px == x[i] & pz == y[j])))
      ABCE <- rbind.data.frame(ABCE, ABCE_Box_ij)
      
    }
  }
}

# Solve: 2*2 - x + 4*x = dim(ABCE)[1]
sdb <- (dim(ABCE)[1] - 4)/3 # number of subdivided boxes

# New box dimensions for ggplot() and geom_tile()
widths <- with(gridder, c(rep(bw, 4 - sdb), rep(bw/2, 4*sdb))) 
heights <- with(gridder, c(rep(bh, 4 - sdb), rep(bh/2, 4*sdb))) 
ABCE <- cbind(ABCE, heights, widths)

# Loop 3 (64) ======================================== 

dim(ABCE)[1] # [1] 16

LoopData <- data.frame()

for(r in 1:dim(ABCE)[1]){ # Iterate through rows (boxes) of ABCE
  
  if(ABCE$Count[r] > cutoff){ 
    
    # Divided Box Bounds Lower/Upper
    Dbbl.x <- ABCE[r,"px"] - ABCE[r, "widths"]/2 
    Dbbu.x <- ABCE[r,"px"] + ABCE[r, "widths"]/2
    Dbbl.y <- ABCE[r, "pz"] - ABCE[r, "heights"]/2
    Dbbu.y <- ABCE[r, "pz"] + ABCE[r, "heights"]/2
    
    # Filter original data
    Box_r <- with(ABCE, filter(hitter, 
                               px >=  Dbbl.x & px <= Dbbu.x,
                               pz >=  Dbbl.y & pz <= Dbbu.y))
    
    # Divided box centers
    Dbc.x <- with(ABCE, seq(Dbbl.x, Dbbu.x, , 5)[c(2,4)])
    Dbc.y <- with(ABCE, seq(Dbbl.y, Dbbu.y, , 5)[c(2,4)])
    
    # Griddify sub_sub_box
    gridder_r <- with(Box_r, 
                      as.image(hit, 
                               cbind.data.frame(px, pz), 
                               nx=2, ny=2, 
                               grid=list(x = Dbc.x, y=Dbc.y)
                      )
    )
    
    # For ABCE, for book keeping, for plotting
    heights <- rep(ABCE[r,5]/2, 4)
    widths <- rep(ABCE[r,6]/2, 4)
    
    # Important-stuff data frame
    ABCE_Box_r <- with(gridder_r, 
                       cbind(expand.grid(x,y), 
                             as.vector(z), 
                             as.vector(weights), 
                             heights, 
                             widths))
    
    names(ABCE_Box_r) <- c("px", "pz", "Hitting", "Count", "heights", "widths")
    
    LoopData <- rbind.data.frame(LoopData, ABCE_Box_r)
  }
}

# Boxes subdivided 
sdb <- sum(as.numeric(ABCE$Count > cutoff))
# Remove subdivided, combine with new
ABCE <- rbind.data.frame(filter(ABCE, Count <= cutoff), LoopData)

ggplot(ABCE, aes(px, pz, fill=Hitting)) + 
  with(ABCE, geom_tile(width = widths, height = heights)) + 
  coord_equal() + 
  scale_fill_distiller(palette = "Spectral", trans="reverse") +
  geom_text(aes(fill = Hitting, # print counts
                label = Count), size = 3.5)

# Loop 4 (256) ====================================== 

dim(ABCE)[1] # [1] 49 (of 64 possible)

LoopData <- data.frame()

for(r in 1:dim(ABCE)[1]){ # Iterate through rows (boxes) of ABCE
  
  if(ABCE$Count[r] > cutoff){ 
    
    # Divided Box Bounds Lower/Upper
    Dbbl.x <- ABCE[r,"px"] - ABCE[r, "widths"]/2 
    Dbbu.x <- ABCE[r,"px"] + ABCE[r, "widths"]/2
    Dbbl.y <- ABCE[r, "pz"] - ABCE[r, "heights"]/2
    Dbbu.y <- ABCE[r, "pz"] + ABCE[r, "heights"]/2
    
    # Filter original data
    Box_r <- with(ABCE, filter(hitter, 
                               px >=  Dbbl.x & px <= Dbbu.x,
                               pz >=  Dbbl.y & pz <= Dbbu.y))
    
    # Divided box centers
    Dbc.x <- with(ABCE, seq(Dbbl.x, Dbbu.x, , 5)[c(2,4)])
    Dbc.y <- with(ABCE, seq(Dbbl.y, Dbbu.y, , 5)[c(2,4)])
    
    # Griddify sub_sub_box
    gridder_r <- with(Box_r, 
                      as.image(hit, 
                               cbind.data.frame(px, pz), 
                               nx=2, ny=2, 
                               grid=list(x = Dbc.x, y=Dbc.y)
                      ))
    
    # For ABCE, for book keeping, for plotting
    heights <- rep(ABCE[r,5]/2, 4)
    widths <- rep(ABCE[r,6]/2, 4)
    
    # Important-stuff data frame
    ABCE_Box_r <- with(gridder_r, 
                       cbind(expand.grid(x,y), 
                             as.vector(z), 
                             as.vector(weights), 
                             heights, 
                             widths))
    
    names(ABCE_Box_r) <- c("px", "pz", "Hitting", "Count", "heights", "widths")
    
    LoopData <- rbind.data.frame(LoopData, ABCE_Box_r)
  }
}

sum(as.numeric(ABCE$Count > cutoff)) # number boxes subdivided
# Remove subdivided, combine with new
ABCE <- rbind.data.frame(filter(ABCE, Count <= cutoff), LoopData)

ggplot(ABCE, aes(px, pz, fill=Hitting)) + 
  with(ABCE, geom_tile(width = widths, height = heights)) + 
  coord_equal() + 
  scale_fill_distiller(palette = "Spectral", trans="reverse") +
  geom_text(aes(fill = Hitting, # print counts
                label = Count), size = 3.5)

# Loop 5 (4^5) ===========================================

dim(ABCE)[1] 

LoopData <- data.frame()

for(r in 1:dim(ABCE)[1]){ # Iterate through rows (boxes) of ABCE
  
  if(ABCE$Count[r] > cutoff){ 
    
    # Divided Box Bounds Lower/Upper
    Dbbl.x <- ABCE[r,"px"] - ABCE[r, "widths"]/2 
    Dbbu.x <- ABCE[r,"px"] + ABCE[r, "widths"]/2
    Dbbl.y <- ABCE[r, "pz"] - ABCE[r, "heights"]/2
    Dbbu.y <- ABCE[r, "pz"] + ABCE[r, "heights"]/2
    
    # Filter original data
    Box_r <- with(ABCE, filter(hitter, 
                               px >=  Dbbl.x & px <= Dbbu.x,
                               pz >=  Dbbl.y & pz <= Dbbu.y))
    
    # Divided box centers
    Dbc.x <- with(ABCE, seq(Dbbl.x, Dbbu.x, , 5)[c(2,4)])
    Dbc.y <- with(ABCE, seq(Dbbl.y, Dbbu.y, , 5)[c(2,4)])
    
    # Griddify sub_sub_box
    gridder_r <- with(Box_r, 
                      as.image(hit, 
                               cbind.data.frame(px, pz), 
                               nx=2, ny=2, 
                               grid=list(x = Dbc.x, y=Dbc.y)
                      ))
    
    # For ABCE, for book keeping, for plotting
    heights <- rep(ABCE[r,5]/2, 4)
    widths <- rep(ABCE[r,6]/2, 4)
    
    # Important-stuff data frame
    ABCE_Box_r <- with(gridder_r, 
                       cbind(expand.grid(x,y), 
                             as.vector(z), 
                             as.vector(weights), 
                             heights, 
                             widths))
    
    names(ABCE_Box_r) <- c("px", "pz", "Hitting", "Count", "heights", "widths")
    
    LoopData <- rbind.data.frame(LoopData, ABCE_Box_r)
  }
}

sum(as.numeric(ABCE$Count > cutoff)) # number boxes subdivided
# Remove subdivided, combine with new
ABCE <- rbind.data.frame(filter(ABCE, Count <= cutoff), LoopData)

ggplot(ABCE, aes(px, pz, fill=Hitting)) + 
  with(ABCE, geom_tile(width = widths, height = heights)) + 
  coord_equal() + 
  scale_fill_distiller(palette = "Spectral", trans="reverse") +
  geom_text(aes(fill = Hitting, # print counts
                label = Count), size = 3.5)

# End grid loops ==========================================

knots <- select_(ABCE, "px", "pz") # knots
rm(list=setdiff(ls(), c("knots", "hitter"))) #

hitter <- mutate(hitter, # Convert to polar, translate origin
                 r = sqrt( (px + 2)^2 + (pz - 3.5)^2), 
                 theta = atan2(pz - 3.5, px + 2)) 

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

# knots <- ABCE[,c("px","pz")]
# ggplot(data = ABCE, aes(x = px, y = pz)) + geom_point() + coord_equal()

# All covariates ===================================
m1 <- spGLM(hit ~ r + theta + 
              r2 + theta2 + 
              r_theta + r2_theta2, 
             data = hitter, family="binomial", 
             coords=with(hitter, cbind(px, pz)), 
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
# run.time = 19.5 mins, n = 1000, kn = 97, n_b<100, 30K samples
# 7 mins, n = 1000, knots = 49, 30K samples
# 33 mins, n = 3000, kn = 49, 50K samples
# 54 mins, n = 3000, kn = 49, 80K samples
m1$run.time

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

# Trace plots
samps <- as.data.frame(m1$p.beta.theta.samples)
library("reshape2")
samps <-  cbind.data.frame(rep(1:n.samples, 9), melt(samps)) 
samps <- filter(samps, variable != "XB")
names(samps) <- c("iter", "param", "value")

ggplot(data = samps, aes(x = iter, y = value)) + 
  geom_line() + 
  facet_wrap(~param, scales = "free")

ggsave("n3000_XB.jpeg", height = 8.5, width = 32, path = "/Users/ABC/Desktop/Research/Images")

print(summary(window(m1$p.beta.theta.samples, start=1)))

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

