library("ggplot2")
library(fields)
library("dplyr")
library("reshape2")
library("rstan")

hitter <- read.csv("~/Desktop/Research/Data/hitter.csv")

# POLAR GLM =======================================

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
mod.polar <- glm(hit ~ r*theta + I(r^2)*I(theta^2), 
                 family = binomial, data = hitter)
summary(mod.polar)
beta_GLM <- coefficients(mod.polar)[2:7] # excluding intercept

# Coefficients:
#                   Estimate Std. Error z value Pr(>|z|)     STAN
# (Intercept)       -4.08386    0.70059  -5.829 5.57e-09 *** beta0
# r                  1.19259    0.50627   2.356 0.018490 *   beta[1]
# theta             -1.92672    1.90092  -1.014 0.310786     beta[2]
# I(r^2)            -0.31913    0.08911  -3.581 0.000342 *** beta[3]
# I(theta^2)        -3.92708    1.10219  -3.563 0.000367 *** beta[4]
# r:theta           -1.64726    0.88792  -1.855 0.063568 .   beta[5]
# I(r^2):I(theta^2) -0.46130    0.20613  -2.238 0.025228 *   beta[6]

# library(fmsb) # for Nagalkerke R^2
# NagelkerkeR2(mod.polar) # $R2 [1] 0.0456851

# Fit with rms (Regression Modeling Strategies) package =====
hitter <- mutate(hitter, r2 <- r^2, theta2 <- theta^2)
library(rms)
?lrm
mod2 <- lrm(hit ~ r*theta + r2*theta2, data = hitter)
mod2
print(mod2)



# Plot ===================

# Points for plotting through the hitting zone 
hitzone <- cbind(expand.grid(seq(-1.5, 1.5, length = 50), 
                             seq(0, 4, length = 70)))
names(hitzone) <- c("px", "pz")

# Corresponding polar coords
hitzone <- mutate(hitzone, 
                  r = sqrt( (px + 2)^2 + (pz - 3.5)^2), 
                  theta = atan2(pz - 3.5, px + 2) )

hitzone <- mutate(hitzone, p = predict(mod.polar, newdata = hitzone, type = "response")) # type = "response" for `p' instead of `logit'

sum(as.numeric(is.na(hitzone$p)))

max(hitzone$p)

kZone <- data.frame(x = c(-0.95, -0.95, 0.95, 0.95, -0.95), 
                    y = c(1.6, 3.5, 3.5, 1.6, 1.6))

ggplot(aes(px, pz, fill = p), data = hitzone) + 
  geom_tile() + geom_path(aes(x, y, fill=NULL), 
                          data = kZone, lwd = 1.5, 
                          col = "blue", linetype = 2) + 
  coord_equal() + xlim(-1.5, 1.5) + ylim(1, 4) +
  scale_fill_distiller(palette = "Spectral", 
                       limits = c(0.0, .170), trans="reverse",
                       guide = guide_legend(title = expression(hat(p))))

sum(as.numeric(is.na(hitzone$p)))

# Plot layers ====== #
#   ggtitle("Polar Covariate GLM \n Success Probability") +
#   xlab("Feet from \n Middle of Home Plate") +
#   ylab("Feet Off Ground") +
#   theme(legend.key.size = unit(2, "cm"), 
#         legend.text = element_text(size = 30),
#         legend.title = element_text(size = 40),
#         legend.title.align = 0.25,
#         axis.title.x = element_text(size=28),
#         axis.title.y = element_text(size=28),
#         title = element_text(size = 28),
#         axis.text = element_text(size = 28)) 

# ggsave("Hitter_Polar_GLM.pdf", height = 8.5, width = 8.5)

# Confidence Intervals ==================================

PredCI <- predict(mod.polar, 
                  newdata=hitzone, 
                  type = "link", # use type = "response" for p
                  se.fit = TRUE) 

# Linear predictor fits, CIs
fit <- PredCI$fit
upper <- PredCI$fit + 1.96*PredCI$se.fit # MLE is Normally distributed
lower <- PredCI$fit - 1.96*PredCI$se.fit

# Expected value of response fits, CIs
fit2 <- mod.polar$family$linkinv(fit)
upper2 <- mod.polar$family$linkinv(upper)
lower2 <-mod.polar$family$linkinv(lower)

# Plot
ggplot(aes(px, pz, fill = lower2), data = hitzone) + 
  # change "fill = ?" for CI bounds
  geom_tile() + geom_path(aes(x, y, fill=NULL), 
                          data = kZone, lwd = 1.5, 
                          col = "blue", linetype = 2) + 
  coord_equal() + xlim(-1.5, 1.5) + ylim(1, 4) +
  scale_fill_distiller(palette = "Spectral", 
                       limits = c(.170, 0.0), trans="reverse",
                       guide = guide_legend(title = expression(hat(p))))

# CI Profile plot, diagonal across hitting zone ====

range(hitter$px) # [1] -1.722  1.748
range(hitter$pz) # [1] 0.503 4.245

# Old school!
# y - y_1 = m(x - x_1)
# m = (y_2 - y_1)/(x_2 - x_1)
# (x_1, y_1) = (-1.722, 0.503)
# (x_2, y_2) = (1.748, 4.245)
x_1 <- range(hitter$px)[1]
x_2 <- range(hitter$px)[2]
y_1 <- range(hitter$pz)[1]
y_2 <- range(hitter$pz)[2]
m <- (y_2 - y_1)/(x_2 - x_1)
f <- function(x) {m*(x - x_1) + y_1}

newdata.diag <- data.frame(px = seq(-1.722, 1.748, length = 1000))
newdata.diag <- mutate(newdata.diag, pz = f(px))

newdata.diag <- mutate(newdata.diag, 
                  r = sqrt( (px + 2)^2 + (pz - 3.5)^2), 
                  theta = atan2(pz - 3.5, px + 2) )


newdata.diag <- mutate(newdata.diag, p = predict(mod.polar, newdata = newdata.diag, type = "response")) # type = "response" for `p' instead of `logit'

PredCI <- predict(mod.polar, 
                  newdata=newdata.diag, 
                  type = "link", # use type = "response" for p
                  se.fit = TRUE) 

# Linear predictor fits, CIs
fit <- PredCI$fit
upper <- PredCI$fit + 1.96*PredCI$se.fit
lower <- PredCI$fit - 1.96*PredCI$se.fit

# Expected value of response fits, CIs
fit2 <- mod.polar$family$linkinv(fit)
upper2 <- mod.polar$family$linkinv(upper)
lower2 <-mod.polar$family$linkinv(lower)

newdata.diag <- cbind(newdata.diag, upper2, lower2)
  
ggplot(aes(px, p, colour = p), data = newdata.diag) +
  geom_smooth(aes(px, p, ymin = lower2, ymax = upper2), 
              data = newdata.diag, 
              stat = "identity", size = 4) +
  geom_line(aes(px, upper2, colour = upper2), size = 2) +
  geom_line(aes(px, lower2, colour = lower2), size = 2) +
  scale_colour_distiller(palette="Spectral", trans="reverse")

ggsave("PolarGLM_ProfileCI.pdf",  height = 8.5, width = 15)


# H-L GoF test ===============
# (pg 133 Myers)
# H_0: Well fit
# H_A: Lack of fit 

library(ResourceSelection)
hoslem.test(mod.polar$y, fitted(mod.polar)) # p-value = 0.8217

hitter <- mutate(hitter, p.hat = predict(mod.polar, newdata = righties, type = "response"))


# Grid Loops =========================

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

# Polar GLM model heat map on variable resolution grid ======

# Convert to polar, tranlate origin
ABCE <- mutate(ABCE,
                 r = sqrt( (px + 2)^2 + (pz - 3.5)^2), 
                 theta = atan2(pz - 3.5, px + 2)) 


ABCE <- mutate(ABCE, p = predict(mod.polar, newdata = ABCE, type = "response")) # type = "response" for `p' instead of `logit'

max(ABCE_GLM$p, na.rm = TRUE)

kZone <- data.frame(x = c(-0.95, -0.95, 0.95, 0.95, -0.95), 
                    y = c(1.6, 3.5, 3.5, 1.6, 1.6))

ggplot(ABCE, aes(px, pz, fill=p)) + 
  with(ABCE, geom_tile(width = widths, height = heights)) + 
  coord_equal() + 
  scale_fill_distiller(palette = "Spectral", trans="reverse",
                       guide = guide_legend(title = expression(hat(p)))) +
  geom_text(aes(fill = Hitting, # print counts
                label = Count), size = 3.5)

# ggsave("Variable_res_polar_glm.pdf", height = 8.5, width = 8.5)

# Spatial GLMM with Stan  ===========================
# rstan_options(auto_write = TRUE)
# options(mc.cores = parallel::detectCores())

hitter <- read.csv("~/Desktop/Baseball Research/Data/hitter.csv")

hitter <- mutate(hitter,
                 r = sqrt( (px + 2)^2 + (pz - 3.5)^2), 
                 theta = atan2(pz - 3.5, px + 2),
                 r2 = r^2, theta2 = theta^2,
                 r_theta = r*theta, 
                 r2_theta2 = r2*theta2) 

hitter <- sample_n(hitter, size = 25) # Random subsample

covs <- select(hitter, r, theta, r2, theta2, r_theta, r2_theta2) # covariates

# qr(...) - QR decomposition 
Q_matrix <- qr.Q(qr(covs)) 
R_matrix <- qr.R(qr(covs))
all.equal(as.matrix(covs), Q_matrix %*% R_matrix, check.attributes = FALSE)

# Prior distribution variance

# Var(theta) = Var(R*beta) = R*Var(beta)*R' ~= R*Var(b_i)*I_n*R'
scale <- 5   # scale on beta prior: b ~ N(0, 5) where sd = 5
theta_var <- scale^2*diag(R_matrix %*% t(R_matrix)) # theta variances
theta_SDs <- sqrt(theta_var) # theta standard deviations

hitter_dat <- with(hitter, list(hit = hit, 
                                X = covs,
                                Q = as.data.frame(Q_matrix),
                                R = as.data.frame(R_matrix),
                                SDs = theta_SDs, # Prior SDs
                                px_pz = hitter[,c("px", "pz")],
                                N = dim(hitter)[1], 
                                p = dim(covs)[2]))

fit_hitter6 <- stan(file = "Hitter6.stan", data = hitter_dat, iter = 500, chains = 3, thin = 1)

fit_hitter <- stan(fit=fit_hitter5, data=hitter_dat, iter=500, chains=3)

print(fit_hitter6, pars=c("beta0","beta", "l", "sigma"), digits = 3)

hitter_bayes <- extract(fit_hitter, 
                    permuted = FALSE, 
                    pars=c("l","sigma","beta0","beta")) 
                    # all draws, all chains

# saveRDS(hitter_bayes, "hitter1000_bayes") # save array
# hitter_stan1000 <- readRDS("hitter1000_bayes") # restore to environment

print(fit_hitter, pars=c("l","sigma","beta0","beta"), digits = 3) # Clean, complete, summary information

# rstan functions ========= 
# summary(fit_hitter, 
#         probs = c(0.025, 0.5, 0.975), 
#         pars=c("l","sigma","beta0","beta"))
# monitor(hitter, digits = 3)
# dim(hitter500) # [1] 250   2 510 --> draws * chains * params
  # Recall: random effect Z_i, for i = 1,...,N 
  # Example: hitter500[1, 1, 1]
  # Example: names(hitter500[1,,1]) # [1] "chain:1" "chain:2"
  # Example:  mean(hitter500[,1,"l"]) # Chain 1: l samples
  # Example: str(hitter500[,1,]) # Chain 1: iterations * parameters
pairs(fit_hitter4, pars = c("l", "sigma", "beta0", "beta"))
stan_plot(fit_hitter4, pars=c("l","sigma","beta0","beta")) # Pstr intrvls & pt estmts 
stan_trace(fit_hitter9000_WOSC, pars=c("beta0","beta")) # Traceplots
stan_hist(fit_hitter5, pars=c("l","sigma","beta0","beta")) # Histograms 
stan_ac(fit_hitter5, pars=c("l","sigma","beta0","beta")) # Autocorrelation 
  # stan_diag(fit_hitter5) # Dgnstcs fr Hmltnn-Mnt-Crl & NUTS
  # stan_dens(fit_hitter4) # kernal density estimates
  # stan_scat(fit_hitter4) # scatterplot
  # sampling(...) # Draw samples from a Stan model
hitter_stan <- as.data.frame(fit_hitter5, permuted = TRUE) 
  # Coerce the draws (without warmup) to: 
  # an array [as.array(...)], 
  # matrix [as.matrix(...)] or 
  # data frame (must be permuted = TRUE).

# Squared Exponential Covariance parameter exploratory plots ======
data <- data.frame(seq(0, 0.5, length = 100))
names(data) <- "d"
f <- function(d, sig, l){sig^2*exp(-d^2/(2*l^2))}
sig <- 0.12 # SD of P(Success)
l <- 0.1 # length-scale parameter
data$ecf <- with(data, f(d, sig, l)) # exp cov function
ggplot(data = data, aes(d, ecf)) + geom_line() +
  ggtitle("Exponential Covariance")

# ggsave("ExpCov.pdf", height = 6, width = 8.5)

# LogNormal plots ======
mu <- -2 # mu <- -1.5
sig <- 1 # sig <- 1.5
data <- data.frame(seq(0, 2, length = 100))
names(data) <- "x"
data$c <- with(data, dlnorm(x, mu, sig))
ggplot(data = data, aes(x, c)) + geom_line() +
  ggtitle("LogNormal as *Length-Scale* Parameter Prior") 
  # ggtitle("LogNormal as *Scale* Parameter Prior")
exp(mu + sig^2/2) # mean
sqrt((exp(sig^2) - 1)*exp(2*mu + sig^2)) # SD

# Cauchy pdf plot ======
data <- data.frame(seq(-10, 10, length = 100))
names(data) <- "x"
data$c <- with(data, dcauchy(x, 0, 5))
ggplot(data = data, aes(x, c)) + geom_line()

# Johnny Peralta Simulation =====================================

# Simulate from variable resolution map, calculate interesting test statistic, compare observed to permutation distribution

# Aggregated version ======================

# > rbinom(10,2,0.5) # 10 draws from Binomial(2, 0.5)
# > rbinom(1,10,0.5) # 1 draw from Bimomial(10, 0.5)

nsim <- 1000 # number of simulations
dat <- as.data.frame(matrix(ncol = nsim, nrow = dim(ABCE)[1]))
# Match ABCE: rows = boxes, columns = simulations

for(i in 1:dim(ABCE)[1]){ # 1:total_boxes
  dat[i,] <- rbinom(nsim, ABCE[i,"Count"], ABCE[i, "p"])/ABCE[i,"Count"]} 
  # nsim draws from Binomial(Count_i, BA_i)

ABCE <- cbind.data.frame(ABCE, dat) # Vital stats, simulated outcomes
round(ABCE[1:3, 1:15], 3)

# Plot one, a few, many 
fill <- sample((dim(ABCE)[2]-nsim+1):dim(ABCE)[2], 1)
ggplot(ABCE, aes(px, pz, fill=ABCE[,fill])) + 
  with(ABCE, geom_tile(width = widths, height = heights)) + 
  coord_equal() + 
  scale_fill_distiller(palette = "Spectral", trans="reverse") +
  geom_text(aes(fill = ABCE[,fill], # print counts
                label = Count), size = 3.5)

# p = model fit
# Count = box sample size

# Test Statistic #1 =========================================== #
# N(0,1) squared: [(phat - p)/(p(1-p)/n)]^1
# column j
chisq_stat1 <- function(j){
  sum(
  (ABCE[,j] - ABCE[, "p"])^2 / 
    ( ABCE[, "p"]*(1-ABCE[, "p"])/ABCE[,"Count"] )
  )}

chisq_stats1 <- as.data.frame(matrix(ncol=1, nrow = nsim))
for(j in 1:nsim){chisq_stats1[j,1] <- chisq_stat1(j+9)}
qplot(V1, data = chisq_stats1, geom = "histogram") + 
  geom_vline(xintercept = chisq_stat1(3))

ggsave("chisq1.pdf", height = 8.5, width = 12.5)

mean(chisq_stats1[,1])

# # Test Statistic #2 ============================================ #
# Counts: (O - E)^2/E
# column j

chisq_stat2 <- function(j){
  sum(
  (ABCE[,"Count"]*ABCE[,j] - ABCE[,"Count"]*ABCE[,"p"])^2 / 
    (ABCE[,"Count"]*ABCE[,"p"])
  )}

chisq_stats2 <- as.data.frame(matrix(ncol=1, nrow = nsim))
for(j in 1:nsim){chisq_stats2[j,1] <- chisq_stat2(j+9)}
qplot(V1, data = chisq_stats2, geom = "histogram") + 
  geom_vline(xintercept = chisq_stat2(3))

ggsave("chisq2.pdf", height = 8.5, width = 12.5)


# Do test of distribution: are these guys Chi-Sq_97?
mean(chisq_stats2[,1])

# What the heck does this tell us?

# Non-aggregated version, Round 2 ==============
hitter <- mutate(hitter, pfit = predict(mod.polar, newdata = hitter, type = "response"))

# > rbinom(10,2,0.5) # 10 draws from Binomial(2, 0.5)
# > rbinom(1,10,0.5) # 1 draw from Bimomial(10, 0.5)

nsim <- 500 # number of simulations; 500 < 10 mins
dat <- as.data.frame(matrix(ncol = nsim, nrow = dim(hitter)[1]))
# Match ABCE: rows = boxes, columns = simulations

for(i in 1:dim(hitter)[1]){ # 1:total_boxes
  dat[i,] <- rbinom(nsim, 1, hitter[i, "pfit"])} 
# nsim draws from Binomial(1, fitted.p_i)

# H-L GoF test statistic ===============

library(ResourceSelection)
# mod.polar$y = 0/1 response
HL1 <- hoslem.test(mod.polar$y, fitted(mod.polar))$statistic # X-sq = 4.3758 

mod.polar <- glm(dat[,j] ~ r*theta + I(r^2)*I(theta^2), 
                 family = binomial, data = hitter)

HL_stats <- as.data.frame(matrix(ncol=1, nrow = nsim))
for(j in 1:nsim){
  mod.polar <- glm(dat[,j] ~ r*theta + I(r^2)*I(theta^2), 
                   family = binomial, data = hitter)
  HL_stats[j,1] <- hoslem.test(dat[,j], fitted(mod.polar))$statistic}

# For overlaying Chi-sq distribution
chisq10 <- as.data.frame(matrix(ncol = 2, nrow = 1000)) # data 
chisq10[,1] <- seq(0,31, length = 1000) # x values
bw <- (range(HL_stats$V1)[2] - range(HL_stats$V1)[1])/30 # bin width
chisq10[,2] <- 500*bw*dchisq(chisq10[,1], df=8) # density

# For some reason the test statistic seems to have a chi-sq_10 distribution, instead of the expected g-2=8
qplot(V1, data = HL_stats, geom = "histogram") + 
  geom_vline(xintercept = HL1, color = "red", size = 2) + 
  geom_line(data = chisq10, 
            aes(x = V1, y = V2), size = 3, color = "blue")

# ggsave("HLstat_500.pdf", path = "/Users/ABC/Desktop/Baseball Research/Images", height = 8.5, width = 12.5)

mean(as.numeric(HL_stats[,1] > HL1)) # [1] 0.938
