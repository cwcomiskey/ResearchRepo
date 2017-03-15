# Predictive Process Model (PPM), with HMC in Stan 
library(ggplot2)
library(dplyr)
library(fields)
library(rstan)
library(reshape)

hitter <- read.csv("~/Desktop/Research/Data/hitter.csv")

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
rm(list=setdiff(ls(), c("knots", "hitter"))) # Remove all but knots

hitter <- mutate(hitter, # Convert to polar, translate origin
                 r = sqrt( (px + 2)^2 + (pz - 3.5)^2), 
                 theta = atan2(pz - 3.5, px + 2)) 

mod.polar <- glm(hit ~ r*theta + I(r^2)*I(theta^2), 
                 family = binomial, data = hitter)

hitter <- mutate(hitter,
                 r = sqrt( (px + 2)^2 + (pz - 3.5)^2), 
                 theta = atan2(pz - 3.5, px + 2),
                 r2 = r^2, theta2 = theta^2,
                 r_theta = r*theta, 
                 r2_theta2 = r2*theta2) 

hitter <- sample_n(hitter, size = 3000) # Random subsample

covs <- as.matrix(cbind(rep(1, dim(hitter)[1]), select(hitter, r, theta, r2, theta2, r_theta, r2_theta2)) )
beta <- as.matrix(coefficients(mod.polar))

XB <- as.vector(covs %*% beta)

# qr(...) - QR decomposition 
Q_matrix <- qr.Q(qr(covs)) 
R_matrix <- qr.R(qr(covs))
R_inv <- solve(R_matrix)
# all.equal(as.matrix(covs), 
#           Q_matrix %*% R_matrix, 
#           check.attributes = FALSE)

# QR: Prior distribution variance
# Var(theta) = Var(R*beta) = R*Var(beta)*R' ~= R*Var(b_i)*I_n*R'
scale <- 5   # scale on beta prior: b ~ N(0, 5) where sd = 5
theta_SDs <- sqrt(scale^2*diag(R_matrix %*% t(R_matrix))) # theta SDs

# D_star = as.matrix(dist(knots, diag = TRUE)) # m x m - btwn knts
# D_site_star <- matrix(nrow = dim(hitter)[1], # Empty matrix
#                       ncol = dim(knots)[1])  # n x m - obs, knots
# for(i in 1:dim(hitter)[1]) { D_site_star[i,] <- 
#   dist(rbind(hitter[i, c("px", "pz")], knots))[1:dim(knots)[1]]
#   }

# For PPM_HMC.stan and PPM_HMC2.stan (cov_exp_quad(...)) ================= 
# hitter_dat <- with(hitter, list(hit = hit, p = dim(covs)[2], X = covs,
#                                 m = dim(knots)[1], # number of knots
#                                 N = dim(hitter)[1],
#                                 # D_star = D_star, # m x m; knot distances
#                                 # D_site_star = D_site_star, #n x m; dist
#                                 knots = as.matrix(S),
#                                 obs = as.matrix(select_(hitter, "px", "pz")),
#                                 Q = Q_matrix, R = R_matrix, R_inv = R_inv,
#                                 SDs = theta_SDs))

# For PPM_HMC3.stan - Fix covariates  =================== ========== 
hitter_dat <- with(hitter, list(hit = hit, XB = XB,
                                m = dim(knots)[1], # number knots
                                N = dim(hitter)[1],
                                # D_star = D_star, # m x m; knot distances
                                # D_site_star = D_site_star, #n x m; dist
                                knots = as.matrix(knots),
                                obs = as.matrix(select_(hitter, "px", "pz"))))


PPM_fit <- stan(file = "PPM_HMC3.stan", 
                data = hitter_dat, 
                iter = 10000, chains = 3, thin = 1)

PPM_fit2 <- stan(fit=PPM_fit, data=hitter_dat, iter=500, chains=3)

print(PPM_fit, pars=c("beta0","beta", "phi", "eta_sq"), digits = 3)

# Trace plots "by hand"
data <- extract(PPM_fit, permuted = FALSE, 
                pars=c("phi", "eta_sq")) 
samps <- melt(data)
samps <- filter(samps, parameters == "phi")
samps <- select_(samps, "iterations", "chains", "value")
ggplot(data = samps, aes(x = iterations, y = value)) + 
   geom_line() + facet_wrap(~ chains, scales = "free")
#  facet_wrap(~ chains, scales = "free") + 
  

# pairs(PPM_fit, pars = c("phi", "eta_sq", "beta0", "beta"))
stan_plot(PPM_fit, pars=c("phi","eta_sq","beta0","beta")) # 
stan_trace(PPM_fit, pars=c("phi", "eta_sq")) # 
stan_hist(fit_hitter5, pars=c("l","sigma","beta0","beta")) #
stan_ac(PPM_fit, pars=c("beta0","beta", "phi", "eta_sq")) 
# stan_diag(PPM_fit) # Dgnstcs fr Hmltnn-Mnt-Crl & NUTS
# stan_dens(fit_hitter4) # kernal density estimates
# stan_scat(fit_hitter4) # scatterplot
# sampling(...) # Draw samples from a Stan model



