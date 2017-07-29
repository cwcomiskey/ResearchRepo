# R INLA

install.packages("INLA", repos="https://www.math.ntnu.no/inla/R/stable")
# install.packages("INLA", repos="https://www.math.ntnu.no/inla/R/testing")
(inla.upgrade(testing=TRUE))

mutater <- function(hitter){
  mutate(hitter, r = sqrt( (px + 2)^2 + (pz - 3.5)^2), theta = atan2(pz - 3.5, px + 2), r2 = r^2, theta2 = theta^2, r_theta = r*theta, r2_theta2 = r2*theta2)
  }
hitzoner <- function(){
  cbind(expand.grid(seq(-1.5, 1.5, length = 50), 
                    seq(0, 4, length = 70)))
}

# hitter <- read.csv("Data/hitter.csv")
Peralta_n1000 <- read_csv("~/Desktop/ResearchRepo/Data/Peralta_n1000.csv")
hitter <- Peralta_n1000[,-1] 

Peralta_n3000 <- read_csv("~/Desktop/ResearchRepo/Data/Peralta_n3000.csv")
hitter <- Peralta_n3000[,-1] 

hitter$px <- hitter$x; hitter$pz <- hitter$y; hitter$hit <- hitter$res

hitter <- mutater(hitter)
hitter$obs <- 1:dim(hitter)[1]
points <- as.matrix(select(hitter, px, pz)) # observed locations

# (1) Mesh
mesh <- inla.mesh.2d(loc = points, cutoff = 0.3, 
                     offset = c(0.3, 0.3), # max.n = 1000,
                    max.edge = c(0.1, .5)) # Mesh covering

# plot(mesh); # plot(mesh, rgl = TRUE) 

# (2) A matrix
A <- inla.spde.make.A(mesh, loc = points) 
# A <- inla.spde.make.A(mesh)


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
# stack <- inla.stack(...) 

# (5) Model Statment

formula <- hit ~ r + theta + r2 + theta2 + r_theta + r2_theta2 + f(obs, model = SPDE)

# (6) INLA


obs = mesh$idx$loc[hitter$obs]
hitter$obs <- obs

# data <- inla.stack.data(stack)

result <- inla(formula, 
               family = "binomial", 
               data = hitter)


summary(result)

# Plot ========= 

# Points for plotting through the hitting zone 
hitzone <- hitzoner()
names(hitzone) <- c("px", "pz")

# Corresponding polar coords
hitzone <- mutate(hitzone, 
                  r = sqrt( (px + 2)^2 + (pz - 3.5)^2), 
                  theta = atan2(pz - 3.5, px + 2) )

# INLA fixed effect coefficient estimates
b_hat <- result$summary.fixed$"0.5quant" 

# Design matrix
X <- cbind(rep(1, dim(hitzone)[1]), hitzone[,c("r", "theta")])
X <- mutate(X, r2 = r^2, theta2 = theta^2,
            r_theta = r*theta, 
            r2_theta2 = r2*theta2) 

hitzone$linpred <- as.matrix(X) %*% b_hat
hitzone$p <- with(hitzone, 1/(1 + exp(-linpred)))

ggplot(aes(px, pz), data = hitzone) + 
  geom_tile(aes(fill = p)) + 
  sz_fcn() + 
  coord_equal() + xlim(-1.5, 1.5) + ylim(1, 4) +
  spec_fcn() + lab_fcn() +
  # ggtitle("SPDE-INLA, n = 1000")
  # ggtitle("SPDE-INLA, n = 3000")
  # ggtitle("SPDE-INLA, n = 9172")

# ggsave("/Users/ABC/Desktop/ResearchRepo/Images/INLA1000.jpg", width = 8.5, height = 8.5)
# ggsave("/Users/ABC/Desktop/ResearchRepo/Images/INLA3000.jpg", width = 8.5, height = 8.5)
# ggsave("/Users/ABC/Desktop/ResearchRepo/Images/INLA9172.jpg", width = 8.5, height = 8.5)
  

  
