library("ggplot2")
library(fields)
library("dplyr")
library("reshape2")
# library("rstan")

hitter <- read.csv("Data/hitter.csv")

# POLAR GLM =======================================

# Convert to polar, tranlate origin
hitter <- mutate(hitter,
  r = sqrt( (px + 2)^2 + (pz - 3.5)^2), 
  theta = atan2(pz - 3.5, px + 2)) 

# glm() fit ========================== 
mod.polar <- glm(hit ~ r*theta + I(r^2)*I(theta^2), 
                 family = binomial, data = hitter)

# Points for plotting through the hitting zone ====
hitzone <- cbind(expand.grid(px = seq(-1.5, 1.5, length = 50), 
                             pz = seq(1, 4, length = 70)))

# Corresponding polar coords
hitzone <- mutate(hitzone, 
                  r = sqrt( (px + 2)^2 + (pz - 3.5)^2), 
                  theta = atan2(pz - 3.5, px + 2) )

preds <- predict(mod.polar, newdata = hitzone, se.fit = TRUE)
                 # type = "response" for `p', not `logit'

hitzone$logit <- preds$fit
hitzone$SE_logit <- preds$se.fit

inv_logit <- function(x){exp(x)/(1+exp(x))}

p_CI_lower <- function(alpha, logit, SE_logit){
  upper <- logit - qnorm(alpha/2, lower.tail = FALSE)*SE_logit
  inv_logit(upper)
}

p_CI_upper <- function(alpha, logit, SE_logit){
  upper <- logit + qnorm(alpha/2, lower.tail = FALSE)*SE_logit
  inv_logit(upper)
}
  
hitzone <- mutate(hitzone, p = inv_logit(logit))

# Point estimate heat map
library(ggplot2)  
kzone <- data.frame(x = c(-0.75, -0.75, 0.75, 0.75, -0.75),
                      y = c(1.5, 3.5, 3.5, 1.5, 1.5))
ggplot(aes(px, pz), data = hitzone) + 
  geom_tile(data = hitzone, aes(fill = p)) + 
  coord_equal() + 
  geom_path(data = kzone, aes(x = x, y = y), 
            lwd = 1.5, col = "blue", linetype = 2)  + 
  scale_fill_distiller(palette = "Spectral", guide = guide_legend(title = expression(hat(p))))

# ggsave("Hitter_Polar_GLM.pdf", height = 8.5, width = 8.5)

# Confidence Intervals ================================== #
# Recall: MLE is Normally distributed
# lower <- with(hitzone, p - qnorm(alpha/2, lower.tail = FALSE)*SE_p)
# upper <- with(hitzone, p + qnorm(alpha/2, lower.tail = FALSE)*SE_p)

# Creat list: each element is DF[px, pz, plb, p, pub] for alpha = X

heat_mapper <- function(f){
  ggplot(aes(px, pz, fill = f), data = hitzone) + 
    geom_tile() + 
    geom_path(aes(x, y, fill=NULL), 
                            data = kZone, lwd = 1.5, 
                            col = "blue", linetype = 2) + 
    coord_equal() + xlim(-1.5, 1.5) + ylim(1, 4) +
    scale_fill_distiller(palette = "Spectral", 
                         limits = c(0, 0.170), 
                         guide = guide_legend(title = expression(hat(p))))
}
