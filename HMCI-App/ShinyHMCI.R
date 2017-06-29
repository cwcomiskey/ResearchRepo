library("ggplot2")
library(fields)
library("dplyr")
# library("reshape2")
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
                             pz = seq(1, 4, length = 70))) %>%
  mutate(r = sqrt( (px + 2)^2 + (pz - 3.5)^2), 
         theta = atan2(pz - 3.5, px + 2))

preds <- predict(mod.polar, newdata = hitzone, se.fit = TRUE)
                 #  logit scale; use "type = "response" for p 

inv_logit <- function(x){exp(x)/(1+exp(x))}
hitzone <- with(preds, 
                mutate(hitzone, 
                       logit = fit, 
                       SE_logit = se.fit, 
                       phat = inv_logit(logit)
                       )
                )
# Functions, and the rest ======

p_CI_lower <- function(pct, logit, SE_logit){
  upper <- logit - qnorm((1 - pct/100)/2, lower.tail = FALSE)*SE_logit
  inv_logit(upper)
}
p_CI_upper <- function(pct, logit, SE_logit){
  upper <- logit + qnorm((1 - pct/100)/2, lower.tail = FALSE)*SE_logit
  inv_logit(upper)
}

# Point estimate heat map
library(ggplot2)  
hmci <- function(dataset, stat){
ggplot(aes(px, pz), data = dataset) + 
  geom_tile(data = dataset, aes(fill = stat)) + 
  coord_equal() + 
  sz_fcn() + # sz_fcn{varyres}
  spec_fcn() # # spec_fcn{varyres}
}

# ggsave("Hitter_Polar_GLM.pdf", height = 8.5, width = 8.5)

# Confidence Intervals ================================== 

CI_list <- list(hitzone = # first list element
                  select(hitzone, px, pz, logit, SE_logit, phat))
pct <- seq(1, 99, by = 1)

# CI_list[[1]][1,1]

for(i in 1:length(pct)){
  percent <- pct[i] # For CI widths
  CI_list[[i+1]] <- with(CI_list[[1]], 
                         data.frame(plb = p_CI_lower(percent, logit, SE_logit), 
                                    pub = p_CI_upper(percent, logit, SE_logit)
                                    ))} # Contains everything! 

# Be sure to use: with(dataset, shiny_hmci_fcn(...))
shiny_hmci_fcn <- function(dataset, bound){
ggplot(aes(px, pz), data = dataset) + 
  geom_tile(data = test, aes(fill = bound)) + 
  coord_equal() + 
  sz_fcn() + # sz_fcn{varyres}
  spec_fcn() # # spec_fcn{varyres}
} 

test <- cbind.data.frame(CI_list[[1]], CI_list[[10]]) # Create DF: pts, bnds
with(test, shiny_hmci_fcn(dataset = test, plb)) 


