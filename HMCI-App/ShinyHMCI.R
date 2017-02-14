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
hitzone <- cbind(expand.grid(seq(-1.5, 1.5, length = 50), 
                             seq(0, 4, length = 70)))
names(hitzone) <- c("px", "pz")

# Corresponding polar coords
hitzone <- mutate(hitzone, 
                  r = sqrt( (px + 2)^2 + (pz - 3.5)^2), 
                  theta = atan2(pz - 3.5, px + 2) )

preds <- predict(mod.polar, newdata = hitzone, 
                      # type = "response" for `p', not `logit'
                      type = "response", se.fit = TRUE)
hitzone$p <- preds$fit
hitzone$SE_p <- preds$se.fit

kZone <- data.frame(x = c(-0.95, -0.95, 0.95, 0.95, -0.95), 
                    y = c(1.6, 3.5, 3.5, 1.6, 1.6))

# Point estimate heat map
ggplot(aes(px, pz, fill = p), data = hitzone) + 
  geom_tile() + geom_path(aes(x, y, fill=NULL), 
                          data = kZone, lwd = 1.5, 
                          col = "blue", linetype = 2) + 
  coord_equal() + xlim(-1.5, 1.5) + ylim(1, 4) +
  scale_fill_distiller(palette = "Spectral", 
                       limits = c(0.0, .170), trans="reverse",
                       guide = guide_legend(title = expression(hat(p))))

# sum(as.numeric(is.na(hitzone$p)))

# Plot layers (add as desired) ====== 
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

# fits, CIs: E[p]
# alpha <- 0.05
# Recall: MLE is Normally distributed
# lower <- with(hitzone, p - qnorm(alpha/2, lower.tail = FALSE)*SE_p)
# upper <- with(hitzone, p + qnorm(alpha/2, lower.tail = FALSE)*SE_p)

#  ========= Plot CI upper/lower bounds ========= #   
# ggplot(aes(px, pz, fill = upper), data = hitzone) + 
#   geom_tile() + geom_path(aes(x, y, fill=NULL), 
#                           data = kZone, lwd = 1.5, 
#                           col = "blue", linetype = 2) + 
#   coord_equal() + xlim(-1.5, 1.5) + ylim(1, 4) +
#   scale_fill_distiller(palette = "Spectral", 
#                        limits = c(0, 0.170), 
#                        guide = guide_legend(title = expression(hat(p))))



heat_mapper <- function(f){
  ggplot(aes(px, pz, fill = f), data = hitzone) + 
    geom_tile() + geom_path(aes(x, y, fill=NULL), 
                            data = kZone, lwd = 1.5, 
                            col = "blue", linetype = 2) + 
    coord_equal() + xlim(-1.5, 1.5) + ylim(1, 4) +
    scale_fill_distiller(palette = "Spectral", 
                         limits = c(0, 0.170), 
                         guide = guide_legend(title = expression(hat(p))))
}

# # Grid Loops =========================
# 
# cutoff<- 200
# 
# # Loop Zero =====
# 
# GrZero <- summarise(hitter, min.x = min(px), max.x = max(px), # x bounds
#                     min.y = min(pz), max.y = max(pz), # y bounds 
#                     Hitting = mean(hit), Count = dim(hitter)[1], # stats
#                     x = (max(px)+min(px))/2, # x center
#                     y = (max(pz)+min(pz))/2) # y center
# 
# ABCE <- with(GrZero, cbind.data.frame(x, y, round(Hitting, 2), Count))
# names(ABCE) <- c("px", "pz", "Hitting", "Count")
# 
# # Loop 1 (4) =================================== 
# 
# # Add if(){} statemtent here
# # if(count > cutoff){Do everything else!!}
# 
# ABCE <-  with(GrZero, mutate(ABCE, width = max.x - min.x, height = max.y - min.y))
# 
# # Magic step
# gridder <- with(hitter, as.image(hit, cbind.data.frame(px, pz), nx = 2, ny = 2)) 
# 
# # Manually add (bounds) and correct (centers)
# gridder$xbb <- with(GrZero, seq(min.x, max.x, , 5))[c(1,3,5)] # x box boundaries
# gridder$ybb <- with(GrZero, seq(min.y, max.y, , 5))[c(1,3,5)] # y box boundaries 
# gridder$x <- with(GrZero, seq(min.x, max.x, , 5))[c(2,4)] # x box centers
# gridder$y <- with(GrZero, seq(min.y, max.y, , 5))[c(2,4)] # y box centers
# 
# # Create important-stuff matrix: box centers, BA, counts
# ABCE <- with(gridder, cbind(expand.grid(x, y), as.vector(z), as.vector(weights)))
# 
# names(ABCE) <- c("px", "pz", "Hitting", "Count")
# 
# # Loop 2 (16) =================================== 
# 
# gridder$bw <- gridder$xbb[2] - gridder$xbb[1] # box width
# gridder$bh <- gridder$ybb[2] - gridder$ybb[1] # box height
# 
# for(i in 1:2){
#   for(j in 1:2){
#     if(gridder$weights[i,j] > cutoff){
#       
#       # Filter original data
#       Box_ij <- with(gridder, filter(hitter, 
#                                      px >= xbb[i] & px < xbb[i+1], 
#                                      pz >= ybb[j] & pz < ybb[j+1]))
#       
#       # subdivided box centers (sbc)
#       sbc.x <- with(gridder, seq(xbb[i], xbb[i+1], , 5)[c(2,4)])
#       sbc.y <- with(gridder, seq(ybb[j], ybb[j+1], , 5)[c(2,4)])
#       
#       gridder_ij <- with(Box_ij, as.image(hit, cbind.data.frame(px, pz), nx = 2, ny =2, grid = list(x = sbc.x, y = sbc.y))) 
#       
#       ABCE_Box_ij <- with(gridder_ij, cbind(expand.grid(x, y), as.vector(z), as.vector(weights)))
#       
#       names(ABCE_Box_ij) <- c("px", "pz", "Hitting", "Count")
#       
#       # ABCE: Remove old box (i,j), add new boxes ======== #
#       ABCE <- with(gridder, filter(ABCE, !(px == x[i] & pz == y[j])))
#       ABCE <- rbind.data.frame(ABCE, ABCE_Box_ij)
#       
#     }
#   }
# }
# 
# # Solve: 2*2 - x + 4*x = dim(ABCE)[1]
# sdb <- (dim(ABCE)[1] - 4)/3 # number of subdivided boxes
# 
# # New box dimensions for ggplot() and geom_tile()
# widths <- with(gridder, c(rep(bw, 4 - sdb), rep(bw/2, 4*sdb))) 
# heights <- with(gridder, c(rep(bh, 4 - sdb), rep(bh/2, 4*sdb))) 
# ABCE <- cbind(ABCE, heights, widths)
# 
# # Loop 3 (64) ======================================== 
# 
# dim(ABCE)[1] # [1] 16
# 
# LoopData <- data.frame()
# 
# for(r in 1:dim(ABCE)[1]){ # Iterate through rows (boxes) of ABCE
#   
#   if(ABCE$Count[r] > cutoff){ 
#     
#     # Divided Box Bounds Lower/Upper
#     Dbbl.x <- ABCE[r,"px"] - ABCE[r, "widths"]/2 
#     Dbbu.x <- ABCE[r,"px"] + ABCE[r, "widths"]/2
#     Dbbl.y <- ABCE[r, "pz"] - ABCE[r, "heights"]/2
#     Dbbu.y <- ABCE[r, "pz"] + ABCE[r, "heights"]/2
#     
#     # Filter original data
#     Box_r <- with(ABCE, filter(hitter, 
#                                px >=  Dbbl.x & px <= Dbbu.x,
#                                pz >=  Dbbl.y & pz <= Dbbu.y))
#     
#     # Divided box centers
#     Dbc.x <- with(ABCE, seq(Dbbl.x, Dbbu.x, , 5)[c(2,4)])
#     Dbc.y <- with(ABCE, seq(Dbbl.y, Dbbu.y, , 5)[c(2,4)])
#     
#     # Griddify sub_sub_box
#     gridder_r <- with(Box_r, 
#                       as.image(hit, 
#                                cbind.data.frame(px, pz), 
#                                nx=2, ny=2, 
#                                grid=list(x = Dbc.x, y=Dbc.y)
#                       )
#     )
#     
#     # For ABCE, for book keeping, for plotting
#     heights <- rep(ABCE[r,5]/2, 4)
#     widths <- rep(ABCE[r,6]/2, 4)
#     
#     # Important-stuff data frame
#     ABCE_Box_r <- with(gridder_r, 
#                        cbind(expand.grid(x,y), 
#                              as.vector(z), 
#                              as.vector(weights), 
#                              heights, 
#                              widths))
#     
#     names(ABCE_Box_r) <- c("px", "pz", "Hitting", "Count", "heights", "widths")
#     
#     LoopData <- rbind.data.frame(LoopData, ABCE_Box_r)
#   }
# }
# 
# # Boxes subdivided 
# sdb <- sum(as.numeric(ABCE$Count > cutoff))
# # Remove subdivided, combine with new
# ABCE <- rbind.data.frame(filter(ABCE, Count <= cutoff), LoopData)
# 
# ggplot(ABCE, aes(px, pz, fill=Hitting)) + 
#   with(ABCE, geom_tile(width = widths, height = heights)) + 
#   coord_equal() + 
#   scale_fill_distiller(palette = "Spectral", trans="reverse") +
#   geom_text(aes(fill = Hitting, # print counts
#                 label = Count), size = 3.5)
# 
# # Loop 4 (256) ====================================== 
# 
# dim(ABCE)[1] # [1] 49 (of 64 possible)
# 
# LoopData <- data.frame()
# 
# for(r in 1:dim(ABCE)[1]){ # Iterate through rows (boxes) of ABCE
#   
#   if(ABCE$Count[r] > cutoff){ 
#     
#     # Divided Box Bounds Lower/Upper
#     Dbbl.x <- ABCE[r,"px"] - ABCE[r, "widths"]/2 
#     Dbbu.x <- ABCE[r,"px"] + ABCE[r, "widths"]/2
#     Dbbl.y <- ABCE[r, "pz"] - ABCE[r, "heights"]/2
#     Dbbu.y <- ABCE[r, "pz"] + ABCE[r, "heights"]/2
#     
#     # Filter original data
#     Box_r <- with(ABCE, filter(hitter, 
#                                px >=  Dbbl.x & px <= Dbbu.x,
#                                pz >=  Dbbl.y & pz <= Dbbu.y))
#     
#     # Divided box centers
#     Dbc.x <- with(ABCE, seq(Dbbl.x, Dbbu.x, , 5)[c(2,4)])
#     Dbc.y <- with(ABCE, seq(Dbbl.y, Dbbu.y, , 5)[c(2,4)])
#     
#     # Griddify sub_sub_box
#     gridder_r <- with(Box_r, 
#                       as.image(hit, 
#                                cbind.data.frame(px, pz), 
#                                nx=2, ny=2, 
#                                grid=list(x = Dbc.x, y=Dbc.y)
#                       ))
#     
#     # For ABCE, for book keeping, for plotting
#     heights <- rep(ABCE[r,5]/2, 4)
#     widths <- rep(ABCE[r,6]/2, 4)
#     
#     # Important-stuff data frame
#     ABCE_Box_r <- with(gridder_r, 
#                        cbind(expand.grid(x,y), 
#                              as.vector(z), 
#                              as.vector(weights), 
#                              heights, 
#                              widths))
#     
#     names(ABCE_Box_r) <- c("px", "pz", "Hitting", "Count", "heights", "widths")
#     
#     LoopData <- rbind.data.frame(LoopData, ABCE_Box_r)
#   }
# }
# 
# sum(as.numeric(ABCE$Count > cutoff)) # number boxes subdivided
# # Remove subdivided, combine with new
# ABCE <- rbind.data.frame(filter(ABCE, Count <= cutoff), LoopData)
# 
# ggplot(ABCE, aes(px, pz, fill=Hitting)) + 
#   with(ABCE, geom_tile(width = widths, height = heights)) + 
#   coord_equal() + 
#   scale_fill_distiller(palette = "Spectral", trans="reverse") +
#   geom_text(aes(fill = Hitting, # print counts
#                 label = Count), size = 3.5)
# 
# # Loop 5 (4^5) ===========================================
# 
# dim(ABCE)[1] 
# 
# LoopData <- data.frame()
# 
# for(r in 1:dim(ABCE)[1]){ # Iterate through rows (boxes) of ABCE
#   
#   if(ABCE$Count[r] > cutoff){ 
#     
#     # Divided Box Bounds Lower/Upper
#     Dbbl.x <- ABCE[r,"px"] - ABCE[r, "widths"]/2 
#     Dbbu.x <- ABCE[r,"px"] + ABCE[r, "widths"]/2
#     Dbbl.y <- ABCE[r, "pz"] - ABCE[r, "heights"]/2
#     Dbbu.y <- ABCE[r, "pz"] + ABCE[r, "heights"]/2
#     
#     # Filter original data
#     Box_r <- with(ABCE, filter(hitter, 
#                                px >=  Dbbl.x & px <= Dbbu.x,
#                                pz >=  Dbbl.y & pz <= Dbbu.y))
#     
#     # Divided box centers
#     Dbc.x <- with(ABCE, seq(Dbbl.x, Dbbu.x, , 5)[c(2,4)])
#     Dbc.y <- with(ABCE, seq(Dbbl.y, Dbbu.y, , 5)[c(2,4)])
#     
#     # Griddify sub_sub_box
#     gridder_r <- with(Box_r, 
#                       as.image(hit, 
#                                cbind.data.frame(px, pz), 
#                                nx=2, ny=2, 
#                                grid=list(x = Dbc.x, y=Dbc.y)
#                       ))
#     
#     # For ABCE, for book keeping, for plotting
#     heights <- rep(ABCE[r,5]/2, 4)
#     widths <- rep(ABCE[r,6]/2, 4)
#     
#     # Important-stuff data frame
#     ABCE_Box_r <- with(gridder_r, 
#                        cbind(expand.grid(x,y), 
#                              as.vector(z), 
#                              as.vector(weights), 
#                              heights, 
#                              widths))
#     
#     names(ABCE_Box_r) <- c("px", "pz", "Hitting", "Count", "heights", "widths")
#     
#     LoopData <- rbind.data.frame(LoopData, ABCE_Box_r)
#   }
# }
# 
# sum(as.numeric(ABCE$Count > cutoff)) # number boxes subdivided
# # Remove subdivided, combine with new
# ABCE <- rbind.data.frame(filter(ABCE, Count <= cutoff), LoopData)
# 
# ggplot(ABCE, aes(px, pz, fill=Hitting)) + 
#   with(ABCE, geom_tile(width = widths, height = heights)) + 
#   coord_equal() + 
#   scale_fill_distiller(palette = "Spectral", trans="reverse") +
#   geom_text(aes(fill = Hitting, # print counts
#                 label = Count), size = 3.5)
# 
# # End grid loops ==========================================
# 
# # Polar GLM model heat map on variable resolution grid ======
# 
# # Convert to polar, tranlate origin
# ABCE <- mutate(ABCE,
#                  r = sqrt( (px + 2)^2 + (pz - 3.5)^2), 
#                  theta = atan2(pz - 3.5, px + 2)) 
# 
# 
# ABCE <- mutate(ABCE, p = predict(mod.polar, newdata = ABCE, type = "response")) # type = "response" for `p' instead of `logit'
# 
# max(ABCE_GLM$p, na.rm = TRUE)
# 
# kZone <- data.frame(x = c(-0.95, -0.95, 0.95, 0.95, -0.95), 
#                     y = c(1.6, 3.5, 3.5, 1.6, 1.6))
# 
# ggplot(ABCE, aes(px, pz, fill=p)) + 
#   with(ABCE, geom_tile(width = widths, height = heights)) + 
#   coord_equal() + 
#   scale_fill_distiller(palette = "Spectral", trans="reverse",
#                        guide = guide_legend(title = expression(hat(p)))) +
#   geom_text(aes(fill = Hitting, # print counts
#                 label = Count), size = 3.5)
# 
# # ggsave("Variable_res_polar_glm.pdf", height = 8.5, width = 8.5)








