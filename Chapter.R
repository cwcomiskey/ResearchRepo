# Chapter Images, Code

library("ggplot2")
library(fields)
library("dplyr")
library("reshape2")
library("gridExtra")
ff
# Empirical Mothership plot ==============================

hitter <- read.csv("~/Desktop/ResearchRepo/Data/hitter.csv")


coordsR <- with(hitter, cbind.data.frame(px, pz))
hitgridR <- with(hitter, as.image(hit, coordsR, nx = 20, ny = 20)) 
ABC.R <- with(hitgridR, 
              cbind(expand.grid(x, y), as.vector(z)))
names(ABC.R) <- c("Horizontal", "Vertical", "Hitting")

kZone <- data.frame(x = c(-0.95, -0.95, 0.95, 0.95, -0.95), 
                    y = c(1.6, 3.5, 3.5, 1.6, 1.6))

ggplot(ABC.R, aes(Horizontal, Vertical, fill = Hitting)) + 
  geom_tile() + 
  xlim(-1.5, 1.5) + ylim(1, 4) + 
  scale_fill_distiller(palette = "Spectral", trans="reverse", 
                       limits = c(0, 0.18), 
                       guide = guide_legend(title = expression(hat(p)))) + 
  geom_path(aes(x, y, fill=NULL), data = kZone, 
            lwd = 1.5, col = "blue", linetype = 2) + 
  coord_equal() + ggtitle("P(Hit|Swing)") +
  xlab("Feet from \n Middle of Home Plate") +
  ylab("Feet Off Ground") +
  theme(legend.key.size = unit(2, "cm"), 
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 40),
        legend.title.align = 0.25,
        axis.title.x = element_text(size=28),
        axis.title.y = element_text(size=28),
        title = element_text(size = 28),
        axis.text = element_text(size = 28))

# ggsave("Mothership.pdf", height = 8.5, width = 8.5) # righties empirical

# Var-res plots ===============

kZone <- data.frame(x = c(-0.95, -0.95, 0.95, 0.95, -0.95), 
                    y = c(1.6, 3.5, 3.5, 1.6, 1.6))

hitter <- read.csv("~/Desktop/ResearchRepo/Data/hitter.csv")

cutoff <- 200

# Blank (yellow) Slate (1) =========================

GrZero <- summarise(hitter, min.x = min(px), max.x = max(px), # x bounds
                    min.y = min(pz), max.y = max(pz), # y bounds 
                    Hitting = mean(hit), Count = dim(hitter)[1], # stats
                    x = (max(px)+min(px))/2, # x center
                    y = (max(pz)+min(pz))/2) # y center

ABCE <- with(GrZero, cbind.data.frame(x, y, round(Hitting, 2), Count))
names(ABCE) <- c("px", "pz", "Hitting", "Count")


G1 <- ggplot(ABCE, aes(px, pz, fill = Hitting)) + 
  with(GrZero, geom_tile(
    width = max.x - min.x, height = max.y - min.y)) + 
  coord_equal() +
  scale_fill_distiller(palette = "Spectral", trans="reverse") + 
  geom_text(aes(label = Count), size = 3.5) # ****

# ggsave("Chapter1x1.pdf", height = 8.5, width = 8.5)

# Loop 1 (4) =================================== 

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

G4 <- ggplot(ABCE, aes(px, pz, fill = Hitting)) + 
  geom_tile() + coord_equal() +
  scale_fill_distiller(palette = "Spectral", trans="reverse") + 
  geom_text(aes(fill = Hitting, label = Count), size = 3.5)

# ggsave("Chapter2x2.pdf", path = "/Users/ABC/Desktop/Baseball Research/Images", height = 8.5, width = 8.5) 
# ggsave("Movie2.jpg", height = 8.5, width = 8.5) 

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
      
      gridder_ij <- with(Box_ij, as.image(hit, cbind.data.frame(px, pz), nx = 2, ny =2, grid = list(x = sbc.x, y = sbc.y))) # specify new box centers
      
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


G16 <- ggplot(ABCE, aes(px, pz, fill=Hitting)) + 
  geom_tile(width = widths, height = heights) + 
  coord_equal() + 
  scale_fill_distiller(palette = "Spectral", trans="reverse") +
  geom_text(aes(fill = Hitting, # print counts
                label = Count), size = 3.5)


# ggsave("Chapter4x4.pdf", height = 8.5, width = 8.5) 

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

ABCE <- filter(ABCE, Count != "NA", Hitting != "NA")

G64 <- ggplot(ABCE, aes(px, pz, fill=Hitting)) + 
  with(ABCE, geom_tile(width = widths, height = heights)) + 
  coord_equal() + 
  scale_fill_distiller(palette = "Spectral", trans="reverse") +
  geom_text(aes(fill = Hitting, # print counts
                label = Count), size = 3.5) # +
# geom_path(aes(x, y, fill=NULL), data = kZone, 
#         lwd = 1.5, col = "blue", linetype = 2) 

sum(as.numeric(ABCE$Count > 200))

# ggsave("Chapter8x8.pdf", height = 8.5, width = 8.5)

# ggsave("Chapter8x8_100.pdf", height = 8.5, width = 8.5)

# sum(as.numeric(ABCE$Count > 250))

# Loop 4 (256) ======================================

dim(ABCE)[1] # [1] 49 (of 64 possible)

ABCE <- filter(ABCE, Count != "NA")

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
ABCE <- filter(ABCE, Count != "NA", Hitting != "NA")


G256 <- ggplot(ABCE, aes(px, pz, fill=Hitting)) + 
  with(ABCE, geom_tile(width = widths, height = heights)) + 
  coord_equal() + 
  scale_fill_distiller(palette = "Spectral", trans="reverse") +
  geom_text(aes(fill = Hitting, # print counts
                label = Count), size = 2.5)

# ggsave("Movie5.jpg", height = 8.5, width = 8.5)

# ggsave("Chapter16x16_100.pdf", height = 8.5, width = 8.5)

# Fun
# sum(as.numeric(ABCE$Count < 50))
# ggplot(aes(x = "Peralta", y = Count), data = ABCE) + geom_boxplot()

# Loop 5 (1024) ===========================================

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

# ggsave("Chapter32x32_100.pdf", height = 8.5, width = 8.5)

# Grid - Increasing resolution on `batter == 425509' =======
pdf("Chapter_VarRes.pdf", width = 8, height = 10, paper = "USr")
grid.arrange(G1, G4, G16, G64, G256,  ncol = 3)
dev.off()

# Varying resolution heat maps ===================

gridder <- with(hitter, as.image(hit, cbind.data.frame(px, pz), nx = 3, ny = 3)) 
# counts_image <- gridder$weights 
# ci <- counts_image # counts image, for surgery
# z <- gridder$z; # box BAs image

counts <- with(gridder, cbind(expand.grid(x,y), as.vector(weights)))
names(counts) <- c("px", "pz", "count")
counts <- filter(counts, count != "NA")

heatmapper <- function(hitterdata, NX, NY){
  # x, y, and "hit" as "visual-spatial" matrix
  hitgridR <- with(hitterdata, 
                   as.image(hit, cbind.data.frame(px, pz), nx = NX, ny = NY)) 
  # melt
  ABC.R <- with(hitgridR,  cbind(expand.grid(x, y), as.vector(z))) 
  names(ABC.R) <- c("Horizontal", "Vertical", "Hitting") 
  kZone <- data.frame(x = c(-0.95, -0.95, 0.95, 0.95, -0.95), 
                      y = c(1.6, 3.5, 3.5, 1.6, 1.6))
  ABC.R <- filter(ABC.R, Hitting != "NA")
  ggplot(ABC.R, aes(Horizontal, Vertical, fill = Hitting)) + 
    geom_tile() + 
    #  xlim(-1.5, 1.5) + ylim(1, 4) + 
    scale_fill_distiller(palette = "Spectral", trans="reverse", 
                         limits = c(0, 0.5), 
                         guide = guide_legend(title = expression(hat(p)))) + 
    geom_path(aes(x, y, fill=NULL), data = kZone, 
              lwd = 1.5, col = "blue", linetype = 2) + 
    coord_equal() # + ggtitle("P(Hit|Swing)") +
  #  xlab("Feet from \n Middle of Home Plate") +
  #  ylab("Feet Off Ground") # +
  #   theme(legend.key.size = unit(2, "cm"), 
  #         legend.text = element_text(size = 30),
  #         legend.title = element_text(size = 40),
  #         legend.title.align = 0.25,
  #         axis.title.x = element_text(size=28),
  #         axis.title.y = element_text(size=28),
  #         title = element_text(size = 28),
  #         axis.text = element_text(size = 28)) 
  
} # heat map function 

# Increasing resolution on `batter == 425509' =======
res <- c(2, 4, 8, 16, 32, 64, 128) # granularities 
map_res <- vector("list")
for(i in 1:length(res)){map_res[[i]] <- heatmapper(hitter, NX = res[i], NY = res[i])}

grid.arrange(map_res[[2]], map_res[[3]], map_res[[4]], map_res[[5]], map_res[[6]], map_res[[7]], ncol = 3)

# ggsave("Res_grid.jpg", height = 8.5, width = 8.5)

# ********* Now figure out the pdf() thing ************

# Three lines of attack
# (1) pdf() - make sure using whole page (think plot window smush)
# (2) grid.arrange() - spacing 
# (3) ggsave() - spacing
# (4) ggplot() - spacing

pdf("Test.pdf", width = 8, height = 10, paper = "USr")
grid.arrange(map_res[[2]], map_res[[3]], map_res[[4]], map_res[[5]], map_res[[6]], map_res[[7]], ncol = 3)
dev.off()



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
# doh <- glm(hit ~ px*pz + I(px^2)*I(pz^2), 
#            family = binomial, data = hitter )

mod.polar <- glm(hit ~ r*theta + I(r^2)*I(theta^2), 
                 family = binomial, data = hitter)
summary(mod.polar)

# H-L GoF test ===============
# (pg 133 Myers)
# H_0: Well fit
# H_A: Lack of fit 

library(ResourceSelection)
hoslem.test(mod.polar$y, fitted(mod.polar)) # p-value = 0.8217
hoslem.test(doh$y, fitted(doh))

hitter <- mutate(hitter, p.hat = predict(mod.polar, newdata = righties, type = "response"))

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

sum(as.numeric(is.na(hitzone$p)))

ggplot(aes(px, pz, fill = p), data = hitzone) + 
  geom_tile() + geom_path(aes(x, y, fill=NULL), 
                          data = kZone, lwd = 1.5, 
                          col = "blue", linetype = 2) + 
  coord_equal() + xlim(-1.5, 1.5) + ylim(1, 4) +
  scale_fill_distiller(palette = "Spectral", 
                       limits = c(0.0, .170), trans="reverse",
                       guide = guide_legend(title = expression(hat(p)))) +

# Plot layers ====== #
  ggtitle("Polar Covariate GLM \n Success Probability") +
  xlab("Feet from \n Middle of Home Plate") +
  ylab("Feet Off Ground") +
  theme(legend.key.size = unit(2, "cm"),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 40),
        legend.title.align = 0.25,
        axis.title.x = element_text(size=28),
        axis.title.y = element_text(size=28),
        title = element_text(size = 28),
        axis.text = element_text(size = 28))

# ggsave("Peralta_polar.pdf", height = 8.5, width = 8.5, path = "/Users/ABC/Desktop/Research/Images")

