# Baseball =====

install.packages("RSQLite")
install.packages("dplyr")
install.packages("pitchRx")
install.packages("RMySQL")
install.packages("gridExtra")
install.packages("reshape2")
library("gridExtra")
library("RMySQL")
library("ggplot2")
library("pitchRx")
library("RSQLite")
library(fields)
library("dplyr")
library("reshape2")

# All Data =============================================

db <- src_mysql("Baseball", create = FALSE, user = "root", password = "Koufax32")

atbat <- tbl(db, "atbats") # representation

pitches <- tbl(db, "pitches") # representation
tbl_vars(pitches) # all variables

# Select columns, filter rows - 'pitches' ====== #
sw <- tbl(db, "pitches") %>%
  filter(des %in% c("Foul", "Foul (Runner Going)", 
                     "Foul Tip", "In play, no out", 
                     "In play, out(s)", "Swinging Strike",
                      "Swinging Strike (Blocked)")) %>% 
# filter(pitch_type %in% c("FF", "FT", "SI", "FA")) %>%
  select(px, pz, des, ab_id, pitch_id, id, type) 
tbl_vars(sw)

# Select columns - 'atbats' ======== #
# *ab_id* links 'atbat' and 'pitches' [Analyzing Baseball Data with R]
atbats <- tbl(db, "atbats") %>%
  select(ab_id, stand, batter)

# collect() # --- From REPRESENTATION to DATA ====== #
pitches <- left_join(sw, atbats) %>% 
  collect(n = Inf) # REPRESENTATION becomes DATA; THE KEY STEP

# Summary statistics
length(unique(pitches$batter)) # [1] 3369 hitters
at_bats <- table(pitches$batter)
summary(as.numeric(at_bats))

# Missing locations
sum(is.na(pitches$pz))
pitches <- filter(pitches, px != "NA") # omit missing location pitches

names(pitches)
table(pitches$type)

write.csv(pitches, file = "pitches.csv")

# `Righties' data ===========================================
righties <- pitches %>% filter(stand == "R") %>%
  mutate(hit = as.numeric(des == "In play, no out")) %>%
  filter(abs(px) < 4) # 5 way too big values

mean(righties$hit)

# write.csv(righties, file = "righties.csv")

length(unique(righties$batter)) # 1926 righties

# `Hitter' data  ========================

# righties <- read.csv("~/Desktop/Baseball Research/righties.csv")
# n_s <- as.data.frame(table(righties$batter)) # table of swings
# summary(righties$px); summary(righties$pz)
# names(n_s) <- c("Hitter", "Swings")
# n_s <- arrange(n_s, Swings) # arrange ABs in ascending order
# summary(n_s$Swings) # AB summary statistics
# n <- 9245

# Identify 9245 swing hitter ID ===== #
# hitter <- filter(n_s, Swings %in% 9245) 

# Data on 9245 swings by batter 425509
hitter <- filter(righties, batter == 425509)
hitter <- hitter[,-c(1,2)] # junk columns
hitter <- filter(hitter, abs(px) < 1.75 & pz < 4.25 & pz > 0.5) # wild pitches

# write.csv(hitter, file = "hitter.csv")

# hitter <- read.csv("~/Desktop/Baseball Research/Data/hitter.csv")

# Swing `counts' heat map ===============================

hitter <- read.csv("~/Desktop/Baseball Research/Data/hitter.csv")

kZone <- data.frame(x = c(-0.95, -0.95, 0.95, 0.95, -0.95), 
                    y = c(1.6, 3.5, 3.5, 1.6, 1.6))

gridder <- with(hitter, 
                as.image(Z = hit, ind = cbind.data.frame(px, pz), 
                         nx = 5, ny =5))
    # gridder$x # horizontal box centers    
    # gridder$y # vertical box centers    
    # gridder$z # box BAs
    # gridder$weights # box counts as image matrix
    # gridder$ind # indices 
        # original observations mapped to box address (X,Z)
        # box 1,1 is lower left; box 6,6 is upper right
    # gridder$xd # original observations mapped to box center (px, pz)

# fields.convert.grid(): convert box centers to box boundaries
x.bounds <- fields.convert.grid(gridder$x) # horizontal boundaries
y.bounds <- fields.convert.grid(gridder$y) # vertical boundaries

# tidy data frame (get ready to visualize with ggplot)
counts <- with(gridder, cbind(expand.grid(x,y), as.vector(weights)))
  # as.vector() - convert matrix to vector columnwise
  # expand.grid() - turn two 5x1 box center vectors into 25x2

names(counts) <- c("px", "pz", "count")
counts <- filter(counts, count != "NA")

# Frankenstein heatmaps =======================================

# Subset `hitter' to box (3,4) ================================ #

# Just swings in box (3,4); `x.bounds' from above
hitter.3.4 <- filter(hitter, 
                     px >= x.bounds[3] & px < x.bounds[4], 
                     pz >= y.bounds[4] & pz < y.bounds[5])

# Original box dimensions
x1.0 <- x.bounds[4] - x.bounds[3] # box width
y1.0 <- y.bounds[5] - y.bounds[4] # box height

# Subdivided box dimensions; for geom_tile()
widths.3.4 <- rep(x1.0/2, 4) # subdivided box width
heights.3.4 <- rep(y1.0/2, 4) # subdivided box height

gridder.3.4 <- with(hitter.3.4, 
                    as.image(hit, 
                             cbind.data.frame(px, pz), 
                             nx = 2, ny =2))

counts.3.4 <- with(gridder.3.4, 
                   cbind(expand.grid(x,y), as.vector(weights)))

names(counts.3.4) <- c("px", "pz", "count")
counts.3.4 <- filter(counts.3.4, count != "NA")

# Add box centers for subdivided boxes 
# ** If you run this twice in same session without reloading, it will be wrong because px becomes px_old, and px.3.4 becomes px in the original run
counts.3.4 <- mutate(counts.3.4, px_old = px, pz_old = pz,
                     px = ifelse(px == min(px), 
                                    px + (max(px) - min(px))/4,
                                    px - (max(px) - min(px))/4),
                     pz = ifelse(pz == min(pz), 
                                    pz + (max(pz) - min(pz))/4,
                                    pz - (max(pz) - min(pz))/4) )
                                    

ggplot(counts.3.4, aes(px, pz, fill=count)) + 
  geom_tile(width = widths.3.4, height = heights.3.4) + 
  coord_equal() + 
  scale_fill_distiller(palette = "Spectral", trans="reverse") +
  # geom_path(aes(x, y, fill=NULL), data = kZone, 
  #          lwd = 1.5, col = "blue", linetype = 2) +
  geom_text(aes(fill = count, # print counts
                label = count), size = 3.5)

# `counts': Remove old box (3,4), add new =================== #
x1.0 <- x.bounds[3] - x.bounds[2] # also above
y1.0 <- y.bounds[5] - y.bounds[4] # also above

counts2.0 <- filter(counts, !(px >= x.bounds[3] & px < x.bounds[4] & 
                              pz >= y.bounds[4] & pz < y.bounds[5]))

counts2.0 <- rbind.data.frame(counts2.0, counts.3.4[,-c(4, 5)])
heights <- c(rep(y1.0, 24), rep(y1.0/2, 4))
widths <- c(rep(x1.0, 24), rep(x1.0/2, 4))
counts2.0 <- cbind(counts2.0, heights, widths)

ggplot(counts2.0, aes(px, pz, fill=count)) + geom_tile(width = widths, height = heights) + 
  coord_equal() + 
  scale_fill_distiller(palette = "Spectral", trans="reverse") +
  # geom_path(aes(x, y, fill=NULL), data = kZone, 
  #          lwd = 1.5, col = "blue", linetype = 2) +
  geom_text(aes(fill = count, # print counts
                label = count), size = 3.5)

ggsave("Frankenstein.pdf", height = 8.5, width = 8.5)

# geom_rect() and geom_tile() do the same thing
# geom_rect uses locations of four corners (xmin, xmax, ymin and ymax). 
# geom_tile uses center of tile and its size (x, y, width, height). 

# End Frankenstein heatmaps ===================================

ggplot(counts, aes(px, pz, fill=count)) + geom_tile() + 
  coord_equal() + 
  scale_fill_distiller(palette = "Spectral", trans="reverse") +
  # geom_path(aes(x, y, fill=NULL), data = kZone, 
  #           lwd = 1.5, col = "blue", linetype = 2) +
  geom_text(aes(fill = count, label = count), size = 3.5)

# ggsave("Counts.pdf", height = 8.5, width = 8.5)

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

*********

# Plots: Samples from batter == 425509 ====================== 
n_samp <- 10
samp <- sample_n(hitter, size = n_samp)
heatmapper(samp, 4, 4)

n_samp <- c(50, 100, 500, 1000, 5000) # sample sizes
res <- c(6, 12, 18, 24, 36) # granularities
# Granularity will increase with sample size
map_samp <- vector("list")
for(i in 1:length(n_samp)){map_samp[[i]] <- heatmapper(sample_n(hitter, size = n_samp[i]), NX = res[i], NY = res[i])}

# Increasing sample size and resolution on `batter == 425509'
grid.arrange(map_samp[[1]], map_samp[[2]], map_samp[[3]], map_samp[[4]], map_samp[[5]], ncol = 3)

# Plot =====================================================
coordsR <- with(righties, cbind.data.frame(px, pz))
hitgridR <- with(righties, as.image(hit, coordsR, nx = 90, ny = 72)) 
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

ggsave("Mothership.pdf", height = 8.5, width = 8.5) # righties empirical

# Lefties ===================================================

lefties <- pitches %>% filter(stand == "L") %>%
  mutate(hit = as.numeric(des == "In play, no out")) %>%
  filter(abs(px) < 4) # 5 way too big values

# Plot ================== #

coordsR <- with(lefties, cbind.data.frame(px, pz))
hitgridR <- with(lefties, as.image(hit, coordsR, nx = 90, ny = 72)) 
ABC.R <- with(hitgridR, cbind(expand.grid(x, y), as.vector(z)))
names(ABC.R) <- c("Horizontal", "Vertical", "Hitting")

kZone <- data.frame(x = c(-0.95, -0.95, 0.95, 0.95, -0.95), 
                    y = c(1.6, 3.5, 3.5, 1.6, 1.6))

ggplot(ABC.R, aes(Horizontal, Vertical, fill = Hitting)) + 
  geom_tile() + 
  xlim(-1.5, 1.5) + ylim(1, 4) + 
  scale_fill_distiller(palette = "Spectral", trans="reverse", 
                       limits = c(.18, 0)) + 
  geom_path(aes(x, y, fill=NULL), data = kZone, 
            lwd = 1.5, col = "blue", linetype = 2) + 
  coord_equal() + ggtitle("P(Hit|Swing)")

# Rectangular GLM ===================================

mod <- glm(hit ~ px*pz + 
             I(px^2)*I(pz^2) + 
             I(px^3)*I(pz^3), 
           family = binomial, data = righties)
summary(mod)

zone <- cbind(expand.grid(seq(-1.2, 1.2, length = 100), 
                          seq(1, 3.5, length = 100)))
names(zone) <- c("px", "pz")

zone$hit <- predict(mod, newdata = zone, type = "response")
sum(predict(mod, newdata = righties, type = "response"))


ggplot(zone, aes(px, pz, fill = hit)) + 
  geom_tile() + 
  xlim(-1.5, 1.5) + ylim(1,4) + 
  scale_fill_distiller(palette = "Spectral", 
                       limits = c(.17, 0), trans="reverse") + 
  geom_path(aes(x, y, fill=NULL), data = kZone, 
            lwd = 1.5, col = "blue", linetype = 2) + 
  coord_equal() + ggtitle("P(Hit|Swing), GLM")

ggsave("Mothership_GLM.pdf", height = 10.5, width = 8.5)

# Hosmer-Lemeshow GOF test ============================== #
# LARGE P-VALUE --> ADEQUATE FIT; PG 133 MYERS (GLM)
# H_0: Well fit
# H_A: Lack of fit library(ResourceSelection)
hoslem.test(mod$y, fitted(mod)) # p-value < 2.2e-16
# Reject null: **NOT** well fit!! 

# POLAR GLM =======================================

#    r  <- sqrt( (px+a)^2 + (pz - b)^2)
# theta <- atan2( (pz - b), (px+a) )

# px = r*cos(theta) - a
# pz = r*sin(theta) + b

# About 18 inches off plate; 0.95 + 1.5 ft
# (-2.45, 5.05) ==> (0,0)
# Use (-2, 3.5) - based on loess max (shenanigans)

# Convert to polar, tranlate origin
righties <- righties %>% mutate(
  r = sqrt( (px + 2)^2 + (pz - 3.5)^2), 
  theta = atan2(pz - 3.5, px + 2)) 

# Fit glm ========================== %
mod.polar <- glm(hit ~ r*theta + I(r^2)*I(theta^2) + 
                   I(r^3)*I(theta^3), 
           family = binomial, data = righties)

summary(mod.polar)

# Points for plotting through the hitting zone
hitzone <- cbind(expand.grid(seq(-1.5, 1.5, length = 50), 
                          seq(0, 4, length = 70)))
names(hitzone) <- c("px", "pz")

# Corresponding polar coords
hitzone <- mutate(hitzone, 
                  r = sqrt( (px + 2)^2 + (pz - 3.5)^2), 
                  theta = atan2(pz - 3.5, px + 2) )

hitzone <- hitzone %>% 
  # Fitted values, using glm() fit
  mutate(p = predict(mod.polar, newdata = hitzone, 
                     type = "response")) 
  # type = "response" for `p' instead of `logit'

max(hitzone$p, na.rm = TRUE)

kZone <- data.frame(x = c(-0.95, -0.95, 0.95, 0.95, -0.95), 
                      y = c(1.6, 3.5, 3.5, 1.6, 1.6))

ggplot(aes(px, pz, fill = p), data = hitzone) + 
  geom_tile() + geom_path(aes(x, y, fill=NULL), 
            data = kZone, lwd = 1.5, 
            col = "blue", linetype = 2) + 
  coord_equal() + xlim(-1.5, 1.5) + ylim(1, 4) +
  scale_fill_distiller(palette = "Spectral", 
        limits = c(.170, 0.0), trans="reverse",
        guide = guide_legend(title = expression(hat(p)))) +
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
  
ggsave("Mothership_Polar_GLM.pdf", height = 8.5, width = 8.5)

# Hosmer-Lemeshow GOF test (Polar GLM) (pg 133 Myers)===============
# H_0: Well fit
# H_A: Lack of fit 

library(ResourceSelection)
hoslem.test(mod.polar$y, fitted(mod.polar)) # p-value = 0.1513

righties <- righties %>% mutate(p.hat = predict(mod.polar, newdata = righties, type = "response"))

# Diagnostics - Deviance Residuals qq-plot =======
dr <- residuals(mod.polar, type = "deviance")
DevRes <- cbind.data.frame(dr, righties$p.hat)
DevRes2 <- sort(DevRes, dr)
dr.sort <- sort(dr)
qplot(p.hat, data = righties, xlim = c(0, 0.14), binwidth = 0.001)
qplot(dr, xlim = c(-.6, 0), binwidth = 0.001)
qplot(dr, xlim = c(2, 2.6), binwidth = 0.001)

qplot(sample = dr, stat="qq")
qplot(dr, geom = histogram)

qqnorm(dr)
qqline(dr)

library("binomTools")
?Residuals
sdr <- Residuals(mod.polar, type = "standard.deviance")
qqnorm(sdr)

# Max shenanigans =========================================== 

# Split: Horizontal
# Apply: Max(hit)
# Combine: New df with (x, y) for max(hit)

ABC.R <- add_rownames(ABC.R)
max1 <- filter(ABC.R, Horizontal > -1.6 & 
                 Horizontal < 1.2 & 
                 Vertical < 3.5 & 
                 Vertical > 1.2)
lapply(max1, range)
max1 <- mutate(group_by(max1, Horizontal), 
               m = max(Hitting, na.rm = TRUE)) # Find max
max1 <- max1 %>% filter(Hitting == m) # Retain only max
max2 <- ABC.R[as.numeric(max1$rowname),] # Subset directly

ggplot(max2) + geom_point(aes(Horizontal, Vertical)) + 
  geom_smooth(aes(Horizontal, Vertical), 
              method = "lm", se = FALSE) +
  geom_smooth(aes(Horizontal, Vertical), 
              method = "loess", se = FALSE)
