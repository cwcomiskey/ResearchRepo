# Frankenstein heatmaps =======================================


# Swing `counts'  ===============================

hitter <- read.csv("~/Desktop/Baseball Research/Data/hitter.csv")

kZone <- data.frame(x = c(-0.95, -0.95, 0.95, 0.95, -0.95), 
                    y = c(1.6, 3.5, 3.5, 1.6, 1.6))

gridder <- with(hitter, 
                as.image(hit, cbind.data.frame(px, pz), nx = 5, ny =5))
# gridder$weights # box counts as image matrix
# gridder$z # box BAs
# gridder$x # horizontal box centers    
# gridder$y # vertical box centers
# gridder$ind # original observations mapped to box address
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

# Subset `hitter' to box (3,4) ================================ 

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
                    as.image(hit, cbind.data.frame(px, pz), nx = 2, ny =2))

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


# `counts': Remove old box (3,4), add new =================== 
x1.0 <- x.bounds[3] - x.bounds[2] # also above
y1.0 <- y.bounds[5] - y.bounds[4] # also above

counts2.0 <- filter(counts, !(px >= x.bounds[3] & px < x.bounds[4] & 
                                pz >= y.bounds[4] & pz < y.bounds[5]))

counts2.0 <- rbind.data.frame(counts2.0, counts.3.4[,-c(4, 5)])
heights <- c(rep(y1.0, 24), rep(y1.0/2, 4))
widths <- c(rep(x1.0, 24), rep(x1.0/2, 4))
counts2.0 <- cbind(counts2.0, heights, widths)

# geom_rect() and geom_tile() do the same thing
# geom_rect uses locations of four corners (xmin, xmax, ymin and ymax). 
# geom_tile uses center of tile and its size (x, y, width, height). 


