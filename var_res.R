# Variable-resolution heat maps

# Dependencies, will be in DESCRIPTION file ========== #
library("ggplot2")
library(fields)
library("dplyr")
library("reshape2")

hitter <- read.csv("~/Desktop/ResearchRepo/Data/hitter.csv") # for DATA file
cutoff<- 200 # Function argument

# Loop Zero =====

# GrZero <- summarise(hitter,
#                     min.x = min(px),
#                     max.x = max(px), # x bounds
#                     min.y = min(pz),
#                     max.y = max(pz), # y bounds
#                     Hitting = mean(hit),     # BA
#                     Count = dim(hitter)[1],  # Count
#                     x = (max(px)+min(px))/2, # x center
#                     y = (max(pz)+min(pz))/2) # y center

# ABCE <- with(GrZero,
#              cbind.data.frame(
#                px = x, pz = y,
#                Hitting = round(Hitting, 2),
#                Count,
#                widths = max.x - min.x,
#                heights = max.y - min.y
#                )
#              )

ABCE <- with(hitter,
             cbind.data.frame(
               px = (max(px)+min(px))/2,
               pz = (max(pz)+min(pz))/2,
               Hitting = round(mean(hit), 3),
               Count = dim(hitter)[1],
               widths = max(px) - min(px),
               heights = max(pz) - min(pz)
             )
)

mapit(ABCE)

# Loop 1 (4) ===================================

# ABCE <-  with(GrZero, mutate(ABCE, width = max.x - min.x, height = max.y - min.y))

# as.image(...)
gridder <- with(hitter, as.image(hit, cbind.data.frame(px, pz), nx = 2, ny = 2))

# Manually add bounds and centers
# gridder$xbb <- with(GrZero, seq(min.x, max.x, , 5))[c(1,3,5)] # x box boundaries
# gridder$ybb <- with(GrZero, seq(min.y, max.y, , 5))[c(1,3,5)] # y box boundaries
# gridder$x <- with(GrZero, seq(min.x, max.x, , 5))[c(2,4)] # x box centers
# gridder$y <- with(GrZero, seq(min.y, max.y, , 5))[c(2,4)] # y box centers

gridder$xbb <- with(hitter, seq(min(px), max(px), , 5))[c(1,3,5)] # x box boundaries
gridder$ybb <- with(hitter, seq(min(pz), max(pz), , 5))[c(1,3,5)] # y box boundaries

gridder$x <- with(hitter, seq(min(px), max(px), , 5))[c(2,4)] # x box centers
gridder$y <- with(hitter, seq(min(pz), max(pz), , 5))[c(2,4)] # y box centers

gridder$bw <- gridder$xbb[2] - gridder$xbb[1] # box widths
gridder$bh <- gridder$ybb[2] - gridder$ybb[1] # box heights

# Create important-stuff matrix: box centers, BA, counts
ABCE <- with(gridder, cbind(
  expand.grid(px = x, pz = y),
  Hitting = as.vector(z),
  Count = as.vector(weights),
  widths = rep(bw, 2),
  heights = rep(bh, 2)
                            )
             )

mapit(ABCE, widths, heights)

# Loop 2 (16) ===================================

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

      gridder_ij <- with(Box_ij,
                         as.image(hit,
                                  cbind.data.frame(px, pz),
                                  nx = 2, ny =2,
                                  grid = list(x = sbc.x, y = sbc.y)
                                  )
                         )

      ABCE_Box_ij <- with(gridder_ij, cbind(expand.grid(px = x, pz = y),
                                            Hitting = as.vector(z),
                                            Count = as.vector(weights)
                                            )
                          )

      # names(ABCE_Box_ij) <- c("px", "pz", "Hitting", "Count")

      # ABCE: Remove old box (i,j), add new boxes ======== #
      ABCE <- with(gridder, filter(ABCE, !(px == x[i] & pz == y[j]))) # remove
      ABCE <- rbind.data.frame(ABCE, ABCE_Box_ij) # add

    }
  }
}

# Solve: 2*2 - x + 4*x = dim(ABCE)[1]
sdb <- (dim(ABCE)[1] - 4)/3 # number of subdivided boxes

# New box dimensions for ggplot() and geom_tile()
widths <- with(gridder, c(rep(bw, 4 - sdb), rep(bw/2, 4*sdb)))
heights <- with(gridder, c(rep(bh, 4 - sdb), rep(bh/2, 4*sdb)))
ABCE <- cbind(ABCE, heights, widths)

mapit(ABCE)

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
