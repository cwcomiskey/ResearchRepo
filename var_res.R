# Variable-resolution heat maps

# Dependencies, will be in DESCRIPTION file ========== #
# library("ggplot2")
# library(fields)
# library("dplyr")
# library("reshape2")

hitter <- read.csv("~/Desktop/ResearchRepo/Data/hitter.csv") # for DATA file
cutoff<- 200 # Function argument

# Loop Zero =====

# GrZero <- summarise(hitter,min.x = min(px),max.x = max(px), min.y = min(pz),max.y = max(pz), Hitting = mean(hit), Count = dim(hitter)[1], x = (max(px)+min(px))/2, y = (max(pz)+min(pz))/2)

# ABCE <- with(GrZero,cbind.data.frame(px = x, pz = y,Hitting = round(Hitting, 2),Count,widths = max.x - min.x,heights = max.y - min.y))

ABCE <- with(hitter,
             cbind.data.frame(
               px = (max(px)+min(px))/2,       # center
               pz = (max(pz)+min(pz))/2,       # center
               Hitting = round(mean(hit), 3),  # mean
               Count = dim(hitter)[1],         # obs
               widths = max(px) - min(px),
               heights = max(pz) - min(pz)
             )
)

mapit(ABCE)

# Loop 1 (4) ===================================

# ABCE <-  with(GrZero, mutate(ABCE, width = max.x - min.x, height = max.y - min.y))

# as.image(...)
gridder <- with(hitter,
                as.image(hit, cbind.data.frame(px, pz), nx = 2, ny = 2))

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
  widths = rep(bw, 4),
  heights = rep(bh, 4)
                            )
             )

mapit(ABCE, widths, heights)

# Loop 2 (16) ===================================
counter <- 0
for(i in 1:2){
  for(j in 1:2){
    if(gridder$weights[i,j] > cutoff){ # Why gridder, not ABCE?

      counter <- counter + 1

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
                                            Count = as.vector(weights),
                                            widths = rep(gridder$bw/2, 2),
                                            heights = rep(gridder$bh/2, 2)
                                            )
                          )

      # ABCE: Remove old box (i,j), add new boxes ======== #
      ABCE <- with(gridder, filter(ABCE, !(px == x[i] & pz == y[j]))) # remove
      ABCE <- rbind.data.frame(ABCE, ABCE_Box_ij) # add

    }
  }
}

# # # Solve: 2*2 - x + 4*x = dim(ABCE)[1]
# sdb <- (dim(ABCE)[1] - 4)/3 # number of subdivided boxes; replace with "counter"
#
# # # New box dimensions for ggplot() and geom_tile()
# widths <- with(gridder, c(rep(bw, 4 - counter), rep(bw/2, 4*counter)))
# heights <- with(gridder, c(rep(bh, 4 - counter), rep(bh/2, 4*counter)))
# ABCE <- cbind(ABCE, heights, widths)

mapit(ABCE)

# Loop 3 (64) ========================================

dim(ABCE)[1] # [1] 16
counter <- 0
LoopData <- data.frame()

for(r in 1:dim(ABCE)[1]){ # Iterate through rows (boxes) of ABCE

  if(ABCE$Count[r] > cutoff){
    counter <- counter + 1

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

    # # For ABCE, for book keeping, for plotting
    # heights <- rep(ABCE[r,"heights"]/2, 4)
    # widths <- rep(ABCE[r,"widths"]/2, 4)

    # Important-stuff data frame
    ABCE_Box_r <- with(gridder_r,
                       cbind(expand.grid(px = x, pz = y),
                             Hitting = as.vector(z),
                             Count = as.vector(weights),
                             heights = rep(ABCE[r,"heights"]/2, 4),
                             widths = rep(ABCE[r,"widths"]/2, 4)))

    LoopData <- rbind.data.frame(LoopData, ABCE_Box_r)
  }
}

# Remove subdivided, combine with new
ABCE <- rbind.data.frame(filter(ABCE, Count <= cutoff), LoopData)

mapit(ABCE)

# Loop 4 (256) ======================================
counter <- 0
dim(ABCE)[1] # [1] 49 (of 64 possible)

LoopData <- data.frame()

for(r in 1:dim(ABCE)[1]){ # Iterate through rows (boxes) of ABCE

  if(ABCE$Count[r] > cutoff){
    counter <- counter + 1

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

    # # For ABCE, for book keeping, for plotting
    # heights <- rep(ABCE[r,5]/2, 4)
    # widths <- rep(ABCE[r,6]/2, 4)

    # Important-stuff data frame
    ABCE_Box_r <- with(gridder_r,
                       cbind(expand.grid(px = x, pz = y),
                             Hitting = as.vector(z),
                             Count = as.vector(weights),
                             heights = rep(ABCE[r,"heights"]/2, 4),
                             widths = rep(ABCE[r,"widths"]/2, 4)))

    LoopData <- rbind.data.frame(LoopData, ABCE_Box_r)
  }
}

sum(as.numeric(ABCE$Count > cutoff)) # number boxes subdivided

# Remove subdivided, combine with new
ABCE <- rbind.data.frame(filter(ABCE, Count <= cutoff), LoopData)

mapit(ABCE)

# Loop 5 (4^5) ===========================================
counter <- 0
LoopData <- data.frame()

for(r in 1:dim(ABCE)[1]){ # Iterate through rows (boxes) of ABCE

  if(ABCE$Count[r] > cutoff){
    counter <- counter + 1

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
#
#     # For ABCE, for book keeping, for plotting
#     heights <- rep(ABCE[r,5]/2, 4)
#     widths <- rep(ABCE[r,6]/2, 4)

    # Current box important-stuff data frame
    ABCE_Box_r <- with(gridder_r,
                       cbind(expand.grid(px = x, pz = y),
                             Hitting = as.vector(z),
                             Count = as.vector(weights),
                             heights = rep(ABCE[r,"heights"]/2, 4),
                             widths = rep(ABCE[r,"widths"]/2, 4)))

    LoopData <- rbind.data.frame(LoopData, ABCE_Box_r)
  }
}

# Remove subdivided, combine with new
ABCE <- rbind.data.frame(filter(ABCE, Count <= cutoff), LoopData)

mapit(ABCE)
