# Variable-resolution heat maps

# Dependencies, will be in DESCRIPTION file ========== #
# library("ggplot2")
# library(fields)
# library("dplyr")
# library("reshape2")

hitter <- read.csv("~/Desktop/ResearchRepo/Data/hitter.csv") # for DATA file
cutoff<- 200 # Function argument

# Loop 0 (4^0 =    1 possible) =====

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

# Loop 1 (4^1 =    4 possible) ===================================

# as.image(...)
gridder <- with(hitter,
                as.image(hit, cbind.data.frame(px, pz), nx = 2, ny = 2))

xseq <- with(hitter, seq(min(px), max(px), , 5))
yseq <- with(hitter, seq(min(pz), max(pz), , 5))

gridder$xbb <- xseq[c(1,3,5)] # x box boundaries
gridder$ybb <- yseq[c(1,3,5)] # y box boundaries

gridder$x <- xseq[c(2,4)] # x box centers
gridder$y <- yseq[c(2,4)] # y box centers

gridder$bw <- with(gridder, xbb[2] - xbb[1]) # box widths
gridder$bh <- with(gridder, ybb[2] - ybb[1]) # box heights

# Box: centers, p_hat, counts, widths, heights
ABCE <- with(gridder, cbind(
  expand.grid(px = xseq[c(2,4)], pz = yseq[c(2,4)]),
  Hitting = as.vector(z),
  Count = as.vector(weights),
  width = rep(bw, 4),
  height = rep(bh, 4)
                            )
             )

ABCE <- mutate(ABCE,
       xlb = px - width/2, xub = px + width/2,
       ylb = pz - height/2, yub = pz + height/2
)

mapit(ABCE)

# Loop 2 (4^2 =   16 possible) ==============================
counter <- 0
for(i in 1:2){ # Should change: use ABCE, instead of gridder$weight
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
                                            widths = rep(gridder$bw/2, 4),
                                            heights = rep(gridder$bh/2, 4)
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

# Loop 3 (4^3 =   64 possible) ========================================
counter <- 0
LoopData <- data.frame()

for(r in 1:dim(ABCE)[1]){ # Iterate through rows (boxes) of ABCE

  if(ABCE$Count[r] > cutoff){
    counter <- counter + 1     # Counting chopping bloxes

    # Target Box Bounds Lower/Upper
    Dbbl.x <- ABCE[r,"px"] - ABCE[r, "widths"]/2
    Dbbu.x <- ABCE[r,"px"] + ABCE[r, "widths"]/2
    Dbbl.y <- ABCE[r, "pz"] - ABCE[r, "heights"]/2
    Dbbu.y <- ABCE[r, "pz"] + ABCE[r, "heights"]/2

    # Filter original data, to target box
    Box_r <- with(ABCE, filter(hitter,
                               px >=  Dbbl.x & px <= Dbbu.x,
                               pz >=  Dbbl.y & pz <= Dbbu.y))

    # Divided box centers
    Dbc.x <- seq(Dbbl.x, Dbbu.x, , 5)[c(2,4)]
    Dbc.y <- seq(Dbbl.y, Dbbu.y, , 5)[c(2,4)]

    # Griddify sub_sub_box
    gridder_r <- with(Box_r,
                      as.image(hit,
                               cbind.data.frame(px, pz),
                               nx=2, ny=2,
                               grid=list(x = Dbc.x, y=Dbc.y)
                      )
    )

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

# Loop 4 (4^4 =  256 possible) ======================================
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
    Dbc.x <- seq(Dbbl.x, Dbbu.x, , 5)[c(2,4)]
    Dbc.y <- seq(Dbbl.y, Dbbu.y, , 5)[c(2,4)]

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

# Loop 5 (4^5 = 1024 possible) ===========================================
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
