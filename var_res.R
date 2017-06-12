# Variable-resolution heat maps

# i <- 0
# while(i < 100){ i <- i + 1}

# Dependencies, will be in DESCRIPTION file ========== #
# library("ggplot2")
# library(fields)
# library("dplyr")
# library("reshape2")

hitter <- read.csv("~/Desktop/ResearchRepo/Data/hitter.csv") # for DATA file
cutoff<- 100 # Function argument

ABCE_List <- list()
iter <- 0

# Loop 0 (4^0 =    1 possible) =====

iter <- iter + 1

ABCE <- with(hitter,
             cbind.data.frame(
               px = (max(px)+min(px))/2,       # center
               pz = (max(pz)+min(pz))/2,       # center
               Hitting = round(mean(hit), 3),  # mean
               Count = dim(hitter)[1],         # obs
               width = max(px) - min(px),
               height = max(pz) - min(pz)
             )
)

ABCE_List[[iter]] <- ABCE
mapit(ABCE)

# Loop 1 (4^1 =    4 possible) ===================================

iter <- iter + 1

# as.image(...)
gridder <- with(hitter,
                as.image(hit,
                         cbind.data.frame(px, pz),
                         nx = 2, ny = 2))

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
  expand.grid(px = xseq[c(2,4)],
              pz = yseq[c(2,4)]),
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

ABCE_List[[iter]] <- ABCE

mapit(ABCE)

# Loop 2 (4^2 =   16 possible) ==============================

iter <- iter + 1
counter <- 0
LoopData <- data.frame()

for(r in 1:dim(ABCE)[1]){

  if(ABCE$Count[r] > cutoff){
    counter <- counter + 1  # Box fatalities

      # Filter original data
      Box_r <- with(ABCE, filter(hitter,
                                 px >= xlb[r] & px < xub[r],
                                 pz >= ylb[r] & pz < yub[r]))

      # subdivided box centers
      xbc <- with(ABCE, seq(xlb[r], xub[r], , 5)[c(2,4)]) # x
      ybc <- with(ABCE, seq(ylb[r], yub[r], , 5)[c(2,4)]) # y

      gridder_r <- with(Box_r,
                         as.image(hit,
                                  cbind.data.frame(px, pz),
                                  nx = 2, ny =2,
                                  grid = list(x = xbc, y = ybc)
                                  )
                         )

      ABCE_Box_r <- with(gridder_r, cbind(expand.grid(px = x, pz = y),
                                          Hitting = as.vector(z),
                                          Count = as.vector(weights),
                                          width = rep(ABCE$width[r]/2, 4),
                                          height = rep(ABCE$height[r]/2, 4)
                                          )
                         )

      ABCE_Box_r <- mutate(ABCE_Box_r,
                     xlb = px - width/2, xub = px + width/2,
                     ylb = pz - height/2, yub = pz + height/2)

      LoopData <- rbind.data.frame(LoopData, ABCE_Box_r)
    }
  }

ABCE <- rbind.data.frame(filter(ABCE, Count <= cutoff), LoopData)

ABCE_List[[iter]] <- ABCE

mapit(ABCE)

# Loop 3 (4^3 =   64 possible) ========================================

iter <- iter + 1
counter <- 0
LoopData <- data.frame()

for(r in 1:dim(ABCE)[1]){

  if(ABCE$Count[r] > cutoff){
    counter <- counter + 1

    # Filter original data, to target box
    Box_r <- with(ABCE, filter(hitter,
                               px >=  xlb[r] & px <= xub[r],
                               pz >=  ylb[r] & pz <= yub[r]))

    # Divided box centers
    xbc <- with(ABCE, seq(xlb[r], xub[r], , 5)[c(2,4)]) # x box center
    ybc <- with(ABCE, seq(ylb[r], yub[r], , 5)[c(2,4)]) # y box center

    # Griddify sub_sub_box
    gridder_r <- with(Box_r,
                      as.image(hit,
                               cbind.data.frame(px, pz),
                               nx=2, ny=2,
                               grid=list(x = xbc, y=ybc)
                      )
    )

    # Important-stuff data frame
    ABCE_Box_r <- with(gridder_r,
                       cbind(expand.grid(px = x, pz = y),
                             Hitting = as.vector(z),
                             Count = as.vector(weights),
                             height = rep(ABCE[r,"height"]/2, 4),
                             width = rep(ABCE[r,"width"]/2, 4)))


    ABCE_Box_r <- mutate(ABCE_Box_r,
                         xlb = px - width/2, xub = px + width/2,
                         ylb = pz - height/2, yub = pz + height/2)

    LoopData <- rbind.data.frame(LoopData, ABCE_Box_r)
  }
}

# Remove subdivided, combine with new
ABCE <- rbind.data.frame(filter(ABCE, Count <= cutoff), LoopData)

ABCE_List[[iter]] <- ABCE

mapit(ABCE)

# Loop 4 (4^4 =  256 possible) ======================================
counter <- 0
iter <- iter + 1
LoopData <- data.frame()

for(r in 1:dim(ABCE)[1]){ # Iterate through rows (boxes) of ABCE

  if(ABCE$Count[r] > cutoff){
    counter <- counter + 1

    # Filter original data
    Box_r <- with(ABCE, filter(hitter,
                               px >=  xlb[r] & px <= xub[r],
                               pz >=  ylb[r] & pz <= yub[r]))

    # Divided box centers
    xbc <- with(ABCE, seq(xlb[r], xub[r], , 5))[c(2,4)]
    ybc <- with(ABCE, seq(ylb[r], yub[r], , 5))[c(2,4)]

    # Griddify sub_sub_box
    gridder_r <- with(Box_r,
                      as.image(hit,
                               cbind.data.frame(px, pz),
                               nx=2, ny=2,
                               grid=list(x = xbc, y=ybc)
                      ))


    # Important-stuff data frame
    ABCE_Box_r <- with(gridder_r,
                       cbind(expand.grid(px = x, pz = y),
                             Hitting = as.vector(z),
                             Count = as.vector(weights),
                             height = rep(ABCE[r,"height"]/2, 4),
                             width = rep(ABCE[r,"width"]/2, 4)))

    ABCE_Box_r <- mutate(ABCE_Box_r,
                         xlb = px - width/2, xub = px + width/2,
                         ylb = pz - height/2, yub = pz + height/2)

    LoopData <- rbind.data.frame(LoopData, ABCE_Box_r)
  }
}


# Remove subdivided, combine with new
ABCE <- rbind.data.frame(filter(ABCE, Count <= cutoff), LoopData)

ABCE_List[[iter]] <- ABCE

mapit(ABCE)

# Loop 5 (4^5 = 1024 possible) ===========================================

iter <- iter + 1
counter <- 0
LoopData <- data.frame()

for(r in 1:dim(ABCE)[1]){ # Iterate through rows (boxes) of ABCE

  if(ABCE$Count[r] > cutoff){
    counter <- counter + 1

    # Filter original data
    Box_r <- with(ABCE, filter(hitter,
                               px >=  xlb[r] & px <= xub[r],
                               pz >=  ylb[r] & pz <= yub[r]))

    # Divided box centers
    xbc <- with(ABCE, seq(xlb[r], xub[r], , 5))[c(2,4)]
    ybc <- with(ABCE, seq(ylb[r], yub[r], , 5))[c(2,4)]

    # Griddify sub_sub_box
    gridder_r <- with(Box_r,
                      as.image(hit,
                               cbind.data.frame(px, pz),
                               nx=2, ny=2,
                               grid=list(x = xbc, y=ybc)
                      ))

    # Current box important-stuff data frame
    ABCE_Box_r <- with(gridder_r,
                       cbind(expand.grid(px = x, pz = y),
                             Hitting = as.vector(z),
                             Count = as.vector(weights),
                             height = rep(ABCE[r,"height"]/2, 4),
                             width = rep(ABCE[r,"width"]/2, 4)))

    ABCE_Box_r <- mutate(ABCE_Box_r,
                         xlb = px - width/2, xub = px + width/2,
                         ylb = pz - height/2, yub = pz + height/2)

    LoopData <- rbind.data.frame(LoopData, ABCE_Box_r)
  }
}


# Remove subdivided, combine with new
ABCE <- rbind.data.frame(filter(ABCE, Count <= cutoff), LoopData)
ABCE_List[[iter]] <- ABCE

mapit(ABCE)

mapit(ABCE_List[[6]])

