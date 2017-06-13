# Variable-resolution heat maps

# i <- 0
# while(i < 100){ i <- i + 1}

# Dependencies, will be in DESCRIPTION file ========== #
library("ggplot2")
library(fields)
library("dplyr")
library("reshape2")

# WHILE LOOP!! ======

hitter <- read.csv("~/Desktop/ResearchRepo/Data/hitter.csv") # for DATA file
cutoff<- 100 # Function argument


# Iteration 0 =================== #
ABCE_List <- list()
iter <- 0

ABCE <- with(hitter,
             cbind.data.frame(
               px = (max(px)+min(px))/2,       # center
               pz = (max(pz)+min(pz))/2,       # center
               Hitting = round(mean(hit), 3),  # mean
               Count = dim(hitter)[1],         # obs
               width = max(px) - min(px),
               height = max(pz) - min(pz),
               xlb = min(px), xub = max(px),
               ylb = min(pz), yub = max(pz)
             )
)

ABCE_List[[iter + 1]] <- ABCE
mapit(ABCE)

# While loop ======== #
while(sum(ABCE$Count > cutoff) > 0) {

  iter <- iter + 1
  counter <- 0
  LoopData <- data.frame()

  for(r in 1:dim(ABCE)[1]){

    if(ABCE$Count[r] > cutoff){
      counter <- counter + 1  # Box fatalities

      # For subdivision
      Box_r <- with(ABCE, filter(hitter,
                                 px >= xlb[r] & px < xub[r],
                                 pz >= ylb[r] & pz < yub[r]))

      # For as.image
      xbc <- with(ABCE, seq(xlb[r], xub[r], , 5)[c(2,4)]) # x
      ybc <- with(ABCE, seq(ylb[r], yub[r], , 5)[c(2,4)]) # y

      # as.image(...), for data as image conversion
      gridder_r <- with(Box_r,
                        as.image(hit,
                                 cbind.data.frame(px, pz),
                                 nx = 2, ny =2,
                                 grid = list(x = xbc, y = ybc)
                        )
      )

      # Collate, store box data - to add back
      ABCE_Box_r <- with(gridder_r, cbind(expand.grid(px = x, pz = y),
                                          Hitting = as.vector(z),
                                          Count = as.vector(weights),
                                          width = rep(ABCE$width[r]/2, 4),
                                          height = rep(ABCE$height[r]/2, 4)
      )
      )

      # Add a couple more columns
      ABCE_Box_r <- mutate(ABCE_Box_r,
                           xlb = px - width/2, xub = px + width/2,
                           ylb = pz - height/2, yub = pz + height/2)

      LoopData <- rbind.data.frame(LoopData, ABCE_Box_r)

    }}

  ABCE <- rbind.data.frame(filter(ABCE, Count <= cutoff), LoopData)

  ABCE_List[[iter + 1]] <- ABCE }

mapit(ABCE_List[[6]])

# YES!! END WHILE STATEMENT =======



hitter <- read.csv("~/Desktop/ResearchRepo/Data/hitter.csv") # for DATA file
cutoff<- 100 # Function argument

# Iteration 0 ==========
ABCE_List <- list()
iter <- 0

ABCE <- with(hitter,
             cbind.data.frame(
               px = (max(px)+min(px))/2,       # center
               pz = (max(pz)+min(pz))/2,       # center
               Hitting = round(mean(hit), 3),  # mean
               Count = dim(hitter)[1],         # obs
               width = max(px) - min(px),
               height = max(pz) - min(pz),
               xlb = min(px), xub = max(px),
               ylb = min(pz), yub = max(pz)
             )
)

ABCE_List[[iter + 1]] <- ABCE
mapit(ABCE)
# while(sum(ABCE$Count > cutoff) > 0)

# Loop 1 (4^1 =    4 possible) ===================================

iter <- iter + 1
counter <- 0
LoopData <- data.frame()

for(r in 1:dim(ABCE)[1]){

  if(ABCE$Count[r] > cutoff){
    counter <- counter + 1  # Box fatalities

    # For subdivision
    Box_r <- with(ABCE, filter(hitter,
                               px >= xlb[r] & px < xub[r],
                               pz >= ylb[r] & pz < yub[r]))

    # For as.image
    xbc <- with(ABCE, seq(xlb[r], xub[r], , 5)[c(2,4)]) # x
    ybc <- with(ABCE, seq(ylb[r], yub[r], , 5)[c(2,4)]) # y

# as.image(...), for data as image conversion
gridder_r <- with(Box_r,
                  as.image(hit,
                           cbind.data.frame(px, pz),
                           nx = 2, ny =2,
                           grid = list(x = xbc, y = ybc)
                  )
)

# Collate, store box data - to add back
ABCE_Box_r <- with(gridder_r, cbind(expand.grid(px = x, pz = y),
                                    Hitting = as.vector(z),
                                    Count = as.vector(weights),
                                    width = rep(ABCE$width[r]/2, 4),
                                    height = rep(ABCE$height[r]/2, 4)
                                    )
                   )

# Add a couple more columns
ABCE_Box_r <- mutate(ABCE_Box_r,
                     xlb = px - width/2, xub = px + width/2,
                     ylb = pz - height/2, yub = pz + height/2)

LoopData <- rbind.data.frame(LoopData, ABCE_Box_r)

  }}

ABCE <- rbind.data.frame(filter(ABCE, Count <= cutoff), LoopData)

ABCE_List[[iter + 1]] <- ABCE

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

ABCE_List[[iter + 1]] <- ABCE

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

ABCE_List[[iter + 1]] <- ABCE

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

ABCE_List[[iter + 1]] <- ABCE

mapit(ABCE)

# Loop 5 (4^5 = 1024 possible) ======================================

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
ABCE_List[[iter + 1]] <- ABCE

mapit(ABCE)

mapit(ABCE_List[[6]])



ggplot(ABCE, aes(px, pz, fill=Hitting)) +
  with(ABCE, geom_tile(width = width, height = height)) +
  coord_equal() +
  scale_fill_distiller(palette = "Spectral")
