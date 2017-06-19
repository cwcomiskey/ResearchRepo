# Variable-resolution heat maps

# Dependencies, will be in DESCRIPTION file ========== #
library("ggplot2")
library(fields)
library("reshape2")
library("dplyr")

hitter <- read.csv("~/Desktop/ResearchRepo/Data/hitter.csv")
cutoff<- 100 # Function argument
ABCE_List <- list()
iter <- 0

# Setup; zeroth iteration ==========
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

while(sum(ABCE$Count > cutoff) > 0) {

  iter <- iter + 1             # Count "while" loops (iterations)
  counter <- 0                 # Count box fatalities
  LoopData <- data.frame()     # Record within-loop data

  for(r in 1:dim(ABCE)[1]){    # (r)ows of ABCE

    if(ABCE$Count[r] > cutoff){

      counter <- counter + 1

      # Box to subdivide
      Box_r <- with(ABCE, filter(hitter,
                                 px >= xlb[r] & px <= xub[r],
                                 pz >= ylb[r] & pz <= yub[r]))

      # x/y box centers for as.image
      xbc <- with(ABCE, seq(xlb[r], xub[r], , 5)[c(2,4)]) # x
      ybc <- with(ABCE, seq(ylb[r], yub[r], , 5)[c(2,4)]) # y

      # as.image(...), for p_box
      gridder_r <- with(Box_r,
                        as.image(hit,
                                 cbind.data.frame(px, pz),
                                 nx = 2, ny =2,
                                 grid = list(x = xbc, y = ybc)
                                 )
                        )

      # Organize, save box data to add back
      ABCE_Box_r <- with(gridder_r, cbind(expand.grid(px = x, pz = y),
                                          Hitting = as.vector(z),
                                          Count = as.vector(weights),
                                          width = rep(ABCE$width[r]/2, 4),
                                          height = rep(ABCE$height[r]/2, 4)
                                          )
                         )

      # Add x/y lower/upper bounds for next round
      ABCE_Box_r <- mutate(ABCE_Box_r,
                           xlb = px - width/2,
                           xub = px + width/2,
                           ylb = pz - height/2,
                           yub = pz + height/2
                           )

      LoopData <- rbind.data.frame(LoopData, ABCE_Box_r)
      } # ** END "if" STATEMENT **
    }   # ** END "for" LOOP     **

  ABCE <- rbind.data.frame(filter(ABCE, Count <= cutoff), LoopData)

  ABCE_List[[iter + 1]] <- ABCE } # ** END "while" LOOP **

# mapit(ABCE_List[[6]])
