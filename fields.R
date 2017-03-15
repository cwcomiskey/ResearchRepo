# fields.convert.grid() =====================================
# midpoints to boundaries

# fields.convert.grid() : Takes a vector of n values assumed to be midpoints of a grid and returns the n+1 boundaries. See how this is used in discretize.image() with the cut function. This function will handle unequally spaced grid values.

function (midpoint.grid) 
{
  x <- sort(midpoint.grid)
  n <- length(x)
  xi <- (x[2:n] + x[1:(n - 1)])/2 # all the middle boundaries
  x1 <- x[1] - (x[2] - x[1])/2 # left boundary
  xnp1 <- x[n] + (x[n] - x[(n - 1)])/2 # right boundary
  c(x1, xi, xnp1) # combine
}
<environment: namespace:fields>

# discretize.image() =================================

# for experimentation
x <- with(hitter, cbind.data.frame(px, pz))

# discretize.image: Takes a vector of locations and a 2-d grid and figures out to which boxes they belong. The output matrix 'ind' (indexes) has the grid locations. (No observation at locations, as in `as.image()' )

# expand = A scalar or two column vector that will expand the grid beyond the range of the observations.

function (x, 
          m = 5, n = 5, 
          grid = NULL, 
          expand = c(1, 1), 
          boundary.grid = FALSE, 
          na.rm = TRUE) 
{
  out <- list()
  
  if (length(expand) == 1) 
    expand <- rep(expand, 2)
  
  # range() returns a vector containing the minimum and maximum
  
  # Create grid center points, named `grid'
  if (is.null(grid)) {
    grid <- list()
    xr <- range(x[, 1], na.rm = na.rm) # x range
    deltemp <- (xr[2] - xr[1]) * (expand[1] - 1) * 0.5 
        # deltemp = zero by default
    grid$x <- seq(xr[1] - deltemp, xr[2] + deltemp, , m)
              # > seq(1, 10, ,3)
              # [1]  1.0  5.5 10.0
    yr <- range(x[, 2], na.rm = na.rm) # y range
    deltemp <- (yr[2] - yr[1]) * (expand[2] - 1) * 0.5
    grid$y <- seq(yr[1] - deltemp, yr[2] + deltemp, , n)
              # > seq(1, 10, ,3)
              # [1]  1.0  5.5 10.0
  }
  
  # GRID (box) BOUNDARIES 
  # If not provided, make 'em from midpoints
  if (!boundary.grid) {
    xcut <- fields.convert.grid(grid$x)
    ycut <- fields.convert.grid(grid$y)
  }
  # ...otherwise, just grab 'em
  else {
    xcut <- grid$x
    ycut <- grid$y
  }
  
  # cut(x, ...) divides the range of x into intervals and *codes the values in x according to which interval they fall.* The leftmost interval corresponds to level one, the next leftmost to level two and so on.
  
    # ADDRESS (index) of EVERY POINT in x
    # xcut, ycut = box coundaries
  index <- list(as.numeric(cut(x[, 1], xcut)), # horiz (x) box address for EVERY POINT
                as.numeric(cut(x[, 2], ycut))) # vert (y) box address of EVERY POINT

# > head(cbind(index[[1]], index[[2]])) # basically `index'
#        [,1] [,2]
#   [1,]    3    3
#   [2,]    3    3
#   [3,]    3    3
#   [4,]    4    3
#   [5,]    3    3
#   [6,]    3    3
  
  m <- length(xcut) - 1 # number of horiz boxes
  n <- length(ycut) - 1 # number of vert boxes
  grid <- grid
  tempHist <- table(index[[1]], index[[2]]) # table of box counts!

# > tempHist    
#        1    2    3    4    5
#   1    2   10   85   48    3
#   2    8  328 1135  676   38
#   3   33  849 2185 1190   88
#   4   43  581 1236  476   19
#   5    3   46   77   17    1
  
  ix <- as.numeric(dimnames(tempHist)[[1]]) # x indices (box numbers)
  iy <- as.numeric(dimnames(tempHist)[[2]]) # y indices (box numbers)
  hist <- matrix(0, m, n) # empty matrix
  hist[ix, iy] <- tempHist # Matrix of counts (I think)

#   > hist[ix, iy]  
#        [,1] [,2] [,3] [,4] [,5]
#   [1,]    2   10   85   48    3
#   [2,]    8  328 1135  676   38
#   [3,]   33  849 2185 1190   88
#   [4,]   43  581 1236  476   19
#   [5,]    3   46   77   17    1
  
  # IF USING BOX CENTERS, map all points to box center
  if (!boundary.grid) { 
    loc <- cbind(grid$x[index[[1]]], grid$y[index[[2]]]) 
  }
  # ...if not, nevermind; nothing additional to map to
  else {
    out$loc <- NA
  }
  
#   > head(loc)
#          [,1]  [,2]
#   [1,] 0.0130 2.374
#   [2,] 0.0130 2.374
#   [3,] 0.0130 2.374
#   [4,] 0.8805 2.374
#   [5,] 0.0130 2.374
#   [6,] 0.0130 2.374
  
  return(list(m = m, n = n, 
              grid = grid, # grid boxes (centers, I think)
              index = index, # point to box mapping (BY INDEX: i.e. (3,4))
              ix = ix, # x indices
              iy = iy, # y indices
              hist = hist, # box counts
              loc = loc)) # point to box mapping (BY CENTER: i.e. (1.353, 2.471))
}
<environment: namespace:fields>

# as.image() ================================

# with(hitter, as.image(Z = hit, ind = cbind.data.frame(px, pz), nx = 5, ny =5))

Z <- hitter$hit
ind <- cbind.data.frame(hitter$px, hitter$pz)
nx <- 5; ny <- 5

# Discretizes a set of 2-d locations to a grid (as.image()), and produces a image object with the z values in the right cells. For cells with more than one Z value the average is used.

# boundary.grid	- If FALSE grid points are assumed to be the grid midpoints. If TRUE they are the grid box boundaries. 

# grid	- A list with components x and y of equally spaced values describing the centers of the grid points (or boundaries if `boundary.grid = TRUE).

function (Z, 
          ind = NULL, # box index of points in Z
          grid = NULL, # see above
          x = NULL, 
          weights = rep(1, length(Z)), 
          na.rm = FALSE, 
          nx = 64, 
          ny = 64, 
          boundary.grid = FALSE, 
          nrow = NULL, 
          ncol = NULL, 
          FUN = NULL) 
{
  Z <- c(Z) # c() - combine values into vector or list
  
  if (!is.null(ind)) {
    x <- ind
  }
  
  if (!is.null(nrow) & !is.null(ncol)) {
    nx <- nrow # nrow = depreciated (obsolete, replaced)
    ny <- ncol # ncol = depreciated
  }
  
  if (any(is.na(weights)) | any(is.na(c(x)))) {
    stop("missing values in weights or x")
  }
  
  temp <- discretize.image(x, 
                           m = nx, 
                           n = ny, 
                           grid = grid, 
                           boundary.grid = boundary.grid)
      # ``The function discretize.image() is a useful tool for "registering" irregular 2-d points to a grid.''
  
#   return(list(m = m, n = n, 
#               grid = grid, # grid boxes (centers, I think)
#               index = index, # point to box mapping (BY INDEX: i.e. (3,4))
#               ix = ix, # x indices
#               iy = iy, # y indices
#               hist = hist, # box counts
#               loc = loc)) # point--> box center mapping
  
  grid <- temp$grid
  w <- z <- matrix(NA, nrow = temp$m, ncol = temp$n)
  tempw <- tapply(weights, temp$index, sum, na.rm = FALSE)
  if (is.null(FUN)) {
    tempz <- tapply(Z * weights, temp$index, sum, na.rm = FALSE)
    tempz <- tempz/tempw
  }
  else {
    tempz <- tapply(Z, temp$index, FUN, na.rm = FALSE)
  }
  
  z[temp$ix, temp$iy] <- tempz
  w[temp$ix, temp$iy] <- tempw
  call <- match.call()
  list(x = grid$x, 
       y = grid$y, 
       z = z, 
       call = call, 
       ind = cbind(temp$index[[1]], 
                  temp$index[[2]]), 
       weights = w, 
       xd = cbind(grid$x[temp$index[[1]]], 
                  grid$y[temp$index[[2]]]), 
       call = match.call(), 
       FUN = FUN)
}

<environment: namespace:fields>

