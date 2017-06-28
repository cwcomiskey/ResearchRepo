#' A variable-resolution heat map generator
#'
#' This function creates variable resolution heat maps accroding to a stopping rule
#' @param cutoff Box subdivisions cease when a box sample size drops below the cutoff
#' @param fun Function to apply to responses in each box
#' @param dataset data frame with spatial data: x-coordinates (x), y coordinates (y), and Bernoulli responses at those locations (res)
#' @max The maximum number of subdivision iterations the algorithm will perform
#' @return A list containing a data frame for each iteration of the subdivision algorithm; and a vector of the number of boxes eligible for subdivision at each iteration.
#'
#' @examples
#' data(dataset)
#' names(dataset) <- c("x", "y", "res", "other")
#' data <- varyres(dataset, mean, 200)
#' mapit(data[[4]])

# devtools::document()
# devtools::install("~/Desktop/ResearchRepo/varyres")
# devtools::install_github('cwcomiskey/ResearchRepo','cwcomiskey')
# devtools::load_all()

# devtools::use_vignette("my-vingette")

# vr <- varyres(hitter, cutoff = 200, max = 6)
# mapit_simple(vr[[5]])

varyres <- function(dataset, fun = mean, cutoff, max = 6){

info_list <- list()       # list of information, for return()
iter <- 0                 # subdivision iterations
elig_boxes <- numeric()   # eligible boxes (count > cutoff)

info <- with(dataset,
             cbind.data.frame(
               x = (max(x)+min(x))/2,       # center
               y = (max(y)+min(y))/2,       # center
               statistic = fun(res),
               count = dim(dataset)[1],          # obs
               width = max(x) - min(x),        # box width
               height = max(y) - min(y),       # box height
               xlb = min(x), xub = max(x),     # box x lower/upper bound
               ylb = min(y), yub = max(y)      # box y lower/upper bound
               )
             )

info_list[[iter + 1]] <- info

while(sum(info$count > cutoff) > 0 & iter < max) {

  iter <- iter + 1             # Count "while" loops (iterations)
  counter <- 0                 # Count box fatalities
  loop_data <- data.frame()    # Record within-loop data

  for(r in 1:dim(info)[1]){    # (r)ows of "info" data frame

    if(info$count[r] > cutoff){

      counter <- counter + 1

      # Create box to subdivide
      box_r <- with(info, dplyr::filter(dataset,
                                 x >= xlb[r] & x <= xub[r],
                                 y >= ylb[r] & y <= yub[r]))

      # x/y box centers for as.image
      xbc <- with(info, seq(xlb[r] - (1e-6), xub[r] + (1e-6), , 5)[c(2,4)]) # x
      ybc <- with(info, seq(ylb[r] - (1e-6), yub[r] + (1e-6), , 5)[c(2,4)]) # y

      # as.image(...), for p_b
      box_r_image <- with(box_r,
                        fields::as.image(res, FUN = fun,
                                 cbind.data.frame(x, y),
                                 grid = list(x = xbc, y = ybc)
                                 )
                        )

      # Organize, save box data to add back
      box_r_info <- with(box_r_image,
                         cbind(expand.grid(x = x, y = y),
                               statistic = as.vector(z),
                               count = as.vector(weights),
                               width = rep(info$width[r]/2, 4),
                               height = rep(info$height[r]/2, 4)
                               )
                         )

      # Remove zero count boxes (gray zero boxes)
      box_r_info <- dplyr::filter(box_r_info,
                                  count != "NA",
                                  statistic != "NA")

      # Add x/y lower/upper bounds for next round
      box_r_info <- dplyr::mutate(box_r_info,
                           xlb = x - width/2,
                           xub = x + width/2,
                           ylb = y - height/2,
                           yub = y + height/2
                           )

      loop_data <- rbind.data.frame(loop_data, box_r_info)

      } # end "if" statement
    }   # end "for" loop

  info <- rbind.data.frame(dplyr::filter(info, count <= cutoff),
                           loop_data)

  info_list[[iter + 1]] <- info
  elig_boxes[iter] <- counter

  } # end "while" loop **

info_list[[iter+2]] <- elig_boxes
return(info_list)
}
