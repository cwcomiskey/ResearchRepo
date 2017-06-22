#' A variable-resolution heat map generator
#'
#' This function creates variable resolution heat maps accroding to a stopping rule
#' @param cutoff Box subdivisions cease when a box sample size drops below the cutoff
#' @param dataset data frame with spatial data: x-coordinates (px), y coordinates (pz), and Bernoulli responses at those locations (hit)
#'
#' @examples
#' data(dataset)
#' data <- varyres(dataset, 200)
#' mapit(data[[4]])

# devtools::document()
# devtools::install("varyres")
# devtools::install_github('cwcomiskey/ResearchRepo','cwcomiskey')

varyres <- function(dataset, cutoff){

info_list <- list()       # list of information, for return()
iter <- 0                 # subdivision iterations
elig_boxes <- numeric()   # eligible boxes (count > cutoff)

info <- with(dataset,
             cbind.data.frame(
               px = (max(px)+min(px))/2,       # center
               pz = (max(pz)+min(pz))/2,       # center
               statistic = round(mean(hit), 3),  # mean (WILL WANT OTHER 'FUN')
               count = dim(dataset)[1],          # obs
               width = max(px) - min(px),        # box width
               height = max(pz) - min(pz),       # box height
               xlb = min(px), xub = max(px),     # box x lower/upper bound
               ylb = min(pz), yub = max(pz)      # box y lower/upper bound
               )
             )

info_list[[iter + 1]] <- info

while(sum(info$count > cutoff) > 0) {

  iter <- iter + 1             # Count "while" loops (iterations)
  counter <- 0                 # Count box fatalities
  loop_data <- data.frame()    # Record within-loop data

  for(r in 1:dim(info)[1]){    # (r)ows of "info" data frame

    if(info$count[r] > cutoff){

      counter <- counter + 1

      # Create box to subdivide
      box_r <- with(info, dplyr::filter(dataset,
                                 px >= xlb[r] & px <= xub[r],
                                 pz >= ylb[r] & pz <= yub[r]))

      # x/y box centers for as.image
      xbc <- with(info, seq(xlb[r] - (1e-6), xub[r] + (1e-6), , 5)[c(2,4)]) # x
      ybc <- with(info, seq(ylb[r] - (1e-6), yub[r] + (1e-6), , 5)[c(2,4)]) # y

      # as.image(...), for p_b
      box_r_image <- with(box_r,
                        fields::as.image(hit,
                                 # Need to allow 'FUN' other than mean(...)
                                 cbind.data.frame(px, pz),
                                 nx = 2, ny =2,
                                 grid = list(x = xbc, y = ybc)
                                 )
                        )

      # Organize, save box data to add back
      box_r_info <- with(box_r_image,
                         cbind(expand.grid(px = x, pz = y),
                               statistic = as.vector(z),
                               count = as.vector(weights),
                               width = rep(info$width[r]/2, 4),
                               height = rep(info$height[r]/2, 4)
                               )
                         )

      # Add x/y lower/upper bounds for next round
      box_r_info <- dplyr::mutate(box_r_info,
                           xlb = px - width/2,
                           xub = px + width/2,
                           ylb = pz - height/2,
                           yub = pz + height/2
                           )

      loop_data <- rbind.data.frame(loop_data, box_r_info)

      } # ** END "if" STATEMENT **
    }   # ** END "for" LOOP     **

  info <- rbind.data.frame(dplyr::filter(info, count <= cutoff),
                           loop_data)

  info_list[[iter + 1]] <- info
  elig_boxes[iter] <- counter

  } # ** END "while" LOOP **

info_list[[iter+2]] <- elig_boxes
return(info_list)
}
