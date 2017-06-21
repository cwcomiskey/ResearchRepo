#' A variable-resolution heat map generator
#'
#' This function creates variable resolution heat maps accroding to a stopping rule
#' @param cutoff Box subdivisions cease when a box sample size drops below the cutoff
#' @param px vector of location horizontal components
#' @param pz vector of location vertical components
#' @param hit Bernoulli responses associated with (px, pz) location
#'
#' @examples
#' data(hitter)
#' data <- with(hitter, varyres(px, pz, hit, 200)
#' mapit(data[[4]])

# devtools::document()

varyres <- function(px, pz, hit, cutoff){

info_list <- list()
iter <- 0
fatalities <- numeric()

info <- with(hitter,
             cbind.data.frame(
               px = (max(px)+min(px))/2,       # center
               pz = (max(pz)+min(pz))/2,       # center
               hitting = round(mean(hit), 3),  # mean
               count = dim(hitter)[1],         # obs
               width = max(px) - min(px),
               height = max(pz) - min(pz),
               xlb = min(px), xub = max(px),
               ylb = min(pz), yub = max(pz)
               )
             )

info_list[[iter + 1]] <- info

while(sum(info$count > cutoff) > 0) {

  iter <- iter + 1             # Count "while" loops (iterations)
  counter <- 0                 # Count box fatalities
  loop_data <- data.frame()     # Record within-loop data

  for(r in 1:dim(info)[1]){    # (r)ows of ABCE

    if(info$count[r] > cutoff){

      counter <- counter + 1

      # Box to subdivide
      box_r <- with(info, dplyr::filter(hitter,
                                 px >= xlb[r] & px <= xub[r],
                                 pz >= ylb[r] & pz <= yub[r]))

      # x/y box centers for as.image
      xbc <- with(info, seq(xlb[r], xub[r], , 5)[c(2,4)]) # x
      ybc <- with(info, seq(ylb[r], yub[r], , 5)[c(2,4)]) # y

      # as.image(...), for p_box
      gridder_r <- with(box_r,
                        fields::as.image(hit,
                                 cbind.data.frame(px, pz),
                                 nx = 2, ny =2,
                                 grid = list(x = xbc, y = ybc)
                                 )
                        )

      # Organize, save box data to add back
      box_r_info <- with(gridder_r, cbind(expand.grid(px = x, pz = y),
                                          hitting = as.vector(z),
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

  info <- rbind.data.frame(dplyr::filter(info, count <= cutoff), loop_data)

  info_list[[iter + 1]] <- info
  fatalities[iter] <- counter

  } # ** END "while" LOOP **

info_list[[iter+2]] <- fatalities
return(info_list)
}
