
#------------------------------------------------------------------------------

#' The function creates a block bootstrapped sample of the original foi dataset,
#'  by ensuring that points are sampled at certain geographical distance from the
#'  other points.
#'
#' @title Generate a block bootstrapped sample of the original FOI data.
#'
#' @param data_df dataframe of foi predictions dataset at admin unit 1 resolution.
#'
#' @inheritParams full_routine_bootstrap
#'
#' @export


grid_and_boot <- function(data_df, parms){

  helper <- function(i, data_df, parms) {

    xx <- grid_up(data_df, parms, rnd_dist = FALSE)

    yy <- do_boostrap(xx)

    cbind(unique_id = seq_len(nrow(yy)), yy)

  }


  do_boostrap <- function(dataset){
    #browser()
    idx <- unname(split(seq_len(nrow(dataset)), dataset$cell))
    pick <- sample(idx, size = length(idx), replace = TRUE)
    dataset[unlist(pick), ]
  }


  grid_up <- function(dataset, parms, rnd_dist){

    grid_size <- parms$grid_size

    rd <- 0
    rd2 <- 0

    if (rnd_dist) {

      # draw random distance values
      rd <- runif(n = 1, min = 0, max = grid_size)
      rd2 <- runif(n = 1, min = 0, max = grid_size)

    }

    # add rd to lat.grid and long.grid variables
    dataset$lat.grid <- floor((dataset$latitude - rd) / grid_size)
    dataset$long.grid <- floor((dataset$longitude - rd2) / grid_size)
    min.long <- min(dataset$long.grid)
    width.long <- max(dataset$long.grid) - min.long + 1
    min.lat <- min(dataset$lat.grid)

    dataset$cell <- (dataset$lat.grid - min.lat) * width.long + dataset$long.grid - min.long

    dataset

  }


  no_samples <- parms$no_samples

  loop(seq_len(no_samples),
       helper,
       data_df = foi_data,
       parms = parameters,
       parallel = FALSE)

}
