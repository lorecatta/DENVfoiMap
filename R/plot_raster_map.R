
#------------------------------------------------------------------------------

#' The function assignes equally spaced latitude and longitude coordinates to
#' each 1/6 degree pixel and shapes the output for making a map using \code{\link{ggplot}}.
#'
#' @title Preprocess data for ggplot map
#'
#' @param pred2 dataframe of data.
#'
#' @param var_to_plot character of the name of the variable to plot.
#'
#' @inheritParams full_routine_bootstrap
#'
#' @importFrom methods as
#'
#' @importFrom raster raster
#'
#' @export


map_preprocess <- function(pred2, var_to_plot, parms) {

  prediction_df_to_matrix <- function(lats, lons, df_long, statsc){

    df_long$lat.int <- floor(df_long$latitude * 6 + 0.5)
    df_long$long.int <- floor(df_long$longitude * 6 + 0.5)

    lats.int <- lats * 6
    lons.int <- lons * 6

    mat <- matrix(NA, nrow = length(lons), ncol = length(lats))

    i.lat <- findInterval(df_long$lat.int, lats.int)
    i.lon <- findInterval(df_long$long.int, lons.int)

    mat[cbind(i.lon, i.lat)] <- df_long[, statsc]

    mat
  }

  bbox <- parms$coord_limits

  x1 <- bbox[1]
  x2 <- bbox[2]
  y1 <- bbox[3]
  y2 <- bbox[4]

  gr_size <- parms$resample_grid_size

  res <- (1 / 120) * gr_size

  lats <- seq(y1, y2, by = res)
  lons <- seq(x1, x2, by = res)

  pred_mat <- prediction_df_to_matrix(lats, lons, pred2, var_to_plot)

  pred_mat_ls <- list(x = lons,
                      y = lats,
                      z = pred_mat)

  pred_r_mat <- raster(pred_mat_ls)

  pred_r_spdf <- as(pred_r_mat, "SpatialPixelsDataFrame")

  as.data.frame(pred_r_spdf)

}


#------------------------------------------------------------------------------

#' The function makes and saves locally a raster map using \code{\link{ggplot}}.
#'
#' @title Plot and save a raster map
#'
#' @param pred_df dataframe of data.
#'
#' @param my_col colours to use for plotting.
#'
#' @param ttl character string of legend title. Default = NULL.
#'
#' @inheritParams full_routine_bootstrap
#'
#' @importFrom ggplot2 ggplot geom_tile scale_fill_gradientn guide_colourbar
#' element_blank coord_fixed
#'
#' @export


quick_raster_map <- function(pred_df, my_col, ttl = NULL, parms) {

  bbox <- parms$coord_limits

  x1 <- bbox[1]
  x2 <- bbox[2]
  y1 <- bbox[3]
  y2 <- bbox[4]

  ggplot() +
    geom_tile(data = pred_df, aes(x = x, y = y, fill = layer)) +
    scale_fill_gradientn(colours = my_col,
                         na.value = "grey80",
                         guide = guide_colourbar(title = ttl,
                                                 barwidth = 1.5,
                                                 barheight = 4.5)) +
    coord_fixed(xlim = c(x1, x2), ylim = c(y1, y2)) +
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          legend.justification = c(0, 0),
          legend.position = c(0, 0),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          panel.background = element_blank(),
          panel.border = element_blank())

}
