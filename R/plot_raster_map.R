
#------------------------------------------------------------------------------

#' The function makes and saves locally a raster map using \code{\link{image.plot}}.
#'
#' @title Plot and save a raster map
#'
#' @param pred_df dataframe of data.
#'
#' @param statistic character of the name of the variable to plot.
#'
#' @param my_col colours to use for plotting.
#'
#' @param out_pt output directory.
#'
#' @param out_name output file name.
#'
#' @param z_range range of values to show on plot. Default = NULL.
#'
#' @param shp shapefile object. Default = NULL.
#'
#' @param key_ttl character string of legend title. Default = NULL.
#'
#' @importFrom fields image.plot
#'
#' @importFrom grDevices dev.off png
#'
#' @importFrom graphics image par plot text
#'
#' @export


quick_raster_map <- function(pred_df,
                             statistic,
                             my_col,
                             out_pt,
                             out_name,
                             z_range = NULL,
                             shp = NULL,
                             key_ttle = NULL) {

  # browser()

  n_col <- length(my_col)

  gr_size <- 20

  res <- (1 / 120) * gr_size

  lats <- seq(-90, 90, by = res)
  lons <- seq(-180, 180, by = res)


  # ---------------------------------------- load data


  pred_df$lat.int <- floor(pred_df$latitude * 6 + 0.5)
  pred_df$long.int <- floor(pred_df$longitude * 6 + 0.5)

  lats.int <- lats * 6
  lons.int <- lons * 6

  mat <- matrix(NA, nrow = length(lons) - 1, ncol = length(lats) - 1)

  i.lat <- findInterval(pred_df$lat.int, lats.int)
  i.lon <- findInterval(pred_df$long.int, lons.int)

  mat[cbind(i.lon, i.lat)] <- pred_df[, statistic]

  dir.create(out_pt, FALSE, TRUE)

  png(file.path(out_pt, out_name),
      width = 16,
      height = 5.5,
      units = "cm",
      pointsize = 12,
      res = 300)

  par(mar = c(0,0,0,0), oma = c(0,0,0,0))

  ticks <- pretty(pred_df[, statistic], n = 5)
  lower <- min(ticks)
  upper <- max(ticks)

  if(!is.null(z_range)){

    lower <- min(z_range)
    upper <- max(z_range)

  }

  image(lons,
        lats,
        mat,
        col = my_col,
        zlim = c(lower, upper),
        xlim = c(-180, 180),
        ylim = c(-60, 60),
        asp = 1,
        axes = FALSE)

  if(!is.null(shp)){

    plot(shp, border = "gray40", lwd = 0.05, add = TRUE)

  }

  image.plot(lons,
             lats,
             mat,
             col = my_col,
             zlim = c(lower, upper),
             legend.only = TRUE,
             legend.width = 1,
             legend.shrink = 0.75,
             breaks = seq(lower, upper, length.out = n_col + 1),
             axis.args = list(cex.axis = 0.8),
             smallplot = c(0.025, 0.065, 0.1, 0.5))

  if(!is.null(key_ttle)){

    text(-172, 7, key_ttle, cex = 0.9, adj = c(0, NA))

  }

  par(mar = par("mar"))

  on.exit(dev.off())

}
