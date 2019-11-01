
#' #------------------------------------------------------------------------------
#'
#' #' The function makes and saves locally a raster map using \code{\link{image.plot}}.
#' #'
#' #' @title Plot and save a raster map
#' #'
#' #' @param pred_df dataframe of data.
#' #'
#' #' @param statistic character of the name of the variable to plot.
#' #'
#' #' @param my_col colours to use for plotting.
#' #'
#' #' @param out_pt output directory.
#' #'
#' #' @param out_name output file name.
#' #'
#' #' @param z_range range of values to show on plot. Default = NULL.
#' #'
#' #' @param shp shapefile object. Default = NULL.
#' #'
#' #' @param key_ttle character string of legend title. Default = NULL.
#' #'
#' #' @importFrom fields image.plot
#' #'
#' #' @importFrom grDevices dev.off png
#' #'
#' #' @importFrom graphics image par plot text
#' #'
#' #' @export
#'
#'
#' quick_raster_map <- function(pred_df,
#'                              statistic,
#'                              my_col,
#'                              out_pt,
#'                              out_name,
#'                              z_range = NULL,
#'                              shp = NULL,
#'                              key_ttle = NULL) {
#'
#'   # browser()
#'
#'   n_col <- length(my_col)
#'
#'   gr_size <- 20
#'
#'   res <- (1 / 120) * gr_size
#'
#'   lats <- seq(-90, 90, by = res)
#'   lons <- seq(-180, 180, by = res)
#'
#'
#'   # ---------------------------------------- load data
#'
#'
#'   pred_df$lat.int <- floor(pred_df$latitude * 6 + 0.5)
#'   pred_df$long.int <- floor(pred_df$longitude * 6 + 0.5)
#'
#'   lats.int <- lats * 6
#'   lons.int <- lons * 6
#'
#'   mat <- matrix(NA, nrow = length(lons) - 1, ncol = length(lats) - 1)
#'
#'   i.lat <- findInterval(pred_df$lat.int, lats.int)
#'   i.lon <- findInterval(pred_df$long.int, lons.int)
#'
#'   mat[cbind(i.lon, i.lat)] <- pred_df[, statistic]
#'
#'   dir.create(out_pt, FALSE, TRUE)
#'
#'   png(file.path(out_pt, out_name),
#'       width = 16,
#'       height = 5.5,
#'       units = "cm",
#'       pointsize = 12,
#'       res = 300)
#'
#'   par(mar = c(0,0,0,0), oma = c(0,0,0,0))
#'
#'   ticks <- pretty(pred_df[, statistic], n = 5)
#'   lower <- min(ticks)
#'   upper <- max(ticks)
#'
#'   if(!is.null(z_range)){
#'
#'     lower <- min(z_range)
#'     upper <- max(z_range)
#'
#'   }
#'
#'   image(lons,
#'         lats,
#'         mat,
#'         col = my_col,
#'         zlim = c(lower, upper),
#'         xlim = c(-180, 180),
#'         ylim = c(-60, 60),
#'         asp = 1,
#'         axes = FALSE)
#'
#'   if(!is.null(shp)){
#'
#'     plot(shp, border = "gray40", lwd = 0.05, add = TRUE)
#'
#'   }
#'
#'   image.plot(lons,
#'              lats,
#'              mat,
#'              col = my_col,
#'              zlim = c(lower, upper),
#'              legend.only = TRUE,
#'              legend.width = 1,
#'              legend.shrink = 0.75,
#'              breaks = seq(lower, upper, length.out = n_col + 1),
#'              axis.args = list(cex.axis = 0.8),
#'              smallplot = c(0.025, 0.065, 0.1, 0.5))
#'
#'   if(!is.null(key_ttle)){
#'
#'     text(-172, 7, key_ttle, cex = 0.9, adj = c(0, NA))
#'
#'   }
#'
#'   par(mar = par("mar"))
#'
#'   on.exit(dev.off())
#'
#' }

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

  pred_r_mat <- raster::raster(pred_mat_ls)

  pred_r_spdf <- as(pred_r_mat, "SpatialPixelsDataFrame")

  as.data.frame(pred_r_spdf)

}

quick_raster_map <- function(countries, pred_r_df, my_col, ttl, parms, out_pt, out_name) {

  bbox <- parms$coord_limits

  x1 <- bbox[1]
  x2 <- bbox[2]
  y1 <- bbox[3]
  y2 <- bbox[4]

  p <- ggplot() +
    geom_tile(data = pred_r_df, aes(x = x, y = y, fill = layer)) +
    geom_sf(data = countries, fill = NA, color = "black", size = 0.4) +
    scale_fill_gradientn(colours = my_col,
                         na.value = "grey80",
                         guide = guide_colourbar(title = ttl,
                                                 barwidth = 1.5,
                                                 barheight = 4.5)) +
    coord_sf(xlim = c(x1, x2), ylim = c(y1, y2), expand = FALSE) +
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          legend.justification = c(0, 0),
          legend.position = c(0, 0),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          panel.background = element_blank(),
          panel.border = element_blank())

  dir.create(out_pt, FALSE, TRUE)

  png(file.path(out_pt, out_name),
      width = 10,
      height = 10,
      units = "cm",
      pointsize = 12,
      res = 300)

  print(p)

  on.exit(dev.off())

}
