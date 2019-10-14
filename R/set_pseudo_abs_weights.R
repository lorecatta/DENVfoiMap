get_sat_area_wgts <- function(foi_data, parms){

  b <- parms$shape_1
  c <- parms$shape_2
  d <- parms$shape_3

  data_sub <- foi_data[foi_data$type == "pseudoAbsence", ]

  b + (c - b) * (1 - (1 / (1 + (data_sub$Shape_Area / d))))

}

get_area_scaled_wgts <- function(foi_data, wgt_limits){

  x <- foi_data[foi_data$type== "pseudoAbsence", "Shape_Area"]

  area_limits <- c(min(x), max(x))

  y <- rep(0, length(x))

  y[which(x==min(x))] <- wgt_limits[1]
  y[which(x==max(x))] <- wgt_limits[2]

  between_lims_ids <- which(y == 0)
  between_lims <- y[between_lims_ids]

  look_up_t <- cbind(x, y)

  interp_wgts <- vapply(look_up_t[between_lims_ids, "x"],
                        approx_one,
                        numeric(1),
                        a = area_limits,
                        b = wgt_limits)

  look_up_t[between_lims_ids, "y"] <- interp_wgts

  look_up_t[,"y"]

}

approx_one <- function(i, a, b){
  approx(a, b, xout = i)$y
}

set_wgts_to_sero_cells <- function(foi_data, pxl_data, parms){

  res <- (1 / 120) * parms$resample_grid_size

  id_fld <- parms$id_fld

  pxl_data[pxl_data$type == "serology", "new_weight"] <- 0

  sero_points <- foi_data[foi_data$type == "serology", ]

  pxl_data$lat.int <- round(pxl_data$latitude / res)
  pxl_data$long.int <- round(pxl_data$longitude / res)

  sero_points$lat.int <- round(sero_points$latitude / res)
  sero_points$long.int <- round(sero_points$longitude / res)

  sero_points$cell <- 0
  sero_points$no_square <- 0

  for (i in seq_len(nrow(sero_points))){

    sero_long <- sero_points[i, "long.int"]
    sero_lat <- sero_points[i, "lat.int"]
    data_id <- sero_points[i, id_fld]

    matches <- pxl_data[, id_fld] == data_id & pxl_data$type == "serology" & pxl_data$lat.int == sero_lat & pxl_data$long.int == sero_long

    if(sum(matches) != 0){

      # message(i)
      # print(sum(matches))

      cell_id <- which(matches == TRUE)[1]
      sero_points[i, "cell"] <- cell_id
      pxl_data[cell_id, "new_weight"] <- 1

    } else {

      sero_points[i, "no_square"] <- 1

    }

  }

  missing_square <- sero_points[sero_points$no_square == 1, ]

  message("missing squares = ", nrow(missing_square))

  pxl_data

}
