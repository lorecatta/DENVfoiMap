
#------------------------------------------------------------------------------

#' The function calculates case weights for admin unit 1 level pseudo absence points
#' assuming that the case weight value is a saturating function of the area (km) of
#' the admin unit.
#'
#' @title Calculate case weights for admin unit 1 level pseudo absences
#'
#' @inheritParams preprocess_adm_data
#'
#' @export


get_sat_area_wgts <- function(foi_data, parms){

  b <- parms$sat_functions_shapes[1]
  c <- parms$sat_functions_shapes[2]
  d <- parms$sat_functions_shapes[3]

  data_sub <- foi_data[foi_data$type == "pseudoAbsence", ]

  b + (c - b) * (1 - (1 / (1 + (data_sub$Shape_Area / d))))

}


#------------------------------------------------------------------------------

#' The function sets to 1 the case weights of the 1/6 degree resolution cells where
#' the original serology data points were located. It sets to 0 the case weights of
#' all the other 1/6 degree resolution cells in the data points admin unit. This
#' allows to fit the serology data points as point data.
#'
#' @title Calculate case weights for serology 1/6 degree resolution cells
#'
#' @inheritParams preprocess_pxl_data
#'
#' @export


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

  # message("missing squares = ", nrow(missing_square))

  pxl_data

}
