
#------------------------------------------------------------------------------

#' The function sets the case weigths and applies an offset to the FOI response variable
#' (offset is needed during the EM fitting) in the original admin unit level FOI dataset.
#'
#' @title Pre-process the original admin unit level FOI dataset
#'
#' @param foi_data dataframe of admin unit level FOI data.
#'
#' @inheritParams full_routine_bootstrap
#'
#' @export


preprocess_adm_data <- function(parms, foi_data) {

  var_to_fit <- parms$dependent_variable

  all_wgt <- parms$all_wgt

  foi_offset <- parms$foi_offset

  pseudoAbs_value <- parms$pseudoAbs_value[var_to_fit]

  foi_data[foi_data$type == "pseudoAbsence", var_to_fit] <- pseudoAbs_value

  foi_data$new_weight <- all_wgt

  pAbs_wgt <- get_sat_area_wgts(foi_data, parms)

  foi_data[foi_data$type == "pseudoAbsence", "new_weight"] <- pAbs_wgt

  if(var_to_fit == "FOI" | var_to_fit == "Z"){

    foi_data[, var_to_fit] <- foi_data[, var_to_fit] + foi_offset

  }


  foi_data

}


#------------------------------------------------------------------------------

#' The function joins the original admin unit level FOI estimates to the 1/6 degree resolution
#' FOI dataset and also sets the case weigths for each 1/6 degree resolution data point.
#'
#' @title Pre-process the 1/6 degree resolution FOI dataset
#'
#' @param foi_data dataframe of admin unit level FOI data.
#'
#' @inheritParams preprocess_adm_data
#'
#' @export


preprocess_pxl_data <- function(parms, foi_data, pxl_data) {

  join_fields <- parms$grp_flds

  id_field <- parms$id_fld

  # join (filtering)

  pxl_data_2 <- inner_join(pxl_data, foi_data[, c(join_fields, "type", "new_weight")])

  pxl_data_3 <- set_wgts_to_sero_cells(foi_data, pxl_data_2, parms)

  if(length(unique(pxl_data_3[, id_field])) != nrow(foi_data)){

    stop("Some data points are missing their cell")

  }

  pxl_data_3

}
