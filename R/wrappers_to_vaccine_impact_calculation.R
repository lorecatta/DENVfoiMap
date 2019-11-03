
#------------------------------------------------------------------------------

#' The function edits the original look up table of the impact of the Sanofi Pasteur
#' dengue vaccine for different R0 values, as output by the Ferguson et al 2016
#' (Science) model. Specifically it adds to it the R0 -> vaccine impact for R0 = 0
#' and for R0 = max R0 predicted by the random forest model.
#' It also removes some columns of original vaccine impact lookup table.
#'
#' @title Pre-process the vaccine impact look up table
#'
#' @param look_up_table original look up table of vaccine impact
#'
#' @param R0_preds numeric vector of R0 predictions
#'
#' @export


pre_process_vaccine_lookup_table <- function(look_up_table, R0_preds) {

  look_up_table <- look_up_table[,-1]

  max_R0_to_lookup <- ceiling(max(R0_preds))

  new_first_row <- cbind(R0 = 0, look_up_table[1, 2:18])
  new_last_row <- cbind(R0 = max_R0_to_lookup, look_up_table[nrow(look_up_table), 2:18])

  look_up_table_2 <- rbind(new_first_row, look_up_table, new_last_row)

  as.matrix(look_up_table_2)

}
