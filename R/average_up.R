average_up <- function(pxl_df, grp_flds, var_names){

  by_grp <- pxl_df %>% group_by(.dots = grp_flds)

  wtd_mean_pixel_data <- by_grp %>% summarise_at(var_names,
    funs(weighted.mean(., population, na.rm = TRUE)))

  mean_pixel_data <- by_grp %>% summarise(
    population = sum(population))

  aggreg_pixel_data <- left_join(wtd_mean_pixel_data, mean_pixel_data)

  as.data.frame(aggreg_pixel_data)

}

multi_col_average_up <- function(i, x, grp_flds){

  dat <- x[, c("population", grp_flds, i)]
  average_up(dat, grp_flds, i)

}

remove_pop_col <- function(i){
  i[, setdiff(names(i), c("ID_0", "population"))]
}

how_many_below_1 <- function(x){

  sum(x < 1)

}


#------------------------------------------------------------------------------

# fitted_sero_cell_to_adm

#' \code{fitted_sero_cell_to_adm} replace the mean of all 1/6 degree predictions
#'   within adim unit 1 level serology data with the value of the prediction in
#'   1/6 degree resolution cell where the original serology data point belongs.
#'   This allows to fit the serology data points as point data.
#'
#' @param join_all dataframe of foi estimates and predictions at admin unit 1 resolution.
#'
#' @param sqr_dataset_2 dataframe of foi estimates and predictions at 1/6 degree resolution.
#'
#' @importFrom dplyr %>% left_join mutate select
#'
#' @return a joined dataframe
#'
#' @export


fitted_sero_cell_to_adm <- function(join_all, sqr_dataset_2, fld_1, fld_2) {

  fitted_sero_cells <- sqr_dataset_2[sqr_dataset_2$type == "serology" & sqr_dataset_2$new_weight == 1, fld_1]

  id_field <- fld_1[1]

  join_all %>%
    left_join(fitted_sero_cells, by = id_field) %>%
    mutate(mean_p_i = ifelse(type == "serology", p_i, mean_p_i)) %>%
    select(fld_2)

}
