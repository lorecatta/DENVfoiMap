
#------------------------------------------------------------------------------

#' The function aggregates foi predictions at 1/6 degree resolutions to
#' adim unit 1 level by taking the population-weighted average of the
#' 1/6 degree resolution predictions within each admin unit 1.
#'
#' @title Aggregate FOI predictions at 1/6 degree resolutions to adim unit level
#'
#' @param pxl_df dataframe of foi predictions at 1/6 degree resolution.
#'
#' @param grp_flds character string of column names by which the 1/6 degree
#'   resolution predictions are aggregated by.
#'
#' @inheritParams full_routine_bootstrap
#'
#' @importFrom dplyr %>% group_by summarise_at summarise left_join funs
#'
#' @importFrom stats weighted.mean
#'
#' @export


average_up <- function(pxl_df, grp_flds, covariates_names){

  by_grp <- pxl_df %>% group_by(.dots = grp_flds)

  wtd_mean_pixel_data <- by_grp %>% summarise_at(covariates_names,
    funs(weighted.mean(., population, na.rm = TRUE)))

  mean_pixel_data <- by_grp %>% summarise(
    population = sum(by_grp$population))

  aggreg_pixel_data <- left_join(wtd_mean_pixel_data, mean_pixel_data)

  as.data.frame(aggreg_pixel_data)

}


#------------------------------------------------------------------------------

#' The function replaces the mean of all 1/6 degree predictions within adim unit 1 level
#' serology data with the value of the prediction in 1/6 degree resolution cell
#' where the original serology data point belongs. This allows to fit the
#' serology data points as point data.
#'
#' @title Modify predictions of serology data points at 1/6 degree resolution
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
