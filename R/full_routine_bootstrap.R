
#------------------------------------------------------------------------------

#' The function preprocess one single bootstrap sample of the original FOI data
#' and fits a random forest model to it using the EM algorithm.
#'
#' @title Pre-process and fit a random forest model to a sample of FOI data
#'
#' @param parms list of user-defined parameters.
#'
#' @param original_foi_data dataframe of the original foi estimates dataset at
#'   admin unit 1 resolution.
#'
#' @param all_squares dataframe of global covariates at 1/6 degree resolution.
#'
#' @param covariates_names character vector of covariates names.
#'
#' @param boot_sample dataframe of the bootstrapped dataset of foi estimates and
#'   covariates at admin unit 1 resolution (not pre-processed).
#'
#' @inheritParams exp_max_algorithm
#'
#' @importFrom dplyr left_join inner_join
#'
#' @return the random forest model object returned by \code{\link{ranger}}.
#'
#' @export


full_routine_bootstrap <- function(parms,
                                   original_foi_data,
                                   adm_covariates,
                                   all_squares,
                                   covariates_names,
                                   boot_sample){

  var_to_fit <- parms$dependent_variable

  number_of_predictors <- parms$no_predictors

  grp_flds <- parms$grp_flds


  # ---------------------------------------------------------------------------


  my_predictors <- covariates_names[1:number_of_predictors]

  orig_foi <- preprocess_adm_data(parms, original_foi_data)

  parms_2 <- parms
  parms_2$grp_flds <- c(parms_2$grp_flds, "data_id")

  foi_data <- boot_sample

  foi_data_2 <- preprocess_adm_data(parms, foi_data)

  pxl_data_2 <- preprocess_pxl_data(parms_2, foi_data_2, all_squares)

  training_dataset <- foi_data_2[, c(var_to_fit, my_predictors, "new_weight")]

  RF_obj <- fit_ranger_RF(parms = parms,
                          dependent_variable = var_to_fit,
                          covariates_names = my_predictors,
                          training_dataset = training_dataset,
                          my_weights = "new_weight")

  p_i <- make_ranger_predictions(RF_obj, pxl_data_2, my_predictors)

  pxl_data_2$p_i <- p_i

  names(foi_data_2)[names(foi_data_2) == var_to_fit] <- "o_j"

  pxl_data_3 <- inner_join(pxl_data_2, foi_data_2[, c(grp_flds, "o_j")])

  pxl_dts_grp <- pxl_data_3 %>%
    group_by(.dots = grp_flds) %>%
    summarise(pop_sqr_sum = sum(population))

  pxl_data_4 <- left_join(pxl_data_3, pxl_dts_grp)

  pxl_data_4$pop_weight <- pxl_data_4$population / pxl_data_4$pop_sqr_sum

  RF_obj_optim <- exp_max_algorithm(parms = parms,
                                    adm_dataset = foi_data_2,
                                    pxl_dataset = pxl_data_4,
                                    covariates_names = my_predictors,
                                    adm_covariates = adm_covariates)

  RF_obj_optim

}
