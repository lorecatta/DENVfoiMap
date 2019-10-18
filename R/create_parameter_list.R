
#------------------------------------------------------------------------------

#' The function creates a list of user-defined parameters.
#'
#' @title Create a list of user-defined parameters
#'
#' @param no_predictors number of selected covariates used for model fitting and making predictions.
#'
#' @param dependent_variable character string of the fitted response variable (FOI).
#'
#' @param resample_grid_size resolution of the foi predictions (in km). Default = 20 (1/6 degree).
#'
#' @param grid_size size of the grid used for block bootstrapping.
#'
#' @param no_samples number of bootstrap samples.
#'
#' @param vec_phis_R0_1 numeric vector of length = 4 of the relative infectiousness of primary,
#'  secondary, tertiary and quaternary dengue infections, when assuming only primary and
#'  secondary infections are infectious.
#'
#' @param vec_phis_R0_2 numeric vector of length = 4 of the relative infectiousness of primary,
#'  secondary, tertiary and quaternary dengue infections, when assuming all four infections
#'  are infectious.
#'
#' @param prop_sympt numeric vector of length = 4 of the proportions of primary, secondary,
#'  tertiary and quaternary infections which are symptomatic.
#'
#' @param prop_hosp numeric vector of length = 4 of the proportions of primary, secondary,
#'  tertiary and quaternary infections requiring hospitalization.
#'
#' @param FOI_grid numeric vector of force of infection values used for mapping FOI to
#'  number of infections, cases hopsitalizations and R0.
#'
#' @param sf_vals scaling factor used to model the effect of transmission-reducing interventions.
#'
#' @param sat_functions_shapes parameters of the saturating function used for setting pseudo absences
#'  case weights.
#'
#' @param no_trees number of trees of the random forest model passed to \code{\link{ranger}}.
#'
#' @param min_node_size minimal node size of the random forest model passed to \code{\link{ranger}}.
#'
#' @param all_wgt numeric value of case weights for all data points.
#'
#' @param pseudoAbs_value numeric value of pseudo absences for different response variables.
#'
#' @param foi_offset numeric value to offset model FOI predictions during fitting with
#'  Expectation Maximization.
#'
#' @param base_info character string of key ID variable in the 1/6 degree resolution global
#'  covariate dataset.
#'
#' @param extra_params list of additional parameters. Default = NULL.
#'
#' @inheritParams fit_ranger_RF
#'
#' @export


create_parameter_list <- function(no_predictors = 16,
                                  dependent_variable = "FOI",
                                  resample_grid_size = 20,
                                  grid_size = 5,
                                  no_samples = 1,
                                  vec_phis_R0_1 = c(1, 1, 0, 0),
                                  vec_phis_R0_2 = c(1, 1, 1, 1),
                                  prop_sympt = c(0.45, 0.85, 0.15, 0.15),
                                  prop_hosp = c(0.04, 0.1, 0.04, 0.04),
                                  FOI_grid = seq(0.002, 0.2, 0.002), #seq(0, 0.2, 0.0002),
                                  sf_vals = seq(1, 0.1, -0.1),
                                  sat_functions_shapes = c(0, 5, 1.6e6),
                                  no_trees = 500,
                                  min_node_size = 20,
                                  all_wgt = 1,
                                  pseudoAbs_value = c(FOI = -0.02,
                                                      R0_1 = 0.5,
                                                      R0_2 = 0.5,
                                                      R0_3 = 0.5,
                                                      Z = -0.02),
                                  foi_offset = 0.03,
                                  EM_iter = 10,
                                  base_info = c("cell",
                                                "latitude",
                                                "longitude",
                                                "population",
                                                "ID_0",
                                                "ID_1",
                                                "ID_2"),
                                  extra_params = NULL) {

  pm_list <- list()

  pm_list$no_predictors <- no_predictors
  pm_list$dependent_variable <- dependent_variable
  pm_list$resample_grid_size <- resample_grid_size
  pm_list$grid_size <- grid_size
  pm_list$no_samples <- no_samples
  pm_list$vec_phis_R0_1 <- vec_phis_R0_1
  pm_list$vec_phis_R0_2 <- vec_phis_R0_2
  pm_list$prop_sympt <- prop_sympt
  pm_list$prop_hosp <- prop_hosp
  pm_list$FOI_grid <- FOI_grid
  pm_list$sf_vals <- sf_vals
  pm_list$sat_functions_shapes <- sat_functions_shapes
  pm_list$no_trees <- no_trees
  pm_list$min_node_size <- min_node_size
  pm_list$all_wgt <- all_wgt
  pm_list$pseudoAbs_value <- pseudoAbs_value
  pm_list$foi_offset <- foi_offset
  pm_list$EM_iter <- EM_iter
  pm_list$base_info <- base_info

  if(sum(!is.na(match(names(extra_params), names(pm_list))))!=0){

    stop (message(cat("Extra params in ... share names with default param names. Please check:\n",
                      names(extra_params)[!is.na(match(names(extra_params),names(pm_list)))])))

  }

  append(pm_list, extra_params)

}
