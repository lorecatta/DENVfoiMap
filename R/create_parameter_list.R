create_parameter_list <- function(no_predictors = 16,
                                  dependent_variable = "FOI",
                                  grid_size = 5,
                                  resample_grid_size = 20,
                                  no_samples = 1,
                                  vec_phis_R0_1 = c(1, 1, 0),
                                  vec_phis_R0_2 = c(1, 1, 1),
                                  prop_sympt = c(0.45, 0.85, 0.15),
                                  Q_1 = 0.04,
                                  Q_3 = 0.04,
                                  Q_2 = 0.1,
                                  FOI_grid = seq(0, 0.2, 0.001), #seq(0, 0.2, 0.0002),
                                  sf_vals = seq(1, 0.1, -0.1),
                                  shape_1 = 0,
                                  shape_2 = 5,
                                  shape_3 = 1.6e6,
                                  no_trees = 500,
                                  min_node_size = 20,
                                  all_wgt = 1,
                                  pseudoAbs_value = c(FOI = -0.02,
                                                      R0_1 = 0.5,
                                                      R0_2 = 0.5,
                                                      R0_3 = 0.5,
                                                      Z = -0.02),
                                  foi_offset = 0.03,
                                  covariates_dir = "stepwise_v6",
                                  EM_iter = 10,
                                  extra_params = NULL) {

  pm_list <- list()

  pm_list$no_predictors <- no_predictors
  pm_list$dependent_variable <- dependent_variable
  pm_list$grid_size <- grid_size
  pm_list$resample_grid_size <- resample_grid_size
  pm_list$no_samples <- no_samples
  pm_list$vec_phis_R0_1 <- vec_phis_R0_1
  pm_list$vec_phis_R0_2 <- vec_phis_R0_2
  pm_list$prop_sympt <- prop_sympt
  pm_list$Q_1 <- Q_1
  pm_list$Q_3 <- Q_3
  pm_list$Q_2 <- Q_2
  pm_list$FOI_grid <- FOI_grid
  pm_list$sf_vals <- sf_vals
  pm_list$shape_1 <- shape_1
  pm_list$shape_2 <- shape_2
  pm_list$shape_3 <- shape_3
  pm_list$no_trees <- no_trees
  pm_list$min_node_size <- min_node_size
  pm_list$all_wgt <- all_wgt
  pm_list$pseudoAbs_value <- pseudoAbs_value
  pm_list$foi_offset <- foi_offset
  pm_list$covariates_dir <- covariates_dir
  pm_list$EM_iter <- EM_iter

  if(sum(!is.na(match(names(extra_params), names(pm_list))))!=0){

    stop (message(cat("Extra params in ... share names with default param names. Please check:\n",
                      names(extra_params)[!is.na(match(names(extra_params),names(pm_list)))])))

  }

  append(pm_list, extra_params)

}
