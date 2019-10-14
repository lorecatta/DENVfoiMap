
#------------------------------------------------------------------------------

# fit_ranger_RF

#' \code{fit_ranger_RF} fit a random forest model using \code{ranger}.
#'
#' @param sqr_dataset_2 dataframe of foi estimates and predictions at 1/6 degree resolution.
#'
#' @param training_dataset a dataframe of the dataset used for model training.
#'
#' @param my_weights character string of the name of the column of \code{training dataset}
#'   with case weights.
#'
#' @inheritParams full_routine_bootstrap
#'
#' @inheritParams create_parameter_list
#'
#' @importFrom ranger ranger
#'
#' @return the random forest model object returned by \code{ranger}
#'
#' @export


fit_ranger_RF <- function(parms,
                          dependent_variable,
                          covariates_names,
                          training_dataset,
                          my_weights){

  num_threads <- parms$ranger_threads
  min_node_size <- parms$min_node_size
  no_trees <- parms$no_trees

  wgts <- training_dataset[, my_weights]

  train <- training_dataset[, c(dependent_variable, covariates_names)]

  ranger(formula = paste0(dependent_variable, "~ ."),
         data = train,
         num.trees = no_trees,
         importance = "impurity",
         case.weights = wgts,
         write.forest = TRUE,
         min.node.size = min_node_size,
         verbose = TRUE,
         num.threads = num_threads)

}


#------------------------------------------------------------------------------

# make_ranger_predictions

#' \code{make_ranger_predictions} make predictions using a Ranger model object.
#'
#' @param mod_obj the random forest model object returned by \code{ranger}.
#'
#' @param dataset the dataframe of the covariate dataset for which to make new predictions.
#'
#' @inheritParams full_routine_bootstrap
#'
#' @export


make_ranger_predictions <- function(mod_obj, dataset, covariates_names){

  x_data <- subset(dataset, , covariates_names, drop = FALSE)

  preds <- predict(mod_obj, x_data)

  preds$predictions

}
