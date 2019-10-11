#------------------------------------------------------------------------------

# fit_ranger_RF

#' \code{fit_ranger_RF} fit a random forest model using \code{ranger}.
#'
#' @param dependent_variable character string of the fitted response variable (FOI).
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

make_ranger_predictions <- function(mod_obj, dataset, sel_preds){

  x_data <- subset(dataset, , sel_preds, drop = FALSE)

  preds <- predict(mod_obj, x_data)

  preds$predictions

}
