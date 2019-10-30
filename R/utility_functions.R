

#------------------------------------------------------------------------------

#' The function is a wrapper to lapply and parLapply.
#'
#' @title Parallel computation wrapper
#'
#' @param ... aaditional arguments passed on to lapply or parLapply.
#'
#' @param parallel logical. Is parLapply used?
#'
#' @export


loop <- function(..., parallel) {
  if (parallel) {
    parallel::parLapply(NULL, ...)
  } else {
    lapply(...)
  }
}
