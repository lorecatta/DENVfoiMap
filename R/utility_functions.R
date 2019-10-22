
#------------------------------------------------------------------------------

#' The function allows to do parallel computation using the \code{\link{parallel}} package
#'
#' @title Wrapper for parallel computation
#'
#' @param ... aerguement passed on to \code{lapply} or \code{parallel::parLapply}.
#'
#' @param parallel logicla indicating whether to run things in parallel.
#'
#' @export


loop <- function(..., parallel) {
  if (parallel) {
    parallel::parLapply(NULL, ...)
  } else {
    lapply(...)
  }
}
