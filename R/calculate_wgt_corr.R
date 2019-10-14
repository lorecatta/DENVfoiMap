
#------------------------------------------------------------------------------

# calculate_wgt_cor

#' \code{calculate_wgt_cor} calculate a weighted Pearson correlation coefficient
#'
#' @param d.subs a dataframe with data.
#'
#' @param x character string of the name of variable A.
#'
#' @param y character string of the name of variable B.
#'
#' @importFrom weights wtd.cor
#'
#' @return the numeric value of the correlation coefficient.
#'
#' @export


calculate_wgt_cor <- function(d.sub, x, y){

  round(wtd.cor(d.sub[,x], d.sub[,y], weight = d.sub[,"new_weight"])[,"correlation"], 3)

}
