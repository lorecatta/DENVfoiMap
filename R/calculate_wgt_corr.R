
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

calculate_R_squared <- function(df, x, y) {

  y_i <- df[, x]

  f_i <- df[, y]

  y_hat <- mean(y_i)

  e_i <- y_i - f_i

  SS_t <- sum((y_i - y_hat)^2)

  SS_res <- sum(e_i^2)

  round(1 - (SS_res / SS_t), 3)

}

calculate_R_squared_2 <- function(df, obs, mod) {

  frml <- as.formula(paste0(obs, "~", mod))
  summary(lm(frml, data = df))$r.squared

}
