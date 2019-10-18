
#------------------------------------------------------------------------------

#' The function generates a plot of admin unit 1 level FOI observations against
#' predictions with the R2 shown.
#'
#' @title Plot observations vs predictions
#'
#' @param df dataframe admin unit 1 level FOI observations and predictions.
#'
#' @param x character of the column name containing the observations.
#'
#' @param y character of the column name containing the predictions.#
#'
#' @inheritParams quick_raster_map
#'
#' @importFrom stats as.formula lm residuals coef
#'
#' @importFrom ggplot2 aes_string geom_point scale_x_continuous scale_y_continuous
#'  geom_smooth coord_cartesian geom_text aes theme element_text margin unit
#'
#' @export


generic_scatter_plot <- function(df,
                                 x,
                                 y,
                                 out_name,
                                 out_pt) {

  lm_eqn <- function(df, y, x) {

    # fit linear model - no intercept
    frmla <- as.formula(paste0(y, " ~ ", x, " - 1"))
    m <- lm(frmla, df)

    # calculates R2
    r2 <- 1 - crossprod(residuals(m)) / crossprod(df[,y] - mean(df[,y]))
    #r2 <- summary(m)$r.squared # R

    model_slope <- as.numeric(coef(m))

    eq <- substitute(italic(y) == b %.% italic(x)*","~~italic(R)^2~"="~r2,
                     list(b = format(model_slope, digits = 4),
                          r2 = format(r2, digits = 4)))

    as.character(as.expression(eq))

  }

  x_values <- pretty(df[, x], n = 5)
  y_values <- pretty(df[, y], n = 5)
  min_x_value <- min(x_values)
  max_x_value <- max(x_values)
  min_y_value <- min(y_values)
  max_y_value <- max(y_values)

  eq <- lm_eqn(df = df, y = y, x = x)

  eq_df <- data.frame(eq)

  x_text <- ifelse(max_x_value > 2, 3, min_x_value + (0.3 * max_x_value))

  p <- ggplot(df, aes_string(x = x, y = y)) +
    geom_point(aes_string(x = x, y = y), size = 1) +
    scale_x_continuous("Observations",
                       breaks = x_values,
                       labels = x_values) +
    scale_y_continuous("Predictions",
                       breaks = y_values,
                       labels = y_values) +
    geom_smooth(method = "lm", formula = y ~ x - 1, se = FALSE) +
    coord_cartesian(xlim = c(min_x_value, max_x_value),
                    ylim = c(min_y_value, max_y_value)) +
    geom_text(data = eq_df, aes(x = x_text, y = max_y_value, label = eq), parse = TRUE) +
    theme(axis.title.x = element_text(hjust = 0.5, vjust = 1, size = 15, margin = margin(t = 20)),
          axis.title.y = element_text(hjust = 0.5, vjust = 0, size = 15, margin = margin(r = 20)),
          axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = 11),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

  dir.create(out_pt, FALSE, TRUE)

  png(filename = file.path(out_pt, out_name),
      width = 6,
      height = 4,
      units = "cm",
      res = 300)

  print(p)

  on.exit(dev.off())

}
