
#------------------------------------------------------------------------------

# exp_max_algorithm

#' \code{exp_max_algorithm} fit a random forest model to one single
#'   bootstrap sample of the data using the EM algorithm
#'
#' @inheritParams full_routine_bootstrap
#'
#' @param adm_dataset dataframe of the bootstrapped dataset of foi estimates and
#'   covariates at admin unit 1 resolution (pre-processed).
#'
#' @param pxl_dataset dataframe of the bootstrapped dataset of foi estimates and
#'   covariates at 1/6 degree resolution.
#'
#' @importFrom dplyr left_join inner_join %>% group_by summarise
#'
#' @return the random forest model object returned by \code{ranger}
#'
#' @export


exp_max_algorithm <- function(parms,
                              adm_dataset,
                              pxl_dataset,
                              covariates_names,
                              map_col = NULL,
                              RF_obj_path = NULL,
                              RF_obj_name = NULL,
                              diagn_tab_path = NULL,
                              diagn_tab_name = NULL,
                              map_path = NULL,
                              sct_plt_path = NULL,
                              train_dts_path = NULL,
                              train_dts_name = NULL,
                              adm_covariates = NULL){


  var_to_fit <- parms$dependent_variable
  niter <- parms$EM_iter
  id_field <- parms$id_fld
  grp_flds <- parms$grp_flds

  l_f <- parms$pseudoAbs_value[var_to_fit]

  foi_offset <- parms$foi_offset

  l_f_2 <- l_f + foi_offset
  zero_2 <- foi_offset


  # pre processing ------------------------------------------------------------


  diagnostics <- c("RF_ms_i", "ss_i", "ss_j", "min_wgt", "max_wgt", "n_NA_pred", "r_av_sqr", "r_adm")

  out_mat <- matrix(0, nrow = niter, ncol = length(diagnostics))

  colnames(out_mat) <- diagnostics

  for (i in seq_len(niter)){

    # browser()

    cat("iteration =", i, "\n")


    # 1. calculate scaling factors --------------------------------------------


    p_i_by_adm <- pxl_dataset %>% group_by(.dots = grp_flds)

    a_sum <- p_i_by_adm %>% summarise(a_sum = sum(pop_weight * p_i))

    dd <- left_join(pxl_dataset, a_sum)

    dd$wgt_prime <- (dd$pop_weight / dd$p_i) * dd$a_sum
    # dd$wgt_prime <- dd$pop_weight

    dd[dd$type == "serology" & dd$new_weight == 1, "wgt_prime"] <- 1


    # 2. modify the scaling factors to account for background data ------------


    dd$wgt_prime <- dd$wgt_prime * dd$new_weight


    # 3. calculate new pseudo data value --------------------------------------


    psAbs <- dd$type == "pseudoAbsence"

    u_i <- rep(0, nrow(dd))

    if(var_to_fit == "FOI" | var_to_fit == "Z"){

      # u_i[!psAbs] <- (((dd$o_j[!psAbs] - l_f) * (dd$p_i[!psAbs] - l_f)) / (dd$a_sum[!psAbs] - l_f)) + l_f # when using only pop prop weights
      u_i[!psAbs] <- (dd$o_j[!psAbs] * dd$p_i[!psAbs]) / dd$a_sum[!psAbs] # when using updating weights
      u_i[psAbs] <- ifelse(dd$p_i[psAbs] > zero_2, l_f_2, dd$p_i[psAbs])

    } else {

      u_i[!psAbs] <- (dd$o_j[!psAbs] * dd$p_i[!psAbs]) / dd$a_sum[!psAbs]
      u_i[psAbs] <- ifelse(dd$p_i[psAbs] > 1, l_f, dd$p_i[psAbs])

    }

    u_i[dd$type == "serology" & dd$new_weight == 1] <- dd$o_j[dd$type == "serology" & dd$new_weight == 1]

    dd$u_i <- u_i


    # 4. fit RF model ---------------------------------------------------------


    min_wgt <- min(dd$wgt_prime)
    max_wgt <- max(dd$wgt_prime)

    training_dataset <- dd[, c("u_i", covariates_names, "wgt_prime")]

    RF_obj <- fit_ranger_RF(parms = parms,
                            dependent_variable = "u_i",
                            covariates_names = covariates_names,
                            training_dataset = training_dataset,
                            my_weights = "wgt_prime")

    RF_ms_i <- RF_obj$prediction.error


    # 5. make new pixel level predictions -------------------------------------


    p_i <- make_ranger_predictions(RF_obj, dd, covariates_names)

    n_NA_pred <- sum(is.na(p_i))

    dd$p_i <- p_i


    # map of square predictions -----------------------------------------------


    dd_1 <- dd

    if(var_to_fit == "FOI" | var_to_fit == "Z"){

      dd_1$p_i <- dd$p_i - foi_offset

    }

    if(!is.null(map_path)){

      my_col <- colorRamps::matlab.like(100)

      if(!is.null(map_col)){

        my_col <- map_col

      }

      mp_nm <- sprintf("iter_%s%s", i, ".png")

      quick_raster_map(pred_df = dd_1,
                       statistic = "p_i",
                       my_col = my_col,
                       out_pt = map_path,
                       out_name = mp_nm)
    }


    # create a copy for obs vs preds plot and SS calculation ------------------


    dd_2 <- dd


    # fix 20 km predictions ---------------------------------------------------


    if(var_to_fit == "FOI" | var_to_fit == "Z"){

      dd_2$u_i[psAbs] <- zero_2
      dd_2$p_i[psAbs] <- ifelse(dd_2$p_i[psAbs] < zero_2, zero_2, dd_2$p_i[psAbs])

    } else {

      dd_2$u_i[psAbs] <- l_f
      dd_2$p_i[psAbs] <- ifelse(dd_2$p_i[psAbs] < 1, l_f, dd_2$p_i[psAbs])

    }


    # 6. calculate pixel level sum of square ----------------------------------


    ss_i <- sum(dd_2$wgt_prime * (dd_2$p_i - dd_2$u_i)^2)


    # make admin unit level predictions ---------------------------------------


    if(!is.null(adm_covariates)) {

      adm_covariates$adm_pred <- make_ranger_predictions(RF_obj,
                                                      adm_covariates,
                                                      covariates_names)

      cc <- inner_join(adm_dataset, adm_covariates[, c("ID_0", "ID_1", "adm_pred")])

    } else {

      cc <- adm_dataset

    }

    psAbs_adm <- cc$type == "pseudoAbsence"

    if(var_to_fit == "FOI" | var_to_fit == "Z"){

      cc$o_j[psAbs_adm] <- zero_2
      cc$adm_pred[psAbs_adm] <- ifelse(cc$adm_pred[psAbs_adm] < zero_2,
                                       zero_2,
                                       cc$adm_pred[psAbs_adm])

    } else {

      cc$adm_pred[psAbs_adm] <- ifelse(cc$adm_pred[psAbs_adm] < 1,
                                       l_f,
                                       cc$adm_pred[psAbs_adm])

    }


    # 7. calculate population weighted mean of pixel level predictions --------


    p_i_by_adm <- dd_2 %>% group_by(.dots = grp_flds)

    mean_p_i <- p_i_by_adm %>% summarise(mean_p_i = sum(p_i * pop_weight))

    aa <- inner_join(cc, mean_p_i)

    # take the pixel level prediction (not the mean of all predictions)
    # for serology data
    aa_2 <- fitted_sero_cell_to_adm(aa,
                                    dd_2,
                                    c(id_field, "p_i"),
                                    c(grp_flds, "type", "new_weight", "o_j", "adm_pred", "mean_p_i"))

    if(!is.null(sct_plt_path)){

      # browser()

      # plot of observed admin values vs pop-wgt average predicted square values

      av_sqr_sp_nm <- paste0("pred_vs_obs_av_sqr_iter_", i, ".png")

      generic_scatter_plot(df = aa_2,
                           x = "o_j",
                           y = "mean_p_i",
                           file_name = av_sqr_sp_nm,
                           file_path = sct_plt_path)


      # plot of observed vs predicted admin values ------------------------------


      adm_sp_nm <- paste0("pred_vs_obs_adm_iter_", i, ".png")

      generic_scatter_plot(df = aa_2,
                           x = "o_j",
                           y = "adm_pred",
                           file_name = adm_sp_nm,
                           file_path = sct_plt_path)

    }


    # 8. calculate admin unit level sum of square -----------------------------


    ss_j <- sum(aa_2$new_weight * (aa_2$mean_p_i - aa_2$o_j)^2)


    # calculate correlation of obs vs pixel and adm predictions ---------------


    r_av_sqr <- calculate_wgt_cor(aa_2, "o_j", "mean_p_i")
    r_adm <- calculate_wgt_cor(aa_2, "o_j", "adm_pred")


    # --------------------------------------


    out_mat[i,] <- c(RF_ms_i, ss_i, ss_j, min_wgt, max_wgt, n_NA_pred, r_av_sqr, r_adm)

    pxl_dataset$p_i <- dd$p_i

  }

  if(!is.null(RF_obj_path)){
    write_out_rds(RF_obj, RF_obj_path, RF_obj_name)
  }

  if(!is.null(diagn_tab_path)){
    write_out_rds(out_mat, diagn_tab_path, diagn_tab_name)
  }

  if(!is.null(train_dts_path)){
    write_out_rds(training_dataset, train_dts_path, train_dts_name)
  }

  RF_obj
}
