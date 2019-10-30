
#------------------------------------------------------------------------------

#' The function look up the two R0 values, the number of infections, mild febrile cases and
#' the number of hospitalized cases corresponding to a set of FOI predictions,
#' following the application of a scaling factor representing the assumed
#' reduction in transmission.
#'
#' @title Look up the impact of a transmission-reducing intervention
#'
#' @param foi_data dataframe of processed admin unit level foi data.
#'
#' @param scaling_factor numeric scaling factor value (0-1)
#'
#' @param FOI_to_Inf_list list of FOI -> number of infections look up tables for different countries
#'
#' @param FOI_to_C_list list of FOI -> number of cases look up tables for different countries
#'
#' @param FOI_to_HC_list list of FOI -> number of hospitalized cases look up tables for different countries
#'
#' @param FOI_to_R0_1_list list of FOI -> R0 (assumption 1) look up tables for different countries
#'
#' @param FOI_to_R0_2_list list of FOI -> R0 (assumption 2) look up tables for different countries
#'
#' @inheritParams create_lookup_tables
#'
#' @importFrom stats approx
#'
#' @export


wrapper_to_replicate_R0_and_burden <- function(foi_data,
                                               scaling_factor,
                                               FOI_to_Inf_list,
                                               FOI_to_C_list,
                                               FOI_to_HC_list,
                                               FOI_to_R0_1_list,
                                               FOI_to_R0_2_list,
                                               parms){


  helper <- function(i, ...){


    # browser()

    # ---------------------------------------------------------------------------
    # get the right look up table for each square


    # cat("pixel number =", i, "\n")

    var_to_fit <- parms$dependent_variable
    no_fits <- parms$no_samples
    fit_type <- parms$fit_type
    FOI_grid <- parms$FOI_grid

    FOI_grid_res <- max(FOI_grid) / length(FOI_grid)

    idx <- foi_data[i, "age_id"]


    # ---------------------------------------------------------------------------
    # get vector of foi values and number of people in each square


    if(fit_type == "boot") {

      preds <- foi_data[i, "p_i"]

    } else {

      preds <- foi_data[i, "best"]

    }

    N <- foi_data[i, "population"]


    # get burden measures -------------------------------------------------------


    if (var_to_fit == "FOI" | var_to_fit == "Z") {

      FOI_to_R0_1 <- FOI_to_R0_1_list[[idx]]
      FOI_to_R0_2 <- FOI_to_R0_2_list[[idx]]

      trans_1 <- approx(FOI_to_R0_1[, "x"], FOI_to_R0_1[, "y"], xout = preds)$y
      trans_2 <- approx(FOI_to_R0_2[, "x"], FOI_to_R0_2[, "y"], xout = preds)$y

      # use the 1D look up tables (C and HC) obtained using
      # the average values of the proportion of symptomatic (fixed)

      FOI_to_Inf <- FOI_to_Inf_list[[idx]]
      FOI_to_C <- FOI_to_C_list[[idx]]
      FOI_to_HC <- FOI_to_HC_list[[idx]]

      if (scaling_factor == 1) {

        # DO NOT use R0 to calculate I, C and HC

        Infections_pc <- approx(FOI_to_Inf[, "x"], FOI_to_Inf[, "y"], xout = preds)$y
        Cases_pc <- approx(FOI_to_C[, "x"], FOI_to_C[, "y"], xout = preds)$y
        Hosp_cases_pc <- approx(FOI_to_HC[, "x"], FOI_to_HC[, "y"], xout = preds)$y

        Infections <- Infections_pc * N
        Cases <- Cases_pc * N
        HCases <- Hosp_cases_pc * N

        var_names <- c("transformed_1", "transformed_2", "I_num", "C_num", "H_num")

        out <- rbind(trans_1, trans_2, Infections, Cases, HCases)

      } else {

        # DO use R0 to calculate I, C and HC

        red_trans_1 <- trans_1 * scaling_factor
        red_trans_2 <- trans_2 * scaling_factor

        red_preds_1 <- approx(FOI_to_R0_1[, "y"], FOI_to_R0_1[, "x"], xout = red_trans_1)$y
        red_preds_2 <- approx(FOI_to_R0_2[, "y"], FOI_to_R0_2[, "x"], xout = red_trans_2)$y

        Infections_pc_1 <- approx(FOI_to_Inf[, "x"], FOI_to_Inf[, "y"], xout = red_preds_1)$y
        Infections_pc_2 <- approx(FOI_to_Inf[, "x"], FOI_to_Inf[, "y"], xout = red_preds_2)$y

        Cases_pc_1 <- approx(FOI_to_C[, "x"], FOI_to_C[, "y"], xout = red_preds_1)$y
        Cases_pc_2 <- approx(FOI_to_C[, "x"], FOI_to_C[, "y"], xout = red_preds_2)$y

        Hosp_cases_pc_1 <- approx(FOI_to_HC[, "x"], FOI_to_HC[, "y"], xout = red_preds_1)$y
        Hosp_cases_pc_2 <- approx(FOI_to_HC[, "x"], FOI_to_HC[, "y"], xout = red_preds_2)$y

        Infections_1 <- Infections_pc_1 * N
        Cases_1 <- Cases_pc_1 * N
        HCases_1 <- Hosp_cases_pc_1 * N

        Infections_2 <- Infections_pc_2 * N
        Cases_2 <- Cases_pc_2 * N
        HCases_2 <- Hosp_cases_pc_2 * N

        var_names <- c("transformed_r_1",
                       "transformed_r_2",
                       "response_r_1",
                       "response_r_2",
                       "I_num_1",
                       "C_num_1",
                       "H_num_1",
                       "I_num_2",
                       "C_num_2",
                       "H_num_2")

        out <- rbind(red_trans_1,
                     red_trans_2,
                     red_preds_1,
                     red_preds_2,
                     Infections_1,
                     Cases_1,
                     HCases_1,
                     Infections_2,
                     Cases_2,
                     HCases_2)

      }

    }

    if (var_to_fit %in% c("R0_1", "R0_2", "R0_3")) {

      # always use R0 to calculate I, C and HC

      red_preds <- preds * scaling_factor

      FOI_to_R0_list <- get(sprintf("FOI_to_%s_list", var_to_fit))
      FOI_to_R0 <- FOI_to_R0_list[[idx]]
      red_trans <- approx(FOI_to_R0[, "y"], FOI_to_R0[, "x"], xout = red_preds)$y

      # use the 1D look up tables (C and HC) obtained using
      # the average values of the proportion of symptomatic (fixed)

      FOI_to_Inf <- FOI_to_Inf_list[[idx]]
      FOI_to_C <- FOI_to_C_list[[idx]]
      FOI_to_HC <- FOI_to_HC_list[[idx]]

      Infections_pc <- approx(FOI_to_Inf[, "x"], FOI_to_Inf[, "y"], xout = red_trans)$y
      Cases_pc <- approx(FOI_to_C[, "x"], FOI_to_C[, "y"], xout = red_trans)$y
      Hosp_cases_pc <- approx(FOI_to_HC[, "x"], FOI_to_HC[, "y"], xout = red_trans)$y

      Infections <- Infections_pc * N
      Cases <- Cases_pc * N
      HCases <- Hosp_cases_pc * N

      var_names <- c("transformed_r", "response_r", "I_num", "C_num", "H_num")
      out <- rbind(red_trans,
                   red_preds,
                   Infections,
                   Cases,
                   HCases)

    }

    rownames(out) <- var_names
    colnames(out) <- "p_i"
    out

  }

  parallel_2 <- parms$parallel_2

  loop(seq_len(nrow(foi_data)),
       helper,
       foi_data,
       scaling_factor,
       FOI_to_Inf_list,
       FOI_to_C_list,
       FOI_to_HC_list,
       FOI_to_R0_1_list,
       FOI_to_R0_2_list,
       parms,
       parallel = parallel_2)

}
