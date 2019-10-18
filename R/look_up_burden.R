
#------------------------------------------------------------------------------

#' The function generates look up tables for calculating total number of annual dengue infections,
#'  mild febrile cases, cases requiring hospitalization and R0. R0 is estimated for
#'  two different assumptions of the infectiousness of the four dengue infections.
#'
#' @title Create look up tables for burden and R0 calculation
#'
#' @param age_struct dataframe of the proportion of individuals in different 5-yr age groups,
#'  for each country.
#'
#' @param age_band_tags character string of the names of the columns of the `age_structure` dataframe
#'  containing the actual age structure data.
#'
#' @param age_band_L_bounds integers of the lower limits of the age groups.
#'
#' @param age_band_U_bounds integers pf the upper limits of the age groups.
#'
#' @inheritParams full_routine_bootstrap
#'
#' @export


create_lookup_tables <- function(age_struct,
                                 age_band_tags,
                                 age_band_L_bounds,
                                 age_band_U_bounds,
                                 parms){

  wrapper_to_lookup <- function(i,
                                age_struct,
                                tags,
                                FOI_values,
                                my_fun, ...){

    my_FUN <- my_fun
    # my_FUN <- match.fun(my_fun)

    m_j <- age_struct[i, tags]

    vapply(FOI_values,
           my_FUN,
           numeric(1),
           n_j = m_j,
           ...)

  }

  fix_all_lookup_limits <- function(i) {

    rbind(c(x = 0, y = 0), i)

  }

  fix_R0_lookup_limits <- function(i) {

    i[1, "y"] <- 1

    rbind(c(x = 0, y = 0), i)

  }

  cbind_FOI_to_lookup <- function(i, FOI_values) {

    cbind(x = FOI_values, y = i)

  }

  helper <- function(i) {

    cat("variable to look up =", i, "\n")

    FOI_values <- parms$FOI_grid
    parallel_2 <- parms$parallel_2

    if (i == "I") {

      my_fun <- drep::calculate_infections
      out_nm <- sprintf("FOI_to_%s_lookup_tables.rds", i)

      if (!file.exists(out_nm)) {

        message("1D lookup")

        Infection_values <- loop(seq_len(nrow(age_struct)),
                                 wrapper_to_lookup,
                                 age_struct = age_struct,
                                 tags = age_band_tags,
                                 FOI_values = FOI_values,
                                 my_fun = my_fun,
                                 u_lim = age_band_U_bounds,
                                 l_lim = age_band_L_bounds,
                                 parallel = parallel_2)

        lookup_list <- lapply(Infection_values, cbind_FOI_to_lookup, FOI_values)

        lookup_list <- lapply(lookup_list, fix_all_lookup_limits)

      }

    }

    if (i == "C") {

      my_fun <- drep::calculate_cases
      my_weights <- parms$prop_sympt
      out_nm <- sprintf("FOI_to_%s_lookup_tables.rds", i)

      if (!file.exists(out_nm)) {

        message("1D lookup")

        cat("weights vector =", my_weights, "\n")

        case_values <- loop(seq_len(nrow(age_struct)),
                            wrapper_to_lookup,
                            age_struct = age_struct,
                            tags = age_band_tags,
                            FOI_values = FOI_values,
                            my_fun = my_fun,
                            u_lim = age_band_U_bounds,
                            l_lim = age_band_L_bounds,
                            weights_vec = my_weights,
                            parallel = parallel_2)

        lookup_list <- lapply(case_values, cbind_FOI_to_lookup, FOI_values)

        lookup_list <- lapply(lookup_list, fix_all_lookup_limits)

      }

    }

    if (i == "HC") {

      my_fun <- drep::calculate_hosp_cases
      my_weights <- parms$prop_sympt
      out_nm <- sprintf("FOI_to_%s_lookup_tables.rds", i)

      if (!file.exists(out_nm)) {

        message("1D lookup")

        cat("weights vector =", my_weights, "\n")

        HCase_values <- loop(seq_len(nrow(age_struct)),
                             wrapper_to_lookup,
                             age_struct = age_struct,
                             tags = age_band_tags,
                             FOI_values = FOI_values,
                             my_fun = my_fun,
                             u_lim = age_band_U_bounds,
                             l_lim = age_band_L_bounds,
                             parms = parms$prop_hosp,
                             weights_vec = my_weights,
                             parallel = parallel_2)

        lookup_list <- lapply(HCase_values, cbind_FOI_to_lookup, FOI_values)

        lookup_list <- lapply(lookup_list, fix_all_lookup_limits)

      }

    }

    if (i == "R0_1") {

      my_fun <- drep::calculate_R0
      my_weights <- parms$vec_phis_R0_1
      out_nm <- sprintf("FOI_to_%s_lookup_tables.rds", i)

      if (!file.exists(out_nm)) {

        message("1D lookup")

        cat("weights vector =", my_weights, "\n")

        R0_values <- loop(seq_len(nrow(age_struct)),
                          wrapper_to_lookup,
                          age_struct = age_struct,
                          tags = age_band_tags,
                          FOI_values = FOI_values,
                          my_fun = my_fun,
                          u_lim = age_band_U_bounds,
                          l_lim = age_band_L_bounds,
                          phis = my_weights,
                          parallel = parallel_2)

        lookup_list <- lapply(R0_values, cbind_FOI_to_lookup, FOI_values)

        lookup_list <- lapply(lookup_list, fix_all_lookup_limits)

        lookup_list <- lapply(lookup_list, fix_R0_lookup_limits)

      }

    }

    if (i == "R0_2") {

      my_fun <- drep::calculate_R0
      my_weights <- parms$vec_phis_R0_2
      out_nm <- sprintf("FOI_to_%s_lookup_tables.rds", i)

      if (!file.exists(out_nm)) {

        message("1D lookup")

        cat("weights vector =", my_weights, "\n")

        R0_values <- loop(seq_len(nrow(age_struct)),
                          wrapper_to_lookup,
                          age_struct = age_struct,
                          tags = age_band_tags,
                          FOI_values = FOI_values,
                          my_fun = my_fun,
                          u_lim = age_band_U_bounds,
                          l_lim = age_band_L_bounds,
                          phis = my_weights,
                          parallel = parallel_2)

        lookup_list <- lapply(R0_values, cbind_FOI_to_lookup, FOI_values)

        lookup_list <- lapply(lookup_list, fix_all_lookup_limits)

        lookup_list <- lapply(lookup_list, fix_R0_lookup_limits)

      }

    }

    lookup_list

  }

  target_nm <- parms$target_nm

  loop(target_nm,
       helper,
       age_struct,
       age_band_tags,
       age_band_L_bounds,
       age_band_U_bounds,
       parms,
       parallel = FALSE)

}


#------------------------------------------------------------------------------

#' The functions post processes the output of the burden calculation.
#'
#' @title Post-process the output of the burden calculation
#'
#' @param data_matrix a matrix output from \code{\link{wrapper_to_replicate_R0_and_burden}}.
#'
#' @inheritParams full_routine_bootstrap
#'
#' @export


post_processing_burden <- function(data_matrix, parms) {

  base_info <- parms$base_info

  ret1 <- lapply(data_matrix, t)
  ret2 <- do.call("rbind", ret1)
  ret3 <- cbind(sqr_preds_3[, base_info], ret2)

  as.data.frame(ret3)

}
