create_lookup_tables <- function(i,
                                 age_struct,
                                 age_band_tags,
                                 age_band_L_bounds,
                                 age_band_U_bounds,
                                 parms){

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

      # saveRDS(lookup_list, out_nm)

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

      # saveRDS(lookup_list, out_nm_)

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

      # saveRDS(lookup_list, out_nm)

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

      # saveRDS(lookup_list, out_nm)

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

      # saveRDS(lookup_list, out_nm)

    }

  }

  lookup_list

}

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
