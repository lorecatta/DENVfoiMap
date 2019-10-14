wrapper_to_replicate_vaccine_impact <- function(preds,
                                                vaccine_lookup,
                                                screen_age){

  approx_all_ages <- function(j, vaccine_lookup, preds){
    approx(vaccine_lookup[, "R0"], vaccine_lookup[, j], xout = preds)$y
  }

  if (screen_age != 0) {

    out <- approx(vaccine_lookup[, "R0"], vaccine_lookup[, screen_age], xout = preds)$y

    out_2 <- NULL

  } else {

    all_ages <- seq_len(18)

    # look up reduction for each age
    ret_all_ages <- vapply(all_ages[2:length(all_ages)], approx_all_ages, numeric(no_fits), vaccine_lookup, preds)

    # min vacc age is 9
    ret_all_ages_2 <- ret_all_ages[, 8:ncol(ret_all_ages)]

    # find max reduction across ages (columns)
    out <- rowMaxs(ret_all_ages_2)

    # find age of max vaccine impact
    out_2 <- max.col(ret_all_ages_2)

  }

  list(out, out_2)

}
