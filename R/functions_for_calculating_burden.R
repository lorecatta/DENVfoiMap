calculate_infections <- function(FOI, 
                                 n_j, 
                                 age_band_lower_bounds, 
                                 age_band_upper_bounds){
  
  prob_fun <- list("calculate_primary_infection_prob",
                   "calculate_secondary_infection_prob",
                   "calculate_tertiary_infection_prob",
                   "calculate_quaternary_infection_prob")
  
  infection_probabilities <- lapply(
    prob_fun, 
    do.call,
    list(FOI, age_band_lower_bounds, age_band_upper_bounds))
  
  infection_incidences <- lapply(
    infection_probabilities,
    calc_average_prob_infect,
    age_band_upper_bounds, 
    age_band_lower_bounds) 
  
  infection_numbers_j <- lapply(infection_incidences, calculate_case_number, n_j)
  
  total_infection_number <- vapply(infection_numbers_j, sum, numeric(1))
  
  sum(total_infection_number) * 4
  
}

calculate_cases <- function(FOI, 
                            n_j, 
                            age_band_lower_bounds, 
                            age_band_upper_bounds,
                            weights_vec){
  
  prob_fun <- list("calculate_primary_infection_prob",
                   "calculate_secondary_infection_prob",
                   "calculate_tertiary_infection_prob",
                   "calculate_quaternary_infection_prob")
  
  gamma_1 <- weights_vec[1]
  rho <- weights_vec[2] 
  gamma_3 <- weights_vec[3]
  
  infection_probabilities <- lapply(
    prob_fun, 
    do.call,
    list(FOI, age_band_lower_bounds, age_band_upper_bounds))
  
  infection_incidences <- lapply(
    infection_probabilities,
    calc_average_prob_infect,
    age_band_upper_bounds, 
    age_band_lower_bounds) 
  
  I1_rate <- infection_incidences[[1]] 
  I2_rate <- infection_incidences[[2]]
  I3_rate <- infection_incidences[[3]] 
  I4_rate <- infection_incidences[[4]]    
  
  tot_incid_rate_j <- rho * I2_rate + (gamma_1 * I1_rate) + (gamma_3 * (I3_rate + I4_rate))  
  
  case_number_j <- calculate_case_number(tot_incid_rate_j, n_j)
  
  sum(case_number_j) * 4
  
}

calculate_hosp_cases <- function(FOI, 
                                 n_j, 
                                 age_band_lower_bounds, 
                                 age_band_upper_bounds,
                                 parms,
                                 weights_vec){
  
  prob_fun <- list("calculate_primary_infection_prob",
                   "calculate_secondary_infection_prob",
                   "calculate_tertiary_infection_prob",
                   "calculate_quaternary_infection_prob")
  
  gamma_1 <- weights_vec[1]
  rho <- weights_vec[2] 
  gamma_3 <- weights_vec[3]
  
  Q_1 <- parms$Q_1
  Q_2 <- parms$Q_2
  Q_3 <- parms$Q_3
    
  infection_probabilities <- lapply(
    prob_fun, 
    do.call,
    list(FOI, age_band_lower_bounds, age_band_upper_bounds))
  
  infection_incidences <- lapply(
    infection_probabilities,
    calc_average_prob_infect,
    age_band_upper_bounds, 
    age_band_lower_bounds) 
  
  I1_rate <- infection_incidences[[1]] 
  I2_rate <- infection_incidences[[2]]
  I3_rate <- infection_incidences[[3]] 
  I4_rate <- infection_incidences[[4]]    
  
  tot_incid_rate_j <- Q_2 * rho * I2_rate + (Q_1 * gamma_1 * I1_rate) + (gamma_3 * Q_3 * (I3_rate + I4_rate))
  
  case_number_j <- calculate_case_number(tot_incid_rate_j, n_j)
  
  sum(case_number_j) * 4

}

wrapper_to_lookup <- function(i, 
                              age_struct, 
                              tags, 
                              FOI_values, 
                              my_fun, ...){
  
  my_FUN <- match.fun(my_fun)
  
  m_j <- age_struct[i, tags]
  
  vapply(FOI_values,
         my_FUN,
         numeric(1),
         n_j = m_j,
         ...)
  
}

wrapper_to_lookup_2D <- function(i, 
                                 age_struct, 
                                 param_post,
                                 tags, 
                                 FOI_values, 
                                 my_fun,
                                 ...){
  
  my_FUN <- match.fun(my_fun)
    
  m_j <- age_struct[i, tags]
  
  param_sets <- nrow(param_post)
  
  out <- matrix(0, nrow = length(FOI_values), ncol = param_sets)
  
  for (j in seq_len(param_sets)){
    
    my_weights <- param_post[j, ]
    
    out[, j] <- vapply(FOI_values,
                       my_FUN,
                       numeric(1),
                       n_j = m_j,
                       weights_vec = my_weights,
                       ...)
    
  }
  
  out  

}

fix_R0_lookup_limits <- function(i) {
  
  i[1, "y"] <- 1
  
  rbind(c(x = 0, y = 0), i)

}

cbind_FOI_to_lookup <- function(i, FOI_values) {
  
  cbind(x = FOI_values, y = i)

}

interpolate_using_mat_indices <- function(lookup_mat, rowIndices, colIndices, rowIndices_next, FOI_grid, FOI_values){
  
  Infections_pc <- lookup_mat[rowIndices + nrow(lookup_mat) * (colIndices - 1)]
  Infections_pc_next <- lookup_mat[rowIndices_next + nrow(lookup_mat) * (colIndices - 1)]
  
  m <- (Infections_pc_next - Infections_pc) / (FOI_grid[rowIndices_next] - FOI_grid[rowIndices]) # slope formula
  b <- Infections_pc-(m*FOI_grid[rowIndices]) # solve for b
  
  m * (FOI_values) + b
  
}
