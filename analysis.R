
# -----------------------------------------------------------------------------
#
# FOI predictions
#
# -----------------------------------------------------------------------------



# define parameters -----------------------------------------------------------


extra_prms <- list(id_fld = "unique_id",
                   grp_flds = c("unique_id", "ID_0", "ID_1"),
                   ranger_threads = NULL,
                   fit_type = "boot",
                   parallel_2 = FALSE,
                   screening_ages = c(9, 16),
                   target_nm = c("I", "C", "HC", "R0_1", "R0_2"),
                   coord_limits = c(-74, -32, -34, 6))

my_col <- colorRamps::matlab.like(100)

map_path <- "figures"


# define variables ------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

all_wgt <- parameters$all_wgt

all_predictors <- predictor_rank$name

base_info <- parameters$base_info

foi_offset <- parameters$foi_offset

coord_limits <- parameters$coord_limits

screening_ages <- parameters$screening_ages


# pre processing --------------------------------------------------------------


foi_data$new_weight <- all_wgt

pAbs_wgt <- get_sat_area_wgts(foi_data, parameters)

foi_data[foi_data$type == "pseudoAbsence", "new_weight"] <- pAbs_wgt


# create one bootstrap sample -------------------------------------------------


foi_data_bsample <- grid_and_boot(data_df = foi_data,
                                  parms = parameters)

# this RF fitting is going to take 20-30 minutes
RF_obj_optim <- full_routine_bootstrap(parms = parameters,
                                       original_foi_data = foi_data,
                                       adm_covariates = admin_covariates,
                                       all_squares = all_sqr_covariates,
                                       covariates_names = all_predictors,
                                       boot_sample = foi_data_bsample[[1]])

# keep only pixels in Brazil as example
BRA_sqr_covariates <- all_sqr_covariates[all_sqr_covariates$ID_0 %in% 33,]

BRA_predictions <- make_ranger_predictions(RF_obj_optim,
                                              dataset = BRA_sqr_covariates,
                                              covariates_names = all_predictors)

BRA_predictions <- BRA_predictions - foi_offset
BRA_predictions[BRA_predictions < 0] <- 0

BRA_sqr_covariates$p_i <- BRA_predictions

# map
mp_nm <- "FOI.png"
map_data_df <- map_preprocess(BRA_sqr_covariates, "p_i", parameters)
quick_raster_map(countries = Brazil_contour,
                 pred_r_df = map_data_df,
                 my_col = my_col,
                 ttl = "FOI",
                 parms = parameters,
                 out_pt = map_path,
                 out_name = mp_nm)



# -----------------------------------------------------------------------------
#
# R0 and Burden
#
# -----------------------------------------------------------------------------



# preprocessing
age_band_tgs <- grep("band", names(age_structure), value = TRUE)
age_band_bnds <- drep::get_age_band_bounds(age_band_tgs)
age_band_L_bounds <- age_band_bnds[, 1]
age_band_U_bounds <- age_band_bnds[, 2]

# create lookup tables
lookup_tabs <- create_lookup_tables(
  age_struct = age_structure,
  age_band_tags = age_band_tgs,
  age_band_L_bounds = age_band_L_bounds,
  age_band_U_bounds = age_band_U_bounds,
  parms = parameters)

# assume a transmission reduction effect of 0% (scale factor = 1)
sf_val <- parameters$sf_vals[1]

# attach look up table id
sqr_preds_2 <- dplyr::inner_join(age_structure[, c("age_id", "ID_0")],
                                 BRA_sqr_covariates,
                                 by = "ID_0")

sqr_preds_3 <- as.matrix(sqr_preds_2)

burden_estimates_raw <- wrapper_to_replicate_R0_and_burden(
  foi_data = sqr_preds_3,
  scaling_factor = sf_val,
  FOI_to_Inf_list = lookup_tabs[[1]],
  FOI_to_C_list = lookup_tabs[[2]],
  FOI_to_HC_list = lookup_tabs[[3]],
  FOI_to_R0_1_list = lookup_tabs[[4]],
  FOI_to_R0_2_list = lookup_tabs[[5]],
  parms = parameters)

burden_estimates <- post_processing_burden(sqr_preds_3, burden_estimates_raw, parameters)

# map the R0 (assumption 1, only primary and secondary infections are infectious)

mp_nm <- "R0_1.png"
map_data_df <- map_preprocess(burden_estimates, "transformed_1", parameters)
quick_raster_map(countries = Brazil_contour,
                 pred_r_df = map_data_df,
                 my_col = my_col,
                 ttl = expression("R"[0]),
                 parms = parameters,
                 out_pt = map_path,
                 out_name = mp_nm)

# map the incidence of infections (per 1000)

burden_estimates$I_num_inc <- burden_estimates$I_num / burden_estimates$population * 1000
mp_nm <- "Infections.png"
map_data_df <- map_preprocess(burden_estimates, "I_num_inc", parameters)
quick_raster_map(countries = Brazil_contour,
                 pred_r_df = map_data_df,
                 my_col = my_col,
                 ttl = "Infections",
                 parms = parameters,
                 out_pt = map_path,
                 out_name = mp_nm)



# -----------------------------------------------------------------------------
#
# Intervention impacts
#
# -----------------------------------------------------------------------------



# transmission reduction ------------------------------------------------------


# assume a transmission reduction effect of 30%
sf_val <- parameters$sf_vals[4]

tr_red_impact_estimates_raw <- wrapper_to_replicate_R0_and_burden(
  foi_data = sqr_preds_3,
  scaling_factor = sf_val,
  FOI_to_Inf_list = lookup_tabs[[1]],
  FOI_to_C_list = lookup_tabs[[2]],
  FOI_to_HC_list = lookup_tabs[[3]],
  FOI_to_R0_1_list = lookup_tabs[[4]],
  FOI_to_R0_2_list = lookup_tabs[[5]],
  parms = parameters)

tr_red_impact_estimates <- post_processing_burden(sqr_preds_3, tr_red_impact_estimates_raw, parameters)

# map the incidence of infections (per 1000) - assumption 1

tr_red_impact_estimates$I_num_1_inc <- tr_red_impact_estimates$I_num_1 / tr_red_impact_estimates$population * 1000
mp_nm <- "Infections_30pc_tr_red_impact.png"
map_data_df <- map_preprocess(tr_red_impact_estimates, "I_num_1_inc", parameters)
quick_raster_map(countries = Brazil_contour,
                 pred_r_df = map_data_df,
                 my_col = my_col,
                 ttl = "Infections",
                 parms = parameters,
                 out_pt = map_path,
                 out_name = mp_nm)


# vaccine ---------------------------------------------------------------------


# e.g. calculating impact on infections, for R0 assumption 1.

R0_1_preds <- burden_estimates$transformed_1

my_look_up_table <- pre_process_vaccine_lookup_table(R0_to_prop_infections_averted_lookup_1, R0_1_preds)

screen_age <- screening_ages[1] # 9

prop_averted <- wrapper_to_replicate_vaccine_impact(R0_preds = R0_1_preds,
                                                    look_up_table = my_look_up_table,
                                                    screen_age = screen_age,
                                                    parms = parameters)

burden_net_vaccine <- (1 - prop_averted[[1]]) * burden_estimates[, "I_num"]

burden_estimates_2 <- cbind(burden_estimates[, base_info], I_vacc_impact = burden_net_vaccine)

burden_estimates_2$I_num_1_inc_v <- burden_estimates_2$I_vacc_impact / burden_estimates_2$population * 1000
mp_nm <- "Infections_vacc_9yr_impact.png"
map_data_df <- map_preprocess(burden_estimates_2, "I_num_1_inc_v", parameters)
quick_raster_map(countries = Brazil_contour,
                 pred_r_df = map_data_df,
                 my_col = my_col,
                 ttl = "Infections",
                 parms = parameters,
                 out_pt = map_path,
                 out_name = mp_nm)
