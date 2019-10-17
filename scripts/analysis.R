
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
                   parallel_2 = TRUE,
                   screening_ages = c(9, 16),
                   target_nm = c("I", "C", "HC", "R0_1", "R0_2"))

my_col <- colorRamps::matlab.like(100)

map_path <- "figures"


# define variables ------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

all_wgt <- parameters$all_wgt

all_predictors <- predictor_rank$name

base_info <- parameters$base_info

parallel_2 <- parameters$parallel_2

screening_ages <- parameters$screening_ages

target_nm <- parameters$target_nm


# pre processing --------------------------------------------------------------


foi_data$new_weight <- all_wgt

pAbs_wgt <- get_sat_area_wgts(foi_data, parameters)

foi_data[foi_data$type == "pseudoAbsence", "new_weight"] <- pAbs_wgt


# create one bootstrap sample -------------------------------------------------


foi_data_bsample <- grid_and_boot(data_df = foi_data,
                                  parms = parameters)

RF_obj_optim <- full_routine_bootstrap(parms = parameters,
                                       original_foi_data = foi_data,
                                       adm_covariates = admin_covariates,
                                       all_squares = all_sqr_covariates,
                                       covariates_names = all_predictors,
                                       boot_sample = foi_data_bsample[[1]])

global_predictions <- make_ranger_predictions(RF_obj_optim,
                                              dataset = all_sqr_covariates,
                                              covariates_names = all_predictors)

all_sqr_covariates$p_i <- global_predictions
saveRDS(all_sqr_covariates, "global_predictions.rds")
# all_sqr_covariates <- readRDS("global_predictions.rds")

all_sqr_covariates_sub <- inner_join(all_sqr_covariates, endemic_ID_0_ID_1)

# map
mp_nm <- "FOI.png"
quick_raster_map(pred_df = all_sqr_covariates_sub,
                 statistic = "p_i",
                 my_col = my_col,
                 out_pt = map_path,
                 out_name = mp_nm,
                 key_ttl = "FOI")



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

# set up a context which integrates nicely the parallel package
ctx <- context::context_save(path = "context",
                             package_sources = provisionr::package_sources(github = "lorecatta/DENVfoiMap"))
context::context_load(ctx)
context::parallel_cluster_start(7, ctx)

# create lookup tables
lookup_tabs <- loop(target_nm,
                    create_lookup_tables,
                    age_struct = age_structure,
                    age_band_tags = age_band_tgs,
                    age_band_L_bounds = age_band_L_bounds,
                    age_band_U_bounds = age_band_U_bounds,
                    parms = parameters,
                    parallel = FALSE)

# assume a transmission reduction effect of 0% (scale factor = 1)
sf_val <- parameters$sf_vals[1]

# attach look up table id
sqr_preds_2 <- inner_join(age_structure[, c("age_id", "ID_0")],
                          all_sqr_covariates_sub,
                          by = "ID_0")

sqr_preds_3 <- as.matrix(sqr_preds_2)

burden_estimates_raw <- loop(seq_len(nrow(sqr_preds_3)),
                         wrapper_to_replicate_R0_and_burden,
                         foi_data = sqr_preds_3,
                         age_struct = age_structure,
                         scaling_factor = sf_val,
                         FOI_to_Inf_list = lookup_tabs[[1]],
                         FOI_to_C_list = lookup_tabs[[2]],
                         FOI_to_HC_list = lookup_tabs[[3]],
                         FOI_to_R0_1_list = lookup_tabs[[4]],
                         FOI_to_R0_2_list = lookup_tabs[[5]],
                         age_band_lower_bounds = age_band_L_bounds,
                         age_band_upper_bounds = age_band_U_bounds,
                         age_band_tags = age_band_tgs,
                         parms = parameters,
                         parallel = parallel_2)

context::parallel_cluster_stop()

burden_estimates <- post_processing_burden(burden_estimates_raw, parameters)

# map the R0 (assumption 1, only primary and secondary infections are infectious)

mp_nm <- "R0_1.png"
quick_raster_map(pred_df = burden_estimates,
                 statistic = "transformed_1",
                 my_col = my_col,
                 out_pt = map_path,
                 out_name = mp_nm,
                 z_range = seq(1, 15, 2),
                 key_ttl = expression("R"[0]))

# map the incidence of infections (per 1000)

burden_estimates$I_num_inc <- burden_estimates$I_num / burden_estimates$population * 1000
mp_nm <- "Infections.png"
quick_raster_map(pred_df = burden_estimates,
                 statistic = "I_num_inc",
                 my_col = my_col,
                 out_pt = map_path,
                 out_name = mp_nm,
                 key_ttl = "Infections")



# -----------------------------------------------------------------------------
#
# Intervention impacts
#
# -----------------------------------------------------------------------------



# transmission reduction ------------------------------------------------------


# assume a transmission reduction effect of 30%
sf_val <- parameters$sf_vals[4]

context::parallel_cluster_start(7, ctx)

tr_red_impact_estimates_raw <- loop(seq_len(nrow(sqr_preds_3)),
                                    wrapper_to_replicate_R0_and_burden,
                                    foi_data = sqr_preds_3,
                                    age_struct = age_structure,
                                    scaling_factor = sf_val,
                                    FOI_to_Inf_list = lookup_tabs[[1]],
                                    FOI_to_C_list = lookup_tabs[[2]],
                                    FOI_to_HC_list = lookup_tabs[[3]],
                                    FOI_to_R0_1_list = lookup_tabs[[4]],
                                    FOI_to_R0_2_list = lookup_tabs[[5]],
                                    age_band_lower_bounds = age_band_L_bounds,
                                    age_band_upper_bounds = age_band_U_bounds,
                                    age_band_tags = age_band_tgs,
                                    parms = parameters,
                                    parallel = parallel_2)

context::parallel_cluster_stop()

tr_red_impact_estimates <- post_processing_burden(tr_red_impact_estimates_raw, parameters)

# map the incidence of infections (per 1000) - assumption 1

tr_red_impact_estimates$I_num_1_inc <- tr_red_impact_estimates$I_num_1 / tr_red_impact_estimates$population * 1000
mp_nm <- "Infections_30pc_tr_red_impact.png"
quick_raster_map(pred_df = tr_red_impact_estimates,
                 statistic = "I_num_1_inc",
                 my_col = my_col,
                 out_pt = map_path,
                 out_name = mp_nm,
                 key_ttl = "Infections")


# vaccine ---------------------------------------------------------------------


# e.g. calculating impact on infections, for R0 assumption 1.

R0_1_preds <- burden_estimates$transformed_1

my_look_up_table <- pre_process_vaccine_lookup_table(R0_to_prop_infections_averted_lookup_1, R0_1_preds)

screen_age <- screening_ages[1] # 9

prop_averted <- wrapper_to_replicate_vaccine_impact(preds = R0_1_preds,
                                                    vaccine_lookup = my_look_up_table,
                                                    screen_age = screen_age)

prop_averted <- do.call("rbind", output1)

burden_net_vaccine <- (1 - prop_averted[[1]]) * burden_estimates[, "I_num"]

burden_estimates <- cbind(burden_estimates[, base_info], I_vacc_impact = burden_net_vaccine)

burden_estimates$I_num_1_inc_v <- burden_estimates$I_vacc_impact / burden_estimates$population * 1000
mp_nm <- "Infections_vacc_9yr_impact.png"
quick_raster_map(pred_df = burden_estimates,
                 statistic = "I_num_1_inc_v",
                 my_col = my_col,
                 out_pt = map_path,
                 out_name = mp_nm,
                 key_ttl = "Infections")
