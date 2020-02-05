## code to prepare `DATASET` dataset goes here

my_dir <- "data-raw"

foi_data <- read.csv(file.path(my_dir, "All_FOI_estimates_and_predictors.csv"),
                     stringsAsFactors = FALSE)

admin_covariates <- read.csv(file.path(my_dir, "admin_covariates.csv"),
                             stringsAsFactors = FALSE)

predictor_rank <- read.csv(file.path(my_dir, "predictor_rank.csv"),
                           stringsAsFactors = FALSE)

age_structure <- read.csv(file.path(my_dir, "age_structure.csv"),
                          stringsAsFactors = FALSE)

age_structure$age_id <- seq_len(nrow(age_structure))

R0_to_prop_cases_averted_lookup_1 <- read.csv(file.path(my_dir,
                                                        "R0_to_prop_cases_averted_lookup_1.csv"),
                                              stringsAsFactors = FALSE)

R0_to_prop_cases_averted_lookup_2 <- read.csv(file.path(my_dir,
                                                        "R0_to_prop_cases_averted_lookup_2.csv"),
                                              stringsAsFactors = FALSE)

R0_to_prop_hosp_averted_lookup_1 <- read.csv(file.path(my_dir,
                                                       "R0_to_prop_hosp_averted_lookup_1.csv"),
                                             stringsAsFactors = FALSE)

R0_to_prop_hosp_averted_lookup_2 <- read.csv(file.path(my_dir,
                                                       "R0_to_prop_hosp_averted_lookup_2.csv"),
                                             stringsAsFactors = FALSE)

R0_to_prop_infections_averted_lookup_1 <- read.csv(file.path(my_dir,
                                                             "R0_to_prop_infections_averted_lookup_1.csv"),
                                                   stringsAsFactors = FALSE)

R0_to_prop_infections_averted_lookup_2 <- read.csv(file.path(my_dir,
                                                             "R0_to_prop_infections_averted_lookup_2.csv"),
                                                   stringsAsFactors = FALSE)

usethis::use_data(foi_data)
usethis::use_data(admin_covariates)
usethis::use_data(predictor_rank)
usethis::use_data(age_structure)
usethis::use_data(R0_to_prop_cases_averted_lookup_1)
usethis::use_data(R0_to_prop_cases_averted_lookup_2)
usethis::use_data(R0_to_prop_hosp_averted_lookup_1)
usethis::use_data(R0_to_prop_hosp_averted_lookup_2)
usethis::use_data(R0_to_prop_infections_averted_lookup_1)
usethis::use_data(R0_to_prop_infections_averted_lookup_2)
