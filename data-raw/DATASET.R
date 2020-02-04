## code to prepare `DATASET` dataset goes here

foi_data <- read.csv(file.path("data-raw",
                               "All_FOI_estimates_and_predictors.csv"),
                     stringsAsFactors = FALSE)

admin_covariates <- read.csv(file.path("data-raw",
                                       "admin_covariates.csv"),
                             stringsAsFactors = FALSE)

predictor_rank <- read.csv(file.path("data-raw",
                                     "predictor_rank.csv"),
                           stringsAsFactors = FALSE)

age_structure <- read.csv(file.path("data-raw",
                                    "age_structure.csv"),
                          stringsAsFactors = FALSE)

age_structure$age_id <- seq_len(nrow(age_structure))

all_sqr_covariates <- readRDS(file.path("data-raw",
                                        "all_squares_env_var_0_1667_deg.rds"))

R0_to_prop_cases_averted_lookup_1 <- read.csv(file.path("data-raw",
                                                        "R0_to_prop_cases_averted_lookup_1.csv"),
                                              stringsAsFactors = FALSE)

R0_to_prop_cases_averted_lookup_2 <- read.csv(file.path("data-raw",
                                                        "R0_to_prop_cases_averted_lookup_2.csv"),
                                              stringsAsFactors = FALSE)

R0_to_prop_hosp_averted_lookup_1 <- read.csv(file.path("data-raw",
                                                       "R0_to_prop_hosp_averted_lookup_1.csv"),
                                             stringsAsFactors = FALSE)

R0_to_prop_hosp_averted_lookup_2 <- read.csv(file.path("data-raw",
                                                       "R0_to_prop_hosp_averted_lookup_2.csv"),
                                             stringsAsFactors = FALSE)

R0_to_prop_infections_averted_lookup_1 <- read.csv(file.path("data-raw",
                                                             "R0_to_prop_infections_averted_lookup_1.csv"),
                                                   stringsAsFactors = FALSE)

R0_to_prop_infections_averted_lookup_2 <- read.csv(file.path("data-raw",
                                                             "R0_to_prop_infections_averted_lookup_2.csv"),
                                                   stringsAsFactors = FALSE)

Brazil_contour <- sf::st_read(dsn = file.path("data-raw"), layer = "Brazil_contour")

usethis::use_data(foi_data)
usethis::use_data(admin_covariates)
usethis::use_data(predictor_rank)
usethis::use_data(age_structure)
usethis::use_data(endemic_ID_0_ID_1)
usethis::use_data(all_sqr_covariates)
usethis::use_data(R0_to_prop_cases_averted_lookup_1)
usethis::use_data(R0_to_prop_cases_averted_lookup_2)
usethis::use_data(R0_to_prop_hosp_averted_lookup_1)
usethis::use_data(R0_to_prop_hosp_averted_lookup_2)
usethis::use_data(R0_to_prop_infections_averted_lookup_1)
usethis::use_data(R0_to_prop_infections_averted_lookup_2)
usethis::use_data(Brazil_contour)
