## code to prepare `DATASET` dataset goes here

foi_data <- read.csv(file.path("data-raw",
                               "All_FOI_estimates_and_predictors.csv"),
                     stringsAsFactors = FALSE)

usethis::use_data(foi_data, overwrite = TRUE)

all_sqr_covariates <- readRDS(file.path("data-raw",
                                        "all_squares_env_var_0_1667_deg.rds"))

usethis::use_data(all_sqr_covariates, overwrite = TRUE)

admin_covariates <- read.csv(file.path("data-raw",
                                       "admin_covariates.csv"),
                             stringsAsFactors = FALSE)

usethis::use_data(admin_covariates, overwrite = TRUE)

predictor_rank <- read.csv(file.path("data-raw",
                                     "predictor_rank.csv"),
                           stringsAsFactors = FALSE)

usethis::use_data(predictor_rank, overwrite = TRUE)

age_structure <- read.csv(file.path("data-raw",
                                    "age_structure.csv"),
                          stringsAsFactors = FALSE)

usethis::use_data(age_structure, overwrite = TRUE)

FOI_to_I_lookup_tables <- readRDS(file.path("data-raw", "FOI_to_I_lookup_tables.rds"))
usethis::use_data(FOI_to_I_lookup_tables, overwrite = TRUE)

FOI_to_C_lookup_tables <- readRDS(file.path("data-raw", "FOI_to_C_lookup_tables.rds"))
usethis::use_data(FOI_to_C_lookup_tables, overwrite = TRUE)

FOI_to_HC_lookup_tables <- readRDS(file.path("data-raw", "FOI_to_HC_lookup_tables.rds"))
usethis::use_data(FOI_to_HC_lookup_tables, overwrite = TRUE)

FOI_to_R0_1_lookup_tables <- readRDS(file.path("data-raw", "FOI_to_R0_1_lookup_tables.rds"))
usethis::use_data(FOI_to_R0_1_lookup_tables, overwrite = TRUE)

FOI_to_R0_2_lookup_tables <- readRDS(file.path("data-raw", "FOI_to_R0_2_lookup_tables.rds"))
usethis::use_data(FOI_to_R0_2_lookup_tables, overwrite = TRUE)

endemic_ID_0_ID_1 <- read.csv(file.path("data-raw",
                                        "endemic_ID_0_ID_1.csv"),
                              stringsAsFactors = FALSE)

ID_0_to_remove <- c(1, 69, 171, 122, 200, 224, 226, 235, 236, 244, 246)

endemic_ID_0_ID_1 <- endemic_ID_0_ID_1[!endemic_ID_0_ID_1$ID_0 %in% ID_0_to_remove,]

usethis::use_data(endemic_ID_0_ID_1, overwrite = TRUE)
