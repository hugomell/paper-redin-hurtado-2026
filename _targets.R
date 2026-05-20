# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Optional browser specification for `tar_visnetwork()`
options(browser = "/usr/local/bin/firefox")

# Load packages required to define the pipeline:
library(here)
library(tarchetypes)
library(targets)

# Set target options:
options(tidyverse.quiet = TRUE)
tar_option_set(
  packages = c("tidyverse", "brms", "cmdstanr"),
  format = "qs", # Optionally set the default storage format. qs is fast.
  seed = 1848
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

# Set up for static branching with `tar_eval()`
# (used to avoid code repetition when fitting multiple models)
model_ids <- c("model1A","model1B", "model2A", "model2B", "model3A", "model3B")
prior_checks_symbol <- rlang::syms(paste0("fit_prior_", model_ids))


# Targets pipeline
list(
    tar_target(file, here("data", "raw", "data_recoded.csv")),
    tar_target(data_raw, read_csv(file)),
    tar_target(
        data_prep,
        data_raw |>
            df_rename() |>
            df_select_cols() |>
            df_recode() |>
            df_add_composites()
    ),
    tar_target(
        df_prior_checks,
        data_prep |>
            na_random_impute() |> df_cols_prior_checks()
    ),
    #tar_target(data_imp, df_recode(df_raw)),
    tar_eval(
        tar_target(symbol,
            fit_brms_model(
                df_prior_checks,
                rhs_models[[model_ids]],
                prior_check = TRUE
            )
        ),
        values = list(model_ids = model_ids, symbol = prior_checks_symbol)
  ),
  # tar_target(fit_prior_1A, fit_brms_model(
  #       df_prior_checks,rhs_models[["model1A"]], prior_check = TRUE
  #   )),
  # tar_target(fit_prior_1B, fit_brms_model(
  #       df_prior_checks, rhs_models[["model1B"]], prior_check = TRUE
  #   )),
  # tar_target(fit_prior_2A, fit_brms_model(
  #       df_prior_checks, rhs_models[["model2A"]], prior_check = TRUE
  #   )),
  # tar_target(fit_prior_2B, fit_brms_model(
  #       df_prior_checks, rhs_models[["model2B"]], prior_check = TRUE
  #   )),
  # tar_target(fit_prior_3A, fit_brms_model(
  #       df_prior_checks, rhs_models[["model3A"]], prior_check = TRUE
  #   )),
  # tar_target(fit_prior_3B, fit_brms_model(
  #       df_prior_checks, rhs_models[["model3B"]], prior_check = TRUE
  #   )),

  # Render Quarto website
  tar_quarto(
    site,
    path = ".",          # root of Quarto project
    quiet = FALSE
  )
)
