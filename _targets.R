# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(here)
library(tarchetypes)

# Set target options:
options(tidyverse.quiet = TRUE)
tar_option_set(
  packages = c("hrbrthemes", "tidyverse", "brms", "cmdstanr"),
  format = "qs", # Optionally set the default storage format. qs is fast.
  seed = 1848
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

# Replace the target list below with your own:
list(
  tar_target(file, here("data", "raw", "data_recoded.csv")),
  tar_target(data_raw, read_csv(file)),
  tar_target(data_renamed, df_rename(data_raw, get_rename_lookup())),
  tar_target(df_raw, df_target_cols(data_renamed, list_composite_vars())),
  tar_target(df_recoded, df_recode(df_raw)),
  tar_target(df_with_composites,
             df_recoded |> 
               df_add_composite_scores(list_composite_vars())),
  tar_target(df_prior_checks,
             df_with_composites |>
               na_random_impute() |> df_cols_prior_checks()),
  tar_target(fit_prior_1A, prior_predictive_checks_1A(df_prior_checks)),
  tar_target(fit_prior_1B, prior_predictive_checks_1B(df_prior_checks)),
  tar_target(fit_prior_2A, prior_predictive_checks_2A(df_prior_checks)),
  tar_target(fit_prior_2B, prior_predictive_checks_2B(df_prior_checks)),
  tar_target(fit_prior_3A, prior_predictive_checks_3A(df_prior_checks)),
  tar_target(fit_prior_3B, prior_predictive_checks_3B(df_prior_checks)),

  # Render Quarto website
  tar_quarto(
    site,
    path = ".",          # root of Quarto project
    quiet = FALSE
  )
)
