# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(here)
library(tarchetypes)
library(targets)

# Set target options:
options(tidyverse.quiet = TRUE)
# Raise the future memory limit to 2GB
options(future.globals.maxSize = 2 * 1024^3)
tar_option_set(
  packages = c("bayesplot", "brms", "cmdstanr", "corrplot", "gt", "mice",
               "patchwork", "tidyverse"),
  format = "qs",
  memory = "transient",
  garbage_collection = TRUE,
  seed = 1848
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source("R")

###############################################################################
# Targets pipeline
###############################################################################

values <- models |>
  dplyr::mutate(
    fit_dep = rlang::syms(paste0("fit_", model)),
    chk_dep = rlang::syms(paste0("chk_", model)),
    pfit_dep     = rlang::syms(paste0("pfit_", model)),
    pp_prior_dep = rlang::syms(paste0("pp_prior_", model)),
    pp_post_dep  = rlang::syms(paste0("pp_post_", model))
  )


n_pp_draws <- 200   # draws per imputation used for predictive checks

# Pipeline section which defines targets for model fitting, sampling issues and
# convergence checks, summarising for all models defined in data frame `models`
# (see `R/globals.R`).
per_model <- tar_map(
  values = values,
  names  = model, # targets: fit_model_multi_1A, chk_..., smp_... tab_..., ...

  # --- regression models ---
  tar_target(
    fit,
    fit_brms_model(data, resp = resp, rhs = rhs, delta = delta),
    format = "rds"
  ),
  tar_target(chk, check_convergence(fit_dep, M = 20, verbose = FALSE)),
  tar_target(smp, sampler_issues(fit_dep, M = 20)),
  tar_target(tab, summarise_model(fit_dep, convergence = chk_dep)),


  # --- prior predictive checks ---
  tar_target(pfit, fit_brms_model(data, resp = resp, rhs = rhs,
                                  delta = delta, prior_check = TRUE),
             format = "rds"),
  tar_target(pp_prior, pp_summarise(pfit_dep, data, M = 20,
                                    ndraws_per_imp = n_pp_draws, corr = FALSE)),
  tar_target(prior_tab,   pp_table(pp_prior_dep)),
  tar_target(prior_stat_tab, pp_stat_table(pp_prior_dep)),
  tar_target(prior_plots, pp_plots(pp_prior_dep, "Prior", zoom = 4,
             trim = c(0.01, 0.99)),
             format = "rds"),
  # --- posterior predictive checks ---
  tar_target(pp_post, pp_summarise(fit_dep, data, M = 20,
                                   ndraws_per_imp = n_pp_draws)),
  tar_target(post_tab,    pp_table(pp_post_dep)),
  tar_target(post_stat_tab,  pp_stat_table(pp_post_dep)),
  tar_target(post_pair_tab, pp_pair_table(pp_post_dep)),
  tar_target(post_corr,   pp_corr_table(pp_post_dep)),
  tar_target(post_plots,  pp_plots(pp_post_dep, "Posterior"), format = "rds")
)

# Full pipeline:
list(
    # Data import
    tar_target(csv_file, here("data", "raw", "data_recoded.csv"),
               format = "file"),
    tar_target(data_raw, read_csv(csv_file)),
    # Data cleaning and recoding
    tar_target(
        data_prep,
        data_raw |>
            df_rename() |>
            df_select_cols_recoding() |>
            df_recode()
    ),
    # Data imputation (Multiple Imputation by Chained Equations)
    tar_target(
        imp,
        data_prep |> na_impute()
    ),
    # Complete datasets with composite scores
    tar_target(
        data_imp,
        mice::complete(imp, action = "all") |>
            map(\(df) df |> df_add_composites(ind_threat, ind_deprivation,
                                              ind_unpredictability))
    ),
    # Alternative complete datasets where indicators for threat and deprivation
    # include the two adversity variables with a high percentage of missing
    # values.
    tar_target(
        data_imp_high_NAs,
        mice::complete(imp, action = "all") |>
            map(\(df) df |> df_add_composites(c(ind_threat, "PhysicalHarm"),
                                              c(ind_deprivation,
                                                "NutritionDep"),
                                              ind_unpredictability))
    ),
    tar_target(
        # list of correlation matrices
        corr_matrices,
        make_corr_matrices(data_imp)
    ),

    tar_target(
        # list of correlation matrices
        corr_matrices_composites_high_NAs,
        make_corr_matrices(data_imp_high_NAs)
    ),

    # Pipeline section defined above that fits and process all models
    per_model,

    # 
    tar_combine(chks, per_model[["chk"]],
                command = clean_names(list(!!!.x), "chk")),
    tar_combine(sampler_tab, per_model[["smp"]],
                command = combine_tables(list(!!!.x), "smp")),
    tar_combine(summary_tab, per_model[["tab"]],
                command = combine_tables(list(!!!.x), "tab")),

    # predictive check tables (one long table each, with a `model` column)
    tar_combine(prior_tab_all, per_model[["prior_tab"]],
                command = combine_tables(list(!!!.x), "prior_tab")),
    tar_combine(prior_stat_tab_all, per_model[["prior_stat_tab"]],
              command = combine_tables(list(!!!.x), "prior_stat_tab")),
    tar_combine(post_tab_all,  per_model[["post_tab"]],
                command = combine_tables(list(!!!.x), "post_tab")),
    tar_combine(post_stat_tab_all,  per_model[["post_stat_tab"]],
              command = combine_tables(list(!!!.x), "post_stat_tab")),
    tar_combine(post_corr_all, per_model[["post_corr"]],
                command = combine_tables(list(!!!.x), "post_corr")),
    tar_combine(post_pair_tab_all, per_model[["post_pair_tab"]],
                command = combine_tables(list(!!!.x), "post_pair_tab")),
    tar_target(post_pair_plot_all, pp_plot_pairs_all(post_pair_tab_all),
               format = "rds"),

    # plots: named lists (one element per model, each with $dens and $stat)
    tar_combine(prior_plots_all, per_model[["prior_plots"]],
                command = clean_names(list(!!!.x), "prior_plots")),
    tar_combine(post_plots_all,  per_model[["post_plots"]],
                command = clean_names(list(!!!.x), "post_plots")),

    tar_target(gt_tab, gt_summary_table(summary_tab,
                                        title = "Bayesian regression results")),
    tar_target(gt_file, {
      dir.create("output", showWarnings = FALSE)
      gt::gtsave(gt_tab, "output/table1.docx")
      "output/table1.docx"
    }, format = "file"),

    # Model comparisons
     tar_target(
        model_comp1A_1C,
        brms::loo(fit_model_vars_high_NA_1A,
                  fit_model_vars_high_NA_1C, compare = TRUE, reloo = TRUE)
     ),
     tar_target(
        model_comp2A_2C,
        brms::loo(fit_model_vars_high_NA_2A,
                  fit_model_vars_high_NA_2C, compare = TRUE, reloo = TRUE)
     ),
     tar_target(
        model_comp3A_3C,
        brms::loo(fit_model_vars_high_NA_3A,
                  fit_model_vars_high_NA_3C, compare = TRUE, reloo = TRUE)
     ),
     tar_target(
        model_comp1A_2A,
        brms::loo(fit_model_vars_high_NA_1A,
                  fit_model_vars_high_NA_2A, compare = TRUE, reloo = TRUE)
     ),
     tar_target(
        model_comp1A_3A,
        brms::loo(fit_model_vars_high_NA_1A,
                  fit_model_vars_high_NA_3A, compare = TRUE, reloo = TRUE)
     ),

    # Render Quarto website
    tar_quarto(
      site,
      path = ".",
      quiet = FALSE
    )
)
