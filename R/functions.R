
# %% Data preparation

# df_rename <- function(df, lookup) {
df_rename <- function(df) {
    df |> rename(all_of(rename_lookup))
}

# df_target_cols <- function(df, composite_vars) {
df_select_cols <- function(df) {
    # Moderators
    moderators <- c("EstimatedAgeArrivalGroup", "EstimatedAgeArrival",
                    "YearsCenter")
    # Covariates and random effects
    covariates <- c("Species_2", "Sex_2")
    random_eff <- "ConfigurationName"

   df |> select(all_of(c(composite_scores$CRS, moderators, covariates,
                         random_eff, composite_scores$Anxiety,
                         composite_scores$Depression, composite_scores$ISS)))
}

df_recode <- function(df) {
    # Recode `EstimatedAgeArrival`: currently a character column instead of
    # numeric because of the "born at the center".
    df |>  mutate(
        EstimatedAgeArrival = case_when(
            EstimatedAgeArrival == "born at the center" ~ 0,
            TRUE ~ suppressWarnings(as.numeric(EstimatedAgeArrival))
    ))
}

df_add_composites <- function(df) {

    # 1. Create the standardized and reverse-coded dataset
    df_transformed <- df |>
      # Standardise all numeric variables
      mutate(across(where(is.numeric), \(x) as.numeric(scale(x)))) |>
      # Switch sign of reverse-coded items
      mutate(across(any_of(vec_rev_items), \(x) -x))
    
    # 2. Calculate composites using the transformed data
    composites <- composite_scores |>
      purrr::map(\(vars) {
        df_transformed |> 
          select(any_of(vars)) |> 
          rowMeans(na.rm = FALSE)
      })

    # 3. Scale the composites to ease convergence during model fitting
    composites_scaled <- composites |>
        map(\(x) as.numeric(scale(x)))
    
    # 4. Bind the scaled composite columns to the transformed dataframe
    bind_cols(df_transformed, composites_scaled)

}



# %% Handling of missing values

# Impute missing values randomly (for prior predictive checks)
na_random_impute <- function(df) {
    cols <- c(names(composite_scores),
              "YearsCenter", "EstimatedAgeArrival")

    df |>
        mutate(
            across(all_of(cols),
                   \(x) replace(x, is.na(x), rnorm(sum(is.na(x)), sd=0.5))
            )
        )
}



# %% Model fitting

df_cols_prior_checks <- function(df) {
    # Moderators
    moderators <- c("EstimatedAgeArrivalGroup", "EstimatedAgeArrival",
                    "YearsCenter")
    # Covariates and random effects
    covariates <- c("Species_2", "Sex_2")
    random_eff <- "ConfigurationName"

    cols <- c(names(composite_scores),
              moderators, covariates, random_eff)
    df |> select(all_of(cols))
}

model_priors <- function() {
  p <- c(
    # --- Anxiety specific ---
    prior(normal(0, 0.5), class = "b", resp = "Anxiety"),
    prior(normal(0, 1), class = "sd", resp = "Anxiety"),
    prior(normal(0, 1), class = "sigma", resp = "Anxiety"),
    
    # --- Depression specific ---
    prior(normal(0, 0.5), class = "b", resp = "Depression"),
    prior(normal(0, 1), class = "sd", resp = "Depression"),
    prior(normal(0, 1), class = "sigma", resp = "Depression"),
    
    # --- ISS specific ---
    prior(normal(0, 0.5), class = "b", resp = "ISS"),
    prior(normal(0, 1), class = "sd", resp = "ISS"),
    prior(normal(0, 1), class = "sigma", resp = "ISS"),
    
    # --- Global correlation priors ---
    prior(lkj(2), class = "cor"),
    prior(lkj(2), class = "rescor")
  )
  
  return(p)
}

fit_brms_model <- function(df, rhs, prior_check = FALSE) {
    bf_anxiety <- bf(as.formula(paste("Anxiety", rhs)))
    bf_depression <- bf(as.formula(paste("Depression", rhs)))
    bf_iss <- bf(as.formula(paste("ISS", rhs)))

    # Combine into a multivariate formula
    mv_formula <- mvbf(bf_anxiety, bf_depression, bf_iss) + set_rescor(TRUE)

    sample_prior_opt = "no"
    if(prior_check) {
        sample_prior_opt = "yes"
    }

    brm(
      formula = mv_formula,
      data = df,
      prior = model_priors(),
      family = gaussian(),
      sample_prior = sample_prior_opt,
      chains = 4,
      cores = 4,
      iter = 4000,
      warmup = 2000,
      seed = 1848,
      backend = "cmdstanr"
    )
}

# "Right-hand side" part of brms syntax for each model
rhs_models <- list(
    model1A = "~
        # Suppress overall intercept
        0 +
        # Global baseline for individual of a given species and sex
        Species_2:Sex_2 +
        # Predictors
        Threat + Deprivation + Unpredictability +
        # Clustering factor
        (1 | p | ConfigurationName)
    ",
    model1B = "~
        # Suppress overall intercept
        0 +
        # Global baseline for individual of a given species and sex
        Species_2:Sex_2 +
        # Predictors
        CRS +
        # Covariates
        Species_2 + Sex_2 +
        # Clustering factor
        (1 | p | ConfigurationName)
    ",
    model2A = "~
        # Suppress overall intercept
        0 +
        # Global baseline for individual of a given species and sex
        Species_2:Sex_2 +
        # Predictors
        Threat*EstimatedAgeArrival + Deprivation*EstimatedAgeArrival +
        Unpredictability*EstimatedAgeArrival +
        # Clustering factor
        (1 | p | ConfigurationName)
    ",
    model2B = "~
        # Suppress overall intercept
        0 +
        # Global baseline for individual of a given species and sex
        Species_2:Sex_2 +
        # Predictor
        CRS*EstimatedAgeArrival +
        # Covariates
        Species_2 + Sex_2 +
        # Clustering factor
        (1 | p | ConfigurationName)
    ",
    model3A = "~
        # Suppress overall intercept
        0 +
        # Global baseline for individual of a given species and sex
        Species_2:Sex_2 +
        # Predictors
        Threat*YearsCenter + Deprivation*YearsCenter +
        Unpredictability*YearsCenter +
        # Covariates
        Species_2 + Sex_2 +
        # Clustering factor
        (1 | p | ConfigurationName)
    ",
    model3B = "~
        # Suppress overall intercept
        0 +
        # Global baseline for individual of a given species and sex
        Species_2:Sex_2 +
        # Predictor
        CRS*YearsCenter +
        # Covariates
        Species_2 + Sex_2 +
        # Clustering factor
        (1 | p | ConfigurationName)
    "
)
