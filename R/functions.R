
# %% Data preparation

df_rename <- function(df) {
    df |> rename(all_of(rename_lookup))
}

df_select_cols_recoding <- function(df) {
    # Moderators
    moderators <- c("EstimatedAgeArrival", "YearsCenter")
    # Covariates and random effects
    covariates <- c("Species_2", "Sex_2")
    random_eff <- "ConfigurationName"

   df |> select(all_of(c(composite_scores$CRS, moderators, covariates,
                         random_eff, composite_scores$Anxiety,
                         composite_scores$Depression, composite_scores$ISS)))
}

df_select_cols_prior_checks <- function(df) {
    # Moderators
    moderators <- c("EstimatedAgeArrival", "YearsCenter")
    # Covariates and random effects
    covariates <- c("Species_2", "Sex_2")
    random_eff <- "ConfigurationName"

    cols <- c(names(composite_scores),
              moderators, covariates, random_eff)

    df |> select(all_of(cols))
}

# Takes a mice object as input, returns a list of imputed dataframes as output
df_select_cols_imputed <- function(imp) {
    # Moderators
    moderators <- c("EstimatedAgeArrival", "YearsCenter")
    # Covariates and random effects
    covariates <- c("Species_2", "Sex_2")
    random_eff <- "ConfigurationName"

    cols <- c(names(composite_scores),
              moderators, covariates, random_eff)

    imputed_list <- complete(imp, action = "all") |>
        map(\(x) select(x, all_of(cols)))

    return(imputed_list)
}

df_recode <- function(df) {
    # Recode `EstimatedAgeArrival`: currently a character column instead of
    # numeric because of the "born at the center".
    df |>  mutate(
        EstimatedAgeArrival = case_when(
            EstimatedAgeArrival == "born at the center" ~ 0,
            TRUE ~ suppressWarnings(as.numeric(EstimatedAgeArrival))
        ),
        Species_2 = case_when(
            Species_2 == "Macaca assamensis" ~ "Macaca_assamensis",
            TRUE ~ Species_2
            )
        )
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

# Impute missing values non randomly (for model fitting)
na_impute <- function(df) {
    predmat <- matrix(1, nrow = length(df), ncol = length(df))
    wheremat <- is.na(df)
    # wheremat[,c(11,12,13)] <- 0
    # predmat[,c(11,12,13)] <- 0
    
    imp <- mice(
      df,
      m = 20,
      predictorMatrix = predmat, 
      where = wheremat,
      # method = meth,          
      seed = 123,
      printFlag = TRUE
    )
  
    imp
}

# %% Model fitting

model_priors <- function() {
  p <- c(
    # --- Anxiety specific ---
    prior(normal(0, 0.5), class = "b", resp = "Anxiety"),
    prior(normal(0, 1), class = "sigma", resp = "Anxiety"),
    prior(normal(0, 1), class = "sd", resp = "Anxiety"),
    
    # --- Depression specific ---
    prior(normal(0, 0.5), class = "b", resp = "Depression"),
    prior(normal(0, 1), class = "sigma", resp = "Depression"),
    prior(normal(0, 1), class = "sd", resp = "Depression"),
    
    # --- ISS specific ---
    prior(normal(0, 0.5), class = "b", resp = "ISS"),
    prior(normal(0, 1), class = "sigma", resp = "ISS"),
    prior(normal(0, 1), class = "sd", resp = "ISS"),
    
    # --- Correlation priors ---
    prior(lkj(2), class = "rescor")
    # prior(lkj(2), class = "cor"),
  )
  
  return(p)
}

fit_brms_model <- function(df, rhs, prior_check = FALSE, mi = TRUE) {
    bf_anxiety <- bf(as.formula(paste("Anxiety", rhs)))
    bf_depression <- bf(as.formula(paste("Depression", rhs)))
    bf_iss <- bf(as.formula(paste("ISS", rhs)))

    # Combine into a multivariate formula
    mv_formula <- mvbf(bf_anxiety, bf_depression, bf_iss) + set_rescor(TRUE)

    sample_prior_opt = "yes"
    if(prior_check) {
        sample_prior_opt = "only"
    }

    if(mi) {
        brm_multiple(
          # formula = mv_formula,
          formula = mv_formula,
          data = df,
          prior = model_priors(),
          family = gaussian(),
          sample_prior = sample_prior_opt,
          control = list(adapt_delta = 0.95),
          chains = 4,
          cores = 4,
          iter = 4000,
          warmup = 2000,
          seed = 1848,
          backend = "cmdstanr",
          save_pars = save_pars(all = TRUE)
        )
    } else {
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
}

# "Right-hand side" part of brms syntax for each model
rhs_models <- list(
    model1A = "~
        # Suppress overall intercept
        0 +
        # Absolute baselines for Males and Females
        Sex_2 +
        # Hierarchical intercept for species (n = 5 species)
        (1 | Species_2) +
        # Predictors
        Threat + Deprivation + Unpredictability
    ",
    model1B = "~
        # Suppress overall intercept
        0 +
        # Absolute baselines for Males and Females
        Sex_2 +
        # Hierarchical intercept for species (n = 5 species)
        (1 | Species_2) +
        # Predictor
        CRS
    ",
    model2A = "~
        # Suppress overall intercept
        0 +
        # Absolute baselines for Males and Females
        Sex_2 +
        # Hierarchical intercept for species (n = 5 species)
        (1 | Species_2) +
        # Predictors
        Threat*EstimatedAgeArrival + Deprivation*EstimatedAgeArrival +
        Unpredictability*EstimatedAgeArrival
    ",
    model2B = "~
        # Suppress overall intercept
        0 +
        # Absolute baselines for Males and Females
        Sex_2 +
        # Hierarchical intercept for species (n = 5 species)
        (1 | Species_2) +
        # Predictors
        CRS*EstimatedAgeArrival
    ",
    model3A = "~
        # Suppress overall intercept
        0 +
        # Absolute baselines for Males and Females
        Sex_2 +
        # Hierarchical intercept for species (n = 5 species)
        (1 | Species_2) +
        # Predictors
        Threat*YearsCenter + Deprivation*YearsCenter +
        Unpredictability*YearsCenter
    ",
    model3B = "~
        # Suppress overall intercept
        0 +
        # Absolute baselines for Males and Females
        Sex_2 +
        # Hierarchical intercept for species (n = 5 species)
        (1 | Species_2) +
        # Predictors
        CRS*YearsCenter
    "
    # model1A = "~
    #     # Suppress overall intercept
    #     0 +
    #     # Global baseline for individual of a given species and sex
    #     Species_2:Sex_2 +
    #     # Predictors
    #     Threat + Deprivation + Unpredictability +
    #     # Clustering factor
    #     (1 | p | ConfigurationName)
    # ",
    # model1B = "~
    #     # Suppress overall intercept
    #     0 +
    #     # Global baseline for individual of a given species and sex
    #     Species_2:Sex_2 +
    #     # Predictors
    #     CRS +
    #     # Clustering factor
    #     (1 | p | ConfigurationName)
    # ",
    #model2A = "~
    #    # Suppress overall intercept
    #    0 +
    #    # Global baseline for individual of a given species and sex
    #    Species_2:Sex_2 +
    #    # Predictors
    #    Threat*EstimatedAgeArrival + Deprivation*EstimatedAgeArrival +
    #    Unpredictability*EstimatedAgeArrival +
    #    # Clustering factor
    #    (1 | p | ConfigurationName)
    #",
    #model2B = "~
    #    # Suppress overall intercept
    #    0 +
    #    # Global baseline for individual of a given species and sex
    #    Species_2:Sex_2 +
    #    # Predictor
    #    CRS*EstimatedAgeArrival +
    #    # Clustering factor
    #    (1 | p | ConfigurationName)
    #",
    #model3A = "~
    #    # Suppress overall intercept
    #    0 +
    #    # Global baseline for individual of a given species and sex
    #    Species_2:Sex_2 +
    #    # Predictors
    #    Threat*YearsCenter + Deprivation*YearsCenter +
    #    Unpredictability*YearsCenter +
    #    # Clustering factor
    #    (1 | p | ConfigurationName)
    #",
    #model3B = "~
    #    # Suppress overall intercept
    #    0 +
    #    # Global baseline for individual of a given species and sex
    #    Species_2:Sex_2 +
    #    # Predictor
    #    CRS*YearsCenter +
    #    # Clustering factor
    #    (1 | p | ConfigurationName)
    #"
)

# Takes as input a `brmsfit` object from fitting a model on multiple
# imputed datasets using `brm_multiple()`, and prints the individual
# convergence measures for each of the `m` imputed datasets.
check_diag_measures <- function(fit, m = 20) {

    draws <- posterior::as_draws_array(fit)
    draws_per_dat <- lapply(1:m,
                            \(i) posterior::subset_draws(draws, chain = i))
    list_diag_measures <- lapply(draws_per_dat, posterior::summarise_draws,
                                 posterior::default_convergence_measures())
    
    purrr::walk(1:m, \(x) {
        print(paste0("--- Imputed dataframe ", x, " : ---"))
        print(list_diag_measures[[x]], n = Inf)
    })

}
