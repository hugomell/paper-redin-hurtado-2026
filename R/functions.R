
# %% Rename variables

get_rename_lookup <- function() {
    c(
        YearsExposed = "Years exposed to adversity",
        PctLifeExposed = "% of lifetime exposed to adversity (years)",
        SelfDirected = "Self-directed",
        SelfAbuse = "Self-abuse",
        GenitalSelf = "Genital self-inspection",
        OtherSelfDirected = "Other self-directed behaviours",
        ScratchingRubbing = "Scratching/ Rubbing",
        SelfGroom = "Self-groom",
        OtherAffiliative = "Other affiliative",
        OtherAgonistic = "Other agonistic",
        SocialPlay = "Social play",
        SociallyConfident = "SociallyConfident(R)",
        CommunicationSkills = "CommunicationSkills(R)",
        NoEyeContact = "(Not)EyeContact",
        Playful = "Playful(R)",
        SpeciesTypicalReaction = "Species-typicalReaction(R)"
    )
}

df_rename <- function(df, lookup) {
    df |> rename(all_of(lookup))
}

# %% Definition of composite variables

list_composite_vars <- function() {
    # Adversity predictors
    ind_threat <- c("LifeExperience", "PhysicalHarm")
    ind_deprivation <- c("TypeRearing", "InfancySocialExposure",
                         "JuvenileSocialExposure", "NutritionDep")
    ind_unpredictability <- c("MotherSeparation", "Relocations")
    ind_cum_risk <- c(ind_threat, ind_deprivation, ind_unpredictability)

    # Response variables
    ind_anxiety <- c("GenitalSelf", "ScratchingRubbing", "OtherSelfDirected",
                     "Yawning", "SelfDirected", "SelfAbuse",
                     "Kinetic", "Oral", "Others", "SociallyTense",
                     "NoEyeContact", "SociallyConfident", "Stereotypes")
    ind_depression <- c("Postural", "Grooming", "SocialPlay", "OtherAffiliative",
                        "OtherAgonistic", "SocialAvoidance", "Lonely",
                        "RestrictedInterests")
    ind_icsf <- c("Rank", "Playful", "BizarreBehaviours", "StaresIntoSpace",
                  "SociallyAwkward", "NoPhysicalCoordinated",
                  "SpeciesTypicalReaction", "CommunicationSkills")

    composite_scores <- list(
        Threat = ind_threat,
        Deprivation = ind_deprivation,
        Unpredictability = ind_unpredictability,
        CRS = ind_cum_risk,
        Anxiety = ind_anxiety,
        Depression = ind_depression,
        ICSF = ind_icsf
    )
    composite_scores
}

# %% Select columns for analysis

df_target_cols <- function(df, composite_vars) {
    # Moderators
    moderators <- c("EstimatedAgeArrivalGroup", "EstimatedAgeArrival",
                    "YearsCenter")
    # Covariates and random effects
    covariates <- c("Species_2", "Sex_2")
    random_eff <- "ConfigurationName"

   df |> select(all_of(c(composite_vars$CRS, moderators, covariates,
                         random_eff, composite_vars$Anxiety,
                         composite_vars$Depression, composite_vars$ICSF)))
}

# %% Recode variables and compute composite scores

vec_rev_items <- function() {
     c(
        # SRS items
        "SociallyConfident", "Playful", "SpeciesTypicalReaction",
        "CommunicationSkills", 
        # Behavioural rates
        "Grooming", "SocialPlay", "OtherAffiliative", "OtherAgonistic"
    )
}

df_recode <- function(df) {
    # Recode `EstimatedAgeArrival`: currently a character column instead of
    # numeric because of the "born at the center". We to need replace it by 0
    # and convert all other values to numbers.
    df <- df |>  mutate(
        EstimatedAgeArrival = case_when(
            EstimatedAgeArrival == "born at the center" ~ 0,
            TRUE ~ suppressWarnings(as.numeric(EstimatedAgeArrival))
    ))
}

df_add_composite_scores <- function(df, scores_list) {
    df |>
        # standardise all numeric variables
        mutate(across(where(is.numeric),
                      scale)) |>
        # switch sign of reverse-coded items
        mutate(across(all_of(vec_rev_items()), \(x) -x)) |>
        bind_cols(scores_list |>
            map(\(vars) {
               df |> select(all_of(vars)) |> rowMeans(na.rm = FALSE)
            })
        )
}

# %% Prior predictive checks

na_random_impute <- function(df) {
    cols <- c(names(list_composite_vars()),
              "YearsCenter", "EstimatedAgeArrival")

    df |>
        mutate(
            across(all_of(cols),
                   \(x) replace(x, is.na(x), rnorm(sum(is.na(x))))
            )
        )
}

df_cols_prior_checks <- function(df) {
    # Moderators
    moderators <- c("EstimatedAgeArrivalGroup", "EstimatedAgeArrival",
                    "YearsCenter")
    # Covariates and random effects
    covariates <- c("Species_2", "Sex_2")
    random_eff <- "ConfigurationName"

    cols <- c(names(list_composite_vars()),
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
    
    # --- ICSF specific ---
    prior(normal(0, 0.5), class = "b", resp = "ICSF"),
    prior(normal(0, 1), class = "sd", resp = "ICSF"),
    prior(normal(0, 1), class = "sigma", resp = "ICSF"),
    
    # --- Global correlation priors ---
    prior(lkj(2), class = "cor"),
    prior(lkj(2), class = "rescor")
  )
  
  return(p)
}

fit_brms_model <- function(df, rhs, prior_check = FALSE) {
    bf_anxiety <- bf(as.formula(paste("Anxiety", rhs)))
    bf_depression <- bf(as.formula(paste("Depression", rhs)))
    bf_icsf <- bf(as.formula(paste("ICSF", rhs)))

    # Combine into a multivariate formula
    mv_formula <- mvbf(bf_anxiety, bf_depression, bf_icsf) + set_rescor(TRUE)

    sample_prior_opt = "no"
    if(prior_check) {
        sample_prior_opt = "only"
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

prior_predictive_checks_1A <- function(df) {
    model <- "~
        # Suppress overall intercept
        0 +
        # Global baseline for individual of a given species and sex
        Species_2:Sex_2 +
        # Predictors
        Threat + Deprivation + Unpredictability +
        # Clustering factor
        (1 | p | ConfigurationName)
    "

    fit_brms_model(df, model, prior_check = TRUE)
}

prior_predictive_checks_1B <- function(df) {
    model <- "~
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
    "

    fit_brms_model(df, model, prior_check = TRUE)
}

prior_predictive_checks_2A <- function(df) {
    model <- "~
        # Suppress overall intercept
        0 +
        # Global baseline for individual of a given species and sex
        Species_2:Sex_2 +
        # Predictors
        Threat*EstimatedAgeArrival + Deprivation*EstimatedAgeArrival +
        Unpredictability*EstimatedAgeArrival +
        # Clustering factor
        (1 | p | ConfigurationName)
    "

    fit_brms_model(df, model, prior_check = TRUE)
}

prior_predictive_checks_2B <- function(df) {
    model <- "~
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
    "

    fit_brms_model(df, model, prior_check = TRUE)
}

prior_predictive_checks_3A <- function(df) {
    model <- "~
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
    "

    fit_brms_model(df, model, prior_check = TRUE)
}

prior_predictive_checks_3B <- function(df) {
    model <- "~
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

    fit_brms_model(df, model, prior_check = TRUE)
}
