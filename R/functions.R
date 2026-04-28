
# %% Import data

get_data <- function(file) {
    read_csv(file)
}

# %% Rename variables

get_recode_lookup <- function() {
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
                     "Yawning", "SelfDirected", "Postural", "SelfAbuse",
                     "Kinetic", "Oral", "Others", "SociallyTense",
                     "NoEyeContact", "SociallyConfident", "Stereotypes")
    ind_depression <- c("Grooming", "SocialPlay", "OtherAffiliative",
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
        "SociallyConfident", "CommunicationSkills", "Playful",
        "SpeciesTypicalReaction",
        # Behavioural rates
        "Grooming", "SocialPlay", "OtherAffiliative"
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

df_prep_indicators <- function(df, rev_items) {
    df |>
        # standardise all numeric variables
        mutate(across(where(is.numeric),
                      scale)) |>
        # switch sign of reverse-coded items
        mutate(across(all_of(rev_items), \(x) -x))
}

df_add_composite_scores <- function(df, scores_list) {
    df |>
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

get_priors <- function() {
    # Note: 'class = b' applies to all fixed effects across all models
    # 'class = sd' applies to the group-level standard deviation
    # 'class = rescor' is the correlation between residuals
    def_univariate_priors <- function(response) {
        c(
            prior(normal(0, 1), class = "Intercept", resp = response),
            prior(normal(0, 0.5), class = "b", resp = response),
            prior(exponential(1), class = "sigma", resp = response),
            prior(exponential(1), class = "sd", resp = response),
        )
    }

    # Define the priors explicitly for each response variable
    priors <- c(
      # --- Priors for Response 1 (Anxiety) ---
      def_univariate_priors("Anxiety"),
      # --- Priors for Response 2 (Depression) ---
      def_univariate_priors("Depression"),
      # --- Priors for Response 3 (ICSF) ---
      def_univariate_priors("ICSF"),

      # --- Residual Correlation ---
      # This is a global metric between the responses, so it does not need a
      # 'resp' tag
      prior(lkj(2), class = "rescor") 
    )
    priors
}

make_bf <- function(response, rhs) {
    rhs <- "~
        # Predictors
        Threat + Deprivation + Unpredictability +
        # Covariates
        Species_2 + Sex_2 +
        # Clustering factor
        (1 | ConfigurationName)
    "
    bf(as.formula(paste(response, rhs)))
}

# model_rhs_formula <- function(model) {
#     rhs <- switch(model,
#     model1A = "~
#             # Predictors
#             Threat + Deprivation + Unpredictability +
#             # Covariates
#             Species_2 + Sex_2 +
#             # Clustering factor
#             (1 | ConfigurationName)
#         "
#     )
#     rhs
# }

prior_predictive_checks_1A <- function(df) {


    # Define individual formulas for each response variable
    bf_anxiety <- make_bf("Anxiety", model)
    bf_depression <- make_bf("Depression")
    bf_icsf <- make_bf("ICSF")

    # Get priors
    my_priors <- get_priors()
    
    # Combine into a multivariate formula
    mv_formula <- mvbf(bf_anxiety, bf_depression, bf_icsf) + set_rescor(TRUE)
    
    fit_prior <- brm(
      formula = mv_formula,
      data = df,
      family = gaussian(),
      prior = my_priors,
      sample_prior = "only",
      chains = 4,
      cores = 4,
      iter = 2000
    )
    fit_prior
}

prior_predictive_checks_1B <- function(df) {
    # Define individual formulas for each response variable
    bf_anxiety <- bf(
        Anxiety ~ 
            # Predictor
            CRS +
            # Covariates
            Species_2 + Sex_2 +
            # Clustering factor
            (1 | ConfigurationName)
    )
    bf_depression <- bf(
        Depression ~
            # Predictor
            CRS +
            # Covariates
            Species_2 + Sex_2 +
            # Clustering factor
            (1 | ConfigurationName)
    )
    bf_icsf <- bf(
        ICSF ~
            # Predictor
            CRS +
            # Covariates
            Species_2 + Sex_2 +
            # Clustering factor
            (1 | ConfigurationName)
    )

    # Get priors
    my_priors <- get_priors()
    
    # Combine into a multivariate formula
    mv_formula <- mvbf(bf_anxiety, bf_depression, bf_icsf) + set_rescor(TRUE)
    
    fit_prior <- brm(
      formula = mv_formula,
      data = df, # Your actual dataframe
      family = gaussian(),
      prior = my_priors,
      sample_prior = "only",
      chains = 4,
      cores = 4,
      iter = 2000
    )
    fit_prior
}

prior_predictive_checks_2A <- function(df) {
    # Define individual formulas for each response variable
    bf_anxiety <- bf(
        Anxiety ~ 
            # Predictors
            Threat*YearsCenter + Deprivation*YearsCenter +
            Unpredictability*YearsCenter +
            # Covariates
            Species_2 + Sex_2 +
            # Clustering factor
            (1 | ConfigurationName)
    )
    bf_depression <- bf(
        Depression ~
            # Predictors
            Threat*YearsCenter + Deprivation*YearsCenter +
            Unpredictability*YearsCenter +
            # Covariates
            Species_2 + Sex_2 +
            # Clustering factor
            (1 | ConfigurationName)
    )
    bf_icsf <- bf(
        ICSF ~
            # Predictors
            Threat*YearsCenter + Deprivation*YearsCenter +
            Unpredictability*YearsCenter +
            # Covariates
            Species_2 + Sex_2 +
            # Clustering factor
            (1 | ConfigurationName)
    )
    
    # Combine into a multivariate formula
    mv_formula <- mvbf(bf_anxiety, bf_depression, bf_icsf) + set_rescor(TRUE)
    
    # Get priors
    my_priors <- get_priors()

    fit_prior <- brm(
      formula = mv_formula,
      data = df, # Your actual dataframe
      family = gaussian(),
      prior = my_priors,
      sample_prior = "only",
      chains = 4,
      cores = 4,
      iter = 2000
    )
    fit_prior
}

prior_predictive_checks_2B <- function(df) {
    # Define individual formulas for each response variable
    bf_anxiety <- bf(
        Anxiety ~ 
            # Predictor
            CRS*YearsCenter +
            # Covariates
            Species_2 + Sex_2 +
            # Clustering factor
            (1 | ConfigurationName)
    )
    bf_depression <- bf(
        Depression ~
            # Predictor
            CRS*YearsCenter +
            # Covariates
            Species_2 + Sex_2 +
            # Clustering factor
            (1 | ConfigurationName)
    )
    bf_icsf <- bf(
        ICSF ~
            # Predictor
            CRS*YearsCenter +
            # Covariates
            Species_2 + Sex_2 +
            # Clustering factor
            (1 | ConfigurationName)
    )
    
    # Combine into a multivariate formula
    mv_formula <- mvbf(bf_anxiety, bf_depression, bf_icsf) + set_rescor(TRUE)
    
    # Get priors
    my_priors <- get_priors()

    fit_prior <- brm(
      formula = mv_formula,
      data = df, # Your actual dataframe
      family = gaussian(),
      prior = my_priors,
      sample_prior = "only",
      chains = 4,
      cores = 4,
      iter = 2000
    )
    fit_prior
}

prior_predictive_checks_3A <- function(df) {
    # Define individual formulas for each response variable
    bf_anxiety <- bf(
        Anxiety ~ 
            # Predictors
            Threat*EstimatedAgeArrival + Deprivation*EstimatedAgeArrival +
            Unpredictability*EstimatedAgeArrival +
            # Covariates
            Species_2 + Sex_2 +
            # Clustering factor
            (1 | ConfigurationName)
    )
    bf_depression <- bf(
        Depression ~
            # Predictors
            Threat*EstimatedAgeArrival + Deprivation*EstimatedAgeArrival +
            Unpredictability*EstimatedAgeArrival +
            # Covariates
            Species_2 + Sex_2 +
            # Clustering factor
            (1 | ConfigurationName)
    )
    bf_icsf <- bf(
        ICSF ~
            # Predictors
            Threat*EstimatedAgeArrival + Deprivation*EstimatedAgeArrival +
            Unpredictability*EstimatedAgeArrival +
            # Covariates
            Species_2 + Sex_2 +
            # Clustering factor
            (1 | ConfigurationName)
    )
    
    # Combine into a multivariate formula
    mv_formula <- mvbf(bf_anxiety, bf_depression, bf_icsf) + set_rescor(TRUE)
    
    # Get priors
    my_priors <- get_priors()

    fit_prior <- brm(
      formula = mv_formula,
      data = df, # Your actual dataframe
      family = gaussian(),
      prior = my_priors,
      sample_prior = "only",
      chains = 4,
      cores = 4,
      iter = 2000
    )
    fit_prior
}

prior_predictive_checks_3B <- function(df) {
    # Define individual formulas for each response variable
    bf_anxiety <- bf(
        Anxiety ~ 
            # Predictor
            CRS*EstimatedAgeArrival +
            # Covariates
            Species_2 + Sex_2 +
            # Clustering factor
            (1 | ConfigurationName)
    )
    bf_depression <- bf(
        Depression ~
            # Predictor
            CRS*EstimatedAgeArrival +
            # Covariates
            Species_2 + Sex_2 +
            # Clustering factor
            (1 | ConfigurationName)
    )
    bf_icsf <- bf(
        ICSF ~
            # Predictor
            CRS*EstimatedAgeArrival +
            # Covariates
            Species_2 + Sex_2 +
            # Clustering factor
            (1 | ConfigurationName)
    )
    
    # Combine into a multivariate formula
    mv_formula <- mvbf(bf_anxiety, bf_depression, bf_icsf) + set_rescor(TRUE)
    
    # Get priors
    my_priors <- get_priors()

    fit_prior <- brm(
      formula = mv_formula,
      data = df, # Your actual dataframe
      family = gaussian(),
      prior = my_priors,
      sample_prior = "only",
      chains = 4,
      cores = 4,
      iter = 2000
    )
    fit_prior
}
