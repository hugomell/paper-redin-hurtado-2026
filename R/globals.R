
################################################################################
# --- READ-ONLY VARIABLES TO DEFINE REGRESSION MODELS --- 
################################################################################

# List of identifiers for each model to be fitted.
# * `model_multi_` is used as a prefix for all multivariate models with Anxiety,
#    Depression and ISS as response variables
# * `model_psychpath_` is used as a prefix for all univariate models with the
#   global psychopathology score as a response variable
ids_comb <- tidyr::expand_grid(c("model_multi_", "model_psychpath_",
                                 "model_vars_high_NA_"),
                          1:3,
                          stringr::str_to_upper(letters[1:4]))
model_ids <- do.call(paste0, c(ids_comb))

# Define the formulas for all regression models
resp_variables <- c(rep(list(c("Anxiety", "Depression", "ISS")), 12),
                    rep("Psychpath", 12),
                    rep(list(c("Anxiety", "Depression", "ISS")), 12))
rhs <- c(
    "# Model 1A
     0 + # Suppress overall intercept
     # Absolute baselines for Males and Females
     Sex_2 +
     # Hierarchical intercept for species (n = 5 species)
     (1 | Species_2) +
     # Predictors
     Threat + Deprivation + Unpredictability
    ",
    "# Model 1B
     0 + # Suppress overall intercept
     # Absolute baselines for Males and Females
     Sex_2 +
     # Hierarchical intercept for species (n = 5 species)
     (1 | Species_2) +
     # Predictor
     CRS
    ",
    "# Model 1C
     0 + # Suppress overall intercept
     # Absolute baselines for Males and Females
     Sex_2 +
     # Hierarchical intercept for species (n = 5 species)
     (1 | Species_2) +
     # Predictor
     CRS_2
    ",
    "# Model 1D
     0 + # Suppress overall intercept
     # Absolute baselines for Males and Females
     Sex_2 +
     # Hierarchical intercept for species (n = 5 species)
     (1 | Species_2) +
     # Predictors
     Harshness + Unpredictability
    ",
    "# Model 2A
     0 + # Suppress overall intercept
     # Absolute baselines for Males and Females
     Sex_2 +
     # Hierarchical intercept for species (n = 5 species)
     (1 | Species_2) +
     # Predictors
     Threat*EstimatedAgeArrival + Deprivation*EstimatedAgeArrival +
     Unpredictability*EstimatedAgeArrival
    ",
    "# Model 2B
     0 + # Suppress overall intercept
     # Absolute baselines for Males and Females
     Sex_2 +
     # Hierarchical intercept for species (n = 5 species)
     (1 | Species_2) +
     # Predictors
     CRS*EstimatedAgeArrival
    ",
    "# Model 2C
     0 + # Suppress overall intercept
     # Absolute baselines for Males and Females
     Sex_2 +
     # Hierarchical intercept for species (n = 5 species)
     (1 | Species_2) +
     # Predictors
     CRS_2*EstimatedAgeArrival
    ",
    "# Model 2D
     0 + # Suppress overall intercept
     # Absolute baselines for Males and Females
     Sex_2 +
     # Hierarchical intercept for species (n = 5 species)
     (1 | Species_2) +
     # Predictors
     Harshness*EstimatedAgeArrival + Unpredictability*EstimatedAgeArrival
    ",
    "# Model 3A
     0 + # Suppress overall intercept
     # Absolute baselines for Males and Females
     Sex_2 +
     # Hierarchical intercept for species (n = 5 species)
     (1 | Species_2) +
     # Predictors
     Threat*YearsCenter + Deprivation*YearsCenter +
     Unpredictability*YearsCenter
    ",
    "# Model 3B
     0 + # Suppress overall intercept
     # Absolute baselines for Males and Females
     Sex_2 +
     # Hierarchical intercept for species (n = 5 species)
     (1 | Species_2) +
     # Predictors
     CRS*YearsCenter
    ",
     "# Model 3C
      0 + # Suppress overall intercept
      # Absolute baselines for Males and Females
      Sex_2 +
      # Hierarchical intercept for species (n = 5 species)
      (1 | Species_2) +
      # Predictors
      CRS_2*YearsCenter
    ",
     "# Model 3D
      0 + # Suppress overall intercept
      # Absolute baselines for Males and Females
      Sex_2 +
      # Hierarchical intercept for species (n = 5 species)
      (1 | Species_2) +
      # Predictors
      Harshness*YearsCenter + Unpredictability*YearsCenter 
    "
)

# Assemble the models information in a data frame
models <-  tibble::tibble(
    model = model_ids,
    resp = resp_variables,
    rhs = rep(rhs, 3),
    delta = 0.95,
    data = c(rep(rlang::syms("data_imp"), 24),
             rep(rlang::syms("data_imp_high_NAs"), 12))
)
models <- models |>
    dplyr::mutate(
        delta = dplyr::if_else(
            model %in% c("model_multi_1D",
                      "model_multi_2B",
                      "model_multi_3B",
                      "model_psychpath_1A",
                      "model_psychpath_1C",
                      "model_psychpath_3B"
                     ),
            0.99,
            delta)
    )


################################################################################
# --- READ-ONLY VARIABLES TO RECODE AND CONSTRUCT REGRESSION VARIABLES ---
################################################################################

# Mapping for consistent column names (<new_name> = "<old_name>")
rename_lookup <- c(
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

# List of items that are reverse-coded
vec_rev_items <- c("Grooming", "SocialPlay", "OtherAffiliative",
                   "OtherAgonistic")
# NB: Reverse coded items from the macaques SRS are already reversed in the
# raw data file.

# Adversity predictors
ind_threat <- "LifeExperience"
ind_deprivation <- c("TypeRearing", "InfancySocialExposure",
                     "JuvenileSocialExposure", "MotherSeparation")
ind_unpredictability <- "Relocations"
ind_cum_risk <- c(ind_threat, ind_deprivation, ind_unpredictability)

# Response variables
ind_anxiety <- c("GenitalSelf", "ScratchingRubbing", "OtherSelfDirected",
                 "Yawning", "SelfDirected", "SelfAbuse",
                 "Kinetic", "Oral", "Others", "SociallyTense",
                 "NoEyeContact", "SociallyConfident", "Stereotypes"
                 )
ind_depression <- c("Postural", "Grooming", "SocialPlay", "OtherAffiliative",
                    "OtherAgonistic", "SocialAvoidance", "Lonely",
                    "RestrictedInterests")
ind_iss <- c("Rank", "Playful", "BizarreBehaviours", "StaresIntoSpace",
              "SociallyAwkward", "NoPhysicalCoordinated",
              "SpeciesTypicalReaction", "CommunicationSkills")

# Definition of the composite indices
composite_scores <- list(
    Threat = ind_threat,
    Deprivation = ind_deprivation,
    Unpredictability = ind_unpredictability,
    CRS = ind_cum_risk, # alternate CRS_2 cumulative and Harshness indices are
                        # computed  directly in function `df_add_composites()`.
    Anxiety = ind_anxiety,
    Depression = ind_depression,
    ISS = ind_iss,
    Psychpath = c(ind_anxiety, ind_depression, ind_iss)
)
