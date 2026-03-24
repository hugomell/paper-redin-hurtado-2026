
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
    ## Moderators
    moderators <- c("EstimatedAgeArrivalGroup", "EstimatedAgeArrival",
                    "YearsCenter")
    ## Covariates and random effects
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
    ## Recode `EstimatedAgeArrival`: currently a character column instead of
    ## numeric because of the "born at the center". We to need replace it by 0
    ## and convert all other values to numbers.
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
               df |> select(all_of(vars)) |> rowMeans(na.rm = TRUE)
            })
        )
}
