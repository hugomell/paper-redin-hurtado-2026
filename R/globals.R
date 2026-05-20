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
vec_rev_items <- c(
  # SRS items
  "SociallyConfident", "Playful", "SpeciesTypicalReaction",
  "CommunicationSkills", 
  # Behavioural rates
  "Grooming", "SocialPlay", "OtherAffiliative", "OtherAgonistic"
)

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
ind_iss <- c("Rank", "Playful", "BizarreBehaviours", "StaresIntoSpace",
              "SociallyAwkward", "NoPhysicalCoordinated",
              "SpeciesTypicalReaction", "CommunicationSkills")

# Definition of the composite indices
composite_scores <- list(
    Threat = ind_threat,
    Deprivation = ind_deprivation,
    Unpredictability = ind_unpredictability,
    CRS = ind_cum_risk,
    Anxiety = ind_anxiety,
    Depression = ind_depression,
    ISS = ind_iss
)
