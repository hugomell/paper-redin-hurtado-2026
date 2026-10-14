
################################################################################
# %% DATA PREPARATION BEFORE IMPUTATION
################################################################################

df_rename <- function(df) {
    df |> rename(all_of(rename_lookup))
}

df_select_cols_recoding <- function(df) {
    # Moderators
    moderators <- c("EstimatedAgeArrival", "YearsCenter")
    # Covariates and random effects
    covariates <- c("Species_2", "Sex_2")
    random_eff <- "ConfigurationName"

   df |> select(all_of(c("PhysicalHarm", "NutritionDep", composite_scores$CRS,
                         moderators, covariates, random_eff,
                         composite_scores$Anxiety, composite_scores$Depression,
                         composite_scores$ISS)))
}

df_select_cols_prior_checks <- function(df) {
    # Moderators
    moderators <- c("EstimatedAgeArrival", "YearsCenter")
    # Covariates and random effects
    covariates <- c("Species_2", "Sex_2")
    random_eff <- "ConfigurationName"

    cols <- c(names(composite_scores), "Harshness", moderators, covariates,
              random_eff)

    df |> select(all_of(cols))
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


################################################################################
# %% HANDLING OF MISSING VALUES
################################################################################

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


################################################################################
# %% COMPOSITE VARIABLES
################################################################################

df_add_composites <- function(df, ind_threat, ind_deprivation,
                              ind_unpredictability) {
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
          rowSums(na.rm = FALSE)
      })

    print(composites)

    # 3. Scale the composites to ease convergence during model fitting
    composites_scaled <- composites |>
        map(\(x) as.numeric(scale(x)))

    print(composites_scaled)
    
    # 4. Bind the scaled composite columns to the transformed dataframe
    df <- bind_cols(df_transformed, composites_scaled)

    print(df)

    # 5. Alternate adversity index as sum of scaled dimensional scores
    df <- df |>
        mutate(
            Harshness = as.numeric(scale(Threat + Deprivation)),
            CRS_2 = as.numeric(scale(Threat + Deprivation + Unpredictability))
        )

    df
}


################################################################################
# %% CORRELATION MATRICES
################################################################################

make_corr_matrix <- function(data_imp, cols) {
    # Functions to perform Fisher transformation and backtransformation
    # (see https://www.gerkovink.com/miceVignettes/Combining_inferences/Combining_inferences.html#correlations)
    fisher.trans <- function(x) 1/2 * log((1 + x) / (1 - x))
    fisher.backtrans <- function(x) (exp(2 * x) - 1) / (exp(2 * x) + 1)

    cor <- data_imp |>
      map(select, all_of(cols)) |>
      map(stats::cor) |>
      map(fisher.trans)

    cor.rect <- Reduce("+", cor) / length(cor)
    M <- fisher.backtrans(cor.rect)
    diag(M) <- 1
    M
}

make_corr_matrices <- function(data_imp) {
    matrices <- list()

    # Correlation matrix for adversity indicators (v1 without "NutritionDep" and
    # "Physical harm")
    cols <- c(ind_threat, ind_deprivation, ind_unpredictability)
    cols_v1 <- cols[!(cols %in% c("NutritionDep", "PhysicalHarm"))]
    matrices$adversity_v1 <- make_corr_matrix(data_imp, cols_v1)
    
    # Correlation matrix for adversity indicators (v2 with "NutritionDep" and
    # "Physical harm")
    matrices$adversity_v2 <- make_corr_matrix(data_imp, cols)

    # Correlation matrix for psychopathological symptoms
    matrices$psychopatho <- make_corr_matrix(
        data_imp,
        c(ind_anxiety, ind_depression, ind_iss)
    )

    # Correlation matrix between aggregate scores + moderators
    cols <- c(
        # Adversity
        "Threat", "Deprivation", "Unpredictability", "Harshness", "CRS",
        "CRS_2",
        # Psychopathological symptoms
        "Anxiety", "Depression", "ISS", "Psychpath",
        # Moderators
        "EstimatedAgeArrival", "YearsCenter"
    )
    matrices$all <- make_corr_matrix(data_imp, cols)

    matrices
}


################################################################################
# %% MODEL FITTING
################################################################################

get_model_info <- function(model_id, col) {
    models |> filter(id == model_id) |> pull({{ col }}) |> unlist()
}

model_priors <- function(model_type) {
    if (model_type == "multivariate") {
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
        )
    } else {
        p <- c(
          prior(normal(0, 0.5), class = "b"),
          prior(normal(0, 1), class = "sigma"),
          prior(normal(0, 1), class = "sd")
        )
    }

  return(p)
}

fit_brms_model <- function(df, resp, rhs, delta = 0.95, prior_check = FALSE) {
    model_type <- NULL
    model_formula <- NULL

    if (length(resp) == 1) {
        model_type <- "univariate"
        model_formula <- bf(reformulate(rhs, response = resp))
    } else {
        model_type <- "multivariate"
        list_formula <- resp |>
            map(\(response) bf(reformulate(rhs, response = response)))
        model_formula <- mvbf(flist = list_formula, rescor = TRUE)
    }
    
    sample_prior_opt = "yes"
    if(prior_check) {
        sample_prior_opt = "only"
    }

    brm_multiple(
          formula = model_formula,
          data = df,
          prior = model_priors(model_type),
          family = gaussian(),
          sample_prior = sample_prior_opt,
          control = list(adapt_delta = delta),
          cores = 12,
          chains = 4,
          iter = 4000,
          warmup = 2000,
          seed = 1848,
          backend = "cmdstanr",
          save_pars = save_pars(all = TRUE)
    )
}


################################################################################
# %% COMPUTATIONAL CHECKS
################################################################################

sampler_issues <- function(fit, M = NULL, max_treedepth = 10) {
  count_issues <- function(f) {
    np <- brms::nuts_params(f)
    np |>
      filter(Parameter %in% c("divergent__", "treedepth__")) |>
      group_by(Chain) |>
      summarise(
        divergences        = sum(Value[Parameter == "divergent__"]),
        max_treedepth_hits = sum(Value[Parameter == "treedepth__"] >= max_treedepth),
        .groups = "drop"
      )
  }


  # brms default combine = TRUE: chains are stacked imputation by imputation
  if (is.null(M)) M <- if (!is.null(fit$rhats)) nrow(fit$rhats) else NULL
  if (is.null(M)) stop("Can't determine the number of imputations; pass it as `M = `.")

  ch <- count_issues(fit)
  k  <- nrow(ch) / M
  if (k != round(k)) stop("Number of chains is not divisible by M.")
  out <- ch |> mutate(imputation = ceiling(row_number() / k), .before = 1)

  out |>
    group_by(imputation) |>
    summarise(across(c(divergences, max_treedepth_hits), sum),
              .groups = "drop")
}

# Takes as input a `brmsfit` object from fitting a model on multiple
# imputed datasets using `brm_multiple()`, and returns convergence measures
# ("Rhat", "Bulk ESS" and "Tail ESS") per parameter for each of the `m` imputed
# datasets.
# Worst case convergence measures per parameter across imputations are
# considered to identify rows that exceed thresholds for each convergence
# measure.
check_convergence <- function(fit, M = NULL,
                              rhat_max = 1.01,
                              ess_min  = NULL,
                              variable = "^(b_|sd_|rescor__|sigma)",
                              verbose  = TRUE) {

  get_draws <- function(x) {
    posterior::as_draws_array(x, variable = variable, regex = TRUE)
  }

  # Split the draws into one set of chains per imputed dataset
  if (is.null(M)) M <- if (!is.null(fit$rhats)) nrow(fit$rhats) else NULL
  if (is.null(M)) stop("Can't determine the number of imputations; pass it as `M = `.")

  draws_all <- get_draws(fit)
  k <- posterior::nchains(draws_all) / M
  if (k != round(k)) stop("Number of chains is not divisible by M.")

  draws_list <- lapply(seq_len(M), function(m)
    posterior::subset_draws(draws_all, chain = ((m - 1) * k + 1):(m * k)))

  # Default ESS threshold: 100 per chain (i.e. 400 for 4 chains per imputation)
  if (is.null(ess_min)) ess_min <- 100 * posterior::nchains(draws_list[[1]])
  # Diagnostics per imputation
  all_diag <- bind_rows(lapply(seq_along(draws_list), function(m) {
    posterior::summarise_draws(draws_list[[m]], "rhat", "ess_bulk", "ess_tail") |>
      mutate(imputation = m, .before = 1)
  })) |>
    rename(parameter = variable, Rhat = rhat, Bulk_ESS = ess_bulk,
           Tail_ESS = ess_tail) |>
    mutate(
      Rhat_flag      = Rhat > rhat_max,
      Bulk_ESS_flag  = Bulk_ESS < ess_min,
      Tail_ESS_flag  = Tail_ESS < ess_min,
      any_flag       = Rhat_flag | Bulk_ESS_flag | Tail_ESS_flag
    )

  # Worst case per parameter across imputations
  worst <- all_diag |>
    group_by(parameter) |>
    summarise(
      max_Rhat      = max(Rhat, na.rm = TRUE),
      min_Bulk_ESS  = min(Bulk_ESS, na.rm = TRUE),
      min_Tail_ESS  = min(Tail_ESS, na.rm = TRUE),
      n_imp_flagged = sum(any_flag),
      .groups = "drop"
    ) |>
    mutate(
      Rhat_flag     = max_Rhat > rhat_max,
      Bulk_ESS_flag = min_Bulk_ESS < ess_min,
      Tail_ESS_flag = min_Tail_ESS < ess_min
    )

  flagged <- filter(all_diag, any_flag)

  if (verbose) {
    cat(sprintf("Imputations checked: %d | Rhat threshold: < %g | ESS threshold: >= %g\n",
                length(draws_list), rhat_max, ess_min))
    if (nrow(flagged) == 0) {
      cat("All Rhat, bulk ESS and tail ESS values satisfy the thresholds.\n")
    } else {
      cat(sprintf("%d parameter/imputation combination(s) flagged:\n",
          nrow(flagged)))
      print(as.data.frame(flagged[, c("imputation", "parameter", "Rhat",
                                      "Bulk_ESS", "Tail_ESS")]),
            row.names = FALSE, digits = 4)
    }
  }

  invisible(list(
    all       = all_diag,   # long format: every parameter x imputation
    worst     = worst,      # worst values per parameter across imputations
    flagged   = flagged,    # only rows violating a threshold
    thresholds = c(rhat_max = rhat_max, ess_min = ess_min)
  ))
}


# %% Summarise models results

summarise_model <- function(fit,
                            convergence   = NULL,   # output of check_convergence()
                            variable = "^(b_|sd_|rescor__|sigma)",
                            prob          = 0.95,
                            point         = c("median", "mean"),
                            digits        = 2,
                            drop_prefix   = TRUE) {

  point <- match.arg(point)

  # Pooled posterior draws (all imputations combined)
  mat <- posterior::as_draws_matrix(
    posterior::as_draws_array(fit, variable = variable, regex = TRUE)
  )
  
  mat <- unclass(mat) # converts to a simple `matrix` / `array` object
  mat <- mat[, !colnames(mat) %in% c(".chain", ".iteration", ".draw"),
             drop = FALSE]


  lo <- (1 - prob) / 2
  hi <- 1 - lo
  ci_label <- sprintf("%g%% CrI", 100 * prob)

  out <- tibble(
    parameter = colnames(mat),
    Estimate  = apply(mat, 2, if (point == "median") median else mean),
    Est.Error = apply(mat, 2, sd),
    Lower     = apply(mat, 2, quantile, probs = lo, names = FALSE),
    Upper     = apply(mat, 2, quantile, probs = hi, names = FALSE),
    # Probability of direction: only meaningful for signed parameters
    pd        = apply(mat, 2, function(x) max(mean(x > 0), mean(x < 0)))
  ) |>
    mutate(pd = if_else(grepl("^(sd_|sigma)", parameter), NA_real_, pd))

  # Convergence diagnostics: worst case across imputations
  if (!is.null(convergence)) {
    out <- out |>
      left_join(
        convergence$worst |>
          select(parameter, Rhat = max_Rhat,
                 Bulk_ESS = min_Bulk_ESS, Tail_ESS = min_Tail_ESS),
        by = "parameter"
      )
  }

  if (drop_prefix) out <- mutate(out, parameter = sub("^b_", "", parameter))

  # Formatted version for reporting
  fmt <- paste0("%.", digits, "f")
  out <- out |>
    mutate(
      `Estimate [CrI]` = sprintf(paste0(fmt, " [", fmt, ", ", fmt, "]"),
                                        Estimate, Lower, Upper),
      .after = parameter
    )

  attr(out, "ci_label") <- ci_label
  out
}

# Same table for a named list of models -> one long data frame
summarise_models <- function(fits, convergence = NULL, ...) {
  stopifnot(!is.null(names(fits)))
  bind_rows(
    lapply(names(fits), function(nm) {
      summarise_model(fits[[nm]], convergence = convergence[[nm]], ...) |>
        mutate(model = nm, .before = 1)
    })
  )
}

# Print-ready table (round numbers, optional flags on convergence)
format_summary_table <- function(tab, digits = 2, flag = TRUE,
                                 rhat_max = 1.01, ess_min = 400) {
  ci <- attr(tab, "ci_label") %||% "CrI"
  out <- tab |>
    mutate(
      pd = ifelse(is.na(pd), "", sprintf("%.3f", pd)),
      across(any_of("Rhat"), ~ sprintf("%.3f", .x)),
      across(any_of(c("Bulk_ESS", "Tail_ESS")), ~ sprintf("%.0f", .x))
    )
  if (flag && "Rhat" %in% names(tab)) {
    out <- out |>
      mutate(
        Rhat     = ifelse(tab$Rhat     > rhat_max, paste0(Rhat, "*"),     Rhat),
        Bulk_ESS = ifelse(tab$Bulk_ESS < ess_min,  paste0(Bulk_ESS, "*"), Bulk_ESS),
        Tail_ESS = ifelse(tab$Tail_ESS < ess_min,  paste0(Tail_ESS, "*"), Tail_ESS)
      )
  }
  out |>
    select(any_of("model"), parameter, `Estimate [CrI]`,
           pd, any_of(c("Rhat", "Bulk_ESS", "Tail_ESS"))) |>
    rename_with(~ sub("CrI", ci, .x, fixed = TRUE), "Estimate [CrI]")
}


################################################################################
# %% PRIOR AND POSTERIOR PREDICTIVE CHECKS
################################################################################

`%||%` <- function(a, b) if (is.null(a)) b else a

# Moment-based skewness: per row of a draws x observations matrix, and for a vector
row_skewness <- function(M) {
  d  <- M - rowMeans(M)
  m2 <- rowMeans(d^2)
  rowMeans(d^3) / m2^1.5
}
skewness <- function(x) {
  x <- x[!is.na(x)]
  d <- x - mean(x)
  mean(d^3) / mean(d^2)^1.5
}

pp_stat_labels <- c(mean = "Mean", sd = "SD", min = "Minimum",
                    max = "Maximum", skew = "Skewness")


pp_summarise <- function(fit, data_list, M = length(data_list),
                         ndraws_per_imp = 200, n_plot_draws = 30,
                         corr = TRUE, responses = NULL, seed = 1848) {
  set.seed(seed)
  n_per <- brms::ndraws(fit) / M
  if (n_per != round(n_per))
    stop("Number of draws is not divisible by the number of imputations.")
  n_use <- min(ndraws_per_imp, n_per)

  res <- lapply(seq_len(M), function(m) {
    # draws of imputation m (chains are stacked imputation by imputation)
    ids_m <- ((m - 1) * n_per + 1):(m * n_per)
    ids   <- ids_m[unique(round(seq(1, n_per, length.out = n_use)))]

    yrep <- brms::posterior_predict(fit, newdata = data_list[[m]], draw_ids = ids)
    if (length(dim(yrep)) == 2) yrep <- array(yrep, dim = c(dim(yrep), 1))
    resp <- responses %||% dimnames(yrep)[[3]] %||%
      all.vars(fit$formula$formula[[2]])[1]
    if (length(resp) != dim(yrep)[3])
      stop("Could not match response names to the prediction array; pass `responses =`.")
    nd <- dim(yrep)[1]

    # per-draw statistics of the replicated data
    stats <- dplyr::bind_rows(lapply(seq_along(resp), function(j) {
      yj <- matrix(yrep[, , j], nrow = nd)
      tibble::tibble(
        imputation = m, draw = seq_len(nd), response = resp[j],
        mean_rep = rowMeans(yj),
        sd_rep   = apply(yj, 1, sd),
        min_rep  = apply(yj, 1, min),
        max_rep  = apply(yj, 1, max),
        skew_rep = row_skewness(yj)
      )
    }))

    # the same statistics for the observed data
    dat <- data_list[[m]]
    num <- function(r) as.numeric(dat[[r]])
    obs <- tibble::tibble(
      imputation = m, response = resp,
      mean_obs = vapply(resp, function(r) mean(num(r), na.rm = TRUE), numeric(1)),
      sd_obs   = vapply(resp, function(r) sd(num(r),   na.rm = TRUE), numeric(1)),
      min_obs  = vapply(resp, function(r) min(num(r),  na.rm = TRUE), numeric(1)),
      max_obs  = vapply(resp, function(r) max(num(r),  na.rm = TRUE), numeric(1)),
      skew_obs = vapply(resp, function(r) skewness(num(r)),           numeric(1))
    )

    cr <- NULL; pr <- NULL; pro <- NULL
    if (corr && length(resp) > 1) {
      Yobs <- vapply(resp, num, numeric(nrow(dat)))
      Cobs <- stats::cor(Yobs, use = "pairwise.complete.obs")
      Crep <- lapply(seq_len(nd), function(d)
        suppressWarnings(stats::cor(matrix(yrep[d, , ], ncol = length(resp)))))
    
      disc <- function(A, B) {
        D <- A - B; low <- D[lower.tri(D)]
        c(frobenius = sqrt(sum(D^2, na.rm = TRUE)),
          max_abs   = max(abs(low), na.rm = TRUE),
          mean_abs  = mean(abs(low), na.rm = TRUE))
      }
      partner <- (seq_len(nd) + floor(nd / 2) - 1) %% nd + 1
      obs_rep <- t(vapply(Crep, disc, numeric(3), B = Cobs))
      rep_rep <- t(vapply(seq_len(nd), function(d) disc(Crep[[d]], Crep[[partner[d]]]), numeric(3)))
    
      cr <- dplyr::bind_rows(
        tibble::as_tibble(obs_rep) |> dplyr::mutate(comparison = "observed vs replicated"),
        tibble::as_tibble(rep_rep) |> dplyr::mutate(comparison = "replicated vs replicated")
      ) |>
        dplyr::mutate(imputation = m, draw = rep(seq_len(nd), 2), .before = 1)
    
      # --- per-pair correlations ---
      idx  <- which(lower.tri(Cobs), arr.ind = TRUE)
      pair <- paste(resp[idx[, 1]], resp[idx[, 2]], sep = " vs ")
      np   <- nrow(idx)
      mat  <- matrix(vapply(Crep, function(C) C[idx], numeric(np)), nrow = np)  # pairs x draws
    
      pr  <- tibble::tibble(imputation = m,
                            draw    = rep(seq_len(nd), each = np),
                            pair    = rep(pair, times = nd),
                            rep_cor = as.vector(mat))
      pro <- tibble::tibble(imputation = m, pair = pair, obs_cor = Cobs[idx])
    }

    # replicated data for the density plots (imputation 1 only),
    # spread evenly over the retained draws (and so over all chains)
    pd <- NULL
    if (m == 1) {
      k  <- unique(round(seq(1, nd, length.out = min(n_plot_draws, nd))))
      pd <- lapply(seq_along(resp), function(j) list(
        response = resp[j],
        y        = num(resp[j]),
        yrep     = matrix(yrep[k, , j], nrow = length(k))
      ))
    }
    list(stats = stats, obs = obs, corr = cr, pairs = pr, pair_obs = pro,
         plot = pd)
  })

  list(
    stats    = dplyr::bind_rows(lapply(res, `[[`, "stats")),
    obs      = dplyr::bind_rows(lapply(res, `[[`, "obs")),
    corr     = dplyr::bind_rows(lapply(res, `[[`, "corr")),
    pairs    = dplyr::bind_rows(lapply(res, `[[`, "pairs")),
    pair_obs = dplyr::bind_rows(lapply(res, `[[`, "pair_obs")),
    plot     = res[[1]]$plot
  )
}

# ---- Long format: replicated draws and observed values per statistic --------

pp_stats_long <- function(pp, stats = c("mean", "sd", "min", "max", "skew")) {
  rep <- pp$stats |>
    tidyr::pivot_longer(dplyr::ends_with("_rep"), names_to = "statistic",
                        names_pattern = "(.*)_rep", values_to = "value") |>
    dplyr::filter(statistic %in% stats)
  obs <- pp$obs |>
    tidyr::pivot_longer(dplyr::ends_with("_obs"), names_to = "statistic",
                        names_pattern = "(.*)_obs", values_to = "observed") |>
    dplyr::filter(statistic %in% stats) |>
    dplyr::group_by(response, statistic) |>
    dplyr::summarise(observed = mean(observed), .groups = "drop")  # average over imputations
  list(rep = rep, obs = obs)
}

# ---- Tables -----------------------------------------------------------------

pp_table <- function(pp) {
  obs <- pp$obs |>
    dplyr::group_by(response) |>
    dplyr::summarise(observed = mean(mean_obs), .groups = "drop")

  pp$stats |>
    dplyr::group_by(response) |>
    dplyr::summarise(
      rep_mean = mean(mean_rep),
      rep_sd   = sd(mean_rep),
      q025     = quantile(mean_rep, 0.025, names = FALSE),
      med      = median(mean_rep),
      q975     = quantile(mean_rep, 0.975, names = FALSE),
      .groups  = "drop"
    ) |>
    dplyr::left_join(obs, by = "response") |>
    dplyr::mutate(
      Covered      = ifelse(observed >= q025 & observed <= q975, "Yes", "No"),
      `Scaled gap` = (observed - rep_mean) / rep_sd
    ) |>
    dplyr::transmute(
      `response variable` = response,
      `replicated mean`   = rep_mean,
      `replicated sd`     = rep_sd,
      `2.5%`              = q025,
      median              = med,
      `97.5%`             = q975,
      `observed value`    = observed,
      Covered,
      `Scaled gap`
    )
}

# Same columns as pp_table(), plus a `statistic` column
pp_stat_table <- function(pp, stats = c("mean", "sd", "min", "max", "skew")) {
  l <- pp_stats_long(pp, stats)
  l$rep |>
    dplyr::group_by(response, statistic) |>
    dplyr::summarise(
      rep_mean = mean(value),
      rep_sd   = sd(value),
      q025     = quantile(value, 0.025, names = FALSE),
      med      = median(value),
      q975     = quantile(value, 0.975, names = FALSE),
      .groups  = "drop"
    ) |>
    dplyr::left_join(l$obs, by = c("response", "statistic")) |>
    dplyr::mutate(
      Covered      = ifelse(observed >= q025 & observed <= q975, "Yes", "No"),
      `Scaled gap` = (observed - rep_mean) / rep_sd,
      statistic    = factor(statistic, levels = stats,
                            labels = unname(pp_stat_labels[stats]))
    ) |>
    dplyr::arrange(response, statistic) |>
    dplyr::transmute(
      `response variable` = response,
      statistic,
      `replicated mean`   = rep_mean,
      `replicated sd`     = rep_sd,
      `2.5%`              = q025,
      median              = med,
      `97.5%`             = q975,
      `observed value`    = observed,
      Covered,
      `Scaled gap`
    )
}

pp_corr_table <- function(pp) {
  if (nrow(pp$corr) == 0) return(tibble::tibble())
  lab <- c(frobenius = "Frobenius norm", max_abs = "Max abs. difference",
           mean_abs = "Mean abs. difference")
  long <- pp$corr |>
    tidyr::pivot_longer(c(frobenius, max_abs, mean_abs), names_to = "key", values_to = "value")

  base <- long |>
    dplyr::filter(comparison == "replicated vs replicated") |>
    dplyr::group_by(key) |>
    dplyr::summarise(`baseline median` = median(value), .groups = "drop")

  long |>
    dplyr::filter(comparison == "observed vs replicated") |>
    dplyr::group_by(key) |>
    dplyr::summarise(
      mean = mean(value), sd = sd(value),
      `2.5%` = quantile(value, 0.025, names = FALSE),
      median = median(value),
      `97.5%` = quantile(value, 0.975, names = FALSE),
      .groups = "drop"
    ) |>
    dplyr::left_join(base, by = "key") |>
    dplyr::mutate(Ratio = median / `baseline median`,
                  Discrepancy = unname(lab[key])) |>
    dplyr::arrange(match(key, names(lab))) |>
    dplyr::select(Discrepancy, mean, sd, `2.5%`, median, `97.5%`, `baseline median`, Ratio)
}

pp_pair_table <- function(pp) {
  if (nrow(pp$pairs) == 0) return(tibble::tibble())
  lv  <- unique(pp$pairs$pair)
  obs <- pp$pair_obs |>
    dplyr::group_by(pair) |>
    dplyr::summarise(observed = mean(obs_cor), .groups = "drop")   # average over imputations

  pp$pairs |>
    dplyr::group_by(pair) |>
    dplyr::summarise(
      rep_mean = mean(rep_cor),
      rep_sd   = sd(rep_cor),
      q025     = quantile(rep_cor, 0.025, names = FALSE),
      med      = median(rep_cor),
      q975     = quantile(rep_cor, 0.975, names = FALSE),
      .groups  = "drop"
    ) |>
    dplyr::left_join(obs, by = "pair") |>
    dplyr::mutate(
      Covered      = ifelse(observed >= q025 & observed <= q975, "Yes", "No"),
      `Scaled gap` = (observed - rep_mean) / rep_sd,
      pair         = factor(pair, levels = lv)
    ) |>
    dplyr::arrange(pair) |>
    dplyr::transmute(
      pair = as.character(pair),
      `replicated mean` = rep_mean,
      `replicated sd`   = rep_sd,
      `2.5%`            = q025,
      median            = med,
      `97.5%`           = q975,
      `observed correlation` = observed,
      Covered,
      `Scaled gap`
    )
}



# ---- Plots ------------------------------------------------------------------

pp_plot_dens <- function(pp, title, zoom = NULL) {
  plots <- lapply(pp$plot, function(p) {
    g <- bayesplot::ppc_dens_overlay(p$y, p$yrep) + ggplot2::labs(title = p$response)
    if (!is.null(zoom)) {
      g <- g + ggplot2::coord_cartesian(
        xlim = mean(p$y, na.rm = TRUE) + c(-1, 1) * zoom * sd(p$y, na.rm = TRUE))
    }
    g
  })
  patchwork::wrap_plots(plots) + patchwork::plot_annotation(title = title)
}

pp_plot_stat <- function(pp, title) {
  obs <- pp$obs |>
    dplyr::group_by(response) |>
    dplyr::summarise(observed = mean(mean_obs), .groups = "drop")
  ggplot2::ggplot(pp$stats, ggplot2::aes(mean_rep)) +
    ggplot2::geom_histogram(bins = 40, fill = "grey70", colour = "white") +
    ggplot2::geom_vline(data = obs, ggplot2::aes(xintercept = observed),
                        colour = "firebrick", linewidth = 1) +
    ggplot2::facet_wrap(~ response, scales = "free") +
    ggplot2::labs(x = "Replicated mean", y = NULL, title = title)
}

pp_plots <- function(pp, label, zoom = NULL) {
  list(
    dens = pp_plot_dens(pp, paste(label, "predictive check: density overlay"), zoom),
    stat = pp_plot_stat(pp, paste(label, "predictive check: replicated mean"))
  )
}

# ---- Plot: replicated statistic vs observed value, one panel per response x statistic

# trim: optional c(lower, upper) quantiles; replicated values outside are dropped
#       from the histogram for display only (useful for prior checks with extreme draws)
pp_plot_stats <- function(pp, title, stats = c("min", "max", "skew"), trim = NULL) {
  l <- pp_stats_long(pp, stats)
  d <- l$rep
  if (!is.null(trim)) {
    d <- d |>
      dplyr::group_by(response, statistic) |>
      dplyr::filter(value >= quantile(value, trim[1]), value <= quantile(value, trim[2])) |>
      dplyr::ungroup()
  }
  lab <- function(x) factor(x, levels = stats, labels = unname(pp_stat_labels[stats]))
  d$statistic <- lab(d$statistic)
  o <- l$obs
  o$statistic <- lab(o$statistic)

  ggplot2::ggplot(d, ggplot2::aes(value)) +
    ggplot2::geom_histogram(bins = 40, fill = "grey70", colour = "white") +
    ggplot2::geom_vline(data = o, ggplot2::aes(xintercept = observed),
                        colour = "firebrick", linewidth = 1) +
    ggplot2::facet_grid(response ~ statistic, scales = "free") +
    ggplot2::labs(x = "Replicated value (red line = observed)", y = NULL, title = title)
}

# ---- Plot for one model: replicated correlation intervals vs observed --------
pp_plot_pairs <- function(pp, title) {
  s <- pp$pairs |>
    dplyr::group_by(pair) |>
    dplyr::summarise(
      lo95 = quantile(rep_cor, 0.025, names = FALSE), hi95 = quantile(rep_cor, 0.975, names = FALSE),
      lo50 = quantile(rep_cor, 0.25,  names = FALSE), hi50 = quantile(rep_cor, 0.75,  names = FALSE),
      med  = median(rep_cor), .groups = "drop") |>
    dplyr::left_join(
      pp$pair_obs |> dplyr::group_by(pair) |>
        dplyr::summarise(observed = mean(obs_cor), .groups = "drop"),
      by = "pair")

  ggplot2::ggplot(s, ggplot2::aes(y = pair)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dotted", colour = "grey50") +
    ggplot2::geom_linerange(ggplot2::aes(xmin = lo95, xmax = hi95), linewidth = 0.6, colour = "grey55") +
    ggplot2::geom_linerange(ggplot2::aes(xmin = lo50, xmax = hi50), linewidth = 2.2, colour = "grey55") +
    ggplot2::geom_point(ggplot2::aes(x = med), colour = "grey25", size = 2) +
    ggplot2::geom_point(ggplot2::aes(x = observed), colour = "firebrick", shape = 18, size = 4) +
    ggplot2::labs(x = "Correlation (grey: replicated median, 50% and 95% intervals; red: observed)",
                  y = NULL, title = title)
}

pp_plots <- function(pp, label, zoom = NULL, trim = NULL) {
  list(
    dens  = pp_plot_dens(pp, paste(label, "predictive check: density overlay"), zoom),
    stat  = pp_plot_stat(pp, paste(label, "predictive check: replicated mean")),
    stats = pp_plot_stats(pp, paste(label, "predictive check: min, max and skewness"), trim = trim),
    pairs = if (nrow(pp$pairs) > 0)
      pp_plot_pairs(pp, paste(label, "predictive check: pairwise correlations"))
  )
}

# ---- Plot across models, from the combined table (one panel per pair) --------

pp_plot_pairs_all <- function(tab, title = "Posterior predictive check: pairwise correlations") {
  ggplot2::ggplot(tab, ggplot2::aes(y = model)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dotted", colour = "grey50") +
    ggplot2::geom_linerange(ggplot2::aes(xmin = `2.5%`, xmax = `97.5%`), colour = "grey55") +
    ggplot2::geom_point(ggplot2::aes(x = median), colour = "grey25", size = 1.5) +
    ggplot2::geom_point(ggplot2::aes(x = `observed correlation`, colour = Covered),
                        shape = 18, size = 3) +
    ggplot2::scale_colour_manual(values = c(Yes = "firebrick", No = "black"), guide = "none") +
    ggplot2::facet_wrap(~ pair) +
    ggplot2::labs(x = "Correlation (grey: replicated median and 95% interval; diamond: observed)",
                  y = NULL, title = title)
}

################################################################################
# %% REPORTING
################################################################################

gt_summary_table <- function(tab,
                             title        = NULL,
                             subtitle     = NULL,
                             digits       = 2,
                             rhat_max     = 1.01,
                             ess_min      = 400,
                             show_pd      = TRUE,
                             show_diag    = TRUE,
                             highlight    = "#F8D7DA",   # background for flagged cells
                             exclude_pars = "^(Intercept)$",  # regex of rows to drop; NULL keeps all
                             par_labels   = NULL,        # named vector: c(old = "New label")
                             file         = NULL) {      # e.g. "table1.html", ".docx", ".png", ".tex"

  ci_label <- attr(tab, "ci_label") %||% "95% CrI"
  has_diag <- all(c("Rhat", "Bulk_ESS", "Tail_ESS") %in% names(tab))
  has_model <- "model" %in% names(tab)

  if (!is.null(exclude_pars)) tab <- filter(tab, !grepl(exclude_pars, parameter))

  # Optional relabelling of parameters
  if (!is.null(par_labels)) {
    tab <- mutate(tab, parameter = dplyr::coalesce(unname(par_labels[parameter]), parameter))
  }

  # Flag columns (computed on the numeric values, before formatting)
  if (has_diag) {
    tab <- tab |>
      mutate(
        .f_rhat = Rhat     > rhat_max,
        .f_bulk = Bulk_ESS < ess_min,
        .f_tail = Tail_ESS < ess_min
      )
  }

  # Columns to display
  cols <- c("parameter", "Estimate", "Lower", "Upper")
  if (show_pd) cols <- c(cols, "pd")
  if (has_diag && show_diag) cols <- c(cols, "Rhat", "Bulk_ESS", "Tail_ESS")

  gt_tab <- tab |>
    select(any_of("model"), all_of(cols), starts_with(".f_")) |>
    gt(rowname_col = "parameter", groupname_col = if (has_model) "model" else NULL) |>
    fmt_number(columns = c(Estimate, Lower, Upper), decimals = digits) |>
    cols_merge(columns = c(Lower, Upper), pattern = "[{1}, {2}]") |>
    cols_label(
      Lower    = md(paste0("**", ci_label, "**")),
      parameter = "Parameter"
    ) |>
    cols_align(align = "center", columns = -1) |>
    tab_stubhead(label = "Parameter")

  if (show_pd) {
    gt_tab <- gt_tab |>
      fmt_number(columns = pd, decimals = 3) |>
      sub_missing(columns = pd, missing_text = "") |>
      cols_label(pd = "P(direction)")
  }

  if (has_diag && show_diag) {
    gt_tab <- gt_tab |>
      fmt_number(columns = Rhat, decimals = 3) |>
      fmt_number(columns = c(Bulk_ESS, Tail_ESS), decimals = 0) |>
      cols_label(Bulk_ESS = "Bulk ESS", Tail_ESS = "Tail ESS") |>
      # Highlight cells that violate thresholds
      tab_style(
        style     = list(cell_fill(color = highlight), cell_text(weight = "bold")),
        locations = cells_body(columns = Rhat, rows = .f_rhat)
      ) |>
      tab_style(
        style     = list(cell_fill(color = highlight), cell_text(weight = "bold")),
        locations = cells_body(columns = Bulk_ESS, rows = .f_bulk)
      ) |>
      tab_style(
        style     = list(cell_fill(color = highlight), cell_text(weight = "bold")),
        locations = cells_body(columns = Tail_ESS, rows = .f_tail)
      ) |>
      tab_spanner(label = "Convergence", columns = c(Rhat, Bulk_ESS, Tail_ESS))
  }

  # Hide the helper flag columns
  gt_tab <- gt_tab |> cols_hide(columns = starts_with(".f_"))

  if (show_pd || has_diag) {
    gt_tab <- gt_tab |>
      tab_spanner(label = "Posterior", columns = any_of(c("Estimate", "Lower", "Upper", "P(direction)")),
                  gather = FALSE)
  }

  # Titles and footnotes
  if (!is.null(title)) {
    gt_tab <- gt_tab |> tab_header(title = title, subtitle = subtitle)
  }

  notes <- c(
    paste0("Estimates are posterior medians or means pooled across imputed datasets; ",
           "intervals are equal-tailed ", ci_label, "s.")
  )
  if (show_pd) notes <- c(notes, "P(direction) = probability of direction.")
  if (has_diag && show_diag) {
    notes <- c(notes, sprintf(
      "Convergence diagnostics are the worst values across imputations; shaded cells have Rhat > %g or ESS < %g.",
      rhat_max, ess_min))
  }
  for (n in notes) gt_tab <- gt_tab |> tab_source_note(source_note = n)

  # Styling
  gt_tab <- gt_tab |>
    tab_options(
      table.font.size            = px(13),
      heading.align              = "left",
      column_labels.font.weight  = "bold",
      row_group.font.weight      = "bold",
      table.border.top.style     = "solid",
      table.border.bottom.style  = "solid",
      data_row.padding           = px(4)
    )

  if (!is.null(file)) gtsave(gt_tab, file)
  gt_tab
}

plot_prior_posterior <- function(fit, prior_fit,
                                 variable   = "^b_",
                                 exclude    = "Intercept",
                                 responses  = NULL,
                                 par_labels = NULL,
                                 xlim       = NULL,
                                 normalise  = c("each", "common"),
                                 response_colours = NULL,   # named vector, e.g. c(Anxiety = "#0072B2", ...)
                                 title      = NULL) {
  normalise <- match.arg(normalise)

  get_draws <- function(f) {
    m <- posterior::as_draws_array(f, variable = variable, regex = TRUE) |>
      posterior::as_draws_matrix() |>
      unclass()
    m[, !colnames(m) %in% c(".chain", ".iteration", ".draw"), drop = FALSE]
  }
  post  <- get_draws(fit)
  prior <- get_draws(prior_fit)

  cols <- colnames(post)
  if (!is.null(exclude)) cols <- cols[!grepl(exclude, cols)]
  cols <- intersect(cols, colnames(prior))
  if (length(cols) == 0) stop("No matching parameters found in both fits.")

  rng <- xlim
  if (is.null(rng)) {
    q   <- apply(post[, cols, drop = FALSE], 2, quantile, probs = c(0.005, 0.995), names = FALSE)
    rng <- range(q)
    rng <- rng + c(-1, 1) * 0.1 * diff(rng)
  }
  rng <- range(c(rng, 0))

  resp <- if (is.null(responses)) fit$formula$responses else responses
  parse_one <- function(p) {
    rest <- sub("^b_", "", p)
    if (length(resp) < 2) return(c(response = NA_character_, term = rest))
    hit <- resp[startsWith(rest, paste0(resp, "_"))]
    if (length(hit) == 0) return(c(response = NA_character_, term = rest))
    hit <- hit[which.max(nchar(hit))]
    c(response = hit, term = substring(rest, nchar(hit) + 2))
  }
  parsed <- t(vapply(cols, parse_one, character(2)))
  meta <- tibble::tibble(parameter = cols,
                         response  = parsed[, "response"],
                         term      = parsed[, "term"]) |>
    dplyr::mutate(label = if (is.null(par_labels)) term
                          else dplyr::coalesce(unname(par_labels[term]), term))
  lv <- rev(unique(meta$label))

  dens_one <- function(x) {
    d <- stats::density(x, from = rng[1], to = rng[2], n = 512)
    tibble::tibble(x = d$x, h = d$y)
  }
  d <- dplyr::bind_rows(lapply(cols, function(p) {
    dplyr::bind_rows(
      dplyr::mutate(dens_one(post[, p]),  source = "Posterior"),
      dplyr::mutate(dens_one(prior[, p]), source = "Prior")
    ) |> dplyr::mutate(parameter = p, .before = 1)
  }))

  d <- d |>
    dplyr::group_by(parameter, source) |>
    dplyr::mutate(h = if (normalise == "each") h / max(h) else h) |>
    dplyr::ungroup()
  if (normalise == "common") d$h <- d$h / max(d$h[d$source == "Posterior"])

  # ---- colours: grey prior, one colour per response for the posterior ----
  resp_levels <- if (anyNA(meta$response)) "Posterior" else unique(meta$response)
  if (is.null(response_colours)) {
    okabe <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7",
               "#E69F00", "#56B4E9", "#F0E442", "#999999")
    response_colours <- stats::setNames(okabe[(seq_along(resp_levels) - 1) %% length(okabe) + 1],
                                        resp_levels)
  }
  fill_cols <- c(Prior = "grey65", response_colours[resp_levels])
  darken    <- function(col, f = 0.3)
    grDevices::rgb(t(grDevices::col2rgb(col) * (1 - f)), maxColorValue = 255)
  line_cols <- stats::setNames(vapply(fill_cols, darken, character(1)), names(fill_cols))

  d <- d |>
    dplyr::left_join(meta, by = "parameter") |>
    dplyr::mutate(
      label      = factor(label, levels = lv),
      source     = factor(source, levels = c("Prior", "Posterior")),
      fill_group = factor(ifelse(source == "Prior", "Prior",
                                 ifelse(is.na(response), "Posterior", response)),
                          levels = names(fill_cols))
    )

  med <- tibble::tibble(parameter = cols,
                        median = apply(post[, cols, drop = FALSE], 2, median)) |>
    dplyr::left_join(meta, by = "parameter") |>
    dplyr::mutate(label = factor(label, levels = lv))

  g <- ggplot2::ggplot() +
    ggplot2::geom_vline(xintercept = 0, linetype = "dotted", colour = "grey50") +
    ggridges::geom_ridgeline(
      data = dplyr::filter(d, source == "Prior"),
      ggplot2::aes(x = x, y = label, height = h, group = label,
                   fill = fill_group, colour = fill_group),
      scale = 0.9, alpha = 0.6) +
    ggridges::geom_ridgeline(
      data = dplyr::filter(d, source == "Posterior"),
      ggplot2::aes(x = x, y = label, height = h, group = label,
                   fill = fill_group, colour = fill_group),
      scale = 0.9, alpha = 0.75) +
    ggplot2::geom_point(data = med, ggplot2::aes(x = median, y = label), size = 1.6) +
    ggplot2::scale_fill_manual(values = fill_cols, name = NULL, drop = FALSE) +
    ggplot2::scale_colour_manual(values = line_cols, guide = "none", drop = FALSE) +
    ggplot2::scale_y_discrete(expand = ggplot2::expansion(add = c(0.3, 1))) +
    ggplot2::coord_cartesian(xlim = rng) +
    ggplot2::labs(
      x = "Regression coefficient", y = NULL, title = title,
      caption = paste0("Grey = prior; colour = posterior (one per response); dot = posterior median. Heights ",
                       if (normalise == "each") "are scaled so that each density peaks at the same height."
                       else "are on a common density scale.")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom", panel.grid.minor = ggplot2::element_blank())

  if (!anyNA(meta$response)) {
    g <- g + ggplot2::facet_grid(response ~ ., scales = "free_y", space = "free_y")
  }
  g
}
