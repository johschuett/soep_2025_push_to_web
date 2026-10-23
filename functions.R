################################################################################
## Start mode experiment - SOEP-CORE 2025
## Shared functions
################################################################################

# ------------------------------------------------------------------------------
# IMPUTATION
# run_mice(): CART-based MI with locked variables
#
# locked_vars: character vector of variables NOT to impute and NOT to use
#              as predictors (e.g. assignment variable, risk-set variables)
# ------------------------------------------------------------------------------
run_mice <- function(dat, locked_vars = NULL, m = 5, maxit = 10, seed = 235) {
  pred <- quickpred(dat, mincor = 0.05)
  meth <- make.method(dat)
  meth[] <- "cart"
  
  for (v in locked_vars) {
    if (v %in% colnames(dat)) {
      meth[v]    <- ""
      pred[v, ]  <- 0
      pred[, v]  <- 0
    }
  }
  
  mice(dat, m = m, maxit = maxit, method = meth,
       predictorMatrix = pred, seed = seed)
}

# ------------------------------------------------------------------------------
# Linear Probability Model (MI)
# mi_lpm(): fit linear probability model on multiple imputed datasets
# ------------------------------------------------------------------------------
mi_lpm <- function(data_imp, covariates, response, subset_expr = NULL, interact = FALSE) {
  # reformat factorial response to numeric
  data_imp$data <- mutate(data_imp$data, !!response := as.numeric(as.character(.data[[response]])))
  
  if (is.null(subset_expr)) {
    # fit model on full data
    if (!interact) {
      # no treatment interaction with control matrix
      fit <- with(data_imp,
                  lm(reformulate(covariates, response)))
    }
    else {
      # treatment interaction with control matrix
      fit <- with(data_imp,
                  lm(as.formula(formulate_interact_treatment(response, covariates[covariates != "start_arm"], "start_arm"))))
    }}
  else {
    # fit model on subsetted data
    if (!interact) {
      # no treatment interaction with control matrix
      fit <- with(data_imp,
                  lm(reformulate(covariates, response),
                     subset = eval(parse(text = subset_expr))))
    }
    else {
      # treatment interaction with control matrix
      fit <- with(data_imp,
                  lm(as.formula(formulate_interact_treatment(response, covariates[covariates != "start_arm"], "start_arm")),
                     subset = eval(parse(text = subset_expr))))
}}}

# ------------------------------------------------------------------------------
# Two-stage least squares / IV (MI)
# mi_2sls(): fit two stage least squares model on multiple imputed datasets
# ------------------------------------------------------------------------------
mi_2sls <- function(data_imp, covariates, response, subset_expr = "real25 == \"1\" & bpbefhpmax > 1", interact = FALSE) {
  # reformat factorial response to numeric
  data_imp$data <- mutate(data_imp$data, !!response := as.numeric(as.character(.data[[response]])))
  
  formula <- formulate_2sls(response, covariates[covariates != "start_arm"], interact = interact)
  print(formula)
  
  with(data_imp, ivreg(as.formula(formula), subset = eval(parse(text = subset_expr))))
}

# ------------------------------------------------------------------------------
# ROBUST POOLING (MI)
# pool_robust(): pool across imputations using heteroscedasticity-consistent
#                within-variances instead of the model-based ones.
#                Works for lm() and ivreg() fits.
# ------------------------------------------------------------------------------
pool_robust <- function(fit_mi, type = "HC1") {
  fits  <- if (inherits(fit_mi, "mira")) fit_mi$analyses else fit_mi
  dfcom <- min(vapply(fits, df.residual, numeric(1)))
  
  robust <- lapply(fits, function(f)
    coeftest(f, vcov. = vcovHC(f, type = type), save = TRUE))
  
  withCallingHandlers(
    pool(as.mira(robust), dfcom = dfcom),
    warning = function(w) {
      if (grepl("exponentiate.*not supported", conditionMessage(w)))
        invokeRestart("muffleWarning")
    })
}

# ------------------------------------------------------------------------------
# ORDERED LOGIT (MI)
# fit_polr_MI(): fit ordered logit across imputed datasets,
#                pool via Rubin's rules
#
# subset_expr: string passed to subset(), e.g. "testsplit == 2 & realW == 1"
#              pass NULL to use the full dataset
# ------------------------------------------------------------------------------
fit_polr_MI <- function(dlist,
                        fml,
                        y,
                        subset_expr = NULL,
                        interact_mode = FALSE,
                        mode_var = "start_arm") {
  
  rhs <- attr(terms(fml), "term.labels")
  fml2 <- make_formula(y, rhs, interact_mode = interact_mode, mode_var = mode_var)
  
  fits <- lapply(seq_along(dlist), function(i) {
    d <- if (!is.null(subset_expr)) subset(dlist[[i]], eval(parse(text = subset_expr)))
    else dlist[[i]]
    
    X <- model.matrix(fml2, data = d)[, -1, drop = FALSE]
    orig_names <- colnames(X)
    safe_names <- make.names(orig_names, unique = TRUE)
    colnames(X) <- safe_names
    
    dat_r <- data.frame(y = d[[y]], X, check.names = FALSE)
    names(dat_r)[1] <- y
    dat_r[[y]] <- as.ordered(dat_r[[y]])
    
    fml_r <- reformulate(safe_names, response = y)
    m <- MASS::polr(fml_r, data = dat_r, Hess = TRUE)
    
    m$call$formula <- fml_r
    m$call$data    <- dat_r
    attr(m, "name_map") <- setNames(orig_names, safe_names)  # keep for later translation
    m
  })
  
  pooled <- pool(as.mira(fits))
  
  list(
    fits = fits,
    smod = summary(pooled, conf.int = TRUE, conf.level = 0.95)
  )
}

# ------------------------------------------------------------------------------
# Partial proportional odds model (MI)
# fit_polr_MI(): fit partial proportional odds model across imputed datasets,
#                pool via Rubin's rules
# ------------------------------------------------------------------------------
fit_clm_ppo_MI <- function(dlist,
                           fml,
                           y,
                           subset_expr = NULL,
                           interact_mode = FALSE,
                           mode_var = "start_arm",
                           nominal_terms = NULL) {
  
  rhs <- attr(terms(fml), "term.labels")
  fml2 <- make_formula(y, rhs, interact_mode = interact_mode, mode_var = mode_var)
  
  fits <- lapply(seq_along(dlist), function(i) {
    d <- if (!is.null(subset_expr)) subset(dlist[[i]], eval(parse(text = subset_expr)))
    else dlist[[i]]
    
    X <- model.matrix(fml2, data = d)[, -1, drop = FALSE]
    orig_names <- colnames(X)
    safe_names <- make.names(orig_names, unique = TRUE)
    colnames(X) <- safe_names
    
    dat_r <- data.frame(y = d[[y]], X, check.names = FALSE)
    names(dat_r)[1] <- y
    dat_r[[y]] <- as.ordered(dat_r[[y]])
    
    nom_safe <- safe_names[orig_names %in% nominal_terms | safe_names %in% nominal_terms]
    
    loc_safe <- setdiff(safe_names, nom_safe)
    
    fml_r   <- if (length(loc_safe) > 0) reformulate(loc_safe, response = y) else as.formula(paste(y, "~ 1"))
    fml_nom <- if (length(nom_safe) > 0) reformulate(nom_safe) else NULL
    
    m <- if (!is.null(fml_nom)) {
      ordinal::clm(fml_r, nominal = fml_nom, data = dat_r, link = "logit")
    } else {
      ordinal::clm(fml_r, data = dat_r, link = "logit")
    }
    
    m$call$formula <- fml_r
    m$call$nominal <- fml_nom
    m$call$data    <- dat_r
    attr(m, "name_map") <- setNames(orig_names, safe_names)
    m
  })
  
  pooled <- pool(as.mira(fits))
  
  list(
    fits = fits,
    smod = summary(pooled, conf.int = TRUE, conf.level = 0.95)
  )
}

# ------------------------------------------------------------------------------
# PROPORTIONAL ODDS DIAGNOSTICS (MI)
# brant_offenders(): brant test across imputations, returns terms violating the
#                    PO assumption in *every* imputed dataset
# ------------------------------------------------------------------------------
brant_offenders <- function(fits) {
  brant_list <- lapply(fits, brant)
  p_by_term  <- sapply(brant_list, function(b) b[, "probability"])
  colnames(p_by_term) <- paste0("p.value_", seq_len(ncol(p_by_term)))
  
  p_tbl <- as_tibble(p_by_term, rownames = "term") |>
    filter(if_all(starts_with("p.value_"), ~ .x < 0.05))
  
  cat("\n--- robust PO offenders ---\n"); print(p_tbl)
  p_tbl |> filter(term != "Omnibus") |> pull(term)
}

# ------------------------------------------------------------------------------
# ORDERED LOGIT + PPO REFIT (MI)
# fit_po_pair(): fit ordered logit for one outcome, then refit as partial
#                proportional odds model with the PO assumption relaxed for the
#                robust offenders identified by brant_offenders()
# ------------------------------------------------------------------------------
fit_po_pair <- function(dlist, fml, y, subset_expr = NULL,
                        interact_mode = FALSE, mode_var = "start_arm") {
  
  po <- fit_polr_MI(dlist, fml, y = y,
                    subset_expr   = subset_expr,
                    interact_mode = interact_mode,
                    mode_var      = mode_var)
  
  nominal_terms <- brant_offenders(po$fits)
  
  ppo <- fit_clm_ppo_MI(dlist, fml, y = y,
                        subset_expr   = subset_expr,
                        interact_mode = interact_mode,
                        mode_var      = mode_var,
                        nominal_terms = nominal_terms)
  
  list(po = po, ppo = ppo)
}

# ------------------------------------------------------------------------------
# MODEL REPORTING
# report_model(): print pooled coefficients and n for a fit_polr_MI() /
#                 fit_clm_ppo_MI() result
# ------------------------------------------------------------------------------
report_model <- function(m, name) {
  cat("\n--- ", name, ": pooled coefficients ---\n", sep = ""); print(m$smod)
  cat("\n--- ", name, ": observations ---\n", sep = ""); print(nobs(m$fits[[1]]))
  invisible(NULL)
}

# ------------------------------------------------------------------------------
# AVERAGE MARGINAL CONTRASTS (MI)
# ame_MI(): average marginal effect of a design-matrix column on the predicted
#           probability of each outcome category, pooled via Rubin's rules.
#           Works for polr and clm fits from fit_polr_MI() / fit_clm_ppo_MI(),
#           and is invariant to whether the term is in the location or the
#           threshold part, so PO violations do not affect interpretation.
# ------------------------------------------------------------------------------
ame_MI <- function(fit_obj, variable = "start_armCAWI.start") {
  
  per_imp <- lapply(fit_obj$fits, function(f)
    avg_comparisons(
      f,
      variables = setNames(list(0:1), variable),
      type      = "prob") |>
      as_tibble() |>
      select(group, estimate, std.error))
  
  bind_rows(per_imp, .id = ".imp") |>
    group_by(group) |>
    summarise(
      n_imp = n(),
      qbar  = mean(estimate),
      ubar  = mean(std.error^2),
      b     = var(estimate),
      .groups = "drop") |>
    mutate(
      total     = ubar + (1 + 1 / n_imp) * b,
      se        = sqrt(total),
      df        = (n_imp - 1) * (1 + ubar / ((1 + 1 / n_imp) * b))^2,
      t         = qbar / se,
      p.value   = 2 * pt(-abs(t), df),
      conf.low  = qbar - qt(0.975, df) * se,
      conf.high = qbar + qt(0.975, df) * se) |>
    select(group, estimate = qbar, se, conf.low, conf.high, p.value)
}

# ------------------------------------------------------------------------------
# FORMULA HELPER
# formulate_interact_treatment(): construct model formula with mode interaction
# make_formula(): construct model formula, optionally with mode interaction (legacy)
# formulate_2sls(): construct iv model formula, optionally with mode interaction
# ------------------------------------------------------------------------------
formulate_interact_treatment <- function(y, controls, treatment_term = "start_arm") {
  paste0(y, " ~ ", treatment_term, " + ", paste0(controls, collapse = " + "),
         " + ", treatment_term, " * (", paste0(controls, collapse = " + "), ")")
}

make_formula <- function(y, rhs, interact_mode = FALSE, mode_var = "start_arm") {
  if (interact_mode) {
    as.formula(paste0(y, " ~ ", mode_var, " * (", paste(rhs, collapse = " + "), ")"))
  } else {
    reformulate(rhs, response = y)
}}

formulate_2sls <- function(y, controls, treatment_term = "hmode25_cawi", instrument_term = "start_arm", interact = FALSE) {
  if (!interact) {
    paste0(y, " ~ ", treatment_term, " + ", paste0(controls, collapse = " + "), " | ", instrument_term, " + ", paste0(controls, collapse = " + "))
  }
  else {
    paste0(y, " ~ ", treatment_term, " + ", paste0(controls, collapse = " + "), " + ", treatment_term, " * (", paste0(controls, collapse = " + "),
           ") | ", instrument_term, " + ", paste0(controls, collapse = " + "), " + ", instrument_term, " * (", paste0(controls, collapse = " + "), ")")
}}

# ------------------------------------------------------------------------------
# INPUT DATA CLEANER
# clean_input_data(): turn haven-imported data into data with dbl or fct columns
# ------------------------------------------------------------------------------
clean_input_data <- function(df, factor_cols = NULL) {
  clean_df <- df |> zap_label() |> zap_labels() |> zap_formats()
  
  if (!is.null(factor_cols)) {
    mutate(
      clean_df,
      across(all_of(factor_cols), ~ factor(as.character(.)))
    )
  } else {
    clean_df
  }
}

# ------------------------------------------------------------------------------
# MISSING RECODE FOR FACTORS
# neg_to_na_factor(): turn missing factor levels to NA
# ------------------------------------------------------------------------------
neg_to_na_factor <- function(x) {
  lvls <- levels(x)
  x    <- if_else(suppressWarnings(as.integer(as.character(x))) < 0, NA_character_, as.character(x))
  factor(x, levels = lvls[!lvls %in% as.character(-99:-1)])
}

# ------------------------------------------------------------------------------
# COEF TABLE
# coef_table_compare(): side-by-side table of pooled estimates of one or across several
#                       models, same term selection and ordering logic as
#                       coef_plot_compare(), plus pooled model statistics
#                       (n, no. imputations, R^2, adj. R^2).
#                       R^2 is pooled on the Fisher-z scale of sqrt(R^2).
#                       Elements of pooled_list may be mipo objects or the mira
#                       objects returned by mi_lpm() / mi_2sls(); with
#                       weak_iv = TRUE and mira input from mi_2sls(), first-stage
#                       weak-instrument F statistics (mean [min, max] across
#                       imputations) are appended.
# ------------------------------------------------------------------------------
coef_table <- function(pooled_list, keep_terms = NULL, order_from = NULL,
                       digits = 3, stars = TRUE, weak_iv = FALSE) {
  
  fits_list   <- map(pooled_list, ~ if (inherits(.x, "mira")) .x$analyses else NULL)
  pooled_list <- map(pooled_list, ~ if (inherits(.x, "mira")) pool_robust(.x) else .x)
  
  coefs <- imap_dfr(pooled_list, ~ summary(.x, conf.int = TRUE) |>
                      as_tibble() |>
                      mutate(model = .y)) |>
    mutate(term = as.character(term), term = str_remove_all(term, "`")) |>
    left_join(covariate_labels, by = "term") |>
    mutate(main_effect = str_detect(label, "×", negate = TRUE))
  
  if (!is.null(keep_terms)) {
    coefs <- coefs |> filter(term %in% keep_terms)
  } else {
    coefs <- coefs |> filter(term != "(Intercept)")
  }
  
  if (is.null(order_from)) {
    order_from <- coefs |>
      count(model) |>
      slice_max(n, n = 1, with_ties = FALSE) |>
      pull(model)
  }
  
  term_order <- coefs |>
    distinct(term, main_effect) |>
    mutate(pos = match(term, covariate_labels$term)) |>
    arrange(desc(main_effect), pos) |>
    pull(term)
  
  fmt_cell <- function(est, se, p) {
    star <- if (stars) ifelse(p < .01, "***", ifelse(p < .05, "**", ifelse(p < .1, "*", ""))) else ""
    paste0(formatC(est, format = "f", digits = digits), star,
           " (", formatC(se, format = "f", digits = digits), ")")
  }
  
  pool_r2 <- function(r2) {
    r    <- sqrt(pmax(r2, 0))
    zbar <- mean(0.5 * log((1 + r) / (1 - r)))
    ((exp(2 * zbar) - 1) / (exp(2 * zbar) + 1))^2
  }
  
  body <- coefs |>
    filter(term %in% term_order) |>
    mutate(term  = factor(term, levels = term_order),
           model = factor(model, levels = names(pooled_list)),
           cell  = fmt_cell(estimate, std.error, p.value)) |>
    select(term, label, model, cell) |>
    pivot_wider(names_from = model, values_from = cell, names_expand = TRUE) |>
    arrange(term) |>
    transmute(term = label,
              across(all_of(names(pooled_list)), ~ replace_na(.x, "")))
  
  stats <- imap_dfr(names(pooled_list), function(nm, i) {
    f <- fits_list[[nm]]
    if (is.null(f)) return(tibble(model = nm, `Number of covariates` = "", `Pooled R²` = "",
                                  Imputations = "", Observations = ""))
    tibble(model                  = nm,
           `Number of covariates` = as.character(length(coef(f[[1]])) - 1L),
           `Pooled R²`            = formatC(pool_r2(map_dbl(f, ~ summary(.x)$r.squared)),
                                            format = "f", digits = digits),
           Imputations            = as.character(length(f)),
           Observations           = format(round(mean(map_dbl(f, nobs))), big.mark = ","))
  }) |>
    pivot_longer(-model, names_to = "term", values_to = "value") |>
    pivot_wider(names_from = model, values_from = value)
  
  if (weak_iv) {
    iv <- imap_dfr(fits_list, function(fits, nm) {
      if (is.null(fits) || !inherits(fits[[1]], "ivreg")) return(tibble())
      map_dfr(fits, ~ summary(.x, diagnostics = TRUE)$diagnostics |>
                as_tibble(rownames = "test")) |>
        filter(str_detect(test, "Weak instruments")) |>
        summarise(n_endog = n_distinct(test),
                  min_f   = min(statistic)) |>
        transmute(model = nm,
                  term  = if_else(n_endog > 1,
                                  "Weak instr. F (min.)",
                                  "Weak instr. F"),
                  value = formatC(min_f, format = "f", digits = 1))
    })
    
    if (nrow(iv) == 0) {
      warning("weak_iv = TRUE but no ivreg mira objects supplied; pass the mira fits, not pool() output")
    } else {
      obs_row <- stats |> filter(term == "Observations")
      stats   <- bind_rows(stats |> filter(term != "Observations"),
                           iv |> pivot_wider(names_from = model, values_from = value),
                           obs_row)
    }
  }
  
  bind_rows(body, stats) |>
    mutate(across(all_of(names(pooled_list)), ~ replace_na(.x, "")))
}
