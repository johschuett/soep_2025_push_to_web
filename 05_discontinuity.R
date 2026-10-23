################################################################################
## Start mode experiment - SOEP-CORE 2025
## Analysis 05: mode-induced measurement discontinuity
################################################################################

chg_covars <- setdiff(covariates, c("real24", grep("^worries", covariates, value = TRUE)))

# outcome, 2024 baseline, and interior baseline categories per item
h4a_specs <- list(
  `Subj. health`      = list(y = "health",          lag = "bop_154_raw",    int = c("2", "3", "4")),
  `Worries economy`   = list(y = "worry_economy",   lag = "bop_198_01_raw", int = "2"),
  `Worries finances`  = list(y = "worry_finances",  lag = "bop_198_02_raw", int = "2"),
  `Worries health`    = list(y = "worry_health",    lag = "bop_198_04_raw", int = "2"),
  `Worries migration` = list(y = "worry_migration", lag = "bop_198_10_raw", int = "2"))

## CAPI in 2024, CAPI or CAWI in 2025, valid outcome
h4a_subset <- function(y)
  sprintf("real24 == \"1\" & real25 == \"1\" & bohmode == \"1\" & bphmode %%in%% c(\"1\", \"2\") & !is.na(%s)", y)

## prefix: "d_" signed change, "up_" / "down_" shift of >= 1 category
## interior: restrict to non-boundary 2024 categories; the baseline control
## drops out where only one interior category remains
h4a_fit <- function(prefix, interior = FALSE)
  lapply(h4a_specs, function(sp) {
    y  <- paste0(prefix, sp$y)
    ss <- h4a_subset(y)
    rhs <- c(sp$lag, chg_covars)
    if (interior) {
      ss  <- sprintf("%s & %s %%in%% c(%s)",
                     ss, sp$lag, paste0('"', sp$int, '"', collapse = ", "))
      rhs <- c(if (length(sp$int) > 1) sp$lag, chg_covars)
    }
    mi_2sls(data_imp_recoded, rhs, y, ss)
  })

#-------------------------------------------------------------------------------
# Models 21-25: signed change ~ hmode25_cawi + baseline + X | start_arm instrument
# bpp_178_*: 1 = big worries ... 3 = none, so a negative estimate means more
# worries under CAWI; bpp_142: 1 = very good ... 5 = bad, so a positive estimate
# means worse reported health
#-------------------------------------------------------------------------------
m21m25 <- h4a_fit("d_")
coef_table(m21m25, keep_terms = "hmode25_cawi1", weak_iv = TRUE)

#-------------------------------------------------------------------------------
# Models 26-34: probability of an upward / downward shift of at least one
# category (LPM). Decomposes Models 21-25: opposite signs indicate a pure
# location shift, like signs additional response instability
#-------------------------------------------------------------------------------
m26m30 <- h4a_fit("up_")
m31m35 <- h4a_fit("down_")
coef_table(m26m30, keep_terms = "hmode25_cawi1", weak_iv = TRUE)
coef_table(m31m35, keep_terms = "hmode25_cawi1", weak_iv = TRUE)

#-------------------------------------------------------------------------------
# Robustness: Models 21-25 on interior 2024 baseline categories only, where
# movement in either direction is possible (rules out a bounding artefact)
#-------------------------------------------------------------------------------
m21m25_int <- h4a_fit("d_", interior = TRUE)
coef_table(m21m25_int, keep_terms = "hmode25_cawi1", weak_iv = TRUE)
