################################################################################
## Start mode experiment - SOEP-CORE 2025
## Term labelling
################################################################################

#-------------------------------------------------------------------------------
# LABEL LOOKUP
#-------------------------------------------------------------------------------
label_lookup <- c(
  "(Intercept)"              = "Constant",
  "start_armCAWI-start"      = "CAWI-start",
  "hmode25_cawi1"            = "HH interview CAWI (instr.)",
  "con_week"                 = "First contact (CW)",
  "screens_received"         = "No. screens received",
  "items_in_batteries"       = "No. items within batteries received",
  "real241"                  = "Part. 2024",
  "bphhgr"                   = "HH size",
  "refresher1"               = "2024 refresher",
  "email_known1"             = "Email address known",
  "age_below501"             = "Age <50",
  "female1"                  = "Female",
  "west1"                    = "West Ger.",
  "education_high1"          = "Tertiary educ.",
  "full_time1"               = "Full-time empl.",
  "living_together_married1" = "Living together married",
  "no_migback1"              = "No migration backgr.",
  "hghinc24_hundreds"        = "HH income (100s)",
  "risk_averse_driving1"     = "Risk averse: driving",
  "risk_averse_finances1"    = "Risk averse: finances",
  "risk_averse_leisure1"     = "Risk averse: leisure",
  "risk_averse_career1"      = "Risk averse: career",
  "risk_averse_health1"      = "Risk averse: health",
  "risk_averse_strangers1"   = "Risk averse: strangers",
  "worries_economy1"         = "Worries: economy",
  "worries_finances1"        = "Worries: finances",
  "worries_health1"          = "Worries: health",
  "worries_migration1"       = "Worries: migration",
  "mbits_gg400"              = "Broadband availab.",
  "bop_154_raw2"             = "Subj. health good",
  "bop_154_raw3"             = "Subj. health satisfactory",
  "bop_154_raw4"             = "Subj. health poor",
  "bop_154_raw5"             = "Subj. health bad",
  "bop_198_01_raw2"          = "Worries economy some",
  "bop_198_01_raw3"          = "Worries economy no",
  "bop_198_02_raw2"          = "Worries finances some",
  "bop_198_02_raw3"          = "Worries finances no",
  "bop_198_04_raw2"          = "Worries health some",
  "bop_198_04_raw3"          = "Worries health no",
  "bop_198_10_raw2"          = "Worries migration some",
  "bop_198_10_raw3"          = "Worries migration no")

#-------------------------------------------------------------------------------
# COLLECT ALL TERMS APPEARING IN ANY MODEL
#-------------------------------------------------------------------------------
label_data <- dlist[[1]]

# RHS-only formulas: model.matrix() never needs the response, and this avoids
# rows being dropped where the response is NA
rhs_main <- c("start_arm", "hmode25_cawi", "con_week", "screens_received",
              "items_in_batteries", setdiff(covariates, "start_arm"), "bop_154_raw",
              "bop_198_01_raw", "bop_198_02_raw", "bop_198_04_raw", "bop_198_10_raw")

mm_names <- function(rhs, interact = NULL) {
  f <- reformulate(rhs)
  if (!is.null(interact) && interact %in% rhs)
    f <- reformulate(c(rhs, paste0(interact, ":(",
                                   paste(setdiff(rhs, interact), collapse = " + "), ")")))
  colnames(model.matrix(f, label_data))
}

all_terms <- unique(c(
  mm_names(rhs_main),
  mm_names(rhs_main, interact = "start_arm"),
  mm_names(rhs_main, interact = "hmode25_cawi")))

#-------------------------------------------------------------------------------
# BUILD LABELS, INTERACTIONS JOINED BY " × "
#-------------------------------------------------------------------------------
covariate_labels <- tibble(
  term  = all_terms,
  label = vapply(all_terms, function(t) {
    if (t %in% names(label_lookup)) return(unname(label_lookup[[t]]))
    parts <- strsplit(t, ":", fixed = TRUE)[[1]]
    if (!all(parts %in% names(label_lookup))) return(NA_character_)
    paste(label_lookup[parts], collapse = " \u00d7 ")
  }, character(1)))

#-------------------------------------------------------------------------------
# CHECK: EVERY TERM LABELLED
#-------------------------------------------------------------------------------
unlabelled <- covariate_labels |> filter(is.na(label) | label == "") |> pull(term)
if (length(unlabelled) > 0) {
  cat("\n--- unlabelled terms: ---\n"); print(unlabelled)
  warning("label_lookup is missing entries; see above")
}

rm(label_data, rhs_main, mm_names, all_terms, unlabelled)
