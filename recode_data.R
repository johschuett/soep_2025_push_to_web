################################################################################
## Start mode experiment - SOEP-CORE 2025
## Data recoding for analyses
################################################################################

#-------------------------------------------------------------------------------
# EXPAND IMPUTED DATA TO LONG FORMAT
#-------------------------------------------------------------------------------
# include = TRUE keeps the original (.imp == 0) block, which still contains NA.
# as.mids() derives "was missing" from is.na() on that block alone, so every
# recode must propagate NA into it. if_else(), "|" and case_when() without a
# TRUE branch do; %in% and a TRUE fallback do not.
data_long <- complete(data_imp, "long", include = TRUE)

#-------------------------------------------------------------------------------
# RECODE
#-------------------------------------------------------------------------------
# imputed inputs (no "-1" category needed):
#   boh_64, bogeburt, bosex, bop_154, bop_198_*, pgisced11,
#   pgemplst, pgfamstd, bpbula, migback, mbits_gg400
# non-imputed inputs (raw missing codes / NA retained):
#   bpemail, bohergs, bohmode, bphergs, bphmode, bppteil, bpbefhpmax,
#   bphhgr, con_week, sample1, core_gruppe, paradata
data_long <-
  data_long |>
  mutate(
    start_arm = factor(if_else(core_gruppe == "1", "CAPI-start", "CAWI-start"),
                       levels = c("CAPI-start", "CAWI-start")),
    age = 2025 - bogeburt,
    age_below50 = factor(if_else(age < 50, "1", "0"), levels = c("0", "1")),
    female = factor(if_else(bosex == "2", "1", "0"), levels = c("0", "1")),
    
    # %in% does not propagate NA -> guard explicitly
    west = factor(case_when(is.na(bpbula) ~ NA_character_,
                            !(bpbula %in% as.character(seq(11, 16))) ~ "1",
                            TRUE ~ "0"), levels = c("0", "1")),
    
    # no TRUE branch: unmatched (incl. NA) falls through to NA
    education = factor(case_when(as.numeric(as.character(pgisced11)) < 3 ~ "1",
                                 as.numeric(as.character(pgisced11)) < 5 ~ "2",
                                 as.numeric(as.character(pgisced11)) >= 5 ~ "3"),
                       levels = c("1", "2", "3")),
    education_high = factor(if_else(education == "3", "1", "0"), levels = c("0", "1")),
    full_time = factor(if_else(pgemplst == "1", "1", "0"), levels = c("0", "1")),
    
    # "|" propagates NA, unlike %in%
    living_together_married = factor(if_else(pgfamstd == "1" | pgfamstd == "7", "1", "0"),
                                     levels = c("0", "1")),
    no_migback = factor(if_else(migback == "1", "1", "0"), levels = c("0", "1")),
    
    hghinc24_hundreds = boh_64 / 100,
    
    # not imputed: bpemail keeps raw codes, "-2" == no e-mail on file
    email_known = factor(if_else(bpemail == "1", "1", "0"), levels = c("0", "1")),
    
    # not imputed: NA on bohergs means not realised, so "0" is substantive here
    real24 = factor(if_else(!is.na(bohergs) & as.numeric(as.character(bohergs)) %in% c(1, 3, seq(10, 19)), "1", "0"),
                    levels = c("0", "1")),
    
    hmode25_cawi = factor(if_else(bphmode == "2", "1", "0"), levels = c("0", "1")),
    
    across(
      all_of(paste0("bop_198_", c("01", "02", "04", "10"))),
      ~ factor(if_else(.x == "3", "0", "1"), levels = c("0", "1")),
      .names = "worries_{.col}"),
    
    # collapse two worst health outcomes into one category for both 2024 and 2025 for the level 4 analysis
    bop_154 = factor(if_else(as.numeric(as.character(bop_154)) >= 4, "4", 
                             as.character(bop_154)),
                     levels = c("1", "2", "3", "4")),
    
    bpp_142 = factor(if_else(as.numeric(as.character(bpp_142)) >= 4, "4", 
                             as.character(bpp_142)),
                     levels = c("1", "2", "3", "4")),
    
    # sample1 is not imputed; assumed complete
    refresher = factor(if_else(sample1 %in% as.character(seq(49, 51)), "1", "0"),
                       levels = c("0", "1"))) |>
  rename(
    worries_economy       = worries_bop_198_01,
    worries_finances      = worries_bop_198_02,
    worries_health        = worries_bop_198_04,
    worries_migration     = worries_bop_198_10)

# item nonresponse on the level-4 outcomes only ("-1" = refusal / no answer).
# Raw codes are taken from data_filtered, since impute_data.R has already
# collapsed all negatives to NA in data_long.
sens_items <- c("bpp_142", "bpp_178_01", "bpp_178_02", "bpp_178_04", "bpp_178_10", "bph_126")

inr_sens <-
  data_filtered |>
  distinct(pid, .keep_all = TRUE) |>
  select(pid, all_of(sens_items)) |>
  mutate(across(all_of(sens_items), ~ suppressWarnings(as.integer(as.character(.x))))) |>
  mutate(
    # asked = valid answer or explicit refusal; other negatives (-2, -3, -5, -8)
    # and NA mean the item was never posed
    n_sens_asked = rowSums(across(all_of(sens_items), ~ !is.na(.x) & .x >= -1)),
    n_sens_inr   = rowSums(across(all_of(sens_items), ~ !is.na(.x) & .x == -1)),
    inr_sens     = factor(case_when(n_sens_asked == 0 ~ NA_character_,
                                    n_sens_inr   >  0 ~ "1",
                                    TRUE              ~ "0"),
                          levels = c("0", "1"))) |>
  select(pid, n_sens_asked, n_sens_inr, inr_sens)

data_long <- left_join(data_long, inr_sens, by = "pid")
rm(sens_items, inr_sens)

# dependent variables (built from non-imputed gross/paradata; NA retained)
speeding_cut       <- quantile(bpquality$pe2_speeders$speeding, probs = 0.9, na.rm = TRUE)
straightlining_cut <- quantile(bpquality$pe2_straightliners$straightlining, probs = 0.9, na.rm = TRUE)
inr_cut            <- quantile(bpquality$pe2_nonresponders$inr, probs = 0.9, na.rm = TRUE)

data_long <-
  data_long |>
  mutate(
    real25 = factor(if_else(as.numeric(as.character(bphergs)) %in% c(1, 3, seq(10, 19)), "1", "0"), levels = c("0", "1")),
    real25_compliant = factor(if_else(real25 == "1" & ((start_arm == "CAPI-start" & bphmode == "1") | (start_arm == "CAWI-start" & bphmode == "2")), "1", "0"), levels = c("0", "1")),
    complete25 = factor(if_else(real25 == "1" & bppteil == bpbefhpmax, "1", "0"), levels = c("0", "1")),
    speeding_high = factor(if_else(speeding >= speeding_cut, "1", "0"), levels = c("0", "1")),
    straightlining_high = factor(if_else(straightlining >= straightlining_cut, "1", "0"), levels = c("0", "1")),
    inr_high = factor(if_else(inr >= inr_cut, "1", "0"), levels = c("0", "1")))

rm(speeding_cut, straightlining_cut, inr_cut)

# wave-on-wave change in the level-4 items (H4a). Built from data_filtered, not
# data_long: the 2025 items are not part of the imputation model, so a change
# score only exists where both waves are observed. The 2024 baselines are taken
# raw for the same reason. d_ = signed, absd_ = magnitude (primary DV),
# chg_ = any shift of at least one category.
chg_items <- tibble(
  name = c("health", "worry_economy", "worry_finances", "worry_health", "worry_migration"),
  pre  = c("bop_154", "bop_198_01", "bop_198_02", "bop_198_04", "bop_198_10"),
  post = c("bpp_142", "bpp_178_01", "bpp_178_02", "bpp_178_04", "bpp_178_10"))

chg_dv <-
  data_filtered |>
  distinct(pid, .keep_all = TRUE) |>
  select(pid, all_of(c(chg_items$pre, chg_items$post))) |>
  mutate(across(-pid, ~ { v <- suppressWarnings(as.integer(as.character(.x)))
  if_else(v < 0, NA_integer_, v) }))

for (i in seq_len(nrow(chg_items))) {
  nm <- chg_items$name[i]
  chg_dv[[paste0("d_",    nm)]] <- chg_dv[[chg_items$post[i]]] - chg_dv[[chg_items$pre[i]]]
  chg_dv[[paste0("up_",   nm)]] <- as.numeric(chg_dv[[paste0("d_", nm)]] >=  1)
  chg_dv[[paste0("down_", nm)]] <- as.numeric(chg_dv[[paste0("d_", nm)]] <= -1)
}

chg_dv <- chg_dv |>
  mutate(bop_154_raw    = factor(bop_154,    levels = 1:5),
         bop_198_01_raw = factor(bop_198_01, levels = 1:3),
         bop_198_02_raw = factor(bop_198_02, levels = 1:3),
         bop_198_04_raw = factor(bop_198_04, levels = 1:3),
         bop_198_10_raw = factor(bop_198_10, levels = 1:3)) |>
  select(pid, ends_with("_raw"), starts_with("d_"),
         starts_with("up_"), starts_with("down_"))

data_long <- left_join(data_long, chg_dv, by = "pid")
rm(chg_items, chg_dv)

#-------------------------------------------------------------------------------
# KEEP ONLY RELEVANT COLUMNS
#-------------------------------------------------------------------------------
data_long <-
  data_long |>
  select(
    .imp, .id, hid, pid,                                              # mids bookkeeping
    sample1, refresher, start_arm,                                    # identificators
    bpbefhpmax, bppteil, bphhgr, bphergs, bpemail, bpintk, bpintza,   # 2025 gross variables
    bpwein, bphmode, bppinstrument, bpbula, real25, real25_compliant,
    hmode25_cawi, complete25, con_week, email_known,
    real24, bohmode,                                                  # 2024 gross variable
    speeding, straightlining, inr, screens_received,                  # 2025 paradata
    items_in_batteries, speeding_high, straightlining_high,
    inr_high, inr_sens,
    age_below50, female, west,                                        # more 2025 gross variables
    education_high, full_time, living_together_married, no_migback,   # 2024 net variables
    hghinc24_hundreds, worries_economy, worries_finances,
    worries_health, worries_migration,
    mbits_gg400,                                                      # 2024 broadband availability
    bop_154,                                                          # 2024 net variable subjective health (two worst levels collapsed)
    bop_198_01, bop_198_02, bop_198_04, bop_198_10,                   # 2024 worries, uncollapsed (H3c baseline)
    bpp_142, bph_126,                                                 # 2025 net data
    bpp_178_01, bpp_178_02, bpp_178_04, bpp_178_10,                   # 2025 worries, uncollapsed (H3c outcome)
    ends_with("_raw"), starts_with("d_"), starts_with("up_"),         # longitudinal change variables (H4a outcome)
    starts_with("down_"))

#-------------------------------------------------------------------------------
# CENTER CONTINUOUS COVARIATES
#-------------------------------------------------------------------------------
# done on the long frame, before as.mids(), so that the treatment coefficient
# reflects the average marginal effect rather than the effect at an arbitrary
# reference point. Subtracting a constant preserves NA, so the .imp == 0 block
# keeps its missingness pattern and the round trip stays intact.
center_vars <- c("con_week", "bphhgr", "hghinc24_hundreds", "mbits_gg400")

center_vals <- data_long |>
  filter(.imp != "0") |>
  summarise(across(all_of(center_vars), ~ mean(.x, na.rm = TRUE)))

cat("\n--- centering constants: ---\n"); print(center_vals)

data_long <- data_long |>
  mutate(across(all_of(center_vars), ~ .x - center_vals[[cur_column()]]))

#-------------------------------------------------------------------------------
# BACK TO MIDS
#-------------------------------------------------------------------------------
data_imp_recoded <- as.mids(data_long)
rm(data_long, center_vars)

#-------------------------------------------------------------------------------
# CHECK THAT NA COUNTS MATCH BETWEEN RAW AND RECODED VARIABLES
#-------------------------------------------------------------------------------
cat("\n--- rows per imp slot: ---\n")
sapply(data_imp_recoded$imp, function(x) if (is.null(x)) 0 else nrow(x)) |> print()

#-------------------------------------------------------------------------------
# COMPLETED DATASETS FOR THE polr / clm MODELS
#-------------------------------------------------------------------------------
dlist <- lapply(seq_len(data_imp_recoded$m), function(i) complete(data_imp_recoded, i))

#-------------------------------------------------------------------------------
# CHECK THE DATA
#-------------------------------------------------------------------------------
data_recoded <- dlist[[1]] |> as_tibble()
str(data_recoded)
suppressWarnings(describe(data_recoded))

#-------------------------------------------------------------------------------
# COVARIATE SET
#-------------------------------------------------------------------------------
covariates <- c(
  "start_arm",                                                      # treatment
  "real24", "bphhgr", "refresher", "email_known",                   # household
  "age_below50", "female", "west", "education_high", "full_time",   # head of hh
  "living_together_married", "no_migback",
  "hghinc24_hundreds",                                              # income
  "worries_economy", "worries_finances", "worries_health",
  "worries_migration",
  "mbits_gg400")                                                    # broadband

stopifnot(all(covariates %in% names(data_recoded)))
