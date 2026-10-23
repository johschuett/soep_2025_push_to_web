################################################################################
## Start mode experiment - SOEP-CORE 2025
## Data preparation for analyses
################################################################################

#-------------------------------------------------------------------------------
# IMPORT DATA
#-------------------------------------------------------------------------------
# BO = 2024
bohbrutto <- read_dta(file.path(PATH_RESTR_RAW, "bohbrutto.dta"))
bopbrutto <- read_dta(file.path(PATH_RESTR_RAW, "bopbrutto.dta"))
bop       <- read_dta(file.path(PATH_RESTR_RAW, "bop.dta"))
boh       <- read_dta(file.path(PATH_RESTR_RAW, "boh.dta"))
pgen      <- read_dta(file.path(PATH_RESTR, "pgen.dta"))
ppathl    <- read_dta(file.path(PATH_RESTR, "ppathl.dta"))
regionl   <- read_dta(file.path(PATH_RESTR, "regionl.dta"))

# BP = 2025
bpinst    <- read_dta(file.path(PATH_RESTR_RAW, "instrumentation2025.dta"))
bphbrutto <- read_dta(file.path(PATH_RESTR_RAW, "bphbrutto.dta"))
bppbrutto <- read_dta(file.path(PATH_RESTR_RAW, "bppbrutto.dta"))
bpp       <- read_dta(file.path(PATH_RESTR_RAW, "bpp.dta"))
bph       <- read_dta(file.path(PATH_RESTR_RAW, "bph.dta"))

# data sets that require entirely different paths
bpeinsatz <- read_dta(file.path(PATH_EXTRA, "7713-soep-core-2025-einsatzstichprobe.dta"))
bpcontact <- read_dta(file.path(PATH_EXTRA, "contact2025.dta"))

# data quality and respondent behaviour indicators 2025
load(file.path(PATH_EXTRA, "24072026.RData"),  temp_env <- new.env())
bpquality <- as.list(temp_env)
rm(temp_env)

# breitbandatlas 2024
brtbnd24 <- read_csv(file.path(PATH_EXTRA, "brtbnd_2024.csv"), col_types = "dcccddddddd")

#-------------------------------------------------------------------------------
# MERGE
#-------------------------------------------------------------------------------
data_merged <-
# 2025 einsatz
clean_input_data(
  bpeinsatz |> select(hid, sample1, core_gruppe, brutto_nextWave_final),
  c("sample1", "core_gruppe", "brutto_nextWave_final")) |>
  group_by(hid) |>
  slice_head(n = 1) |>
  ungroup() |>
  
  # 2025 hh gross data
  left_join(
    clean_input_data(
      bphbrutto |>
        select(-c("cid", "sample1", "syear", "bphtyp", "bphmatrix", "bphaapor",
                  "bpudi", "bpdatummo", "bpdatumtg", "iyear", "bphader", "bphadq",
                  "bphtel", "bpincentive1", "bpincentive2", "bpnextwave",
                  "bpbula_ew", "intid", "bpdrstp_intid", "bpdrstp1")),
      c("bphergs", "bpemail", "bphmode", "bpbula")),
    by = "hid") |>
  
  # 2025 hh net data (hh income)
  left_join(clean_input_data(bph |> select(hid, bph_126)), by = "hid") |>
  
  # 2024 hh gross data
  left_join(
    clean_input_data(bohbrutto |> select(hid, bohergs, bohmode), c("bohergs", "bohmode")),
    by = "hid") |>
  
  # 2024 hh net data (hh income)
  left_join(clean_input_data(boh |> select(hid, boh_64)), by = "hid") |>
  
  # get 2024 anchor
  left_join(
    clean_input_data(bopbrutto |> select(hid, pid, bogeburt, bosex, bostell, bostell_papi), "bosex") |>
      filter(bostell == "0" | bostell_papi == "0") |>
      select(-c("bostell", "bostell_papi")),
    by = "hid") |>
  
  # get 2024 anchor net data (health and worries)
  left_join(
    clean_input_data(
      bop |> select(
        pid, bop_154,
        all_of(paste0("bop_198_", c("01", "02", "04", "10")))),
      c("bop_154", paste0("bop_198_", c("01", "02", "04", "10")))),
    by = "pid") |>
  
  # anchor's 2025 pe instrument (subjective health and worries)
  left_join(
    clean_input_data(
      bpp |> select(
        pid, instrument, bpp_142,
        all_of(paste0("bpp_178_", c("01", "02", "04", "10")))),
      c("instrument", "bpp_142", paste0("bpp_178_", c("01", "02", "04", "10")))) |>
      rename(bppinstrument = instrument),
    by = "pid") |>
  
  # get education
  left_join(
    clean_input_data(pgen |> select(pid, syear, pgisced11, pgemplst), "pgemplst") |>
      group_by(pid) |>
      summarise(pgisced11 = max(pgisced11), pgemplst = last(pgemplst[order(syear)]), .groups = "drop") |>
      mutate(pgisced11 = factor(pgisced11)),
    by = "pid") |>
  
  # get marital status
  left_join(
    clean_input_data(pgen |> select(pid, pgfamstd), "pgfamstd") |>
      filter(!(as.numeric(as.character(pgfamstd)) < 0)) |>
      group_by(pid) |>
      slice_tail(n = 1) |>
      ungroup(),
    by = "pid") |>
  
  # get migration background
  left_join(
    clean_input_data(ppathl |> select(pid, syear, migback), "migback") |>
      filter(as.numeric(as.character(migback)) > 0) |>
      group_by(pid) |>
      slice_tail(n = 1) |>
      ungroup() |>
      select(-syear),
    by = "pid") |>
  
  # get gkz
  left_join(clean_input_data(regionl |> filter(syear == 2024) |> select(hid, gkz)), by = "hid") |>
  mutate(
    gkz = case_when(
      gkz == 16061097                ~ 16061,
      gkz %in% c(16076004, 16076084) ~ 16076,
      TRUE                           ~ gkz)) |>
  
  # get broadband availability
  left_join(
    clean_input_data(
      brtbnd24 |>
        rename(gkz = id) |>
        select(gkz, mbits_gg400) |>
        mutate(gkz = case_when(
          gkz ==  5315 ~  5315000,
          gkz ==  9162 ~  9162000,
          gkz ==  2000 ~  2000000,
          gkz ==  4011 ~  4011000,
          gkz == 11000 ~ 11000000,
          TRUE         ~      gkz
        ))),
    by = "gkz",
    relationship = "many-to-one") |>
  
  # get calendar week of first contact
  left_join(
    clean_input_data(bpcontact |> select(hid, kondat)) |>
      filter(!(is.na(kondat) | trimws(kondat) == "")) |>
      mutate(
        con_year = as.integer(substr(kondat, 1, 4)),
        con_month = as.integer(substr(kondat, 5, 6)),
        con_day = as.integer(substr(kondat, 7, 8)),
        con_date = ymd(paste(con_year, con_month, con_day)),
        con_week = isoweek(con_date)
      ) |>
      # field started in cw 12 of 2025
      filter(con_year == 2025 & con_week >= 12) |>
      group_by(hid) |>
      arrange(con_month, con_day, .by_group = TRUE) |>
      slice_head(n = 1) |>
      ungroup() |>
      select(hid, con_week),
    by = "hid") |>
  
  # get speeding, straightlining, and inr for anchor person
  left_join(
    bpquality$pe2_speeders |> select(pid, speeding, screens_received),
    by = "pid") |>
  left_join(
    bpquality$pe2_straightliners |> select(-mode) |> rename(items_in_batteries = items_n),
    by = "pid") |>
  left_join(
    bpquality$pe2_nonresponders |> select(pid, inr),
    by = "pid")

# sort identifiers
data_merged <- select(data_merged, hid, pid, everything())

#-------------------------------------------------------------------------------
# FILTER
#-------------------------------------------------------------------------------
data_filtered <-
data_merged |>
  filter(brutto_nextWave_final == "1")
