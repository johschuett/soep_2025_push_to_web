################################################################################
## Start mode experiment - SOEP-CORE 2025
## Fieldwork outcomes
################################################################################

# household participation rates by start condition and refresher/panel status
tmp <- data_recoded |> filter(start_arm == "CAPI-start")
round(prop.table(table(tmp$refresher, tmp$real25), margin = 1) * 100, 1)

tmp <- data_recoded |> filter(start_arm == "CAWI-start")
round(prop.table(table(tmp$refresher, tmp$real25), margin = 1) * 100, 1)
rm(tmp)

data_fieldwork <- data_recoded |>
  right_join(
    clean_input_data(
      bpinst |> select(hid, instrument, bpstatus, bpvalid, bpmode),
      c("instrument", "bpstatus", "bpvalid", "bpmode")) |>
    filter(instrument %in% c("376", "377") & bpstatus %in% c("1", "9") & bpvalid == "1"),
    by = "hid") |>
  filter(!is.na(start_arm)) |>
  mutate(bpmode = factor(case_when(
    bpmode == "1" ~ "CAPI",
    bpmode == "2" ~ "CAWI",
    bpmode == "3" ~ "CATI",
    bpmode == "4" ~ "PAPI",
    bpmode == "6" ~ "CASI",
    TRUE          ~ NA),
    levels = c("CAPI", "CAWI", "PAPI", "CASI", "CATI")))

# assigned versus realised mode
data_fieldwork <- data_fieldwork |> filter(real25 == "1")
table(data_fieldwork$start_arm, data_fieldwork$bpmode, useNA = "ifany")
round(prop.table(table(data_fieldwork$start_arm, data_fieldwork$bpmode, useNA = "ifany"), margin = 1) * 100, 1)

# interview duration
clean_input_data(
  bpinst |> select(hid, instrument, bpstatus, bpvalid, bpmode, bpduration),
  c("instrument", "bpstatus", "bpvalid", "bpmode")) |>
  filter(instrument == "373" & bpstatus %in% c("1", "9") & bpvalid == "1") |>
  mutate(bpmode = factor(case_when(
    bpmode == "1" ~ "CAPI",
    bpmode == "2" ~ "CAWI",
    bpmode == "3" ~ "CATI",
    bpmode == "4" ~ "PAPI",
    bpmode == "6" ~ "CASI",
    TRUE          ~ NA),
    levels = c("CAPI", "CAWI", "PAPI", "CASI", "CATI"))) |>
  right_join(data_fieldwork |> select(hid), by = "hid") |> # only from validly participating households
  filter(!is.na(bpmode)) |>
  group_by(bpmode) |>
  summarise(pe_med_dur = median(bpduration), .groups = "drop") |>
  mutate(pe_med_dur = sprintf("%02d:%02d", as.integer(pe_med_dur %/% 1), as.integer(round(pe_med_dur %% 1 * 60)))) |>
  print()

#-------------------------------------------------------------------------------
# Table: household completeness by start arm and realised HH-questionnaire mode
#
# Rows: start arm x mode of the realised household questionnaire
#
# Columns:
#   hh_completeness - share of realised multi-person HHs (bpbefhpmax > 1) with
#                     complete25 == "1", i.e. bppteil == bpbefhpmax
#   n_part_hh       - realised HHs in the cell (all HH sizes)
#   n_part_multi_hh - realised multi-person HHs in the cell (denominator above)
#
# Interpretation (H2d):
#   Only the "all" rows are causally identified: start_arm is randomised, so the
#   arm marginals give the ITT effect on completeness among multi-person HHs
#
#   The mode rows are DESCRIPTIVE ONLY. bphmode is post-treatment and selected:
#   CAWI-start HHs converted to CAPI are those that failed or refused online.
#-------------------------------------------------------------------------------

data_recoded |>
  filter(real25 == "1") |>
  mutate(mode = case_when(bphmode == "2" ~ "part. CAWI",
                          bphmode == "1" ~ "part. CAPI",
                          TRUE           ~ "part. Others")) |>
  (\(d) bind_rows(mutate(d, mode = "all"), filter(d, !is.na(mode))))() |>
  mutate(multi = bpbefhpmax > 1) |>
  summarise(hh_completeness = mean(complete25[multi] == "1", na.rm = TRUE),
            n_part_hh       = n(),
            n_part_multi_hh = sum(multi, na.rm = TRUE),
            .by = c(start_arm, mode)) |>
  mutate(mode            = factor(mode, levels = c("all", "part. CAWI", "part. CAPI", "part. Others")),
         hh_completeness = round(hh_completeness * 100, 1),
         n_part_hh       = format(n_part_hh, big.mark = ","),
         n_part_multi_hh = format(n_part_multi_hh, big.mark = ",")) |>
  arrange(desc(start_arm), mode) |>
  print()
