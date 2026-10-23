################################################################################
## Start mode experiment - SOEP-CORE 2025
## Imputation
################################################################################

#-------------------------------------------------------------------------------
# RECODE MISSING TO NA
#-------------------------------------------------------------------------------
skip_vars <- c(
  grep("^bo|^pg|^migback$|^bpbula$|^mbits_gg400$|^bpp_142$|^bpp_178_|^bph_126$",
       colnames(data_filtered), value = TRUE, invert = TRUE),
  c("bohergs", "bohmode"))

data_formatted <-
  data_filtered |>
  mutate(across(
    .cols = where(is.numeric) & !all_of(skip_vars),
    .fns  = ~ if_else(. < 0, NA_real_, .))) |>
  mutate(across(
    .cols = where(is.factor) & !all_of(skip_vars),
    .fns  = neg_to_na_factor))

rm(skip_vars)

#-------------------------------------------------------------------------------
# CHECK DATA TYPES
#-------------------------------------------------------------------------------
str(data_formatted)
suppressWarnings(describe(data_formatted))

#-------------------------------------------------------------------------------
# START IMPUTATION
#-------------------------------------------------------------------------------
used <- c(setdiff(grep("^bo|^pg", colnames(data_formatted), value = TRUE), c("bohergs", "bohmode", grep("^bop_200", colnames(data_formatted), value = TRUE))), "bpbula", "migback", "mbits_gg400")

cat("\n--- Used for imputation: ---\n")
data_formatted |> select(all_of(used)) |> str()

# inspect missing shares
data_formatted |>
  select(all_of(used)) |>
  summarise(across(everything(), ~ round(mean(is.na(.x)) * 100, 2))) |>
  pivot_longer(everything(), names_to = "column", values_to = "na_pct") |>
  arrange(desc(na_pct))

data_imp <- run_mice(data_formatted, locked_vars = setdiff(colnames(data_formatted), used))
saveRDS(data_imp, file.path(PATH_TMP, "data_imp.rds"))
# data_imp <- readRDS(file.path(PATH_TMP, "data_imp.rds"))

plot(data_imp) |> print()

rm(used)
