################################################################################
## Start mode experiment - SOEP-CORE 2025
## Analysis 01: survey participation
################################################################################

#-------------------------------------------------------------------------------
# M1: real25 ~ start_arm + X
# (full gross sample)
#-------------------------------------------------------------------------------
m1 <- mi_lpm(data_imp   = data_imp_recoded,
             covariates = covariates,
             response   = "real25")

#-------------------------------------------------------------------------------
# M2: real25 ~ start_arm + X + start_arm*X
# (full gross sample)
#-------------------------------------------------------------------------------
m2 <- mi_lpm(data_imp   = data_imp_recoded,
             covariates = covariates,
             response   = "real25",
             interact   = TRUE)

# tests H1a (M1 = primary, M2 = robustness against covariate heterogeneity)
coef_table(
  list(M1 = m1, M2 = m2),
  keep_terms = c("start_armCAWI-start", "age_below501", "education_high1",
                 "full_time1", "email_known1", "west1", "hghinc24_hundreds",
                 "mbits_gg400", "no_migback1", "start_armCAWI-start:age_below501",
                 "start_armCAWI-start:education_high1", "start_armCAWI-start:full_time1",
                 "start_armCAWI-start:email_known1", "start_armCAWI-start:west1",
                 "start_armCAWI-start:hghinc24_hundreds", "start_armCAWI-start:mbits_gg400",
                 "start_armCAWI-start:no_migback1")) |>
  print(n = Inf)

#-------------------------------------------------------------------------------
# M3: real25_compliant ~ start_arm + X
# (restricted to realised households)
#-------------------------------------------------------------------------------
m3 <- mi_lpm(data_imp    = data_imp_recoded,
             covariates  = covariates,
             response    = "real25_compliant",
             subset_expr = "real25 == \"1\"")

#-------------------------------------------------------------------------------
# M4: real25_compliant ~ start_arm + X + start_arm*X
# (restricted to realised households)
#-------------------------------------------------------------------------------
m4 <- mi_lpm(data_imp    = data_imp_recoded,
             covariates  = covariates,
             response    = "real25_compliant",
             subset_expr = "real25 == \"1\"",
             interact    = TRUE)

# tests H1b (selectivity of compliance with the assigned start mode; M4 = primary)
coef_table(
  list(M3 = m3, M4 = m4),
  keep_terms = c("start_armCAWI-start", "age_below501", "education_high1",
                 "full_time1", "email_known1", "west1", "hghinc24_hundreds",
                 "mbits_gg400", "no_migback1", "start_armCAWI-start:age_below501",
                 "start_armCAWI-start:education_high1", "start_armCAWI-start:full_time1",
                 "start_armCAWI-start:email_known1", "start_armCAWI-start:west1",
                 "start_armCAWI-start:hghinc24_hundreds", "start_armCAWI-start:mbits_gg400",
                 "start_armCAWI-start:no_migback1")) |>
  print(n = Inf)
