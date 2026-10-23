################################################################################
## Start mode experiment - SOEP-CORE 2025
## Analysis 03: data quality
################################################################################

#-------------------------------------------------------------------------------
# Model 7: inr_high ~ hmode25_cawi + X |
# start_arm is instrument for hmode25_cawi
# (restricted to hhs with realised anchor person,
# treatment is "mode is cawi or not")
#-------------------------------------------------------------------------------
m7 <- mi_2sls(data_imp_recoded, covariates, "inr_high", "real25 == \"1\" & !is.na(inr_high)")
m7b <- mi_2sls(data_imp_recoded, covariates, "inr_sens", "real25 == \"1\" & !is.na(inr_high)")

#-------------------------------------------------------------------------------
# Model 8: inr_high ~ hmode25_cawi + X + hmode25_cawi*X |
# start_arm is instrument for hmode25_cawi
# (restricted to hhs with realised anchor person,
# treatment is "mode is cawi or not")
#-------------------------------------------------------------------------------
m8 <- mi_2sls(data_imp_recoded, covariates, "inr_high", "real25 == \"1\" & !is.na(inr_high)", interact = TRUE)
m8b <- mi_2sls(data_imp_recoded, covariates, "inr_sens", "real25 == \"1\" & !is.na(inr_high)", interact = TRUE)

# robustness for H3a (first part, M7 = primary), tests H4b
coef_table(list(M7 = m7, M8 = m8),
           weak_iv = TRUE,
           keep_terms = c("hmode25_cawi1",
                          "refresher1",
                          "education_high1",
                          "mbits_gg400",
                          "hmode25_cawi1:refresher1",
                          "hmode25_cawi1:education_high1",
                          "hmode25_cawi1:mbits_gg400")) |>
  print(n = Inf)

# tests H3a (second part: INR on sensitive items; M7b = primary), tests H4b
coef_table(list(M7b = m7b, M8b = m8b),
           weak_iv = TRUE,
           keep_terms = c("hmode25_cawi1",
                          "refresher1",
                          "education_high1",
                          "mbits_gg400",
                          "hmode25_cawi1:refresher1",
                          "hmode25_cawi1:education_high1",
                          "hmode25_cawi1:mbits_gg400")) |>
  print(n = Inf)

#-------------------------------------------------------------------------------
# Model 9: speeding_high ~ hmode25_cawi + X |
# start_arm is instrument for hmode25_cawi
# (restricted to hhs with realised anchor person,
# treatment is "mode is cawi or not")
#-------------------------------------------------------------------------------
m9 <- mi_2sls(data_imp_recoded, covariates, "speeding_high", "real25 == \"1\" & !is.na(speeding_high)")

#-------------------------------------------------------------------------------
# Model 10: speeding_high ~ hmode25_cawi + X + hmode25_cawi*X |
# start_arm is instrument for hmode25_cawi
# (restricted to hhs with realised anchor person,
# treatment is "mode is cawi or not")
#-------------------------------------------------------------------------------
m10 <- mi_2sls(data_imp_recoded, covariates, "speeding_high", "real25 == \"1\" & !is.na(speeding_high)", interact = TRUE)

# robustness for H3b (M9 / M11 = primary), tests H4b
coef_table(list(M9 = m9, M10 = m10),
           weak_iv = TRUE,
           keep_terms = c("hmode25_cawi1",
                          "refresher1",
                          "education_high1",
                          "mbits_gg400",
                          "hmode25_cawi1:refresher1",
                          "hmode25_cawi1:education_high1",
                          "hmode25_cawi1:mbits_gg400")) |>
  print(n = Inf)

#-------------------------------------------------------------------------------
# Model 11: straightlining_high ~ hmode25_cawi + X |
# start_arm is instrument for hmode25_cawi
# (restricted to hhs with realised anchor person,
# treatment is "mode is cawi or not")
#-------------------------------------------------------------------------------
m11 <- mi_2sls(data_imp_recoded, covariates, "straightlining_high", "real25 == \"1\" & !is.na(straightlining_high)")

#-------------------------------------------------------------------------------
# Model 12: straightlining_high ~ hmode25_cawi + X + hmode25_cawi*X |
# start_arm is instrument for hmode25_cawi
# (restricted to hhs with realised anchor person,
# treatment is "mode is cawi or not")
#-------------------------------------------------------------------------------
m12 <- mi_2sls(data_imp_recoded, covariates, "straightlining_high", "real25 == \"1\" & !is.na(straightlining_high)", interact = TRUE)

# robustness for H3b (M9 / M11 = primary), tests H4b
coef_table(list(M11 = m11, M12 = m12),
           weak_iv = TRUE,
           keep_terms = c("hmode25_cawi1",
                          "refresher1",
                          "education_high1",
                          "mbits_gg400",
                          "hmode25_cawi1:refresher1",
                          "hmode25_cawi1:education_high1",
                          "hmode25_cawi1:mbits_gg400")) |>
  print(n = Inf)
