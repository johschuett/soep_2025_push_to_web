################################################################################
## Start mode experiment - SOEP-CORE 2025
## Analysis 02: household completion
################################################################################

#-------------------------------------------------------------------------------
# M5: complete25 ~ hmode25_cawi + X | start_arm is instrument for hmode25_cawi
# (restricted to realised multi-hhs, treatment is "mode is cawi or not")
#-------------------------------------------------------------------------------
m5 <- mi_2sls(data_imp_recoded, c("con_week", covariates), "complete25")

#-------------------------------------------------------------------------------
# M6: complete25 ~ hmode25_cawi + X + hmode25_cawi*X |
# start_arm is instrument for hmode25_cawi
# (restricted to realised multi-hhs, treatment is "mode is cawi or not")
#-------------------------------------------------------------------------------
m6 <- mi_2sls(data_imp_recoded, c("con_week", covariates), "complete25", interact = TRUE)

# robustness for H2a (M5 = primary), tests H2b and H2c
coef_table(list(M5 = m5, M6 = m6),
           weak_iv = TRUE,
           keep_terms = c("hmode25_cawi1",
                          "bphhgr",
                          "age_below501",
                          "education_high1",
                          "mbits_gg400",
                          "hmode25_cawi1:bphhgr",
                          "hmode25_cawi1:age_below501",
                          "hmode25_cawi1:education_high1",
                          "hmode25_cawi1:mbits_gg400")) |>
  print(n = Inf)
