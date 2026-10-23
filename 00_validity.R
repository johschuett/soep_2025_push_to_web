################################################################################
## Start mode experiment - SOEP-CORE 2025
## Analysis 00: validity of random assignment
################################################################################

#-------------------------------------------------------------------------------
# M0: start_arm ~ X
# (full gross sample)
#-------------------------------------------------------------------------------
m0 <- with(data_imp_recoded, {
  start_arm_num <- if_else(start_arm == "CAWI-start", 1, 0)
  lm(reformulate(covariates[covariates != "start_arm"], "start_arm_num"))
})

# pooled statistics
coef_table(list(M0 = m0)) |>
  print(n = Inf)
