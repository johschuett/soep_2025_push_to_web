################################################################################
## Start mode experiment - SOEP-CORE 2025
## Analysis 04: socially desirable answers
################################################################################

#-------------------------------------------------------------------------------
# FORMULAS
#-------------------------------------------------------------------------------
# collapsed worry covariates are dropped: the uncollapsed pre-wave item
# of the respective outcome enters as baseline instead
drop_terms <- grep("^worries", covariates, value = TRUE)

fml_health <- reformulate(setdiff(c("bop_154", covariates), drop_terms),
                          response = "bpp_142")

# worry outcomes: 01 = economy, 02 = own economic situation, 04 = own health, 10 = migration
worry_items <- c(economy = "01", finances = "02", health = "04", migration = "10")

fml_worry <- lapply(worry_items, function(s)
  reformulate(setdiff(c(paste0("bop_198_", s), covariates), drop_terms),
              response = paste0("bpp_178_", s)))

#-------------------------------------------------------------------------------
# Models 13/14: subjective_health ~ start_arm + X
# (ordered logit and partial proportional odds, realised anchor persons)
#-------------------------------------------------------------------------------
h_main <- fit_po_pair(dlist, fml_health, "bpp_142",
                      subset_expr = "real25 == \"1\" & !is.na(bpp_142)")
m13 <- h_main$po   # PO-constrained comparison
m14 <- h_main$ppo  # PO relaxed for robust offenders, primary for H3c

#-------------------------------------------------------------------------------
# Models 15-20: worries ~ start_arm + X
# 15/16: worries economy                (bpp_178_01)
# 17/18: worries own economic situation (bpp_178_02)
# 19/20: worries own health             (bpp_178_04)
# 21/22: worries migration              (bpp_178_10)
# outcome is coded 1 = big worries ... 3 = no worries
#-------------------------------------------------------------------------------
worry_models <- lapply(names(worry_items), function(nm) {
  y <- paste0("bpp_178_", worry_items[[nm]])
  fit_po_pair(dlist, fml_worry[[nm]], y,
              subset_expr = sprintf("real25 == \"1\" & !is.na(%s)", y))
})
names(worry_models) <- names(worry_items)

m15 <- worry_models$economy$po   ; m16 <- worry_models$economy$ppo
m17 <- worry_models$finances$po  ; m18 <- worry_models$finances$ppo
m19 <- worry_models$health$po    ; m20 <- worry_models$health$ppo
m21 <- worry_models$migration$po ; m22 <- worry_models$migration$ppo

for (i in seq(13, 20)) report_model(get(paste0("m", i)), paste0("m", i))

#-------------------------------------------------------------------------------
# H3c: average marginal effect of the start arm on each response category
#-------------------------------------------------------------------------------
h3c_models <- list(
  `Subjective health` = m14,
  `Worries economy`   = m16, 
  `Worries finances`  = m18,
  `Worries health`    = m20,
  `Worries migration` = m22)

h3c_ame <- imap_dfr(h3c_models, ~ ame_MI(.x) |> mutate(outcome = .y)) |>
  mutate(outcome = factor(outcome, levels = names(h3c_models)),
         group   = as.character(group),
         sig     = conf.low > 0 | conf.high < 0)

cat("\n--- H3c: AME of CAWI-start on category probabilities ---\n")
print(h3c_ame, n = Inf)

health_lab <- c("Very good", "Good", "Satisfactory", "Poor/Bad")
worry_lab  <- c("Not concerned at all", "Somewhat concerned", "Very concerned")

h3c_ame <- h3c_ame |>
  mutate(
    lab = if_else(outcome == "Subjective health",
                  health_lab[as.integer(group)],
                  worry_lab[match(group, c("3", "2", "1"))]),
    y   = factor(paste(outcome, lab, sep = "@@"),
                 levels = c(paste("Subjective health", health_lab, sep = "@@"),
                            as.vector(t(outer(levels(outcome)[-1], worry_lab,
                                              paste, sep = "@@"))))))

fig2 <- ggplot(h3c_ame, aes(x = estimate, y = y, colour = sig)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.2) +
  geom_point() +
  scale_colour_manual(values = c(`TRUE` = "black", `FALSE` = "grey60"), guide = "none") +
  scale_x_continuous(breaks = breaks_pretty(n = 3)) +
  scale_y_discrete(limits = rev, labels = \(x) sub(".*@@", "", x)) +
  facet_wrap(~ outcome, scales = "free_y", ncol = 2) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 16) +
  theme(plot.margin = margin(5.5, 15, 5.5, 5.5))

print(fig2)

ggsave(file.path(PATH_TMP, "figure2.pdf"), fig2, width = 25, height = 15, units = "cm")

rm(drop_terms, h_main, worry_models, health_lab, worry_lab)
