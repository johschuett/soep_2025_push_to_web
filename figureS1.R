################################################################################
## Start mode experiment - SOEP-CORE 2025
## Figure S1: realisation and household completion over field time, by mode
##            of the realised household questionnaire
################################################################################

#-------------------------------------------------------------------------------
# Figure 1 split by the mode of the realised household questionnaire, coded as
# in the table at the end of fieldwork_outcomes.R (part. CAWI / part. CAPI /
# part. Others). Two panels side by side with one shared y axis; colour =
# start arm, marker shape = mode. Cumulative shares by field week, start arm
# and mode:
#   left  (dashed) - household realised in that mode, as a share of every
#                    household of the arm's gross sample with a non-missing
#                    real25. The three modes of an arm add up to its dashed
#                    line in Figure 1
#   right (solid)  - household complete (complete25), as a share of the
#                    realised multi-person households (bpbefhpmax > 1) of that
#                    arm and mode, i.e. the H2d base split by mode, as in the
#                    table
#
# The modes are post-treatment and selected: CAWI-start households that end up
# in CAPI are those that did not respond online. The split is descriptive.
#
# The design illustration of Figure 1 is left out. Timing comes from bpinst
# (instrumentation2025), loaded in merge_data.R, as in Figure 1.
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# SETTINGS
#-------------------------------------------------------------------------------
# field start as in figure1.R
field_start <- as.Date("2025-03-17") # Monday of ISO week 12, 2025

# bpinst$instrument, coded as in fieldwork_outcomes.R
hh_instrument <- c("376", "377") # household questionnaire
pe_instrument <- "373"           # PE person questionnaire

figS1_colours  <- c(`CAPI-start` = "#0072B2", `CAWI-start` = "#C27F00")
figS1_lines    <- c(real25 = "dashed", complete25 = "solid")
# panel titles, broken over two lines where long
figS1_outcomes <- c(real25     = "Household participation",
                    complete25 = "Household completeness of participating\nmulti-respondent households")
# mode of the realised household questionnaire: the levels fieldwork_outcomes.R
# assigns, their key labels and marker shapes
figS1_modes    <- c(`part. CAWI`   = "Participated in CAWI",
                    `part. CAPI`   = "Participated in CAPI",
                    `part. Others` = "Participated in other mode")
figS1_shapes   <- c(`part. CAWI` = 16, `part. CAPI` = 17, `part. Others` = 15)

# text size, as in figure1.R: one factor scales every label
figS1_text_scale <- 1.3

#-------------------------------------------------------------------------------
# UNIMPUTED ANALYSIS SAMPLE
#-------------------------------------------------------------------------------
figS1_data <-
  complete(data_imp_recoded, action = 0L) |>
  as_tibble() |>
  select(hid, start_arm, real25, complete25, bpbefhpmax, bphmode) |>
  distinct(hid, .keep_all = TRUE) |>
  filter(!is.na(real25)) |>
  mutate(
    # same code as the table at the end of fieldwork_outcomes.R. It is applied
    # to every household here, but only realised households enter the curves,
    # as there
    mode = case_when(bphmode == "2" ~ "part. CAWI",
                     bphmode == "1" ~ "part. CAPI",
                     TRUE           ~ "part. Others"),
    # realised multi-person households, the base of the solid lines. A missing
    # bpbefhpmax is not counted as multi-person
    multi = real25 == "1" & coalesce(bpbefhpmax > 1, FALSE))

# everything below is keyed on the arm names recode_data.R assigns
stopifnot(setequal(levels(figS1_data$start_arm), names(figS1_colours)))

#-------------------------------------------------------------------------------
# INSTRUMENTATION: END DATE OF EVERY VALID INSTRUMENT (as in figure1.R)
#-------------------------------------------------------------------------------
instr <-
  clean_input_data(
    bpinst |> select(hid, pid, instrument, bpstatus, bpvalid,
                     iyear, bpend, bpend_d, bpend_m),
    c("instrument", "bpstatus", "bpvalid")) |>
  filter(bpstatus %in% c("1", "9"), bpvalid == "1", hid %in% figS1_data$hid) |>
  mutate(
    # negative SOEP missing codes -> NA
    across(c(iyear, bpend_m, bpend_d),
           \(x) if_else(as.numeric(x) > 0, as.numeric(x), NA_real_)),
    # bpend is an ISO-8601 string (YYYY-MM-DD hh:mm); where it is empty, the
    # split day / month variables stand in. Impossible dates -> NA
    end_date = coalesce(ymd(substr(as.character(bpend), 1, 10), quiet = TRUE),
                        make_date(iyear, bpend_m, bpend_d))) |>
  filter(!is.na(end_date))

stopifnot(nrow(instr) > 0)

# household questionnaire: the realisation date
hh_dates <-
  instr |>
  filter(instrument %in% hh_instrument) |>
  summarise(hh_date = min(end_date), .by = hid)

# person questionnaires: a household is complete on the day its last eligible
# person finishes
last_p_dates <-
  instr |>
  filter(instrument %in% pe_instrument) |>
  summarise(last_p_date = max(end_date), .by = hid)

#-------------------------------------------------------------------------------
# FIELD WEEKS AND CUMULATIVE SHARES
#-------------------------------------------------------------------------------
field_week <- \(d) as.integer(floor(as.numeric(d - field_start) / 7)) + 1L

figS1_data <-
  figS1_data |>
  left_join(hh_dates,     by = "hid") |>
  left_join(last_p_dates, by = "hid") |>
  mutate(
    real_date = if_else(real25 == "1", hh_date, as.Date(NA)),
    # only multi-person households count, as they are the solid lines' base
    comp_date = if_else(complete25 == "1" & multi,
                        pmax(hh_date, last_p_date, na.rm = TRUE), as.Date(NA)),
    # anything dated before field_start is counted in field week 1
    real_week = pmax(field_week(real_date), 1L),
    comp_week = pmax(field_week(comp_date), 1L))

max_week <- max(c(figS1_data$real_week, figS1_data$comp_week), na.rm = TRUE)

figS1_rates <-
  expand_grid(start_arm  = factor(levels(figS1_data$start_arm),
                                  levels = levels(figS1_data$start_arm)),
              mode       = names(figS1_modes),
              outcome    = names(figS1_outcomes),
              field_week = 1:max_week) |>
  left_join(
    figS1_data |>
      select(start_arm, mode, real25 = real_week, complete25 = comp_week) |>
      pivot_longer(c(real25, complete25), names_to = "outcome",
                   values_to = "field_week") |>
      filter(!is.na(field_week)) |>
      count(start_arm, mode, outcome, field_week),
    by = c("start_arm", "mode", "outcome", "field_week")) |>
  mutate(n = replace_na(n, 0L)) |>
  arrange(start_arm, mode, outcome, field_week) |>
  mutate(cum_n = cumsum(n), .by = c(start_arm, mode, outcome)) |>
  # denominators: the arm's gross sample for real25 (the same for every
  # mode), the arm's realised multi-person households in that mode for
  # complete25
  left_join(bind_rows(
              expand_grid(count(figS1_data, start_arm, name = "n_base"),
                          mode = names(figS1_modes)) |>
                mutate(outcome = "real25"),
              count(filter(figS1_data, multi), start_arm, mode, name = "n_base") |>
                mutate(outcome = "complete25")),
            by = c("start_arm", "mode", "outcome")) |>
  # an arm x mode cell without realised multi-person households has no
  # solid line
  filter(!is.na(n_base)) |>
  mutate(share   = cum_n / n_base,
         mode    = factor(mode,    levels = names(figS1_modes)),
         outcome = factor(outcome, levels = names(figS1_outcomes)))

cat("\n--- Figure S1, end of field (share in %, base n) ---\n")
figS1_rates |>
  filter(field_week == max_week) |>
  transmute(start_arm, mode, outcome, share = round(100 * share, 1), n_base) |>
  arrange(start_arm, outcome, mode) |>
  print(n = Inf)

#-------------------------------------------------------------------------------
# FIGURE
#-------------------------------------------------------------------------------
# two panels side by side, participation left and completeness right, with
# one shared y axis. Colour = start arm, marker shape = mode
y_breaks     <- seq(0, ceiling(max(figS1_rates$share) * 10) / 10, by = 0.1)
y_top        <- max(c(y_breaks, figS1_rates$share))
x_breaks     <- c(1, seq(5, max_week, by = 5))
# markers along the curves, so the mode can be read along each line. None in
# week 1, where all lines start near 0, and none in the last week, where the
# end labels carry the marker
marker_weeks <- seq(5, max_week - 1, by = 5)

# end-of-field values as direct labels in each panel, nudged apart where lines
# end close; a thin leader joins a nudged label to its line
spread_labels <- function(y, min_gap) {
  o  <- order(y)
  ys <- y[o]
  for (i in seq_along(ys)[-1]) ys[i] <- max(ys[i], ys[i - 1] + min_gap)
  y[o] <- ys
  y
}

figS1_ends <-
  figS1_rates |>
  filter(field_week == max_week) |>
  mutate(label   = percent(share, accuracy = 0.1),
         label_y = spread_labels(share, min_gap = 0.032 * figS1_text_scale * y_top),
         .by = outcome) |>
  # x positions precomputed: aes() is evaluated when the plot is drawn
  mutate(x_line = max_week,
         x_mark = max_week + 0.06 * max_week,
         x_text = max_week + 0.10 * max_week)

figS1 <-
  ggplot(figS1_rates,
         aes(x = field_week, y = share, colour = start_arm, linetype = outcome,
             group = interaction(start_arm, mode, outcome))) +

  # --- grid lines, drawn by hand so they stop at the data region
  annotate("segment", x = 1, xend = max_week, y = y_breaks, yend = y_breaks,
           colour = "grey92", linewidth = 0.3) +

  # --- the curves, with mode markers along them
  geom_line(linewidth = 0.6) +
  geom_point(data = filter(figS1_rates, field_week %in% marker_weeks),
             aes(shape = mode), size = 1.8) +

  # --- end labels: leader, mode marker, value
  geom_segment(data = figS1_ends,
               aes(x = x_line, xend = x_mark, y = share, yend = label_y),
               colour = "grey75", linewidth = 0.25, linetype = "solid",
               inherit.aes = FALSE) +
  geom_point(data = figS1_ends,
             aes(x = x_mark, y = label_y, colour = start_arm, shape = mode),
             size = 1.8, inherit.aes = FALSE) +
  geom_text(data = figS1_ends, aes(x = x_text, y = label_y, label = label),
            hjust = 0, size = 3.0 * figS1_text_scale, colour = "grey20",
            inherit.aes = FALSE) +

  # --- one panel per outcome; the y scale is fixed, so the axis is shared
  # and drawn once, on the left
  facet_wrap(~ outcome, nrow = 1, labeller = as_labeller(figS1_outcomes)) +

  scale_x_continuous(breaks = x_breaks,
                     # room on the right for the end labels
                     expand = expansion(add = c(0.5, 0.30 * max_week))) +
  scale_y_continuous(breaks = y_breaks,
                     labels = scales::label_percent(accuracy = 1),
                     expand = expansion(mult = c(0.01, 0.03))) +
  scale_colour_manual(values = figS1_colours, name = NULL) +
  scale_linetype_manual(values = figS1_lines, guide = "none") +
  scale_shape_manual(values = figS1_shapes, labels = figS1_modes, name = NULL) +
  # key above the panels: arms by colour on one row, modes by marker shape on
  # the row below
  guides(colour = guide_legend(order = 1, nrow = 1,
                               override.aes = list(shape = NA, linetype = "solid",
                                                   linewidth = 0.8)),
         shape  = guide_legend(order = 2, nrow = 1,
                               override.aes = list(colour = "grey25", size = 2.2))) +
  coord_cartesian(clip = "off") +
  labs(x = "Field week", y = "Cumulative share") +
  theme_minimal(base_size = 8.5 * figS1_text_scale) +
  theme(panel.grid        = element_blank(),
        panel.spacing.x   = unit(6, "mm"),
        strip.text        = element_text(hjust = 0, vjust = 0, colour = "grey15",
                                         size = 9 * figS1_text_scale,
                                         lineheight = 0.95,
                                         margin = margin(b = 4)),
        axis.text         = element_text(colour = "grey30",
                                         size = 8.5 * figS1_text_scale),
        axis.title        = element_text(colour = "grey30",
                                         size = 9.7 * figS1_text_scale),
        axis.title.x      = element_text(margin = margin(t = 6)),
        axis.title.y      = element_text(margin = margin(r = 6)),
        legend.position   = "top",
        legend.justification = "left",
        legend.box        = "vertical",
        legend.box.just   = "left",
        legend.spacing.x  = unit(2, "mm"),
        legend.key.width  = unit(9, "mm"),
        legend.text       = element_text(colour = "grey20",
                                         size = 8 * figS1_text_scale),
        legend.margin     = margin(0, 0, 2, 0),
        plot.margin       = margin(6, 14, 6, 6))

print(figS1)

ggsave(file.path(PATH_TMP, "figureS1.pdf"), figS1, width = 20, height = 15, units = "cm")

rm(field_start, hh_instrument, pe_instrument, figS1_colours, figS1_lines,
   figS1_outcomes, figS1_modes, figS1_shapes, figS1_text_scale, figS1_data,
   figS1_rates, figS1_ends, instr, hh_dates, last_p_dates, field_week,
   max_week, y_breaks, y_top, x_breaks, marker_weeks, spread_labels)
