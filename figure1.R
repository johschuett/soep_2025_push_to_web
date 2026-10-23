################################################################################
## Start mode experiment - SOEP-CORE 2025
## Figure 1: realisation and household completion over field time
################################################################################

#-------------------------------------------------------------------------------
# Cumulative shares by field week and start arm
#   dashed - household realised (real25 == "1"), as a share of every household
#            of the gross sample with a non-missing real25
#   solid  - household complete, every eligible person interviewed
#            (complete25), as a share of the realised multi-person households
#            (real25 == "1" & bpbefhpmax > 1), the H2d base
#
# The two curves have different denominators, so they do not add up: the solid
# line can lie above the dashed one. Its end-of-field value is the H2d
# completeness rate. The denominator is the final number of realised
# multi-person households, so the curve counts completed households among them
# week by week.
#
# Timing comes from bpinst (instrumentation2025), loaded in merge_data.R:
# one row per instrument, with its end date, mode and processing status.
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# SETTINGS
#-------------------------------------------------------------------------------
# wave report claims field start was in week 12 of 2025, the same field start
# merge_data.R uses to filter first contacts (con_week >= 12). Instrumentation
# data (and SOEP_Zeitplan_SOEP_CORE_REF_7713_7715_20250520) suggest week 13,
# with first HH interviews starting Wednesday March 26. Date kept as is
field_start <- as.Date("2025-03-17") # Monday of ISO week 12, 2025

# bpinst$instrument, coded as in fieldwork_outcomes.R
hh_instrument <- c("376", "377") # household questionnaire
pe_instrument <- "373"           # PE person questionnaire

fig1_colours  <- c(`CAPI-start` = "#0072B2", `CAWI-start` = "#C27F00")
fig1_lines    <- c(real25 = "dashed", complete25 = "solid")
fig1_outcomes <- c(real25     = "Household participation",
                  # broken over two lines to fit the key
                  complete25 = "Household completeness of participating\nmulti-respondent households")

# text size. Every label is drawn with annotate() or geom_text(), which
# theme(base_size) does not reach, so this one factor scales them all
# (1 = the original sizes). The spacing that depends on label size - key
# columns, end-label gap, axis title offsets - scales with it
fig1_text_scale <- 1.3

#-------------------------------------------------------------------------------
# UNIMPUTED ANALYSIS SAMPLE
#-------------------------------------------------------------------------------
fig1_data <-
  complete(data_imp_recoded, action = 0L) |>
  as_tibble() |>
  select(hid, start_arm, real25, complete25, bpbefhpmax) |>
  distinct(hid, .keep_all = TRUE) |>
  filter(!is.na(real25)) |>
  # realised multi-person households, the base of the solid lines. A missing
  # bpbefhpmax is not counted as multi-person
  mutate(multi = real25 == "1" & coalesce(bpbefhpmax > 1, FALSE))

# everything below is keyed on the arm names recode_data.R assigns
stopifnot(setequal(levels(fig1_data$start_arm), names(fig1_colours)))

#-------------------------------------------------------------------------------
# INSTRUMENTATION: END DATE OF EVERY VALID INSTRUMENT
#-------------------------------------------------------------------------------
instr <-
  clean_input_data(
    bpinst |> select(hid, pid, instrument, bpstatus, bpvalid,
                     iyear, bpend, bpend_d, bpend_m),
    c("instrument", "bpstatus", "bpvalid")) |>
  filter(bpstatus %in% c("1", "9"), bpvalid == "1", hid %in% fig1_data$hid) |>
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
# person finishes. complete25 already certifies that every eligible person
# took part, so for those households this is the latest end date
last_p_dates <-
  instr |>
  filter(instrument %in% pe_instrument) |>
  summarise(last_p_date = max(end_date), .by = hid)

#-------------------------------------------------------------------------------
# FIELD WEEKS AND CUMULATIVE SHARES
#-------------------------------------------------------------------------------
field_week <- \(d) as.integer(floor(as.numeric(d - field_start) / 7)) + 1L

fig1_data <-
  fig1_data |>
  left_join(hh_dates,     by = "hid") |>
  left_join(last_p_dates, by = "hid") |>
  mutate(
    real_date = if_else(real25 == "1", hh_date, as.Date(NA)),
    # completion needs the household questionnaire as well as every person;
    # where no person date is found, the household date stands in. Only
    # multi-person households count, as they are the solid lines' base
    comp_date = if_else(complete25 == "1" & multi,
                        pmax(hh_date, last_p_date, na.rm = TRUE), as.Date(NA)),
    # anything dated before field_start is counted in field week 1
    real_week = pmax(field_week(real_date), 1L),
    comp_week = pmax(field_week(comp_date), 1L))

max_week <- max(c(fig1_data$real_week, fig1_data$comp_week), na.rm = TRUE)

fig1_rates <-
  expand_grid(start_arm  = factor(levels(fig1_data$start_arm),
                                  levels = levels(fig1_data$start_arm)),
              outcome    = names(fig1_outcomes),
              field_week = 1:max_week) |>
  left_join(
    fig1_data |>
      select(start_arm, real25 = real_week, complete25 = comp_week) |>
      pivot_longer(-start_arm, names_to = "outcome", values_to = "field_week") |>
      filter(!is.na(field_week)) |>
      count(start_arm, outcome, field_week),
    by = c("start_arm", "outcome", "field_week")) |>
  mutate(n = replace_na(n, 0L)) |>
  arrange(start_arm, outcome, field_week) |>
  mutate(cum_n = cumsum(n), .by = c(start_arm, outcome)) |>
  # denominators: the gross sample for real25, realised multi-person
  # households for complete25
  left_join(bind_rows(
              count(fig1_data, start_arm, name = "n_base") |>
                mutate(outcome = "real25"),
              count(filter(fig1_data, multi), start_arm, name = "n_base") |>
                mutate(outcome = "complete25")),
            by = c("start_arm", "outcome")) |>
  mutate(share   = cum_n / n_base,
         outcome = factor(outcome, levels = names(fig1_outcomes)))

#-------------------------------------------------------------------------------
# EXPERIMENT ILLUSTRATION AND FIELD-DESIGN TIMELINES
#-------------------------------------------------------------------------------
# Drawn in the panel's own coordinates, so the milestones line up with the
# field weeks of the curves. Nothing here is estimated - it is the design as
# fielded.
fig1_gross_label <- "23,517\nhouseholds\nrandom 50 / 50"

# side = 1 puts the label above that arm's baseline, side = -1 below it.
# Invitations and the first reminder are only a few weeks before the next
# label on their side, so they are broken over two lines to keep clear of it
fig1_design <- tribble(
  ~arm,          ~week, ~side, ~label,
  "CAWI-start",      1,     1, "Invitations\n(all)",
  "CAWI-start",      3,    -1, "First\nreminder",
  "CAWI-start",      6,     1, "Second reminder",
  "CAWI-start",      8,    -1, "CAPI follow-up: Interviews start approaching households",
  "CAPI-start",      1,     1, "Advance letters announcing interviewer will contact household",
  "CAPI-start",      2,    -1, "Interviewers start approaching households")

# vertical geometry, in share units. The data region ends at the top gridline,
# or higher if a curve runs past it, so the layout does not collapse when the
# realised share is well below 50%
y_breaks  <- seq(0, ceiling(max(fig1_rates$share) * 10) / 10, by = 0.1)
u_top     <- max(c(y_breaks, fig1_rates$share))
row_y     <- c(`CAWI-start` = 1.24 * u_top,   # CAWI timeline above the panel
              `CAPI-start` = -0.36 * u_top)  # CAPI timeline below it
stem_len  <- 0.045 * u_top
label_off <- 0.070 * u_top

# horizontal geometry: negative field weeks hold the illustration. Larger
# text widens the box and the y-axis title, so the box moves left with it
x_left  <- -(0.35 + 0.07 * (fig1_text_scale - 1)) * max_week
x_box   <- -(0.21 + 0.08 * (fig1_text_scale - 1)) * max_week # over the arrows' junction
y_box   <-  mean(row_y)               # the arrows run from here to the two rows
x_right <- max_week + 0.11 * max_week

fig1_design <-
  fig1_design |>
  mutate(y0 = row_y[arm],
         y1 = y0 + side * stem_len,
         yl = y0 + side * label_off,
         # x of the label, precomputed: aes() is evaluated when the plot is
         # drawn, so it must not depend on objects this script later removes
         xl = week + 0.010 * max_week,
         vj = if_else(side > 0, 0, 1))

# guide lines dropping from each CAWI-start milestone to the realised curve,
# so a reminder can be read against the response it produced
cawi_guides <-
  fig1_design |>
  filter(arm == "CAWI-start") |>
  select(week, y0) |>
  left_join(fig1_rates |>
              filter(start_arm == "CAWI-start", outcome == "real25") |>
              select(week = field_week, y_end = share),
            by = "week") |>
  filter(!is.na(y_end))

# the milestones sit in weeks 1-8 whatever the field period, so their labels
# have less room the longer it runs: shrink them, within readable bounds
design_size <- fig1_text_scale * max(1.9, min(2.7, 2.5 * 35 / max_week))

x_breaks <- c(1, seq(5, max_week, by = 5))
y_lo     <- min(fig1_design$yl) - 0.13 * u_top
y_hi     <- max(fig1_design$yl) + 0.13 * u_top

# axis numbers and titles sit inside the panel: numbers nearest the data
x_tick_y  <- -0.06 * u_top
x_title_y <- -(0.06 + 0.09 * fig1_text_scale) * u_top
y_tick_x  <- -0.010 * max_week
y_title_x <- -(0.010 + 0.065 * fig1_text_scale) * max_week

# key, drawn by hand in the empty block below the curves: both start arms
# by colour on the top row, the outcomes by line type below them.
# It starts at field week 14, top row at 25%. Row spacing follows the text
# size and u_top, so it is the same on paper whatever the shares; the
# two-line completeness label gets an extra half row. If the key would reach
# below 0%, it moves up instead
leg_x    <- 14
leg_len  <- 0.066 * max_week
leg_gap  <- 0.020 * max_week
# second arm on the same row, after the first arm's name
leg_x2   <- leg_x + leg_len + leg_gap + 0.135 * fig1_text_scale * max_week
leg_step <- 0.048 * fig1_text_scale * u_top
leg_top  <- max(0.25, 3.5 * leg_step + 0.01)
leg_y    <- leg_top - leg_step * c(0, 1, 2.5)  # arms, participation, completeness

# the arm names label the arrows, placed to their left
arm_label_x <- -0.14 * max_week
arm_label_y <- y_box + 0.55 * (row_y - y_box) + sign(row_y - y_box) * 0.07 * u_top

#-------------------------------------------------------------------------------
# FIGURE
#-------------------------------------------------------------------------------
# end-of-field values as direct labels, nudged apart where two lines end close
spread_labels <- function(y, min_gap) {
  o  <- order(y)
  ys <- y[o]
  for (i in seq_along(ys)[-1]) ys[i] <- max(ys[i], ys[i - 1] + min_gap)
  y[o] <- ys
  y
}

fig1_ends <-
  fig1_rates |>
  filter(field_week == max_week) |>
  mutate(label   = percent(share, accuracy = 0.1),
         label_y = spread_labels(share, min_gap = 0.035 * fig1_text_scale * u_top))

fig1 <-
  ggplot(fig1_rates,
         aes(x = field_week, y = share, colour = start_arm, linetype = outcome,
             group = interaction(start_arm, outcome))) +

  # --- grid lines, drawn by hand so they stop at the data region
  annotate("segment", x = 1, xend = max_week, y = y_breaks, yend = y_breaks,
           colour = "grey92", linewidth = 0.3) +

  # --- the experiment: gross sample, randomised into the two start arms.
  # each arrow runs from the box to its arm's design timeline
  annotate("curve", x = x_box, y = y_box,
           xend = 0.85, yend = row_y[["CAWI-start"]],
           curvature = -0.20, ncp = 12, linewidth = 0.5,
           colour = fig1_colours[["CAWI-start"]],
           arrow = arrow(length = unit(2.1, "mm"), type = "closed")) +
  annotate("curve", x = x_box, y = y_box,
           xend = 0.85, yend = row_y[["CAPI-start"]],
           curvature = 0.20, ncp = 12, linewidth = 0.5,
           colour = fig1_colours[["CAPI-start"]],
           arrow = arrow(length = unit(2.1, "mm"), type = "closed")) +
  annotate("text", x = arm_label_x, y = arm_label_y, label = names(row_y), hjust = 1,
           size = 3.1 * fig1_text_scale, fontface = "bold",
           colour = fig1_colours[names(row_y)]) +
  annotate("label", x = x_box, y = y_box, label = fig1_gross_label,
           size = 3.1 * fig1_text_scale, lineheight = 1.05, colour = "grey15",
           # the border width parameter was renamed between ggplot2 3.4 and
           # 3.5, so leave it at the default and stay quiet on both
           fill = "grey96", label.r = unit(1.2, "mm")) +

  # --- field design: one timeline per arm, sharing the x axis with the curves
  annotate("segment", x = 1, xend = max_week, y = row_y, yend = row_y,
           colour = fig1_colours[names(row_y)], linewidth = 0.4, alpha = 0.5) +
  geom_segment(data = cawi_guides, aes(x = week, xend = week, y = y0, yend = y_end),
               colour = fig1_colours[["CAWI-start"]], linetype = "dotted",
               linewidth = 0.3, alpha = 0.8, inherit.aes = FALSE) +
  geom_segment(data = fig1_design, aes(x = week, xend = week, y = y0, yend = y1),
               colour = "grey55", linewidth = 0.25, inherit.aes = FALSE) +
  geom_point(data = fig1_design, aes(x = week, y = y0, colour = arm),
             size = 1.7, inherit.aes = FALSE, show.legend = FALSE) +
  geom_text(data = fig1_design,
            aes(x = xl, y = yl, label = label, vjust = vj),
            hjust = 0, size = design_size, lineheight = 0.95, colour = "grey20",
            inherit.aes = FALSE) +

  # --- axis numbers and titles, inside the panel
  annotate("text", x = y_tick_x, y = y_breaks, hjust = 1,
           size = 3.0 * fig1_text_scale, colour = "grey30",
           label = scales::percent(y_breaks, accuracy = 1)) +
  annotate("text", x = y_title_x, y = u_top / 2, angle = 90,
           # not "of gross sample": the solid lines have another base
           label = "Cumulative share",
           size = 3.4 * fig1_text_scale, colour = "grey30") +
  annotate("text", x = x_breaks, y = x_tick_y, size = 3.0 * fig1_text_scale,
           colour = "grey30", label = as.character(x_breaks)) +
  annotate("text", x = max_week / 2, y = x_title_y, label = "Field week",
           size = 3.4 * fig1_text_scale, colour = "grey30") +

  # --- the curves
  geom_line(linewidth = 0.6) +
  geom_text(data = fig1_ends, aes(y = label_y, label = label),
            x = max_week + 0.6, hjust = 0, size = 3.0 * fig1_text_scale,
            colour = "grey20", inherit.aes = FALSE) +

  # --- key: both start arms by colour on one row, then outcome by line type
  annotate("segment", x = c(leg_x, leg_x2), xend = c(leg_x, leg_x2) + leg_len,
           y = leg_y[1], yend = leg_y[1],
           colour = unname(fig1_colours), linewidth = 0.7) +
  annotate("text", x = c(leg_x, leg_x2) + leg_len + leg_gap, y = leg_y[1],
           hjust = 0, size = 2.8 * fig1_text_scale, colour = "grey20",
           label = names(fig1_colours)) +
  annotate("segment", x = leg_x, xend = leg_x + leg_len,
           y = leg_y[2:3], yend = leg_y[2:3],
           colour = "grey25", linewidth = 0.6, linetype = unname(fig1_lines)) +
  annotate("text", x = leg_x + leg_len + leg_gap, y = leg_y[2:3], hjust = 0,
           size = 2.8 * fig1_text_scale, lineheight = 0.95, colour = "grey20",
           label = unname(fig1_outcomes)) +

  scale_colour_manual(values = fig1_colours) +
  scale_linetype_manual(values = fig1_lines) +
  coord_cartesian(xlim = c(x_left, x_right), ylim = c(y_lo, y_hi),
                  expand = FALSE, clip = "off") +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 10.5) +
  theme(panel.grid      = element_blank(),
        axis.title      = element_blank(),
        axis.text       = element_blank(),
        axis.ticks      = element_blank(),
        legend.position = "none",
        plot.margin     = margin(6, 14, 6, 6))

print(fig1)

ggsave(file.path(PATH_TMP, "figure1.pdf"), fig1, width = 20, height = 15, units = "cm")

rm(field_start, hh_instrument, pe_instrument, fig1_colours, fig1_lines,
   fig1_outcomes, fig1_data, fig1_rates, fig1_gross_label, fig1_design,
   fig1_ends, hh_dates, last_p_dates, instr, cawi_guides, field_week,
   spread_labels, u_top, row_y, stem_len, label_off, x_left, x_box, y_box,
   x_right, x_breaks, y_breaks, y_lo, y_hi, x_tick_y, x_title_y, y_tick_x,
   y_title_x, leg_x, leg_x2, leg_len, leg_gap, leg_step, leg_top, leg_y, arm_label_x,
   arm_label_y, max_week, design_size, fig1_text_scale)
