################################################################################
## Start mode experiment - SOEP-CORE 2025
## Main script
################################################################################

# clean environment
rm(list = ls(all.names = TRUE))

# extend warning list
options(nwarnings = 10000)

# packages
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
library(pacman)
p_load(
  MASS,   # polr()
  ordinal # clm()
)

# unload to keep namespace clean
p_unload(pacman, negate = TRUE)

p_load(
  # data import
  haven,           # read_dta(), zap_labels()
  readr,           # read_csv()
  # data wrangling
  dplyr,           # verbs
  tibble,          # add_column(), deframe()
  stringr,         # str_remove_all(), str_detect()
  lubridate,       # isoweek()
  purrr,           # imap_dfr()
  tidyr,           # pivot_longer(), pivot_wider()
  # imputation
  mice,            # multiple imputation (CART), pool()
  # estimation
  ivreg,           # instrumental variable regression (levels 2-3)
  marginaleffects, # average marginal contrasts (level 4)
  sandwich,        # vcovHC()
  lmtest,          # coeftest()
  # diagnostics and post-estimation
  brant,           # proportional odds assumption
  broom,           # tidy()
  psych,           # describe()
  # output
  ggplot2,         # plotting
  scales,          # scales in plots
  knitr            # kable()
)

# preliminary stuff
source("functions.R", echo = TRUE)
source("paths.R")

# data preparation
source("merge_data.R", echo = TRUE)
source("impute_data.R", echo = TRUE)
source("recode_data.R", echo = TRUE)
source("label_terms.R", echo = TRUE)

# fieldwork
source("fieldwork_outcomes.R", echo = TRUE)
source("figure1.R", echo = TRUE)
source("figureS1.R", echo = TRUE)

# analyses
source("00_validity.R", echo = TRUE)
source("01_participation.R", echo = TRUE)
source("02_hh_completion.R", echo = TRUE)
source("03_quality.R", echo = TRUE)
source("04_desirability.R", echo = TRUE)
source("05_discontinuity.R", echo = TRUE)
