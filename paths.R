################################################################################
## Start mode experiment - SOEP-CORE 2025
## Paths
################################################################################

#-------------------------------------------------------------------------------
# DEFINE PATHS
#-------------------------------------------------------------------------------
os_name <- Sys.info()[["sysname"]]
PATH_ROOT <- switch(
  os_name,
  Windows = file.path("<redacted>"),
  Darwin  = file.path("<redacted>"),
  stop("Root path undefined for OS ", os_name, "!"))

PATH_RESTR     <- file.path(PATH_ROOT, "<redacted>", "Stata", "soepdata")
PATH_RESTR_RAW <- file.path(PATH_RESTR, "raw")

PATH_TMP       <- file.path(PATH_ROOT, "<redacted>")
PATH_EXTRA     <- file.path(PATH_TMP, "extra") # directory containing files from zip file

rm(os_name, PATH_ROOT)
