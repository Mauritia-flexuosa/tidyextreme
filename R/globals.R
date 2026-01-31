# R/globals.R
#' @importFrom rlang .data
#' @importFrom dplyr n
NULL

# Declare global variables to avoid R CMD check notes
utils::globalVariables(
  c(
    ".data", "year", "month", "day", "doy", "date", "datetime",
    "tmax", "tmin", "prcp", "temperature", "precipitation",
    "n_hours", "n_days", "is_dry", "is_wet", "spell_change",
    "spell_id", "cdd_length", "cwd_length", "CDD_max", "CWD_max",
    "temp_10th", "temp_90th", "is_cold_spell", "is_warm_spell",
    "spell_length", "start_date", "end_date", "n_spells",
    "mean_spell_length", "WSDI", "CSDI", "dtr", "DTR_mean",
    "DTR_sd", "R10mm", "R20mm", "R1mm", "Rx1day", "Rx5day",
    "roll_5day", "SDII", "is_wet", "wet_days", "total_prcp",
    "TX25", "TR20", "TX30", "TX35", "TN0", "TX90p", "TN10p",
    "TXx", "TNn", "PRCP_total", "PRCP_days", "PRCP_mean",
    "PRCP_max", "CDD_mean", "CDD_median", "n_dry_spells",
    "CWD_mean", "CWD_median", "n_wet_spells", "season", "time_unit",
    ".", ":=", "wet_days", "extreme_precip", "pr_wet"
  )
)
