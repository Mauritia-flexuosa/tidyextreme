#' List available climate indices
#'
#' @return A data frame with available indices and descriptions
#' @export
list_indices <- function() {
  indices <- data.frame(
    Index = c("prcptot", "r10mm", "r20mm", "rx1day", "rx5day",
              "sdii", "cdd", "cwd", "r95p", "r99p",
              "su", "tr", "tx90p", "tn90p", "tx10p", "tn10p",
              "wsdi", "csdi", "dtr"),
    Description = c("Annual total precipitation",
                    "Number of days with precipitation >= 10mm",
                    "Number of days with precipitation >= 20mm",
                    "Maximum 1-day precipitation",
                    "Maximum 5-day consecutive precipitation",
                    "Simple daily intensity index",
                    "Maximum consecutive dry days",
                    "Maximum consecutive wet days",
                    "Total precipitation from very wet days (95th percentile)",
                    "Total precipitation from extremely wet days (99th percentile)",
                    "Summer days (Tmax > 25 degrees C)",
                    "Tropical nights (Tmin > 20 degrees C)",
                    "Warm days (Tmax > 90th percentile)",
                    "Warm nights (Tmin > 90th percentile)",
                    "Cool days (Tmax < 10th percentile)",
                    "Cool nights (Tmin < 10th percentile)",
                    "Warm spell duration index (>= 6 consecutive days)",
                    "Cold spell duration index (>= 6 consecutive days)",
                    "Mean daily temperature range"),
    Category = c(rep("Precipitation", 10), rep("Temperature", 9))
  )

  return(indices)
}
