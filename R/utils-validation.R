#' @title Basic climate data validation
#' Validates the structure and content of climate data frames.
#' Used internally by all calculation functions.
#'
#' @param df Data frame to validate
#' @param required_cols Character vector of required column names
#' @param time_col Name of the time column
#' @param check_numeric Character vector of columns that must be numeric
#' @param check_positive Character vector of columns that must be non-negative
#'
#' @return Invisible TRUE if validation passes
#'
#' @keywords internal
#' @noRd

validate_climate_data <- function(df, required_cols, time_col,
                                  check_numeric = NULL, check_positive = NULL) {

  if (!is.data.frame(df)) {
    cli::cli_abort(c(
      "Provided object is not a data frame.",
      x = "Received an object of class {class(df)[1]}.",
      i = "The function requires a data frame as input."
    ))
  }

  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "Required columns not found in the data frame.",
      x = "Missing column(s): {missing_cols}.",
      i = "Check column names in your dataset."
    ))
  }

  # Validate time column type using lubridate
  if (!lubridate::is.Date(df[[time_col]]) &&
      !lubridate::is.POSIXct(df[[time_col]])) {
    cli::cli_abort(c(
      "Time column has incorrect type.",
      x = "Column `{time_col}` is class {class(df[[time_col]])[1]}.",
      i = "Convert to Date or POSIXct using lubridate functions."
    ))
  }

  # Check for missing values in time column
  if (any(is.na(df[[time_col]]))) {
    na_count <- sum(is.na(df[[time_col]]))
    cli::cli_abort(c(
      "Time column contains missing values.",
      x = "{na_count} NA value(s) found in column `{time_col}`.",
      i = "Remove or impute missing values before proceeding."
    ))
  }

  # Validate numeric columns
  if (!is.null(check_numeric)) {
    for (col in check_numeric) {
      if (!is.numeric(df[[col]])) {
        cli::cli_abort(c(
          "Numeric column expected but contains non-numeric data.",
          x = "Column `{col}` is class {class(df[[col]])[1]}, not numeric.",
          i = "Ensure the column contains only numeric values."
        ))
      }
    }
  }

  # Validate positive values for precipitation
  if (!is.null(check_positive)) {
    for (col in check_positive) {
      if (any(df[[col]] < 0, na.rm = TRUE)) {
        neg_count <- sum(df[[col]] < 0, na.rm = TRUE)
        cli::cli_abort(c(
          "Column contains unexpected negative values.",
          x = "{neg_count} negative value(s) found in column `{col}`.",
          i = "Precipitation cannot be negative. Check your data."
        ))
      }
    }
  }

  return(invisible(TRUE))
}

#' @title Extract date components using lubridate
#' Adds year, month, day, and day of year columns to a data frame.
#'
#' @param df Data frame containing daily climate data
#' @param date_col Name of the date column (default: "date")
#'
#' @return Data frame with added year, month, day, and doy columns
#' @keywords internal
#' @noRd

extract_date_components <- function(df, date_col = "date") {
  df |>
    dplyr::mutate(
      year = lubridate::year(.data[[date_col]]),
      month = lubridate::month(.data[[date_col]]),
      day = lubridate::day(.data[[date_col]]),
      doy = lubridate::yday(.data[[date_col]])
    )
}
