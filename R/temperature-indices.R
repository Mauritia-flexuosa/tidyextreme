#' Calculate number of summer days (TX > 25°C)
#'
#' Counts the number of days per year when daily maximum temperature
#' exceeds 25°C, following ETCCDI definition SU25.
#'
#' @param df Data frame with climate data
#' @param frequency Temporal frequency: "daily" or "hourly"
#' @param time_col Name of the time column. For daily frequency, the column should be
#'   of class Date or a string in the format YYYY-MM-DD. For hourly frequency, the column
#'   should be of class POSIXct or a string in the format YYYY-MM-DD HH:MM:SS.
#' @param tmax_col Name of maximum temperature column (daily data)
#' @param temp_col Name of temperature column (for single temp or hourly)
#' @param threshold Temperature threshold in °C (default: 25)
#'
#' @return A tibble with columns: year, TX25
#'
#' @examples
#' # Daily data with separate max/min
#' daily_data <- data.frame(
#'   date = seq(as.Date("2000-01-01"), as.Date("2002-12-31"), by = "day"),
#'   tmax = rnorm(1096, mean = 25, sd = 6)
#' )
#'
#' calculate_TX25(
#'   df = daily_data,
#'   frequency = "daily",
#'   time_col = "date",
#'   tmax_col = "tmax"
#' )
#'
#' # Hourly data
#' hourly_data <- data.frame(
#'   datetime = seq(
#'     as.POSIXct("2000-01-01 00:00", tz = "UTC"),
#'     as.POSIXct("2000-01-31 23:00", tz = "UTC"),
#'     by = "hour"
#'   ),
#'   temperature = rnorm(31*24, mean = 22, sd = 4)
#' )
#'
#' calculate_TX25(
#'   df = hourly_data,
#'   frequency = "hourly",
#'   time_col = "datetime",
#'   temp_col = "temperature",
#'   threshold = 25
#' )
#'
#' @export
calculate_TX25 <- function(df, frequency = "daily",
                           time_col = NULL,
                           tmax_col = NULL,
                           temp_col = NULL,
                           threshold = 25) {

  # Prepare daily temperature data
  daily_data <- prepare_daily_data(
    df = df,
    data_type = "temperature",
    frequency = frequency,
    time_col = time_col,
    tmax_col = tmax_col,
    tmin_col = NULL,
    prcp_col = NULL,
    temp_col = temp_col,
    precip_col = NULL
  )

  # Determine which column to use for threshold calculation
  if ("tmax" %in% names(daily_data)) {
    temp_col_to_use <- "tmax"
  } else if ("tmin" %in% names(daily_data)) {
    temp_col_to_use <- "tmin"
    cli::cli_warn(c(
      "Using minimum temperature for TX25 calculation.",
      i = "TX25 typically uses maximum temperature (TX).",
      i = "Results may not follow ETCCDI standard."
    ))
  } else {
    cli::cli_abort(c(
      "No temperature data available for calculation.",
      x = "Neither tmax nor tmin columns found.",
      i = "Provide temperature data for TX25 calculation."
    ))
  }

  # Calculate summer days
  result <- daily_data |>
    extract_date_components(date_col = "date") |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      TX25 = sum(.data[[temp_col_to_use]] > threshold, na.rm = TRUE),
      n_days = sum(!is.na(.data[[temp_col_to_use]])),
      .groups = 'drop'
    )

  # Check data completeness
  if (any(result$n_days < 300)) {
    incomplete_years <- result |>
      dplyr::filter(n_days < 300)
    cli::cli_warn(c(
      "Some years have incomplete data.",
      x = "{nrow(incomplete_years)} year(s) with less than 300 days of data.",
      i = "Consider the impact on annual totals."
    ))
  }

  return(result  |> dplyr::select(-n_days))
}

#' Calculate number of tropical nights (TN > 20°C)
#'
#' Counts the number of days per year when daily minimum temperature
#' exceeds 20°C, following ETCCDI definition TR20.
#'
#' @param df Data frame with climate data
#' @param frequency Temporal frequency: "daily" or "hourly"
#' @param time_col Name of the time column. For daily frequency, the column should be
#'   of class Date or a string in the format YYYY-MM-DD. For hourly frequency, the column
#'   should be of class POSIXct or a string in the format YYYY-MM-DD HH:MM:SS.
#' @param tmin_col Name of minimum temperature column (daily data)
#' @param temp_col Name of temperature column (for single temp or hourly)
#' @param threshold Temperature threshold in °C (default: 20)
#'
#' @return A tibble with columns: year, TR20
#'
#' @examples
#' # Daily data with separate min temperature
#' daily_data <- data.frame(
#'   date = seq(as.Date("2000-01-01"), as.Date("2002-12-31"), by = "day"),
#'   tmin = rnorm(1096, mean = 18, sd = 5)
#' )
#'
#' calculate_TR20(
#'   df = daily_data,
#'   frequency = "daily",
#'   time_col = "date",
#'   tmin_col = "tmin"
#' )
#'
#' # Hourly data (will be aggregated to daily min temperature)
#' hourly_data <- data.frame(
#'   datetime = seq(
#'     as.POSIXct("2000-01-01 00:00", tz = "UTC"),
#'     as.POSIXct("2000-01-31 23:00", tz = "UTC"),
#'     by = "hour"
#'   ),
#'   temperature = rnorm(31*24, mean = 16, sd = 3)
#' )
#'
#' calculate_TR20(
#'   df = hourly_data,
#'   frequency = "hourly",
#'   time_col = "datetime",
#'   temp_col = "temperature",
#'   threshold = 20
#' )
#' @export
calculate_TR20 <- function(df, frequency = "daily",
                           time_col = NULL,
                           tmin_col = NULL,
                           temp_col = NULL,
                           threshold = 20) {

  daily_data <- prepare_daily_data(
    df = df,
    data_type = "temperature",
    frequency = frequency,
    time_col = time_col,
    tmax_col = NULL,
    tmin_col = tmin_col,
    prcp_col = NULL,
    temp_col = temp_col,
    precip_col = NULL
  )

  if ("tmin" %in% names(daily_data)) {
    temp_col_to_use <- "tmin"
  } else if ("tmax" %in% names(daily_data)) {
    temp_col_to_use <- "tmax"
    cli::cli_warn(c(
      "Using maximum temperature for TR20 calculation.",
      i = "TR20 typically uses minimum temperature (TN).",
      i = "Results may not follow ETCCDI standard."
    ))
  } else {
    cli::cli_abort(c(
      "No temperature data available for calculation.",
      x = "Neither tmin nor tmax columns found.",
      i = "Provide temperature data for TR20 calculation."
    ))
  }

  result <- daily_data |>
    extract_date_components(date_col = "date") |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      TR20 = sum(.data[[temp_col_to_use]] > threshold, na.rm = TRUE),
      n_days = sum(!is.na(.data[[temp_col_to_use]])),
      .groups = 'drop'
    )

  return(result  |> dplyr::select(-n_days))
}

#' Calculate monthly maximum value of daily maximum temperature (TXx)
#'
#' Calculates the highest daily maximum temperature for each month,
#' following ETCCDI definition TXx.
#'
#' @param df Data frame with climate data
#' @param frequency Temporal frequency: "daily" or "hourly"
#' @param time_col Name of the time column. For daily frequency, the column should be
#'   of class Date or a string in the format YYYY-MM-DD. For hourly frequency, the column
#'   should be of class POSIXct or a string in the format YYYY-MM-DD HH:MM:SS.
#' @param tmax_col Name of maximum temperature column (daily data)
#' @param temp_col Name of temperature column (for single temp or hourly)
#' @param min_days Minimum days per month for valid calculation (default: 20)
#'
#' @return A tibble with columns: year, month, TXx
#'
#' @examples
#' # Daily data with maximum temperature
#' daily_data <- data.frame(
#'   date = seq(as.Date("2000-01-01"), as.Date("2002-12-31"), by = "day"),
#'   tmax = rnorm(1096, mean = 25, sd = 6)
#' )
#'
#' calculate_TXx(
#'   df = daily_data,
#'   frequency = "daily",
#'   time_col = "date",
#'   tmax_col = "tmax"
#' )
#'
#' # With custom minimum days per month
#' calculate_TXx(
#'   df = daily_data,
#'   frequency = "daily",
#'   time_col = "date",
#'   tmax_col = "tmax",
#'   min_days = 25
#' )
#' @export
calculate_TXx <- function(df, frequency = "daily",
                          time_col = NULL,
                          tmax_col = NULL,
                          temp_col = NULL,
                          min_days = 20) {

  daily_data <- prepare_daily_data(
    df = df,
    data_type = "temperature",
    frequency = frequency,
    time_col = time_col,
    tmax_col = tmax_col,
    tmin_col = NULL,
    prcp_col = NULL,
    temp_col = temp_col,
    precip_col = NULL
  )

  # Use tmax if available, otherwise use whatever temperature column exists
  if ("tmax" %in% names(daily_data)) {
    temp_col_for_max <- "tmax"
  } else if ("tmin" %in% names(daily_data)) {
    temp_col_for_max <- "tmin"
    cli::cli_warn(c(
      "Using minimum temperature for TXx calculation.",
      i = "TXx should use maximum temperature (TX) per ETCCDI.",
      i = "Results are not standard-compliant."
    ))
  } else {
    temp_cols <- setdiff(names(daily_data), "date")
    if (length(temp_cols) > 0) {
      temp_col_for_max <- temp_cols[1]
      cli::cli_warn(c(
        "Using available temperature column for TXx.",
        x = "Column `{temp_col_for_max}` used as proxy for maximum temperature.",
        i = "This deviates from ETCCDI standard which requires TX."
      ))
    } else {
      cli::cli_abort(c(
        "No temperature data available.",
        x = "No temperature columns found in prepared data.",
        i = "Provide temperature data for TXx calculation."
      ))
    }
  }

  result <- daily_data |>
    extract_date_components(date_col = "date") |>
    dplyr::group_by(year, month) |>
    dplyr::summarise(
      TXx = max(.data[[temp_col_for_max]], na.rm = TRUE),
      n_days = dplyr::n(),
      .groups = 'drop'
    ) |>
    dplyr::filter(n_days >= min_days)

  if (nrow(result) == 0) {
    cli::cli_abort(c(
      "No months with sufficient data for calculation.",
      x = "All months have less than {min_days} days of data.",
      i = "Check data completeness in your time series."
    ))
  }

  return(result  |> dplyr::select(-n_days))
}

#' Calculate monthly minimum value of daily minimum temperature (TNn)
#'
#' Calculates the lowest daily minimum temperature for each month,
#' following ETCCDI definition TNn.
#'
#' @param df Data frame with climate data
#' @param frequency Temporal frequency: "daily" or "hourly"
#' @param time_col Name of the time column. For daily frequency, the column should be
#'   of class Date or a string in the format YYYY-MM-DD. For hourly frequency, the column
#'   should be of class POSIXct or a string in the format YYYY-MM-DD HH:MM:SS.
#' @param tmin_col Name of minimum temperature column (daily data)
#' @param temp_col Name of temperature column (for single temp or hourly)
#' @param min_days Minimum days per month for valid calculation (default: 20)
#'
#' @return A tibble with columns: year, month, TNn
#'
#' @examples
#' # Daily data with minimum temperature
#' daily_data <- data.frame(
#'   date = seq(as.Date("2000-01-01"), as.Date("2002-12-31"), by = "day"),
#'   tmin = rnorm(1096, mean = 10, sd = 5)
#' )
#'
#' calculate_TNn(
#'   df = daily_data,
#'   frequency = "daily",
#'   time_col = "date",
#'   tmin_col = "tmin"
#' )
#'
#' # With custom minimum days per month
#' calculate_TNn(
#'   df = daily_data,
#'   frequency = "daily",
#'   time_col = "date",
#'   tmin_col = "tmin",
#'   min_days = 25
#' )
#' @export
calculate_TNn <- function(df, frequency = "daily",
                          time_col = NULL,
                          tmin_col = NULL,
                          temp_col = NULL,
                          min_days = 20) {

  daily_data <- prepare_daily_data(
    df = df,
    data_type = "temperature",
    frequency = frequency,
    time_col = time_col,
    tmax_col = NULL,
    tmin_col = tmin_col,
    prcp_col = NULL,
    temp_col = temp_col,
    precip_col = NULL
  )

  if ("tmin" %in% names(daily_data)) {
    temp_col_for_min <- "tmin"
  } else if ("tmax" %in% names(daily_data)) {
    temp_col_for_min <- "tmax"
    cli::cli_warn(c(
      "Using maximum temperature for TNn calculation.",
      i = "TNn should use minimum temperature (TN) per ETCCDI.",
      i = "Results are not standard-compliant."
    ))
  } else {
    temp_cols <- setdiff(names(daily_data), "date")
    if (length(temp_cols) > 0) {
      temp_col_for_min <- temp_cols[1]
      cli::cli_warn(c(
        "Using available temperature column for TNn.",
        x = "Column `{temp_col_for_min}` used as proxy for minimum temperature.",
        i = "This deviates from ETCCDI standard which requires TN."
      ))
    } else {
      cli::cli_abort(c(
        "No temperature data available.",
        x = "No temperature columns found in prepared data.",
        i = "Provide temperature data for TNn calculation."
      ))
    }
  }

  result <- daily_data |>
    extract_date_components(date_col = "date") |>
    dplyr::group_by(year, month) |>
    dplyr::summarise(
      TNn = min(.data[[temp_col_for_min]], na.rm = TRUE),
      n_days = dplyr::n(),
      .groups = 'drop'
    ) |>
    dplyr::filter(n_days >= min_days)

  return(result  |> dplyr::select(-n_days))
}

#' Calculate number of days with temperature ≥ 30°C
#'
#' Counts the number of days per year when daily temperature
#' is greater than or equal to 30°C.
#'
#' @param df Data frame with climate data
#' @param frequency Temporal frequency: "daily" or "hourly"
#' @param time_col Name of the time column. For daily frequency, the column should be
#'   of class Date or a string in the format YYYY-MM-DD. For hourly frequency, the column
#'   should be of class POSIXct or a string in the format YYYY-MM-DD HH:MM:SS.
#' @param tmax_col Name of maximum temperature column (daily data)
#' @param temp_col Name of temperature column (for single temp or hourly)
#'
#' @return A tibble with columns: year, TX30
#'
#' @examples
#' # Daily data with maximum temperature
#' daily_data <- data.frame(
#'   date = seq(as.Date("2000-01-01"), as.Date("2002-12-31"), by = "day"),
#'   tmax = rnorm(1096, mean = 25, sd = 6)
#' )
#'
#' calculate_TX30(
#'   df = daily_data,
#'   frequency = "daily",
#'   time_col = "date",
#'   tmax_col = "tmax"
#' )
#' @export
calculate_TX30 <- function(df, frequency = "daily",
                           time_col = NULL,
                           tmax_col = NULL,
                           temp_col = NULL) {

  calculate_TX25(
    df = df,
    frequency = frequency,
    time_col = time_col,
    tmax_col = tmax_col,
    temp_col = temp_col,
    threshold = 30
  ) |>
    dplyr::rename(TX30 = TX25)
}

#' Calculate number of days with temperature ≥ 35°C
#'
#' Counts the number of days per year when daily temperature
#' is greater than or equal to 35°C.
#'
#' @param df Data frame with climate data
#' @param frequency Temporal frequency: "daily" or "hourly"
#' @param time_col Name of the time column. For daily frequency, the column should be
#'   of class Date or a string in the format YYYY-MM-DD. For hourly frequency, the column
#'   should be of class POSIXct or a string in the format YYYY-MM-DD HH:MM:SS.
#' @param tmax_col Name of maximum temperature column (daily data)
#' @param temp_col Name of temperature column (for single temp or hourly)
#'
#' @return A tibble with columns: year, TX35
#'
#' @examples
#' # Daily data with maximum temperature
#' daily_data <- data.frame(
#'   date = seq(as.Date("2000-01-01"), as.Date("2002-12-31"), by = "day"),
#'   tmax = rnorm(1096, mean = 25, sd = 6)
#' )
#'
#' calculate_TX35(
#'   df = daily_data,
#'   frequency = "daily",
#'   time_col = "date",
#'   tmax_col = "tmax"
#' )
#' @export
calculate_TX35 <- function(df, frequency = "daily",
                           time_col = NULL,
                           tmax_col = NULL,
                           temp_col = NULL) {

  calculate_TX25(
    df = df,
    frequency = frequency,
    time_col = time_col,
    tmax_col = tmax_col,
    temp_col = temp_col,
    threshold = 35
  ) |>
    dplyr::rename(TX35 = TX25)
}

#' Calculate number of days with temperature < 0°C
#'
#' Counts the number of days per year when daily temperature
#' is less than 0°C.
#'
#' @param df Data frame with climate data
#' @param frequency Temporal frequency: "daily" or "hourly"
#' @param time_col Name of the time column. For daily frequency, the column should be
#'   of class Date or a string in the format YYYY-MM-DD. For hourly frequency, the column
#'   should be of class POSIXct or a string in the format YYYY-MM-DD HH:MM:SS.
#' @param tmin_col Name of minimum temperature column (daily data)
#' @param temp_col Name of temperature column (for single temp or hourly)
#'
#' @return A tibble with columns: year, TN0
#'
#' @examples
#' # Daily data with minimum temperature
#' daily_data <- data.frame(
#'   date = seq(as.Date("2000-01-01"), as.Date("2002-12-31"), by = "day"),
#'   tmin = rnorm(1096, mean = 5, sd = 5)
#' )
#'
#' calculate_TN0(
#'   df = daily_data,
#'   frequency = "daily",
#'   time_col = "date",
#'   tmin_col = "tmin"
#' )
#' @export
calculate_TN0 <- function(df, frequency = "daily",
                          time_col = NULL,
                          tmin_col = NULL,
                          temp_col = NULL) {

  daily_data <- prepare_daily_data(
    df = df,
    data_type = "temperature",
    frequency = frequency,
    time_col = time_col,
    tmax_col = NULL,
    tmin_col = tmin_col,
    prcp_col = NULL,
    temp_col = temp_col,
    precip_col = NULL
  )

  if ("tmin" %in% names(daily_data)) {
    temp_col_to_use <- "tmin"
  } else if ("tmax" %in% names(daily_data)) {
    temp_col_to_use <- "tmax"
    cli::cli_warn(c(
      "Using maximum temperature for TN0 calculation.",
      i = "TN0 typically uses minimum temperature (TN).",
      i = "Results may not follow ETCCDI standard."
    ))
  } else {
    cli::cli_abort(c(
      "No temperature data available for calculation.",
      x = "Neither tmin nor tmax columns found.",
      i = "Provide temperature data for TN0 calculation."
    ))
  }

  result <- daily_data |>
    extract_date_components(date_col = "date") |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      TN0 = sum(.data[[temp_col_to_use]] < 0, na.rm = TRUE),
      n_days = sum(!is.na(.data[[temp_col_to_use]])),
      .groups = 'drop'
    )

  return(result  |> dplyr::select(-n_days))
}

#' Calculate diurnal temperature range (DTR)
#'
#' Calculates the mean and standard deviation of daily temperature range
#' (difference between maximum and minimum temperature) per year.
#'
#' @param df Data frame with climate data
#' @param frequency Temporal frequency: "daily" or "hourly"
#' @param time_col Name of the time column. For daily frequency, the column should be
#'   of class Date or a string in the format YYYY-MM-DD. For hourly frequency, the column
#'   should be of class POSIXct or a string in the format YYYY-MM-DD HH:MM:SS.
#' @param tmax_col Name of maximum temperature column (daily data)
#' @param tmin_col Name of minimum temperature column (daily data)
#' @param temp_col Name of temperature column (hourly data)
#'
#' @return A tibble with columns: year, DTR_mean, DTR_sd, n_days
#'
#' @examples
#' # Daily data with maximum and minimum temperature
#' daily_data <- data.frame(
#'   date = seq(as.Date("2000-01-01"), as.Date("2002-12-31"), by = "day"),
#'   tmax = rnorm(1096, mean = 25, sd = 5),
#'   tmin = rnorm(1096, mean = 15, sd = 5)
#' )
#'
#' calculate_DTR(
#'   df = daily_data,
#'   frequency = "daily",
#'   time_col = "date",
#'   tmax_col = "tmax",
#'   tmin_col = "tmin"
#' )
#' @export
calculate_DTR <- function(df, frequency = "daily",
                          time_col = NULL,
                          tmax_col = NULL,
                          tmin_col = NULL,
                          temp_col = NULL) {

  daily_data <- prepare_daily_data(
    df = df,
    data_type = "temperature",
    frequency = frequency,
    time_col = time_col,
    tmax_col = tmax_col,
    tmin_col = tmin_col,
    prcp_col = NULL,
    temp_col = temp_col,
    precip_col = NULL
  )

  missing_cols <- setdiff(c("tmax", "tmin"), names(daily_data))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "Incomplete data for DTR calculation.",
      x = "Missing column(s): {missing_cols}.",
      i = "DTR requires both maximum and minimum daily temperatures."
    ))
  }

  result <- daily_data |>
    extract_date_components(date_col = "date") |>
    dplyr::mutate(dtr = tmax - tmin) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      DTR_mean = mean(dtr, na.rm = TRUE),
      DTR_sd = stats::sd(dtr, na.rm = TRUE),
      n_days = sum(!is.na(dtr)),
      .groups = 'drop'
    )

  return(result)
}

#' Calculate 90th percentile of daily temperature (TX90p)
#'
#' Calculates the 90th percentile of daily temperature per year,
#' used as threshold for extreme warm days.
#'
#' @param df Data frame with climate data
#' @param frequency Temporal frequency: "daily" or "hourly"
#' @param time_col Name of the time column. For daily frequency, the column should be
#'   of class Date or a string in the format YYYY-MM-DD. For hourly frequency, the column
#'   should be of class POSIXct or a string in the format YYYY-MM-DD HH:MM:SS.
#' @param tmax_col Name of maximum temperature column (daily data)
#' @param temp_col Name of temperature column (for single temp or hourly)
#'
#' @return A tibble with columns: year, TX90p
#'
#' @examples
#' # Daily data with maximum temperature
#' daily_data <- data.frame(
#'   date = seq(as.Date("2000-01-01"), as.Date("2002-12-31"), by = "day"),
#'   tmax = rnorm(1096, mean = 25, sd = 6)
#' )
#'
#' calculate_TX90p(
#'   df = daily_data,
#'   frequency = "daily",
#'   time_col = "date",
#'   tmax_col = "tmax"
#' )
#' @export
calculate_TX90p <- function(df, frequency = "daily",
                            time_col = NULL,
                            tmax_col = NULL,
                            temp_col = NULL) {

  daily_data <- prepare_daily_data(
    df = df,
    data_type = "temperature",
    frequency = frequency,
    time_col = time_col,
    tmax_col = tmax_col,
    tmin_col = NULL,
    prcp_col = NULL,
    temp_col = temp_col,
    precip_col = NULL
  )

  if ("tmax" %in% names(daily_data)) {
    temp_col_to_use <- "tmax"
  } else if ("tmin" %in% names(daily_data)) {
    temp_col_to_use <- "tmin"
    cli::cli_warn(c(
      "Using minimum temperature for TX90p calculation.",
      i = "TX90p typically uses maximum temperature (TX).",
      i = "Results may not follow ETCCDI standard."
    ))
  } else {
    cli::cli_abort(c(
      "No temperature data available for calculation.",
      x = "Neither tmax nor tmin columns found.",
      i = "Provide temperature data for TX90p calculation."
    ))
  }

  result <- daily_data |>
    extract_date_components(date_col = "date") |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      TX90p = stats::quantile(.data[[temp_col_to_use]], 0.9, na.rm = TRUE),
      n_days = sum(!is.na(.data[[temp_col_to_use]])),
      .groups = 'drop'
    )

  return(result  |> dplyr::select(-n_days))
}

#' Calculate 10th percentile of daily temperature (TN10p)
#'
#' Calculates the 10th percentile of daily temperature per year,
#' used as threshold for extreme cold nights.
#'
#' @param df Data frame with climate data
#' @param frequency Temporal frequency: "daily" or "hourly"
#' @param time_col Name of the time column. For daily frequency, the column should be
#'   of class Date or a string in the format YYYY-MM-DD. For hourly frequency, the column
#'   should be of class POSIXct or a string in the format YYYY-MM-DD HH:MM:SS.
#' @param tmin_col Name of minimum temperature column (daily data)
#' @param temp_col Name of temperature column (for single temp or hourly)
#'
#' @return A tibble with columns: year, TN10p
#'
#' @examples
#' # Daily data with minimum temperature
#' daily_data <- data.frame(
#'   date = seq(as.Date("2000-01-01"), as.Date("2002-12-31"), by = "day"),
#'   tmin = rnorm(1096, mean = 10, sd = 5)
#' )
#'
#' calculate_TN10p(
#'   df = daily_data,
#'   frequency = "daily",
#'   time_col = "date",
#'   tmin_col = "tmin"
#' )
#' @export
calculate_TN10p <- function(df, frequency = "daily",
                            time_col = NULL,
                            tmin_col = NULL,
                            temp_col = NULL) {

  daily_data <- prepare_daily_data(
    df = df,
    data_type = "temperature",
    frequency = frequency,
    time_col = time_col,
    tmax_col = NULL,
    tmin_col = tmin_col,
    prcp_col = NULL,
    temp_col = temp_col,
    precip_col = NULL
  )

  if ("tmin" %in% names(daily_data)) {
    temp_col_to_use <- "tmin"
  } else if ("tmax" %in% names(daily_data)) {
    temp_col_to_use <- "tmax"
    cli::cli_warn(c(
      "Using maximum temperature for TN10p calculation.",
      i = "TN10p typically uses minimum temperature (TN).",
      i = "Results may not follow ETCCDI standard."
    ))
  } else {
    cli::cli_abort(c(
      "No temperature data available for calculation.",
      x = "Neither tmin nor tmax columns found.",
      i = "Provide temperature data for TN10p calculation."
    ))
  }

  result <- daily_data |>
    extract_date_components(date_col = "date") |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      TN10p = stats::quantile(.data[[temp_col_to_use]], 0.1, na.rm = TRUE),
      n_days = sum(!is.na(.data[[temp_col_to_use]])),
      .groups = 'drop'
    )

  return(result  |> dplyr::select(-n_days))
}

#' Calculate Warm Spell Duration Index (WSDI)
#'
#' Calculates the number of days with at least 6 consecutive days
#' where temperature exceeds the 90th percentile, following ETCCDI
#' definition WSDI.
#'
#' @param df Data frame with climate data
#' @param frequency Temporal frequency: "daily" or "hourly"
#' @param time_col Name of the time column. For daily frequency, the column should be
#'   of class Date or a string in the format YYYY-MM-DD. For hourly frequency, the column
#'   should be of class POSIXct or a string in the format YYYY-MM-DD HH:MM:SS.
#' @param tmax_col Name of maximum temperature column (daily data)
#' @param temp_col Name of temperature column (for single temp or hourly)
#' @param window_days Window size for percentile calculation (default: 30)
#' @param min_consecutive Minimum consecutive days for warm spell (default: 6)
#'
#' @return A tibble with columns: year, WSDI, n_spells, mean_spell_length
#'
#' @examples
#' # Daily data with maximum temperature
#' set.seed(123)
#' daily_data <- data.frame(
#'   date = seq(as.Date("2000-01-01"), as.Date("2002-12-31"), by = "day"),
#'   tmax = 25 + 10 * sin(seq(0, 4*pi, length.out = 1096)) + rnorm(1096, 0, 5)
#' )
#'
#' calculate_WSDI(
#'   df = daily_data,
#'   frequency = "daily",
#'   time_col = "date",
#'   tmax_col = "tmax"
#' )
#'
#' # With custom window and consecutive days
#' calculate_WSDI(
#'   df = daily_data,
#'   frequency = "daily",
#'   time_col = "date",
#'   tmax_col = "tmax",
#'   window_days = 15,
#'   min_consecutive = 5
#' )
#' @export
calculate_WSDI <- function(df, frequency = "daily",
                           time_col = NULL,
                           tmax_col = NULL,
                           temp_col = NULL,
                           window_days = 30,
                           min_consecutive = 6) {

  daily_data <- prepare_daily_data(
    df = df,
    data_type = "temperature",
    frequency = frequency,
    time_col = time_col,
    tmax_col = tmax_col,
    tmin_col = NULL,
    prcp_col = NULL,
    temp_col = temp_col,
    precip_col = NULL
  )

  if ("tmax" %in% names(daily_data)) {
    temp_col_to_use <- "tmax"
  } else {
    temp_col_to_use <- setdiff(names(daily_data), "date")[1]
    cli::cli_warn(c(
      "Using available temperature column for WSDI.",
      i = "WSDI typically uses maximum temperature (TX).",
      i = "Results may not follow ETCCDI standard."
    ))
  }

  # Calculate rolling 90th percentile
  temp_series <- daily_data[[temp_col_to_use]]
  daily_data$temp_90th <- zoo::rollapply(
    temp_series,
    width = window_days,
    FUN = function(x) stats::quantile(x, 0.9, na.rm = TRUE),
    fill = NA,
    align = "right"
  )

  # Identify warm spells
  result <- daily_data |>
    dplyr::filter(!is.na(temp_90th)) |>
    dplyr::mutate(
      is_warm_spell = .data[[temp_col_to_use]] > temp_90th,
      spell_id = data.table::rleid(is_warm_spell)
    ) |>
    dplyr::filter(is_warm_spell) |>
    extract_date_components(date_col = "date") |>
    dplyr::group_by(year, spell_id) |>
    dplyr::summarise(
      spell_length = dplyr::n(),
      start_date = min(date),
      end_date = max(date),
      .groups = 'drop'
    ) |>
    dplyr::filter(spell_length >= min_consecutive) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      WSDI = sum(spell_length),
      n_spells = dplyr::n(),
      mean_spell_length = mean(spell_length),
      .groups = 'drop'
    )

  return(result)
}

#' Calculate Cold Spell Duration Index (CSDI)
#'
#' Calculates the number of days with at least 6 consecutive days
#' where temperature is below the 10th percentile, following ETCCDI
#' definition CSDI.
#'
#' @param df Data frame with climate data
#' @param frequency Temporal frequency: "daily" or "hourly"
#' @param time_col Name of the time column. For daily frequency, the column should be
#'   of class Date or a string in the format YYYY-MM-DD. For hourly frequency, the column
#'   should be of class POSIXct or a string in the format YYYY-MM-DD HH:MM:SS.
#' @param tmin_col Name of minimum temperature column (daily data)
#' @param temp_col Name of temperature column (for single temp or hourly)
#' @param window_days Window size for percentile calculation (default: 30)
#' @param min_consecutive Minimum consecutive days for cold spell (default: 6)
#'
#' @return A tibble with columns: year, CSDI, n_spells, mean_spell_length
#'
#' @examples
#' # Daily data with minimum temperature
#' set.seed(123)
#' daily_data <- data.frame(
#'   date = seq(as.Date("2000-01-01"), as.Date("2002-12-31"), by = "day"),
#'   tmin = 15 + 8 * sin(seq(0, 4*pi, length.out = 1096)) + rnorm(1096, 0, 3)
#' )
#'
#' calculate_CSDI(
#'   df = daily_data,
#'   frequency = "daily",
#'   time_col = "date",
#'   tmin_col = "tmin"
#' )
#'
#' # With custom window and consecutive days
#' calculate_CSDI(
#'   df = daily_data,
#'   frequency = "daily",
#'   time_col = "date",
#'   tmin_col = "tmin",
#'   window_days = 15,
#'   min_consecutive = 5
#' )
#' @export
calculate_CSDI <- function(df, frequency = "daily",
                           time_col = NULL,
                           tmin_col = NULL,
                           temp_col = NULL,
                           window_days = 30,
                           min_consecutive = 6) {

  daily_data <- prepare_daily_data(
    df = df,
    data_type = "temperature",
    frequency = frequency,
    time_col = time_col,
    tmax_col = NULL,
    tmin_col = tmin_col,
    prcp_col = NULL,
    temp_col = temp_col,
    precip_col = NULL
  )

  if ("tmin" %in% names(daily_data)) {
    temp_col_to_use <- "tmin"
  } else {
    temp_col_to_use <- setdiff(names(daily_data), "date")[1]
    cli::cli_warn(c(
      "Using available temperature column for CSDI.",
      i = "CSDI typically uses minimum temperature (TN).",
      i = "Results may not follow ETCCDI standard."
    ))
  }

  # Calculate rolling 10th percentile
  temp_series <- daily_data[[temp_col_to_use]]
  daily_data$temp_10th <- zoo::rollapply(
    temp_series,
    width = window_days,
    FUN = function(x) stats::quantile(x, 0.1, na.rm = TRUE),
    fill = NA,
    align = "right"
  )

  # Identify cold spells
  result <- daily_data |>
    dplyr::filter(!is.na(temp_10th)) |>
    dplyr::mutate(
      is_cold_spell = .data[[temp_col_to_use]] < temp_10th,
      spell_id = data.table::rleid(is_cold_spell)
    ) |>
    dplyr::filter(is_cold_spell) |>
    extract_date_components(date_col = "date") |>
    dplyr::group_by(year, spell_id) |>
    dplyr::summarise(
      spell_length = dplyr::n(),
      start_date = min(date),
      end_date = max(date),
      .groups = 'drop'
    ) |>
    dplyr::filter(spell_length >= min_consecutive) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      CSDI = sum(spell_length),
      n_spells = dplyr::n(),
      mean_spell_length = mean(spell_length),
      .groups = 'drop'
    )

  return(result)
}
