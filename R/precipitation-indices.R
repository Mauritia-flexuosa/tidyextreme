#' Calculate maximum 1-day precipitation (Rx1day)
#'
#' Calculates the annual maximum 1-day precipitation amount,
#' following ETCCDI definition Rx1day.
#'
#' @param df Data frame with precipitation data
#' @param frequency Temporal frequency: "daily" or "hourly"
#' @param time_col Name of the time column. For daily frequency, the column should be
#'   of class Date or a string in the format YYYY-MM-DD. For hourly frequency, the column
#'   should be of class POSIXct or a string in the format YYYY-MM-DD HH:MM:SS.
#' @param prcp_col Name of precipitation column (daily data)
#' @param precip_col Name of precipitation column (hourly data)
#' @param min_valid_years Minimum years with valid data (default: 1)
#'
#' @return A data.frame with columns: year, Rx1day
#'
#' @examples
#' # Daily precipitation data
#' daily_prcp <- data.frame(
#'   date = seq(as.Date("2000-01-01"), as.Date("2002-12-31"), by = "day"),
#'   rainfall = pmax(0, rgamma(1096, shape = 0.5, scale = 10))
#' )
#'
#' calculate_Rx1day(
#'   df = daily_prcp,
#'   frequency = "daily",
#'   time_col = "date",
#'   prcp_col = "rainfall"
#' )
#'
#' @export
calculate_Rx1day <- function(df, frequency = "daily",
                             time_col = NULL,
                             prcp_col = NULL,
                             precip_col = NULL,
                             min_valid_years = 1) {

  daily_data <- prepare_daily_data(
    df = df,
    data_type = "precipitation",
    frequency = frequency,
    time_col = time_col,
    tmax_col = NULL,
    tmin_col = NULL,
    prcp_col = prcp_col,
    temp_col = NULL,
    precip_col = precip_col
  )

  if (!"prcp" %in% names(daily_data)) {
    cli::cli_abort(c(
      "Precipitation data not available.",
      x = "Column `prcp` not found in daily data.",
      i = "Provide precipitation data for Rx1day calculation."
    ))
  }

  # Check for negative precipitation
  if (any(daily_data$prcp < 0, na.rm = TRUE)) {
    neg_count <- sum(daily_data$prcp < 0, na.rm = TRUE)
    cli::cli_abort(c(
      "Negative precipitation values detected.",
      x = "{neg_count} negative value(s) found in precipitation data.",
      i = "Precipitation cannot be negative. Correct the data."
    ))
  }

  # Calculate Rx1day
  result <- daily_data |>
    extract_date_components(date_col = "date") |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      Rx1day = max(prcp, na.rm = TRUE),
      n_days = sum(!is.na(prcp)),
      .groups = 'drop'
    ) |>
    dplyr::mutate(Rx1day = ifelse(is.infinite(Rx1day), NA_real_, Rx1day))

  # Validate data completeness
  valid_years <- result |>
    dplyr::filter(n_days >= 300)

  if (nrow(valid_years) < min_valid_years) {
    cli::cli_warn(c(
      "Few years have complete precipitation data.",
      x = "Only {nrow(valid_years)} year(s) with 300+ days of data.",
      i = "Results may not be representative of true extremes."
    ))
  }

  return(result  |> dplyr::select(-n_days))
}

#' Calculate maximum consecutive 5-day precipitation (Rx5day)
#'
#' Calculates the annual maximum precipitation amount accumulated
#' over 5 consecutive days, following ETCCDI definition Rx5day.
#'
#' @param df Data frame with precipitation data
#' @param frequency Temporal frequency: "daily" or "hourly"
#' @param time_col Name of the time column. Must be in a format recognizable by lubridate
#'   (e.g., Date for daily data, POSIXct for hourly data). Recommended formats:
#'   - Daily: YYYY-MM-DD (e.g., "2023-01-15")
#'   - Hourly: YYYY-MM-DD HH:MM:SS (e.g., "2023-01-15 14:30:00")
#' @param prcp_col Name of precipitation column (daily data)
#' @param precip_col Name of precipitation column (hourly data)
#'
#' @return A data.frame with columns: year, Rx5day
#'
#' @examples
#' # Daily precipitation data
#' daily_prcp <- data.frame(
#'   date = seq(as.Date("2000-01-01"), as.Date("2002-12-31"), by = "day"),
#'   rainfall = pmax(0, rgamma(1096, shape = 0.5, scale = 10))
#' )
#'
#' # Calculate maximum 5-day precipitation
#' calculate_Rx5day(
#'   df = daily_prcp,
#'   frequency = "daily",
#'   time_col = "date",
#'   prcp_col = "rainfall"
#' )
#'
#' # Hourly precipitation data (converted to daily)
#' hourly_prcp <- data.frame(
#'   datetime = seq(
#'     as.POSIXct("2000-01-01 00:00", tz = "UTC"),
#'     as.POSIXct("2000-01-31 23:00", tz = "UTC"),
#'     by = "hour"
#'   ),
#'   precip = pmax(0, rgamma(31*24, shape = 0.3, scale = 2))
#' )
#'
#' calculate_Rx5day(
#'   df = hourly_prcp,
#'   frequency = "hourly",
#'   time_col = "datetime",
#'   precip_col = "precip"
#' )
#' @export
calculate_Rx5day <- function(df, frequency = "daily",
                             time_col = NULL,
                             prcp_col = NULL,
                             precip_col = NULL) {

  daily_data <- prepare_daily_data(
    df = df,
    data_type = "precipitation",
    frequency = frequency,
    time_col = time_col,
    tmax_col = NULL,
    tmin_col = NULL,
    prcp_col = prcp_col,
    temp_col = NULL,
    precip_col = precip_col
  )

  if (!"prcp" %in% names(daily_data)) {
    cli::cli_abort(c(
      "Precipitation data not available.",
      x = "Column `prcp` not found in daily data.",
      i = "Provide precipitation data for Rx5day calculation."
    ))
  }

  # Check series length
  if (nrow(daily_data) < 5) {
    cli::cli_abort(c(
      "Time series too short for Rx5day calculation.",
      x = "Only {nrow(daily_data)} days of data available.",
      i = "Rx5day requires at least 5 days of data."
    ))
  }

  # Calculate Rx5day
  result <- daily_data |>
    extract_date_components(date_col = "date") |>
    dplyr::arrange(date) |>
    dplyr::group_by(year) |>
    dplyr::mutate(
      roll_5day = zoo::rollsum(prcp, k = 5,
                               fill = NA, align = "right", na.rm = FALSE)
    ) |>
    dplyr::summarise(
      Rx5day = max(roll_5day, na.rm = TRUE),
      n_days = sum(!is.na(prcp)),
      .groups = 'drop'
    ) |>
    dplyr::mutate(Rx5day = ifelse(is.infinite(Rx5day), NA_real_, Rx5day))

  return(result  |> dplyr::select(-n_days))
}

#' Calculate number of heavy precipitation days (R10mm)
#'
#' Counts the number of days per year when precipitation ≥ 10 mm,
#' following ETCCDI definition R10mm.
#'
#' @param df Data frame with precipitation data
#' @param frequency Temporal frequency: "daily" or "hourly"
#' @param time_col Name of the time column. Must be in a format recognizable by lubridate
#'   (e.g., Date for daily data, POSIXct for hourly data). Recommended formats:
#'   - Daily: YYYY-MM-DD (e.g., "2023-01-15")
#'   - Hourly: YYYY-MM-DD HH:MM:SS (e.g., "2023-01-15 14:30:00")
#' @param prcp_col Name of precipitation column (daily data)
#' @param precip_col Name of precipitation column (hourly data)
#' @param threshold Precipitation threshold in mm (default: 10)
#'
#' @return A data.frame with columns: year, R10mm
#'
#' @examples
#' # Daily precipitation data
#' daily_prcp <- data.frame(
#'   date = seq(as.Date("2000-01-01"), as.Date("2002-12-31"), by = "day"),
#'   rainfall = pmax(0, rgamma(1096, shape = 0.5, scale = 10))
#' )
#'
#' # Calculate number of days with precipitation ≥ 10mm
#' calculate_R10mm(
#'   df = daily_prcp,
#'   frequency = "daily",
#'   time_col = "date",
#'   prcp_col = "rainfall"
#' )
#'
#' # With custom threshold (15mm instead of 10mm)
#' calculate_R10mm(
#'   df = daily_prcp,
#'   frequency = "daily",
#'   time_col = "date",
#'   prcp_col = "rainfall",
#'   threshold = 15
#' )
#' @export
calculate_R10mm <- function(df, frequency = "daily",
                            time_col = NULL,
                            prcp_col = NULL,
                            precip_col = NULL,
                            threshold = 10) {

  daily_data <- prepare_daily_data(
    df = df,
    data_type = "precipitation",
    frequency = frequency,
    time_col = time_col,
    tmax_col = NULL,
    tmin_col = NULL,
    prcp_col = prcp_col,
    temp_col = NULL,
    precip_col = precip_col
  )

  if (!"prcp" %in% names(daily_data)) {
    cli::cli_abort(c(
      "Precipitation data not available.",
      x = "Column `prcp` not found in daily data.",
      i = "Provide precipitation data for R10mm calculation."
    ))
  }

  result <- daily_data |>
    extract_date_components(date_col = "date") |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      R10mm = sum(prcp >= threshold, na.rm = TRUE),
      n_days = sum(!is.na(prcp)),
      .groups = 'drop'
    )

  return(result  |> dplyr::select(-n_days))
}

#' Calculate number of very heavy precipitation days (R20mm)
#'
#' Counts the number of days per year when precipitation ≥ 20 mm,
#' following ETCCDI definition R20mm.
#'
#' @param df Data frame with precipitation data
#' @param frequency Temporal frequency: "daily" or "hourly"
#' @param time_col Name of the time column. Must be in a format recognizable by lubridate
#'   (e.g., Date for daily data, POSIXct for hourly data). Recommended formats:
#'   - Daily: YYYY-MM-DD (e.g., "2023-01-15")
#'   - Hourly: YYYY-MM-DD HH:MM:SS (e.g., "2023-01-15 14:30:00")
#' @param prcp_col Name of precipitation column (daily data)
#' @param precip_col Name of precipitation column (hourly data)
#' @param threshold Precipitation threshold in mm (default: 20)
#'
#' @return A data.frame with columns: year, R20mm
#'
#' @examples
#' # Daily precipitation data
#' daily_prcp <- data.frame(
#'   date = seq(as.Date("2000-01-01"), as.Date("2002-12-31"), by = "day"),
#'   rainfall = pmax(0, rgamma(1096, shape = 0.5, scale = 10))
#' )
#'
#' # Calculate number of days with precipitation ≥ 20mm
#' calculate_R20mm(
#'   df = daily_prcp,
#'   frequency = "daily",
#'   time_col = "date",
#'   prcp_col = "rainfall"
#' )
#'
#' # With custom threshold (25mm instead of 20mm)
#' calculate_R20mm(
#'   df = daily_prcp,
#'   frequency = "daily",
#'   time_col = "date",
#'   prcp_col = "rainfall",
#'   threshold = 25
#' )
#'
#' @export
calculate_R20mm <- function(df, frequency = "daily",
                            time_col = NULL,
                            prcp_col = NULL,
                            precip_col = NULL,
                            threshold = 20) {

  daily_data <- prepare_daily_data(
    df = df,
    data_type = "precipitation",
    frequency = frequency,
    time_col = time_col,
    tmax_col = NULL,
    tmin_col = NULL,
    prcp_col = prcp_col,
    temp_col = NULL,
    precip_col = precip_col
  )

  if (!"prcp" %in% names(daily_data)) {
    cli::cli_abort(c(
      "Precipitation data not available.",
      x = "Column `prcp` not found in daily data.",
      i = "Provide precipitation data for R20mm calculation."
    ))
  }

  result <- daily_data |>
    extract_date_components(date_col = "date") |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      R20mm = sum(prcp >= threshold, na.rm = TRUE),
      n_days = sum(!is.na(prcp)),
      .groups = 'drop'
    )

  return(result  |> dplyr::select(-n_days))
}

#' Calculate number of days with precipitation ≥ 1mm (R1mm)
#'
#' Counts the number of days per year when precipitation ≥ 1 mm,
#' representing wet days.
#'
#' @param df Data frame with precipitation data
#' @param frequency Temporal frequency: "daily" or "hourly"
#' @param time_col Name of the time column. Must be in a format recognizable by lubridate
#'   (e.g., Date for daily data, POSIXct for hourly data). Recommended formats:
#'   - Daily: YYYY-MM-DD (e.g., "2023-01-15")
#'   - Hourly: YYYY-MM-DD HH:MM:SS (e.g., "2023-01-15 14:30:00")
#' @param prcp_col Name of precipitation column (daily data)
#' @param precip_col Name of precipitation column (hourly data)
#' @param threshold Precipitation threshold in mm (default: 1)
#'
#' @return A data.frame with columns: year, R1mm
#'
#' @examples
#' # Daily precipitation data
#' daily_prcp <- data.frame(
#'   date = seq(as.Date("2000-01-01"), as.Date("2002-12-31"), by = "day"),
#'   rainfall = pmax(0, rgamma(1096, shape = 0.5, scale = 10))
#' )
#'
#' # Calculate number of days with precipitation ≥ 1mm (wet days)
#' calculate_R1mm(
#'   df = daily_prcp,
#'   frequency = "daily",
#'   time_col = "date",
#'   prcp_col = "rainfall"
#' )
#'
#' # With custom threshold (0.5mm instead of 1mm)
#' calculate_R1mm(
#'   df = daily_prcp,
#'   frequency = "daily",
#'   time_col = "date",
#'   prcp_col = "rainfall",
#'   threshold = 0.5
#' )
#' @export
calculate_R1mm <- function(df, frequency = "daily",
                           time_col = NULL,
                           prcp_col = NULL,
                           precip_col = NULL,
                           threshold = 1) {

  calculate_R10mm(
    df = df,
    frequency = frequency,
    time_col = time_col,
    prcp_col = prcp_col,
    precip_col = precip_col,
    threshold = threshold
  ) |>
    dplyr::rename(R1mm = R10mm)
}

#' Calculate consecutive dry days (CDD)
#'
#' Calculates statistics for dry spells (consecutive days with
#' precipitation < 1 mm), following ETCCDI definition CDD.
#'
#' @param df Data frame with precipitation data
#' @param frequency Temporal frequency: "daily" or "hourly"
#' @param time_col Name of the time column. Must be in a format recognizable by lubridate
#'   (e.g., Date for daily data, POSIXct for hourly data). Recommended formats:
#'   - Daily: YYYY-MM-DD (e.g., "2023-01-15")
#'   - Hourly: YYYY-MM-DD HH:MM:SS (e.g., "2023-01-15 14:30:00")
#' @param prcp_col Name of precipitation column (daily data)
#' @param precip_col Name of precipitation column (hourly data)
#' @param dry_threshold Threshold for dry day in mm (default: 1)
#'
#' @return A data.frame with columns: year, CDD_max, CDD_mean, CDD_median, n_dry_spells
#'
#' @examples
#' # Daily precipitation data
#' daily_prcp <- data.frame(
#'   date = seq(as.Date("2000-01-01"), as.Date("2002-12-31"), by = "day"),
#'   rainfall = pmax(0, rgamma(1096, shape = 0.5, scale = 10))
#' )
#'
#' # Calculate consecutive dry days statistics
#' calculate_CDD(
#'   df = daily_prcp,
#'   frequency = "daily",
#'   time_col = "date",
#'   prcp_col = "rainfall"
#' )
#'
#' # With custom dry threshold (0.5mm instead of 1mm)
#' calculate_CDD(
#'   df = daily_prcp,
#'   frequency = "daily",
#'   time_col = "date",
#'   prcp_col = "rainfall",
#'   dry_threshold = 0.5
#' )
#' @export
calculate_CDD <- function(df, frequency = "daily",
                          time_col = NULL,
                          prcp_col = NULL,
                          precip_col = NULL,
                          dry_threshold = 1) {

  daily_data <- prepare_daily_data(
    df = df,
    data_type = "precipitation",
    frequency = frequency,
    time_col = time_col,
    tmax_col = NULL,
    tmin_col = NULL,
    prcp_col = prcp_col,
    temp_col = NULL,
    precip_col = precip_col
  )

  if (!"prcp" %in% names(daily_data)) {
    cli::cli_abort(c(
      "Precipitation data not available.",
      x = "Column `prcp` not found in daily data.",
      i = "Provide precipitation data for CDD calculation."
    ))
  }

  # Identify dry spells
  result <- daily_data |>
    dplyr::arrange(date) |>
    dplyr::mutate(
      is_dry = prcp < dry_threshold,
      spell_change = is_dry != dplyr::lag(is_dry, default = dplyr::first(is_dry))
    ) |>
    dplyr::mutate(spell_id = cumsum(spell_change)) |>
    dplyr::filter(is_dry) |>
    extract_date_components(date_col = "date") |>
    dplyr::group_by(year, spell_id) |>
    dplyr::summarise(
      cdd_length = dplyr::n(),
      start_date = min(date),
      end_date = max(date),
      .groups = 'drop'
    ) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      CDD_max = max(cdd_length, na.rm = TRUE),
      CDD_mean = mean(cdd_length, na.rm = TRUE),
      CDD_median = stats::median(cdd_length, na.rm = TRUE),
      n_dry_spells = dplyr::n(),
      .groups = 'drop'
    ) |>
    dplyr::mutate(CDD_max = ifelse(is.infinite(CDD_max), 0, CDD_max))

  return(result)
}

#' Calculate consecutive wet days (CWD)
#'
#' Calculates statistics for wet spells (consecutive days with
#' precipitation ≥ 1 mm), following ETCCDI definition CWD.
#'
#' @param df Data frame with precipitation data
#' @param frequency Temporal frequency: "daily" or "hourly"
#' @param time_col Name of the time column. Must be in a format recognizable by lubridate
#'   (e.g., Date for daily data, POSIXct for hourly data). Recommended formats:
#'   - Daily: YYYY-MM-DD (e.g., "2023-01-15")
#'   - Hourly: YYYY-MM-DD HH:MM:SS (e.g., "2023-01-15 14:30:00")
#' @param prcp_col Name of precipitation column (daily data)
#' @param precip_col Name of precipitation column (hourly data)
#' @param wet_threshold Threshold for wet day in mm (default: 1)
#'
#' @return A data.frame with columns: year, CWD_max, CWD_mean, CWD_median, n_wet_spells
#'
#' @examples
#' # Daily precipitation data
#' daily_prcp <- data.frame(
#'   date = seq(as.Date("2000-01-01"), as.Date("2002-12-31"), by = "day"),
#'   rainfall = pmax(0, rgamma(1096, shape = 0.5, scale = 10))
#' )
#'
#' # Calculate consecutive wet days statistics
#' calculate_CWD(
#'   df = daily_prcp,
#'   frequency = "daily",
#'   time_col = "date",
#'   prcp_col = "rainfall"
#' )
#'
#' # With custom wet threshold (5mm instead of 1mm)
#' calculate_CWD(
#'   df = daily_prcp,
#'   frequency = "daily",
#'   time_col = "date",
#'   prcp_col = "rainfall",
#'   wet_threshold = 5
#' )
#' @export
calculate_CWD <- function(df, frequency = "daily",
                          time_col = NULL,
                          prcp_col = NULL,
                          precip_col = NULL,
                          wet_threshold = 1) {

  daily_data <- prepare_daily_data(
    df = df,
    data_type = "precipitation",
    frequency = frequency,
    time_col = time_col,
    tmax_col = NULL,
    tmin_col = NULL,
    prcp_col = prcp_col,
    temp_col = NULL,
    precip_col = precip_col
  )

  if (!"prcp" %in% names(daily_data)) {
    cli::cli_abort(c(
      "Precipitation data not available.",
      x = "Column `prcp` not found in daily data.",
      i = "Provide precipitation data for CWD calculation."
    ))
  }

  # Identify wet spells
  result <- daily_data |>
    dplyr::arrange(date) |>
    dplyr::mutate(
      is_wet = prcp >= wet_threshold,
      spell_change = is_wet != dplyr::lag(is_wet, default = dplyr::first(is_wet))
    ) |>
    dplyr::mutate(spell_id = cumsum(spell_change)) |>
    dplyr::filter(is_wet) |>
    extract_date_components(date_col = "date") |>
    dplyr::group_by(year, spell_id) |>
    dplyr::summarise(
      cwd_length = dplyr::n(),
      start_date = min(date),
      end_date = max(date),
      .groups = 'drop'
    ) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      CWD_max = max(cwd_length, na.rm = TRUE),
      CWD_mean = mean(cwd_length, na.rm = TRUE),
      CWD_median = stats::median(cwd_length, na.rm = TRUE),
      n_wet_spells = dplyr::n(),
      .groups = 'drop'
    ) |>
    dplyr::mutate(CWD_max = ifelse(is.infinite(CWD_max), 0, CWD_max))

  return(result)
}

#' Calculate Simple Daily Intensity Index (SDII)
#'
#' Calculates the mean precipitation amount on wet days (≥ 1 mm),
#' following ETCCDI definition SDII.
#'
#' @param df Data frame with precipitation data
#' @param frequency Temporal frequency: "daily" or "hourly"
#' @param time_col Name of the time column. Must be in a format recognizable by lubridate
#'   (e.g., Date for daily data, POSIXct for hourly data). Recommended formats:
#'   - Daily: YYYY-MM-DD (e.g., "2023-01-15")
#'   - Hourly: YYYY-MM-DD HH:MM:SS (e.g., "2023-01-15 14:30:00")
#' @param prcp_col Name of precipitation column (daily data)
#' @param precip_col Name of precipitation column (hourly data)
#' @param wet_threshold Threshold for wet day in mm (default: 1)
#'
#' @return A data.frame with columns: year, SDII, wet_days, total_prcp
#'
#' @examples
#' # Daily precipitation data
#' daily_prcp <- data.frame(
#'   date = seq(as.Date("2000-01-01"), as.Date("2002-12-31"), by = "day"),
#'   rainfall = pmax(0, rgamma(1096, shape = 0.5, scale = 10))
#' )
#'
#' # Calculate Simple Daily Intensity Index
#' calculate_SDII(
#'   df = daily_prcp,
#'   frequency = "daily",
#'   time_col = "date",
#'   prcp_col = "rainfall"
#' )
#'
#' # With custom wet threshold (5mm instead of 1mm)
#' calculate_SDII(
#'   df = daily_prcp,
#'   frequency = "daily",
#'   time_col = "date",
#'   prcp_col = "rainfall",
#'   wet_threshold = 5
#' )
#' @export
calculate_SDII <- function(df, frequency = "daily",
                           time_col = NULL,
                           prcp_col = NULL,
                           precip_col = NULL,
                           wet_threshold = 1) {

  daily_data <- prepare_daily_data(
    df = df,
    data_type = "precipitation",
    frequency = frequency,
    time_col = time_col,
    tmax_col = NULL,
    tmin_col = NULL,
    prcp_col = prcp_col,
    temp_col = NULL,
    precip_col = precip_col
  )

  if (!"prcp" %in% names(daily_data)) {
    cli::cli_abort(c(
      "Precipitation data not available.",
      x = "Column `prcp` not found in daily data.",
      i = "Provide precipitation data for SDII calculation."
    ))
  }

  result <- daily_data |>
    extract_date_components(date_col = "date") |>
    dplyr::mutate(is_wet = prcp >= wet_threshold) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      SDII = sum(prcp[is_wet], na.rm = TRUE) / sum(is_wet, na.rm = TRUE),
      wet_days = sum(is_wet, na.rm = TRUE),
      total_prcp = sum(prcp, na.rm = TRUE),
      .groups = 'drop'
    ) |>
    dplyr::mutate(SDII = ifelse(is.nan(SDII), 0, SDII))

  return(result)
}

#' Calculate annual precipitation totals and statistics
#'
#' Calculates comprehensive annual precipitation statistics including
#' total precipitation, number of wet days, mean daily precipitation,
#' and maximum daily precipitation.
#'
#' @param df Data frame with precipitation data
#' @param frequency Temporal frequency: "daily" or "hourly"
#' @param time_col Name of the time column. Must be in a format recognizable by lubridate
#'   (e.g., Date for daily data, POSIXct for hourly data). Recommended formats:
#'   - Daily: YYYY-MM-DD (e.g., "2023-01-15")
#'   - Hourly: YYYY-MM-DD HH:MM:SS (e.g., "2023-01-15 14:30:00")
#' @param prcp_col Name of precipitation column (daily data)
#' @param precip_col Name of precipitation column (hourly data)
#' @param wet_threshold Threshold for wet day in mm (default: 1)
#'
#' @return A data.frame with columns: year, PRCP_total, PRCP_days, PRCP_mean, PRCP_max
#'
#' @examples
#' # Daily precipitation data
#' daily_prcp <- data.frame(
#'   date = seq(as.Date("2000-01-01"), as.Date("2002-12-31"), by = "day"),
#'   rainfall = pmax(0, rgamma(1096, shape = 0.5, scale = 10))
#' )
#'
#' # Calculate comprehensive precipitation statistics
#' calculate_PRCPstats(
#'   df = daily_prcp,
#'   frequency = "daily",
#'   time_col = "date",
#'   prcp_col = "rainfall"
#' )
#'
#' # With custom wet threshold (2mm instead of 1mm)
#' calculate_PRCPstats(
#'   df = daily_prcp,
#'   frequency = "daily",
#'   time_col = "date",
#'   prcp_col = "rainfall",
#'   wet_threshold = 2
#' )
#'
#' # Hourly precipitation data (converted to daily)
#' hourly_prcp <- data.frame(
#'   datetime = seq(
#'     as.POSIXct("2000-01-01 00:00", tz = "UTC"),
#'     as.POSIXct("2000-01-31 23:00", tz = "UTC"),
#'     by = "hour"
#'   ),
#'   precip = pmax(0, rgamma(31*24, shape = 0.3, scale = 2))
#' )
#'
#' calculate_PRCPstats(
#'   df = hourly_prcp,
#'   frequency = "hourly",
#'   time_col = "datetime",
#'   precip_col = "precip"
#' )
#' @export
calculate_PRCPstats <- function(df, frequency = "daily",
                                time_col = NULL,
                                prcp_col = NULL,
                                precip_col = NULL,
                                wet_threshold = 1) {

  daily_data <- prepare_daily_data(
    df = df,
    data_type = "precipitation",
    frequency = frequency,
    time_col = time_col,
    tmax_col = NULL,
    tmin_col = NULL,
    prcp_col = prcp_col,
    temp_col = NULL,
    precip_col = precip_col
  )

  if (!"prcp" %in% names(daily_data)) {
    cli::cli_abort(c(
      "Precipitation data not available.",
      x = "Column `prcp` not found in daily data.",
      i = "Provide precipitation data for PRCPstats calculation."
    ))
  }

  result <- daily_data |>
    extract_date_components(date_col = "date") |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      PRCP_total = sum(prcp, na.rm = TRUE),
      PRCP_days = sum(prcp >= wet_threshold, na.rm = TRUE),
      PRCP_mean = mean(prcp, na.rm = TRUE),
      PRCP_max = max(prcp, na.rm = TRUE),
      n_days = sum(!is.na(prcp)),
      .groups = 'drop'
    )

  return(result  |> dplyr::select(-n_days))
}
