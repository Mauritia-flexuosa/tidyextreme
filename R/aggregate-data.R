#' @title Aggregate hourly data to daily temperature statistics
#'
#' @param df_hourly Data frame with hourly temperature data
#' @param time_col Name of the datetime column (must be POSIXct)
#' @param temp_col Name of the hourly temperature column
#' @param tz Timezone (default: "UTC")
#'
#' @return A tibble with columns: date, tmax, tmin
#' @export
aggregate_hourly_temperature <- function(df_hourly,
                                         time_col = "datetime",
                                         temp_col = "temperature",
                                         tz = "UTC") {

  required_cols <- c(time_col, temp_col)
  validate_climate_data(df_hourly, required_cols, time_col,
                        check_numeric = temp_col)

  if (!lubridate::is.POSIXct(df_hourly[[time_col]])) {
    cli::cli_abort(c(
      "Time column must be POSIXct type.",
      x = "Column `{time_col}` is {class(df_hourly[[time_col]])[1]}.",
      i = "Convert with lubridate::as_datetime() before using this function."
    ))
  }

  result <- df_hourly |>
    dplyr::mutate(
      date = lubridate::as_date(.data[[time_col]])
    ) |>
    dplyr::group_by(.data$date) |>
    dplyr::summarise(
      tmax = max(.data[[temp_col]], na.rm = TRUE),
      tmin = min(.data[[temp_col]], na.rm = TRUE),
      n_hours = dplyr::n(),
      .groups = 'drop'
    )

  incomplete_days <- result |>
    dplyr::filter(.data$n_hours < 24)

  if (nrow(incomplete_days) > 0) {
    cli::cli_warn(c(
      "Some days have incomplete hourly data.",
      x = "{nrow(incomplete_days)} day(s) with less than 24 hours of data.",
      i = "Consider imputing missing data for accurate calculations."
    ))
  }

  if (any(is.infinite(c(result$tmax, result$tmin)))) {
    cli::cli_abort(c(
      "Infinite values generated during aggregation.",
      x = "This occurs when all values in a day are NA.",
      i = "Check for missing data in column `{temp_col}`."
    ))
  }

  return(result |> dplyr::select(-"n_hours"))
}

#' @title Aggregate hourly data to daily precipitation statistics
#'
#' @param df_hourly Data frame with hourly precipitation data
#' @param time_col Name of the datetime column (must be POSIXct)
#' @param precip_col Name of the hourly precipitation column
#' @param tz Timezone (default: "UTC")
#'
#' @return A tibble with columns: date, prcp
#' @export
aggregate_hourly_precipitation <- function(df_hourly,
                                           time_col = "datetime",
                                           precip_col = "precipitation",
                                           tz = "UTC") {

  required_cols <- c(time_col, precip_col)
  validate_climate_data(df_hourly, required_cols, time_col,
                        check_numeric = precip_col,
                        check_positive = precip_col)

  if (!lubridate::is.POSIXct(df_hourly[[time_col]])) {
    cli::cli_abort(c(
      "Time column must be POSIXct type.",
      x = "Column `{time_col}` is {class(df_hourly[[time_col]])[1]}.",
      i = "Convert with lubridate::as_datetime() before using this function."
    ))
  }

  result <- df_hourly |>
    dplyr::mutate(
      date = lubridate::as_date(.data[[time_col]])
    ) |>
    dplyr::group_by(.data$date) |>
    dplyr::summarise(
      prcp = sum(.data[[precip_col]], na.rm = TRUE),
      n_hours = dplyr::n(),
      .groups = 'drop'
    )

  return(result |> dplyr::select(-"n_hours"))
}

#' @title Flexible data preparation for ETCCDI indices
#' @noRd
prepare_daily_data <- function(df,
                               data_type = c("temperature", "precipitation"),
                               frequency = "daily",
                               time_col = NULL,
                               tmax_col = NULL,
                               tmin_col = NULL,
                               prcp_col = NULL,
                               temp_col = NULL,
                               precip_col = NULL) {

  data_type <- rlang::arg_match(data_type)

  if (frequency == "hourly") {
    if (!is.null(time_col) && lubridate::is.POSIXct(df[[time_col]])) {
      if (data_type == "temperature") {
        if (is.null(temp_col)) {
          cli::cli_abort(c(
            "Temperature column not specified for hourly data.",
            x = "Parameter `temp_col` must be provided.",
            i = "Specify the column name containing hourly temperature."
          ))
        }
        daily_df <- aggregate_hourly_temperature(
          df_hourly = df,
          time_col = time_col,
          temp_col = temp_col
        )

      } else if (data_type == "precipitation") {
        if (is.null(precip_col)) {
          cli::cli_abort(c(
            "Precipitation column not specified for hourly data.",
            x = "Parameter `precip_col` must be provided.",
            i = "Specify the column name containing hourly precipitation."
          ))
        }
        daily_df <- aggregate_hourly_precipitation(
          df_hourly = df,
          time_col = time_col,
          precip_col = precip_col
        )
      }
    } else {
      cli::cli_abort(c(
        "Invalid time column for hourly data.",
        x = "Column `{time_col}` is not POSIXct.",
        i = "Ensure hourly data has a proper datetime column."
      ))
    }

  } else if (frequency == "daily") {
    if (is.null(time_col)) {
      cli::cli_abort(c(
        "Time column not specified.",
        x = "Parameter `time_col` must be provided for daily data.",
        i = "Specify the column name containing dates."
      ))
    }

    daily_df <- df |>
      dplyr::select(date = tidyselect::all_of(time_col))

    if (data_type == "temperature") {
      if (!is.null(tmax_col)) {
        daily_df <- daily_df |> dplyr::mutate(tmax = df[[tmax_col]])
      }
      if (!is.null(tmin_col)) {
        daily_df <- daily_df |> dplyr::mutate(tmin = df[[tmin_col]])
      }

      if (is.null(tmax_col) && is.null(tmin_col)) {
        if (!is.null(temp_col)) {
          daily_df <- daily_df |>
            dplyr::mutate(
              tmax = df[[temp_col]],
              tmin = df[[temp_col]]
            )
        } else {
          cli::cli_abort(c(
            "No temperature columns specified.",
            x = "Provide either tmax_col/tmin_col or temp_col.",
            i = "Temperature indices require temperature data."
          ))
        }
      }

    } else if (data_type == "precipitation") {
      if (!is.null(prcp_col)) {
        daily_df <- daily_df |> dplyr::mutate(prcp = df[[prcp_col]])
      } else if (!is.null(precip_col)) {
        daily_df <- daily_df |> dplyr::mutate(prcp = df[[precip_col]])
      } else {
        cli::cli_abort(c(
          "No precipitation column specified.",
          x = "Provide either prcp_col or precip_col.",
          i = "Precipitation indices require precipitation data."
        ))
      }
    }

  } else {
    cli::cli_abort(c(
      "Unsupported temporal frequency.",
      x = "Frequency `{frequency}` is not recognized.",
      i = "Use 'hourly' for hourly data or 'daily' for daily data."
    ))
  }

  required_daily_cols <- "date"
  if (data_type == "temperature") {
    if (any(c("tmax", "tmin") %in% names(daily_df))) {
      required_daily_cols <- c(required_daily_cols,
                               intersect(c("tmax", "tmin"), names(daily_df)))
    }
  } else if (data_type == "precipitation") {
    if ("prcp" %in% names(daily_df)) {
      required_daily_cols <- c(required_daily_cols, "prcp")
    }
  }

  validate_climate_data(daily_df, required_daily_cols, "date",
                        check_numeric = setdiff(required_daily_cols, "date"),
                        check_positive = if("prcp" %in% names(daily_df)) "prcp" else NULL)

  return(daily_df)
}
