# Testes para funções individuais de índices climáticos
# Este arquivo testa as funções de precipitation-indices.R e temperature-indices.R

# Configuração inicial
library(testthat)
library(dplyr)
library(lubridate)
library(tidyr)

# Criar função auxiliar para dados de teste
create_test_climate_data <- function(years = 3, seed = 123) {
  set.seed(seed)

  n_days <- years * 365
  test_dates <- seq(as.Date("2000-01-01"), by = "day", length.out = n_days)

  # Dados sintéticos realistas com padrões sazonais
  tibble(
    date = test_dates,
    # Precipitação: distribuição gama com sazonalidade
    prcp = pmax(0, rgamma(n_days, shape = 1.5, rate = 0.5) +
                  sin(yday(test_dates) * 2 * pi / 365) * 3),
    # Temperatura máxima: sazonal com variação
    tmax = 20 + 10 * sin(yday(test_dates) * 2 * pi / 365 - pi/2) + rnorm(n_days, 0, 3),
    # Temperatura mínima: sazonal com variação
    tmin = 10 + 8 * sin(yday(test_dates) * 2 * pi / 365 - pi/2) + rnorm(n_days, 0, 2)
  )
}

# Criar dados de teste com NAs para testar robustez
create_test_data_with_nas <- function(base_data, na_prop = 0.05) {
  data_with_nas <- base_data

  # Adicionar NAs aleatórios
  n_total <- nrow(data_with_nas) * ncol(data_with_nas)
  n_nas <- round(n_total * na_prop)

  for (col in c("prcp", "tmax", "tmin")) {
    if (col %in% names(data_with_nas)) {
      na_indices <- sample(seq_len(nrow(data_with_nas)),
                           round(nrow(data_with_nas) * na_prop))
      data_with_nas[[col]][na_indices] <- NA
    }
  }

  return(data_with_nas)
}

# Testes para precipitation-indices.R
test_that("calculate_Rx1day funciona corretamente", {
  # Dados de teste
  test_data <- create_test_climate_data(years = 3)

  # Teste básico
  suppressWarnings({
    result <- calculate_Rx1day(
      df = test_data,
      frequency = "daily",
      time_col = "date",
      prcp_col = "prcp"
    )
  })

  # Verificar estrutura
  expect_s3_class(result, "data.frame")
  expect_named(result, c("year", "Rx1day"))
  expect_equal(nrow(result), 3)  # 3 anos

  # Verificar valores
  expect_true(all(result$Rx1day >= 0, na.rm = TRUE))

  # Teste com dados horários (simulado)
  hourly_data <- tibble(
    datetime = seq(as.POSIXct("2000-01-01 00:00:00"),
                   as.POSIXct("2000-01-10 23:00:00"),
                   by = "hour"),
    precip = pmax(0, rgamma(length(datetime), shape = 1, rate = 0.5))
  )

  suppressWarnings({
    result_hourly <- calculate_Rx1day(
      df = hourly_data,
      frequency = "hourly",
      time_col = "datetime",
      precip_col = "precip"
    )
  })

  expect_s3_class(result_hourly, "data.frame")
  expect_named(result_hourly, c("year", "Rx1day"))
})

test_that("calculate_Rx5day funciona corretamente", {
  test_data <- create_test_climate_data(years = 3)

  suppressWarnings({
    result <- calculate_Rx5day(
      df = test_data,
      frequency = "daily",
      time_col = "date",
      prcp_col = "prcp"
    )
  })

  expect_s3_class(result, "data.frame")
  expect_named(result, c("year", "Rx5day"))
  expect_equal(nrow(result), 3)

  # Rx5day deve ser >= Rx1day (porque é acumulado em 5 dias)
  suppressWarnings({
    rx1day <- calculate_Rx1day(test_data, time_col = "date", prcp_col = "prcp")
  })
  combined <- left_join(result, rx1day, by = "year")
  expect_true(all(combined$Rx5day >= combined$Rx1day | is.na(combined$Rx5day)))
})

test_that("calculate_R10mm funciona corretamente", {
  test_data <- create_test_climate_data(years = 2)

  # Teste com threshold padrão (10mm)
  suppressWarnings({
    result <- calculate_R10mm(
      df = test_data,
      frequency = "daily",
      time_col = "date",
      prcp_col = "prcp"
    )
  })

  expect_s3_class(result, "data.frame")
  expect_named(result, c("year", "R10mm"))
  expect_true(all(result$R10mm >= 0))

  # Teste com threshold customizado
  suppressWarnings({
    result_custom <- calculate_R10mm(
      df = test_data,
      frequency = "daily",
      time_col = "date",
      prcp_col = "prcp",
      threshold = 5
    )
  })

  # Com threshold menor, deve ter mais dias
  expect_true(all(result_custom$R10mm >= result$R10mm))
})

test_that("calculate_R20mm funciona corretamente", {
  test_data <- create_test_climate_data(years = 2)

  suppressWarnings({
    result <- calculate_R20mm(
      df = test_data,
      frequency = "daily",
      time_col = "date",
      prcp_col = "prcp"
    )
  })

  expect_s3_class(result, "data.frame")
  expect_named(result, c("year", "R20mm"))

  # R20mm deve ser <= R10mm (porque threshold maior)
  suppressWarnings({
    r10mm <- calculate_R10mm(test_data, time_col = "date", prcp_col = "prcp")
  })
  combined <- left_join(result, r10mm, by = "year")
  expect_true(all(combined$R20mm <= combined$R10mm | is.na(combined$R20mm)))
})

test_that("calculate_R1mm funciona corretamente", {
  test_data <- create_test_climate_data(years = 2)

  suppressWarnings({
    result <- calculate_R1mm(
      df = test_data,
      frequency = "daily",
      time_col = "date",
      prcp_col = "prcp"
    )
  })

  expect_s3_class(result, "data.frame")
  expect_named(result, c("year", "R1mm"))
  expect_true(all(result$R1mm >= 0))
})

test_that("calculate_CDD funciona corretamente", {
  test_data <- create_test_climate_data(years = 2)

  suppressWarnings({
    result <- calculate_CDD(
      df = test_data,
      frequency = "daily",
      time_col = "date",
      prcp_col = "prcp"
    )
  })

  expect_s3_class(result, "data.frame")
  expected_cols <- c("year", "CDD_max", "CDD_mean", "CDD_median", "n_dry_spells")
  expect_named(result, expected_cols)

  # Verificar valores razoáveis
  expect_true(all(result$CDD_max >= 0, na.rm = TRUE))
  expect_true(all(result$n_dry_spells >= 0, na.rm = TRUE))
})

test_that("calculate_CWD funciona corretamente", {
  test_data <- create_test_climate_data(years = 2)

  suppressWarnings({
    result <- calculate_CWD(
      df = test_data,
      frequency = "daily",
      time_col = "date",
      prcp_col = "prcp"
    )
  })

  expect_s3_class(result, "data.frame")
  expected_cols <- c("year", "CWD_max", "CWD_mean", "CWD_median", "n_wet_spells")
  expect_named(result, expected_cols)

  # Verificar valores razoáveis
  expect_true(all(result$CWD_max >= 0, na.rm = TRUE))
})

test_that("calculate_SDII funciona corretamente", {
  test_data <- create_test_climate_data(years = 2)

  suppressWarnings({
    result <- calculate_SDII(
      df = test_data,
      frequency = "daily",
      time_col = "date",
      prcp_col = "prcp"
    )
  })

  expect_s3_class(result, "data.frame")
  expected_cols <- c("year", "SDII", "wet_days", "total_prcp")
  expect_named(result, expected_cols)

  # SDII deve ser >= 0
  expect_true(all(result$SDII >= 0, na.rm = TRUE))

  # Calcular precipitação total em dias úmidos
  result <- result |>
    mutate(
      wet_prcp_total = SDII * wet_days
    )

  # A precipitação total deve ser >= precipitação em dias úmidos
  expect_true(all(
    result$total_prcp >= result$wet_prcp_total - 0.001 |
      is.na(result$total_prcp) | is.na(result$wet_prcp_total)
  ))
})

test_that("calculate_PRCPstats funciona corretamente", {
  test_data <- create_test_climate_data(years = 2)

  suppressWarnings({
    result <- calculate_PRCPstats(
      df = test_data,
      frequency = "daily",
      time_col = "date",
      prcp_col = "prcp"
    )
  })

  expect_s3_class(result, "data.frame")
  expected_cols <- c("year", "PRCP_total", "PRCP_days", "PRCP_mean", "PRCP_max")
  expect_named(result, expected_cols)

  # Verificar consistência interna
  expect_true(all(result$PRCP_max <= result$PRCP_total, na.rm = TRUE))
  expect_true(all(result$PRCP_days >= 0, na.rm = TRUE))
})

# Testes para temperature-indices.R
test_that("calculate_TX25 funciona corretamente", {
  test_data <- create_test_climate_data(years = 2)

  suppressWarnings({
    result <- calculate_TX25(
      df = test_data,
      frequency = "daily",
      time_col = "date",
      tmax_col = "tmax"
    )
  })

  expect_s3_class(result, "data.frame")
  expect_named(result, c("year", "TX25"))
  expect_true(all(result$TX25 >= 0))

  # Teste com threshold diferente
  suppressWarnings({
    result_custom <- calculate_TX25(
      df = test_data,
      frequency = "daily",
      time_col = "date",
      tmax_col = "tmax",
      threshold = 30
    )
  })

  expect_named(result_custom, c("year", "TX25"))
})

test_that("calculate_TR20 funciona corretamente", {
  test_data <- create_test_climate_data(years = 2)

  suppressWarnings({
    result <- calculate_TR20(
      df = test_data,
      frequency = "daily",
      time_col = "date",
      tmin_col = "tmin"
    )
  })

  expect_s3_class(result, "data.frame")
  expect_named(result, c("year", "TR20"))
  expect_true(all(result$TR20 >= 0))
})

test_that("calculate_TXx funciona corretamente", {
  test_data <- create_test_climate_data(years = 2)

  suppressWarnings({
    result <- calculate_TXx(
      df = test_data,
      frequency = "daily",
      time_col = "date",
      tmax_col = "tmax"
    )
  })

  expect_s3_class(result, "data.frame")
  expect_named(result, c("year", "month", "TXx"))
  expect_equal(nrow(result), 24)  # 2 anos × 12 meses

  # TXx deve ser >= que qualquer temperatura do mês
  monthly_max <- test_data %>%
    mutate(year = year(date), month = month(date)) %>%
    group_by(year, month) %>%
    summarise(max_temp = max(tmax, na.rm = TRUE), .groups = "drop")

  combined <- left_join(result, monthly_max, by = c("year", "month"))
  expect_true(all(combined$TXx >= combined$max_temp | is.na(combined$TXx)))
})

test_that("calculate_TNn funciona corretamente", {
  test_data <- create_test_climate_data(years = 2)

  suppressWarnings({
    result <- calculate_TNn(
      df = test_data,
      frequency = "daily",
      time_col = "date",
      tmin_col = "tmin"
    )
  })

  expect_s3_class(result, "data.frame")
  expect_named(result, c("year", "month", "TNn"))

  # TNn deve ser <= que qualquer temperatura do mês
  monthly_min <- test_data %>%
    mutate(year = year(date), month = month(date)) %>%
    group_by(year, month) %>%
    summarise(min_temp = min(tmin, na.rm = TRUE), .groups = "drop")

  combined <- left_join(result, monthly_min, by = c("year", "month"))
  expect_true(all(combined$TNn <= combined$min_temp | is.na(combined$TNn)))
})

test_that("calculate_TX30 e calculate_TX35 funcionam corretamente", {
  test_data <- create_test_climate_data(years = 2)

  suppressWarnings({
    result_30 <- calculate_TX30(
      df = test_data,
      frequency = "daily",
      time_col = "date",
      tmax_col = "tmax"
    )

    result_35 <- calculate_TX35(
      df = test_data,
      frequency = "daily",
      time_col = "date",
      tmax_col = "tmax"
    )
  })

  expect_named(result_30, c("year", "TX30"))
  expect_named(result_35, c("year", "TX35"))

  # TX35 deve ser <= TX30 (threshold maior)
  combined <- left_join(result_30, result_35, by = "year")
  expect_true(all(combined$TX35 <= combined$TX30 | is.na(combined$TX35)))
})

test_that("calculate_TN0 funciona corretamente", {
  test_data <- create_test_climate_data(years = 2)

  suppressWarnings({
    result <- calculate_TN0(
      df = test_data,
      frequency = "daily",
      time_col = "date",
      tmin_col = "tmin"
    )
  })

  expect_s3_class(result, "data.frame")
  expect_named(result, c("year", "TN0"))
  expect_true(all(result$TN0 >= 0))
})

test_that("calculate_DTR funciona corretamente", {
  test_data <- create_test_climate_data(years = 2)

  suppressWarnings({
    result <- calculate_DTR(
      df = test_data,
      frequency = "daily",
      time_col = "date",
      tmax_col = "tmax",
      tmin_col = "tmin"
    )
  })

  expect_s3_class(result, "data.frame")
  expected_cols <- c("year", "DTR_mean", "DTR_sd", "n_days")
  expect_named(result, expected_cols)

  # DTR deve ser >= 0
  expect_true(all(result$DTR_mean >= 0, na.rm = TRUE))
})

test_that("calculate_TX90p e calculate_TN10p funcionam corretamente", {
  test_data <- create_test_climate_data(years = 3)  # Mais anos para percentis

  suppressWarnings({
    result_tx90p <- calculate_TX90p(
      df = test_data,
      frequency = "daily",
      time_col = "date",
      tmax_col = "tmax"
    )

    result_tn10p <- calculate_TN10p(
      df = test_data,
      frequency = "daily",
      time_col = "date",
      tmin_col = "tmin"
    )
  })

  expect_named(result_tx90p, c("year", "TX90p"))
  expect_named(result_tn10p, c("year", "TN10p"))

  # Verificar que percentis estão dentro de limites razoáveis
  expect_true(all(result_tx90p$TX90p > -50 & result_tx90p$TX90p < 60, na.rm = TRUE))
  expect_true(all(result_tn10p$TN10p > -50 & result_tn10p$TN10p < 40, na.rm = TRUE))
})

test_that("calculate_WSDI e calculate_CSDI funcionam corretamente", {
  # Criar dados com mais variabilidade para testar spells
  set.seed(123)
  n_days <- 365 * 3
  test_dates <- seq(as.Date("2000-01-01"), by = "day", length.out = n_days)

  test_data <- tibble(
    date = test_dates,
    tmax = 25 + 10 * sin(seq(0, 4*pi, length.out = n_days)) + rnorm(n_days, 0, 5),
    tmin = 15 + 8 * sin(seq(0, 4*pi, length.out = n_days)) + rnorm(n_days, 0, 3)
  )

  # Testar WSDI
  suppressWarnings({
    result_wsdi <- calculate_WSDI(
      df = test_data,
      frequency = "daily",
      time_col = "date",
      tmax_col = "tmax"
    )
  })

  # Testar CSDI
  suppressWarnings({
    result_csdi <- calculate_CSDI(
      df = test_data,
      frequency = "daily",
      time_col = "date",
      tmin_col = "tmin"
    )
  })

  # Verificar estruturas básicas
  expect_named(result_wsdi, c("year", "WSDI", "n_spells", "mean_spell_length"))
  expect_named(result_csdi, c("year", "CSDI", "n_spells", "mean_spell_length"))

  # Valores devem ser não-negativos
  expect_true(all(result_wsdi$WSDI >= 0, na.rm = TRUE))
  expect_true(all(result_csdi$CSDI >= 0, na.rm = TRUE))
})

# Testes de validação e erros
test_that("Funções validam entradas corretamente", {
  test_data <- create_test_climate_data(years = 1)

  # Teste 1: Dados vazios
  expect_error(
    calculate_Rx1day(data.frame()),
    "Time column not specified"
  )

  # Teste 2: Coluna de tempo ausente
  expect_error(
    calculate_Rx1day(test_data %>% select(-date)),
    "Time column not specified"
  )

  # Teste 3: Coluna de precipitação ausente
  expect_error(
    calculate_Rx1day(test_data %>% select(date, tmax, tmin)),
    "Precipitation data not available"
  )

  # Teste 4: Dados com precipitação negativa
  invalid_data <- test_data
  invalid_data$prcp[1] <- -10
  expect_error(
    calculate_Rx1day(invalid_data),
    "Negative precipitation values detected"
  )
})

test_that("Funções lidam com NAs corretamente", {
  test_data <- create_test_climate_data(years = 2)
  data_with_nas <- create_test_data_with_nas(test_data, na_prop = 0.1)

  # Testar que funções não falham com NAs
  expect_silent(
    suppressWarnings(
      calculate_Rx1day(data_with_nas, time_col = "date", prcp_col = "prcp")
    )
  )

  expect_silent(
    suppressWarnings(
      calculate_TX25(data_with_nas, time_col = "date", tmax_col = "tmax")
    )
  )

  # Verificar que resultados ainda são data.frames
  result_rx1day <- suppressWarnings(
    calculate_Rx1day(data_with_nas, time_col = "date", prcp_col = "prcp")
  )
  expect_s3_class(result_rx1day, "data.frame")
})

test_that("Funções com diferentes frequências", {
  # Testar dados horários (simulação simples)
  hourly_data <- tibble(
    datetime = seq(as.POSIXct("2000-01-01 00:00:00"),
                   as.POSIXct("2000-01-31 23:00:00"),
                   by = "hour"),
    temperature = rnorm(length(datetime), mean = 22, sd = 4),
    precipitation = pmax(0, rgamma(length(datetime), shape = 0.5, rate = 0.2))
  )

  # Testar função de temperatura com dados horários
  suppressWarnings({
    result_temp <- calculate_TX25(
      df = hourly_data,
      frequency = "hourly",
      time_col = "datetime",
      temp_col = "temperature"
    )
  })

  expect_s3_class(result_temp, "data.frame")
  expect_named(result_temp, c("year", "TX25"))

  # Testar função de precipitação com dados horários
  suppressWarnings({
    result_precip <- calculate_Rx1day(
      df = hourly_data,
      frequency = "hourly",
      time_col = "datetime",
      precip_col = "precipitation"
    )
  })

  expect_s3_class(result_precip, "data.frame")
  expect_named(result_precip, c("year", "Rx1day"))
})

test_that("Consistência entre funções relacionadas", {
  test_data <- create_test_climate_data(years = 3)

  # SDII deve ser consistente com PRCPstats
  suppressWarnings({
    sdii_result <- calculate_SDII(test_data, time_col = "date", prcp_col = "prcp")
    prcp_result <- calculate_PRCPstats(test_data, time_col = "date", prcp_col = "prcp")
  })

  combined <- left_join(sdii_result, prcp_result, by = "year")

  # Verificar: total_prcp deve ser aproximadamente igual em ambas
  expect_true(all(
    abs(combined$total_prcp - combined$PRCP_total) < 0.01 |
      is.na(combined$total_prcp)
  ))

  # Verificar: wet_days deve ser igual a PRCP_days (com threshold padrão de 1mm)
  expect_true(all(
    combined$wet_days == combined$PRCP_days |
      is.na(combined$wet_days)
  ))
})

test_that("Reprodutibilidade dos cálculos", {
  # Testar que com a mesma seed, os resultados são idênticos
  data1 <- create_test_climate_data(years = 2, seed = 42)
  data2 <- create_test_climate_data(years = 2, seed = 42)

  suppressWarnings({
    result1 <- calculate_Rx1day(data1, time_col = "date", prcp_col = "prcp")
    result2 <- calculate_Rx1day(data2, time_col = "date", prcp_col = "prcp")
  })

  expect_equal(result1$Rx1day, result2$Rx1day)

  suppressWarnings({
    result3 <- calculate_TX25(data1, time_col = "date", tmax_col = "tmax")
    result4 <- calculate_TX25(data2, time_col = "date", tmax_col = "tmax")
  })

  expect_equal(result3$TX25, result4$TX25)
})

# Teste de desempenho para grandes volumes de dados
test_that("Funções são eficientes com grandes volumes", {
  # Criar 20 anos de dados (~7300 dias)
  big_data <- create_test_climate_data(years = 20, seed = 456)

  # Testar que funções não falham
  expect_silent(
    suppressWarnings(
      calculate_Rx1day(big_data, time_col = "date", prcp_col = "prcp")
    )
  )

  expect_silent(
    suppressWarnings(
      calculate_TX25(big_data, time_col = "date", tmax_col = "tmax")
    )
  )

  # Medir tempo de execução (deve ser razoável)
  time_rx1day <- system.time({
    suppressWarnings(
      calculate_Rx1day(big_data, time_col = "date", prcp_col = "prcp")
    )
  })

  # Verificar que leva menos de 5 segundos (ajuste conforme necessário)
  expect_true(time_rx1day["elapsed"] < 5)
})

# Testes de casos especiais
test_that("Casos especiais são tratados corretamente", {
  # Caso 1: Todos os valores são NA
  all_na_data <- tibble(
    date = seq(as.Date("2000-01-01"), by = "day", length.out = 365),
    prcp = NA_real_,
    tmax = NA_real_,
    tmin = NA_real_
  )

  # Deve retornar NAs mas não falhar
  suppressWarnings({
    result <- calculate_Rx1day(all_na_data, time_col = "date", prcp_col = "prcp")
  })
  expect_true(all(is.na(result$Rx1day)))

  # Caso 2: Todos os valores são zero
  all_zero_data <- tibble(
    date = seq(as.Date("2000-01-01"), by = "day", length.out = 365),
    prcp = 0,
    tmax = 0,
    tmin = 0
  )

  suppressWarnings({
    result_zero <- calculate_Rx1day(all_zero_data, time_col = "date", prcp_col = "prcp")
  })
  expect_true(all(result_zero$Rx1day == 0))

  # Caso 3: Apenas um dia de dados
  single_day <- tibble(
    date = as.Date("2000-01-01"),
    prcp = 15.5,
    tmax = 30.2,
    tmin = 20.1
  )

  suppressWarnings({
    result_single <- calculate_Rx1day(single_day, time_col = "date", prcp_col = "prcp")
  })
  expect_equal(nrow(result_single), 1)
  expect_equal(result_single$Rx1day, 15.5)
})

cat("\n✅ Todos os testes foram definidos!\n")
cat("Para executar: testthat::test_file('test_precipitation_temperature_indices.R')\n")
