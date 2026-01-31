## ----instalation--------------------------------------------------------------
# Install from GitHub
#devtools::install_github("mauritia-flexuosa/tidyextreme")

# Or from CRAN (when available)
#install.packages("tidyextreme") # Not yet on CRAN

## ----setup, message=FALSE, warning=FALSE, include=T, echo=T-------------------
set.seed(123)
library(tibble)
library(lubridate)

# Create 10 years of daily data (2000-2009)
n_years <- 10
n_days <- n_years * 365

climate_data <- tibble::tibble(
  date = seq(as.Date("2000-01-01"), by = "day", length.out = n_days),
  
  # Precipitation: seasonal pattern with extreme events
  prcp = pmax(0, rgamma(n_days, shape = 1.2, rate = 0.4) + 
               sin(yday(date) * 2 * pi / 365) * 5),
  
  # Maximum temperature: seasonal with warming trend
  tmax = 20 + 10 * sin(yday(date) * 2 * pi / 365 - pi/2) + 
         rnorm(n_days, 0, 3) + 
         (year(date) - 2000) * 0.1,
  
  # Minimum temperature: seasonal
  tmin = 10 + 8 * sin(yday(date) * 2 * pi / 365 - pi/2) + 
         rnorm(n_days, 0, 2)
)

# Add extreme events
climate_data$prcp[100] <- 150
climate_data$tmax[500:505] <- 42
climate_data$tmin[800:803] <- -8

head(climate_data)

## ----rx1day, message=FALSE, warning=FALSE, include=T, echo=T------------------
library(tidyextreme)

rx1day_result <- calculate_Rx1day(
  df = climate_data,
  frequency = "daily",
  time_col = "date",
  prcp_col = "prcp"
)

head(rx1day_result)

## ----rx5day, message=FALSE, warning=FALSE, include=T, echo=T------------------
rx5day_result <- calculate_Rx5day(
  df = climate_data,
  frequency = "daily",
  time_col = "date",
  prcp_col = "prcp"
)

head(rx5day_result)

## ----r10mm, message=FALSE, warning=FALSE, include=T, echo=T-------------------
r10mm_result <- calculate_R10mm(
  df = climate_data,
  frequency = "daily",
  time_col = "date",
  prcp_col = "prcp"
)

head(r10mm_result)

## ----r20mm, message=FALSE, warning=FALSE, include=T, echo=T-------------------
r20mm_result <- calculate_R20mm(
  df = climate_data,
  frequency = "daily",
  time_col = "date",
  prcp_col = "prcp"
)

head(r20mm_result)

## ----r1mm, message=FALSE, warning=FALSE, include=T, echo=T--------------------
r1mm_result <- calculate_R1mm(
  df = climate_data,
  frequency = "daily",
  time_col = "date",
  prcp_col = "prcp"
)

head(r1mm_result)

## ----cdd, message=FALSE, warning=FALSE, include=T, echo=T---------------------
cdd_result <- calculate_CDD(
  df = climate_data,
  frequency = "daily",
  time_col = "date",
  prcp_col = "prcp"
)

head(cdd_result)

## ----cwd, message=FALSE, warning=FALSE, include=T, echo=T---------------------
cwd_result <- calculate_CWD(
  df = climate_data,
  frequency = "daily",
  time_col = "date",
  prcp_col = "prcp"
)

head(cwd_result)

## ----sdii, message=FALSE, warning=FALSE, include=T, echo=T--------------------
sdii_result <- calculate_SDII(
  df = climate_data,
  frequency = "daily",
  time_col = "date",
  prcp_col = "prcp"
)

head(sdii_result)

## ----annual_precip_stats, message=FALSE, warning=FALSE, include=T, echo=T-----
prcpstats_result <- calculate_PRCPstats(
  df = climate_data,
  frequency = "daily",
  time_col = "date",
  prcp_col = "prcp"
)

head(prcpstats_result)

## ----su25, message=FALSE, warning=FALSE, include=T, echo=T--------------------
tx25_result <- calculate_TX25(
  df = climate_data,
  frequency = "daily",
  time_col = "date",
  tmax_col = "tmax"
)

head(tx25_result)

## ----tr20, message=FALSE, warning=FALSE, include=T, echo=T--------------------
tr20_result <- calculate_TR20(
  df = climate_data,
  frequency = "daily",
  time_col = "date",
  tmin_col = "tmin"
)

head(tr20_result)

## ----txx, message=FALSE, warning=FALSE, include=T, echo=T---------------------
txx_result <- calculate_TXx(
  df = climate_data,
  frequency = "daily",
  time_col = "date",
  tmax_col = "tmax"
)

head(txx_result)

## ----tnn, message=FALSE, warning=FALSE, include=T, echo=T---------------------
tnn_result <- calculate_TNn(
  df = climate_data,
  frequency = "daily",
  time_col = "date",
  tmin_col = "tmin"
)

head(tnn_result)

## ----tx30, message=FALSE, warning=FALSE, include=T, echo=T--------------------
tx30_result <- calculate_TX30(
  df = climate_data,
  frequency = "daily",
  time_col = "date",
  tmax_col = "tmax"
)

head(tx30_result)

## ----tx35, message=FALSE, warning=FALSE, include=T, echo=T--------------------
tx35_result <- calculate_TX35(
  df = climate_data,
  frequency = "daily",
  time_col = "date",
  tmax_col = "tmax"
)

head(tx35_result)

## ----tn0, message=FALSE, warning=FALSE, include=T, echo=T---------------------
tn0_result <- calculate_TN0(
  df = climate_data,
  frequency = "daily",
  time_col = "date",
  tmin_col = "tmin"
)

head(tn0_result)

## ----dtr, message=FALSE, warning=FALSE, include=T, echo=T---------------------
dtr_result <- calculate_DTR(
  df = climate_data,
  frequency = "daily",
  time_col = "date",
  tmax_col = "tmax",
  tmin_col = "tmin"
)

head(dtr_result)

## ----tx90p, message=FALSE, warning=FALSE, include=T, echo=T-------------------
tx90p_result <- calculate_TX90p(
  df = climate_data,
  frequency = "daily",
  time_col = "date",
  tmax_col = "tmax"
)

head(tx90p_result)

## ----tn10p, message=FALSE, warning=FALSE, include=T, echo=T-------------------
tn10p_result <- calculate_TN10p(
  df = climate_data,
  frequency = "daily",
  time_col = "date",
  tmin_col = "tmin"
)

head(tn10p_result)

## ----wsdi, message=FALSE, warning=FALSE, include=T, echo=T--------------------
wsdi_result <- calculate_WSDI(
  df = climate_data,
  frequency = "daily",
  time_col = "date",
  tmax_col = "tmax",
  window_days = 30,
  min_consecutive = 6
)

head(wsdi_result)

## ----csdi, message=FALSE, warning=FALSE, include=T, echo=T--------------------
csdi_result <- calculate_CSDI(
  df = climate_data,
  frequency = "daily",
  time_col = "date",
  tmin_col = "tmin",
  window_days = 30,
  min_consecutive = 6
)

head(csdi_result)

## ----visualization, message=FALSE, warning=FALSE, include=T, echo=T, fig.width=8, fig.height=6----
library(ggplot2)
library(tidyr)
library(dplyr)
# Prepare data for visualization
precip_data <- rx1day_result |>
  left_join(r10mm_result, by = "year") |>
  left_join(sdii_result |> select(year, SDII), by = "year")

# Convert to long format for plotting
precip_long <- precip_data |>
  pivot_longer(cols = -year, names_to = "index", values_to = "value")

# Plot precipitation indices
ggplot(precip_long, aes(x = year, y = value, color = index)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~index, scales = "free_y", ncol = 1) +
  labs(title = "Precipitation Indices Over Time",
       x = "Year",
       y = "Value") +
  theme_minimal() +
  theme(legend.position = "none")

## ----functions, message=FALSE, warning=FALSE, include=T, echo=F---------------
# Create a summary table of indices
indices_table <- tribble(
  ~Index_Code, ~Function, ~Description,
  "Rx1day", "calculate_Rx1day", "Annual maximum 1-day precipitation",
  "Rx5day", "calculate_Rx5day", "Annual maximum consecutive 5-day precipitation",
  "R10mm", "calculate_R10mm", "Number of days with precipitation ≥ 10 mm",
  "R20mm", "calculate_R20mm", "Number of days with precipitation ≥ 20 mm",
  "R1mm", "calculate_R1mm", "Number of days with precipitation ≥ 1 mm (wet days)",
  "CDD", "calculate_CDD", "Consecutive dry days statistics",
  "CWD", "calculate_CWD", "Consecutive wet days statistics",
  "SDII", "calculate_SDII", "Simple daily intensity index (mean precipitation on wet days)",
  "PRCPstats", "calculate_PRCPstats", "Comprehensive annual precipitation statistics",
  "TX25", "calculate_TX25", "Number of summer days (maximum temperature > 25°C)",
  "TR20", "calculate_TR20", "Number of tropical nights (minimum temperature > 20°C)",
  "TXx", "calculate_TXx", "Monthly maximum value of daily maximum temperature",
  "TNn", "calculate_TNn", "Monthly minimum value of daily minimum temperature",
  "TX30", "calculate_TX30", "Number of days with maximum temperature ≥ 30°C",
  "TX35", "calculate_TX35", "Number of days with maximum temperature ≥ 35°C",
  "TN0", "calculate_TN0", "Number of days with minimum temperature < 0°C",
  "DTR", "calculate_DTR", "Diurnal temperature range statistics",
  "TX90p", "calculate_TX90p", "90th percentile of daily maximum temperature",
  "TN10p", "calculate_TN10p", "10th percentile of daily minimum temperature",
  "WSDI", "calculate_WSDI", "Warm spell duration index",
  "CSDI", "calculate_CSDI", "Cold spell duration index"
)

# Display the table
library(DT)
datatable(indices_table, 
          options = list(pageLength = 25, 
                         scrollX = TRUE,
                         columnDefs = list(list(className = 'dt-center', targets = "_all"))),
          caption = "Table 1: Climate Extreme Indices Available in tidyextreme")

## ----session_info, message=FALSE, warning=FALSE, include=T, echo=T------------
cat("tidyextreme version:", as.character(packageVersion("tidyextreme")))

