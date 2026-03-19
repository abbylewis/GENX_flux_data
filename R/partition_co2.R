###############################################
# CO2 flux partitioning: NEE → GPP + Reco
###############################################

# Load packages
library(tidyverse)
library(data.table)
source(here::here("R", "download_gcrew_met.R"))
source(here::here("R", "download_water_level.R"))
Sys.setenv(TZ = "America/New_York")
# Load slopes
df <- read_csv(here::here("processed_data", "L0_for_dashboard.csv")) %>%
  mutate(flux_start = force_tz(flux_start, tzone = "EST"),
         flux_end = force_tz(flux_end, tzone = "EST"),
         TIMESTAMP = force_tz(TIMESTAMP, tzone = "EST")) %>%
  rename(flux_time = TIMESTAMP)

# Update met
download_gcrew_met()
# Update water level
download_water_level()
# Load data
met <- read_csv(here::here("processed_data", "met_2025_dashboard.csv")) %>%
  mutate(
    TIMESTAMP = force_tz(TIMESTAMP, tzone = "EST"),
    Salinity = ifelse(TIMESTAMP > as_datetime("2025-07-22 12:00:00") &
                        TIMESTAMP < as_datetime("2025-07-25 00:00:00"),
                      NA, Salinity
    ),
    Salinity = ifelse(Salinity < 1,
                      NA, Salinity
    )
  ) %>%
  filter(!is.na(TIMESTAMP),
         !duplicated(TIMESTAMP))

# Use met water level
driver <- met %>%
  filter(Depth > 25) %>% # One weird point in met data
  mutate(Depth = Depth - 58) %>%
  rename(Depth_cm = Depth)

write_csv(driver, here::here("processed_data", "met_2025_gapfilled.csv"))

# Format
driver <- driver %>%
  rename(driver_time = TIMESTAMP)
df$DateTime <- as.POSIXct(df$flux_time, tz = "EST")
driver$DateTime <- as.POSIXct(driver$driver_time, tz = "EST")

# Convert to data.table
setDT(df)
setDT(driver)

# Set keys: DateTime is what will be used to join fluxes with met
setkey(df, DateTime)
setkey(driver, DateTime)

# Join and format
merged <- driver[df, roll = "nearest"] %>% # Rolling join: nearest met to each flux
  rename(
    Ta = AirTC_Avg,
    PAR = PAR_Den_C_Avg
  ) %>%
  mutate(
    Depth_above_surf = ifelse(Depth_cm > 0,
      Depth_cm,
      0
    ),
    NEE = CO2_slope_ppm_per_day * # CONVERT TO umolCO2/m2/s
      (150 - Depth_above_surf) / 150 *
      265.8 / (0.08206 * (Ta + 273.15)) / (60 * 60 * 24) / 0.196,
    CH4 = CH4_slope_ppm_per_day * # CONVERT TO umolCH4/m2/s
      (150 - Depth_above_surf) / 150 *
      265.8 / (0.08206 * (Ta + 273.15)) / (60 * 60 * 24) / 0.196,
    N2O = N2O_slope_ppm_per_day * # CONVERT TO umolN2O/m2/s
      (150 - Depth_above_surf) / 150 *
      265.8 / (0.08206 * (Ta + 273.15)) / (60 * 60 * 24) / 0.196
  ) %>%
  ungroup() %>%
  select(all_of(c("MIU_VALVE", "DateTime", "flux_time", "NEE", "CH4", "N2O", "PAR", "Ta", 
                  "CH4_R2", "CO2_R2", "CH4_se")))

# Identify nighttime
par_night_thresh <- 5 # µmol m-2 s-1 threshold to define night
merged[, is_night := PAR < par_night_thresh]

# For each chamber, fit Q10 using nighttime points
# We'll fit the log-linear Q10 via lm on log(NEE) with NEE>0 (since Reco positive release).
# Model: log(Reco) = a + b*(Ta - Tref); where b = ln(Q10)/10. We'll use Tref = 10°C.

# helper function to fit Q10 (log-linear)
fit_q10_lm <- function(dt_night, Tref = 10, min_night = 40) {
  # dt_night: data.table with columns NEE, Ta; NEE must be > 0
  if (nrow(dt_night) < min_night) {
    return(NULL)
  }
  dt_night <- dt_night[NEE > 0 & is.finite(Ta)]
  if (nrow(dt_night) < min_night) {
    return(NULL)
  }
  X <- dt_night[, Ta - Tref]
  Y <- log(dt_night$NEE)
  fit <- try(lm(Y ~ X), silent = TRUE)
  if (inherits(fit, "try-error")) {
    return(NULL)
  }
  coef <- coefficients(fit)
  a <- coef[1]
  b <- coef[2]
  Rref <- exp(a)
  Q10 <- exp(b * 10)
  return(list(Rref = as.numeric(Rref), Q10 = as.numeric(Q10), n = nrow(dt_night), fit = fit))
}

# Function: moving-window parameter estimation per chamber
estimate_params_moving_window <- function(
  dt_ch, window_days = 30, step_days = 1,
  par_night_thresh = 5, Tref = 10
) {
  # dt_ch: data.table for one chamber
  if (nrow(dt_ch) == 0) {
    return(NULL)
  }
  start_time <- min(dt_ch$DateTime, na.rm = TRUE)
  end_time <- max(dt_ch$DateTime, na.rm = TRUE)
  centers <- seq(from = start_time, to = end_time, by = paste0(step_days, " days"))
  res_list <- vector("list", length(centers))
  for (i in seq_along(centers)) {
    center <- centers[i]
    wstart <- center - as.difftime(window_days / 2, units = "days")
    wend <- center + as.difftime(window_days / 2, units = "days")
    wnd <- dt_ch[DateTime >= wstart & DateTime <= wend]
    # nighttime points (PAR-based)
    wnd_night <- wnd[PAR < par_night_thresh & is.finite(NEE) & NEE > 0 & is.finite(Ta)]
    fit <- fit_q10_lm(wnd_night, Tref = Tref)
    if (!is.null(fit)) {
      res_list[[i]] <- data.table(
        MIU_VALVE = dt_ch$MIU_VALVE[1],
        center = center,
        Rref = fit$Rref,
        Q10 = fit$Q10,
        n_night = fit$n
      )
    } else {
      res_list[[i]] <- data.table(
        MIU_VALVE = dt_ch$MIU_VALVE[1],
        center = center,
        Rref = NA_real_,
        Q10 = NA_real_,
        n_night = nrow(wnd_night)
      )
    }
  }
  res_dt <- rbindlist(res_list)
  # drop centers with NA Rref & Q10? Keep for interpolation (will be NA)
  return(res_dt)
}

chambers <- unique(merged$MIU_VALVE)
params_all <- list()

for (ch in chambers) {
  dt_ch <- merged[MIU_VALVE == ch]
  params_ch <- estimate_params_moving_window(dt_ch)
  params_all[[as.character(ch)]] <- params_ch
}
params_dt <- rbindlist(params_all, use.names = TRUE, fill = TRUE)

# Remove rows where center is NA (if any)
params_dt <- params_dt[!is.na(center)]

# Interpolate Rref & Q10 to every flux timestamp
# For each chamber, use linear interpolation of Rref and Q10 over time.
# For timestamps outside params range, use nearest available (rule = 2 in approx -> constant extrapolate)
merged[, Rref_t := NA_real_]
merged[, Q10_t := NA_real_]

make_grid <- function(g) {
  data.table(
    DateTime = seq(min(g$DateTime),
                    max(g$DateTime),
                    by = "130 min"
    ),
    MIU_VALVE = g$MIU_VALVE[1]
  )
}

grid <- merged[, make_grid(.SD), by = MIU_VALVE]

setkey(merged, MIU_VALVE, DateTime)
setkey(grid, MIU_VALVE, DateTime)
merged_grid <- merged[grid, roll = "nearest"] #grab nearest observation
#has to be within 65 min
merged_grid[, time_diff := abs(DateTime - flux_time)]
cols <- setdiff(names(merged_grid), c("DateTime", "MIU_VALVE"))
merged_grid[time_diff > 3900, (cols) := NA]
merged_grid[, c("Ta", "PAR") := NULL]

setkey(merged_grid, DateTime)
setDT(driver)
setkey(driver, DateTime)

merged_grid <- driver[
  merged_grid,
  roll = "nearest"
]

setnames(
  merged_grid,
  old = c("AirTC_Avg", "PAR_Den_C_Avg"),
  new = c("Ta", "PAR")
)

for (ch in chambers) {
  pch <- params_dt[MIU_VALVE == ch & !is.na(Rref) & !is.na(Q10)]
  if (nrow(pch) == 0) next
  # ensure unique centers
  pch <- unique(pch, by = "center")
  x <- as.numeric(pch$center) # seconds since epoch
  yR <- pch$Rref
  yQ <- pch$Q10
  targ_idx <- which(merged_grid$MIU_VALVE == ch)
  xt <- as.numeric(merged_grid$DateTime[targ_idx])
  # approx with rule=2: use nearest outside range
  Rinterp <- approx(x = x, y = yR, xout = xt, rule = 2, ties = "ordered")$y
  Qinterp <- approx(x = x, y = yQ, xout = xt, rule = 2, ties = "ordered")$y
  merged_grid[targ_idx, Rref_t := Rinterp]
  merged_grid[targ_idx, Q10_t := Qinterp]
}

# Predict Reco using time-varying parameters
# Reco = Rref_t * Q10_t ^ ((Ta - Tref)/10)
merged_grid[, Reco := NA_real_]
Tref <- 10
merged_grid[!is.na(Rref_t) & !is.na(Q10_t) & !is.na(Ta),
            Reco := Rref_t * (Q10_t^((Ta - Tref) / 10))]

# Compute daytime GPP = Reco - NEE
merged_grid[, is_day := PAR >= par_night_thresh]
merged_grid[, GPP := NA_real_]
day_mask <- merged_grid$is_day & is.finite(merged_grid$Reco) & is.finite(merged_grid$NEE)
merged_grid[day_mask, GPP := Reco - NEE]
# enforce non-negative GPP if desired
merged_grid[day_mask & GPP < 0, GPP := 0]
merged_grid[is.na(NEE), GPP := NA]
# merged_grid[is.na(NEE), Reco := NA]

write_csv(merged_grid, here::here("processed_data", "partitioned_co2.csv"))

