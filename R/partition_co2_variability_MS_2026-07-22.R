###############################################
# CO2 flux partitioning: NEE → GPP + Reco
###############################################

#### Run for only the analysis period so as to not bias cutoffs ####
START = "2025-03-18"
END = "2025-11-15"

#### Load packages ####
library(tidyverse)
library(data.table)
Sys.setenv(TZ = "EST")

cutoff_start = 240
cutoff_end = 510

#### Load data ####
target <- read_csv(here::here("processed_data", "L0_for_dashboard.csv")) %>%
  mutate(flux_start = force_tz(flux_start, tzone = "EST"),
         flux_end = force_tz(flux_end, tzone = "EST"),
         TIMESTAMP = force_tz(TIMESTAMP, tzone = "EST"),
         CH4_se = log(CH4_se),
         CO2_se = log(CO2_se)) %>%
  #rename(Chamber = MIU_VALVE) %>%
  rename(flux_time = TIMESTAMP) %>%
  filter(!duplicated(flux_time),
         as_date(flux_time) >= START,
         as_date(flux_time) <= END)

raw <- read_csv(here::here("processed_data","raw_comb.csv")) %>%
  filter(gas == "CH₄") %>%
  group_by(group) %>%
  mutate(flux_time = first(TIMESTAMP))

chamber_levels <- c(
  "c_1_amb", "c_2_amb", "c_3_e0.75", "c_4_e1.5", "c_5_e2.25",
  "c_6_e2.25", "c_7_e3.0", "c_8_e3.75", "c_9_e3.75",
  "c_10_e4.5", "c_11_e5.25", "c_12_e6.0"
)

#### QAQC by inital gas concentration ####
filt <- target %>%
  ungroup() %>%
  arrange(flux_time) %>%
  dplyr::mutate(
    #Can't have negative ebullition
    CH4_slope_ppm_per_day = ifelse(!is.na(ebullition) & 
                                     ebullition &
                                     CH4_slope_ppm_per_day_ebullition < 0,
                                   NA,
                                   CH4_slope_ppm_per_day),
    #Methane starting conc
    CH4_init = ifelse(is.na(CH4_slope_ppm_per_day),
                      NA, CH4_init), #Some fluxes were intentionally removed
    run_sd = RcppRoll::roll_sd(
      CH4_init, 
      weights = c(1/6,1/6,1/6,0,1/6,1/6,1/6), normalize = F,
      fill = NA),
    run_mean = RcppRoll::roll_mean(
      CH4_init, 
      weights = c(1,1,1,0,1,1,1), normalize = TRUE,
      fill = NA),
    remove_ch4_init = CH4_init > (run_mean + 3*run_sd),
    #CO2 starting conc
    CO2_init = ifelse(is.na(CO2_slope_ppm_per_day),
                      NA, CO2_init), #Some fluxes were intentionally removed
    run_sd_co2 = RcppRoll::roll_sd(
      CO2_init, 
      weights = c(1/6,1/6,1/6,0,1/6,1/6,1/6), normalize = F,
      fill = NA),
    run_mean_co2 = RcppRoll::roll_mean(
      CO2_init, 
      weights = c(1,1,1,0,1,1,1), normalize = TRUE,
      fill = NA),
    remove_co2_init = CO2_init > (run_mean_co2 + 2*run_sd_co2),
    #Deal with these
    CH4_slope_ppm_per_day = ifelse(!is.na(remove_ch4_init) & remove_ch4_init,
                                   NA,
                                   CH4_slope_ppm_per_day),
    CO2_slope_ppm_per_day = ifelse(!is.na(remove_co2_init) & remove_co2_init,
                                   NA,
                                   CO2_slope_ppm_per_day)
  )

#Visualize
filt %>%
  filter(!ebullition) %>%
  ggplot(aes(x = flux_time, y = CH4_slope_ppm_per_day)) +
  geom_line() +
  theme_minimal() +
  facet_wrap(~MIU_VALVE)

#### Visualize the worst fits #### 

check_these <- filt %>%
  group_by(MIU_VALVE) %>%
  arrange(-CH4_se) %>%
  filter(!ebullition) %>%
  filter(row_number() <= 10) %>%
  select(MIU_VALVE, flux_time, CH4_se, ebullition)

write_csv(check_these, here::here("processed_data/worst_fits.csv"))

to_plot <- check_these %>%
  left_join(raw %>% filter(gas == "CH₄"),
            by = c("flux_time"))

ch <- 6
to_plot %>%
  ungroup() %>%
  filter(MIU_VALVE == ch,
         change_s >= cutoff_start, 
         change_s <= cutoff_end) %>%
  #filter(!ebullition) %>%
  mutate(label = format(TIMESTAMP, "%b-%d")) %>%
  group_by(label) %>%
  mutate(dup = match(group, unique(group))) %>%
  ungroup() %>%
  mutate(label = paste0(label, " (", dup, ")"),
         label = fct_reorder(label, CH4_se)) %>%
  ggplot(aes(x = TIMESTAMP, y = value, shape = ebullition)) + 
  geom_point()+
  facet_wrap(~label, scales = "free") +
  ggtitle(paste0("Chamber ", ch))

#### Add ebullition ####
df <- filt %>%
  mutate(
    # Some were removed intentionally
    CH4_slope_ppm_per_day_ebullition = 
      ifelse(is.na(CH4_slope_ppm_per_day),
             NA,
             CH4_slope_ppm_per_day_ebullition),
    CH4_slope_ppm_per_day = ifelse(!is.na(ebullition) & ebullition == T,
                                   CH4_slope_ppm_per_day_ebullition, 
                                   CH4_slope_ppm_per_day),
    )

df %>%
  ggplot(aes(x = flux_time, y = CH4_slope_ppm_per_day)) +
  geom_line() +
  theme_minimal() +
  coord_cartesian(ylim = c(0,20))+
  facet_wrap(~MIU_VALVE, scales = "free_y")

# Summarize pct removed

# TO DO: add this back

# Load data
driver <- read_csv(here::here("processed_data", "met_2025_L1.csv")) %>%
  # Note this is created in the daily partitioning file
  mutate(TIMESTAMP = with_tz(TIMESTAMP, "EST")) %>%
  filter(as_date(TIMESTAMP) >= START,
         as_date(TIMESTAMP) <= END)

# Format
driver <- driver %>%
  rename(driver_time = TIMESTAMP)
df$DateTime <- as.POSIXct(df$flux_time, tz = "EST")
driver$DateTime <- as.POSIXct(driver$driver_time, tz = "EST")

# Soil temp
temp <- read_csv(here::here("processed_data", "Soil_temp_2025.csv")) %>% 
  mutate(DateTime_EST = with_tz(DateTime_EST)) %>%
  filter(as_date(DateTime_EST) >= START, 
         as_date(DateTime_EST) <= END)
temp <- temp %>%
  rename(temp_time = DateTime_EST) %>%
  mutate(DateTime = as.POSIXct(temp_time, tz = "EST"))

# Convert to data.table
setDT(temp)
setDT(df)
setDT(driver)

# Nearest soil temperature observation within each MIU_VALVE
df_temp <- temp[
  df,
  on = .(MIU_VALVE, DateTime),
  roll = "nearest"
]

# Match meteorological drivers by nearest time
merged <- driver[
  df_temp,
  on = .(DateTime),
  roll = "nearest"
]

# Time difference between flux and matched soil temperature
merged[, temp_time_diff := abs(temp_time - flux_time)]
merged[, met_time_diff := abs(driver_time - flux_time)]

merged[met_time_diff > 30 * 60, # 30 minute window
       c("AirTC_Avg", "PAR_Den_C_Avg", "Depth_cm") := NA]

merged[met_time_diff > 60 * 60, # 60 minute window
       c("SoilTemp_C") := NA]

# Partition!
chamber_height = 156 # cm
chamber_radius = 25 # cm
chamber_area = pi*(chamber_radius/100)^2 # m2
chamber_volume = chamber_height/100 * # m
  chamber_area * 1000 #L

merged <- merged %>% 
  rename(
    Ta = AirTC_Avg,
    PAR = PAR_Den_C_Avg,
    Ebullition_yn = ebullition
  ) %>%
  mutate(
    Depth_above_surf = ifelse(Depth_cm > 0,
      Depth_cm,
      0
    ),
    NEE = CO2_slope_ppm_per_day * # CONVERT TO umolCO2/m2/s
      (chamber_height - Depth_above_surf) / chamber_height *
      chamber_volume / (0.08206 * (Ta + 273.15)) / (60 * 60 * 24) / chamber_area,
    CH4 = CH4_slope_ppm_per_day * # CONVERT TO umolCH4/m2/s
      (chamber_height - Depth_above_surf) / chamber_height *
      chamber_volume / (0.08206 * (Ta + 273.15)) / (60 * 60 * 24) / chamber_area,
    N2O = N2O_slope_ppm_per_day * # CONVERT TO umolN2O/m2/s
      (chamber_height - Depth_above_surf) / chamber_height *
      chamber_volume / (0.08206 * (Ta + 273.15)) / (60 * 60 * 24) / chamber_area
  ) %>%
  ungroup() %>%
  select(all_of(c("MIU_VALVE", "DateTime", "flux_time", "NEE", "CH4", "N2O", 
                  "PAR", "Ta", "SoilTemp_C", "CH4_R2", "CO2_R2", "CH4_se", 
                  "CO2_se", "Ebullition_yn")))

merged %>%
  group_by(MIU_VALVE) %>%
  summarize(n = sum(is.na(CH4)))

# Identify nighttime
par_night_thresh <- 5 # µmol m-2 s-1 threshold to define night
merged[, is_night := PAR < par_night_thresh]

# For each chamber, fit Q10 using nighttime points
# We'll fit the log-linear Q10 via lm on log(NEE) with NEE>0 (since Reco positive release).
# Model: log(Reco) = a + b*(Ta - Tref); where b = ln(Q10)/10. We'll use Tref = 10°C.

# helper function to fit Q10 (log-linear)
fit_q10_lm <- function(dt_night, Tref = 10, min_night = 40) {
  # dt_night: data.table with columns NEE, Ta; NEE must be > 0
  dt_night <- dt_night[
    is.finite(NEE) &
      NEE > 0 &
      is.finite(SoilTemp_C)
  ]
  
  if (nrow(dt_night) < min_night) {
    return(NULL)
  }
  
  X <- dt_night[, SoilTemp_C - Tref]
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
  dt_ch, window_days = 100, step_days = 1,
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
    wnd_night <- wnd[PAR < par_night_thresh & is.finite(NEE) & NEE > 0 & is.finite(SoilTemp_C)]
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
    MIU_VALVE = unique(g$MIU_VALVE)
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
merged_grid[, c("Ta", "PAR", "SoilTemp_C") := NULL]

# Nearest soil temperature observation within each MIU_VALVE
df_temp_merged <- temp[
  merged_grid,
  on = .(MIU_VALVE, DateTime),
  roll = "nearest"
]

# Match meteorological drivers by nearest time
merged_grid_final <- driver[
  df_temp_merged,
  on = .(DateTime),
  roll = "nearest"
]

setnames(
  merged_grid_final,
  old = c("AirTC_Avg", "PAR_Den_C_Avg"),
  new = c("Ta", "PAR")
)

merged_grid_final[
  abs(temp_time - DateTime) > 60 * 60,
  SoilTemp_C := NA_real_
]

merged_grid_final[
  abs(driver_time - DateTime) > 60 * 60,
  c("Ta", "PAR", "Depth_cm", "Salinity") := NA
]

for (ch in chambers) {
  pch <- params_dt[MIU_VALVE == ch & !is.na(Rref) & !is.na(Q10)][order(center)]
  if (nrow(pch) == 0) next
  # ensure unique centers
  pch <- unique(pch, by = "center")
  x <- as.numeric(pch$center) # seconds since epoch
  yR <- pch$Rref
  yQ <- pch$Q10
  targ_idx <- which(merged_grid_final$MIU_VALVE == ch)
  xt <- as.numeric(merged_grid_final$DateTime[targ_idx])
  # approx with rule=2: use nearest outside range
  Rinterp <- approx(x = x, y = yR, xout = xt, rule = 2, ties = "ordered")$y
  Qinterp <- approx(x = x, y = yQ, xout = xt, rule = 2, ties = "ordered")$y
  merged_grid_final[targ_idx, Rref_t := Rinterp]
  merged_grid_final[targ_idx, Q10_t := Qinterp]
}

# Predict Reco using time-varying parameters
# Reco = Rref_t * Q10_t ^ ((Ta - Tref)/10)
merged_grid_final[, Reco := NA_real_]
Tref <- 10
merged_grid_final[!is.na(Rref_t) & !is.na(Q10_t) & !is.na(SoilTemp_C),
            Reco := Rref_t * (Q10_t^((SoilTemp_C - Tref) / 10))]

# Compute daytime GPP = Reco - NEE
merged_grid_final[, is_day := PAR >= par_night_thresh]
merged_grid_final[, GPP := NA_real_]
day_mask <- merged_grid_final$is_day & is.finite(merged_grid_final$Reco) & is.finite(merged_grid_final$NEE)
merged_grid_final[day_mask, GPP := Reco - NEE]
# enforce non-negative GPP
merged_grid_final[day_mask & GPP < 0, GPP := 0]
merged_grid_final[is.na(NEE), GPP := NA]
# merged_grid_final[is.na(NEE), Reco := NA]

merged_export <- merged_grid_final %>%
  as_tibble()

write_csv(merged_export, here::here("processed_data", "partitioned_co2_variability_MS.csv"))

