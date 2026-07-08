###############################################
# CO2 flux partitioning: NEE → GPP + Reco
###############################################

# Run for only the analysis period so as to not bias cutoffs
START = "2025-03-18"
END = "2025-11-15"

# Load packages
library(tidyverse)
library(data.table)
Sys.setenv(TZ = "EST")

# Load slopes
target <- read_csv(here::here("processed_data", "L0_for_dashboard.csv")) %>%
  mutate(flux_start = force_tz(flux_start, tzone = "EST"),
         flux_end = force_tz(flux_end, tzone = "EST"),
         TIMESTAMP = force_tz(TIMESTAMP, tzone = "EST")) %>%
  rename(flux_time = TIMESTAMP) %>%
  filter(!duplicated(flux_time),
         as_date(flux_time) >= START,
         as_date(flux_time) <= END)

#### QAQC by standard error ####
filt <- target %>%
  group_by(MIU_VALVE) %>%
  mutate(
    cutoff = mean(CH4_se, na.rm = TRUE)+3*sd(CH4_se, na.rm = TRUE),
    keep = ifelse(!is.na(CH4_se) & CH4_se < cutoff, TRUE, F),
    cutoff_co2 = mean(CO2_se, na.rm = TRUE)+3*sd(CO2_se, na.rm = TRUE),
    keep_co2 = ifelse(!is.na(CO2_se) & CO2_se < cutoff_co2, TRUE, F)
  )

# visualize
p <- filt %>%
  ggplot(aes(x = CH4_slope_ppm_per_day, y = CH4_se, color = keep, 
             label = paste(flux_time, CH4_se))) +
  geom_point(data = . %>% filter(keep == T)) +
  geom_point(data = . %>% filter(keep == F)) +
  scale_color_manual(values = c("red", "black")) +
  theme_minimal() +
  facet_wrap(~MIU_VALVE, scales = "free")

plotly::ggplotly(p)

p <- filt %>%
  ggplot(aes(x = CH4_slope_ppm_per_day, y = CH4_R2, color = keep, 
             label = paste(flux_time, CH4_se))) +
  geom_point(data = . %>% filter(keep == T)) +
  geom_point(data = . %>% filter(keep == F)) +
  scale_color_manual(values = c("red", "black")) +
  theme_minimal() +
  facet_wrap(~MIU_VALVE, scales = "free")

plotly::ggplotly(p)

filt %>%
  ggplot(aes(x = abs(CO2_slope_ppm_per_day), y = CO2_R2, color = keep_co2, label = flux_time)) +
  geom_point() +
  labs(x = "CO2 slope", y = "R²") +
  scale_color_manual(values = c("red", "black")) +
  theme_minimal() +
  facet_wrap(~MIU_VALVE, scales = "free")

p <- filt %>%
  ggplot(aes(x = flux_time, y = CH4_slope_ppm_per_day, color = keep, 
             label = CH4_se)) +
  geom_point() +
  scale_color_manual(values = c("red", "black")) +
  theme_minimal() +
  facet_wrap(~MIU_VALVE, scales = "free")

plotly::ggplotly(p)

filt %>%
  ggplot(aes(x = flux_time, y = CH4_slope_ppm_per_day, color = keep)) +
  geom_point() +
  scale_color_manual(values = c("red", "black")) +
  theme_minimal() +
  facet_wrap(~MIU_VALVE)

#### Visualize the worst fits #### 

check_these <- filt %>%
  group_by(MIU_VALVE) %>%
  arrange(-CH4_se) %>%
  filter(row_number() <= 100) %>%
  select(MIU_VALVE, flux_time, keep, CH4_se)

write_csv(check_these, here::here("processed_data/worst_fits.csv"))

raw <- read_csv(here::here("processed_data/raw_comb.csv")) %>%
  filter(gas == "CH₄") %>%
  group_by(group) %>%
  mutate(flux_time = first(TIMESTAMP))

aug <- raw %>%
  filter(as_date(TIMESTAMP) == "2025-08-15")

aug %>%
  left_join(removal) %>%
  filter(change_s >= start_cutoff, 
         change_s <= end_cutoff) %>%
  filter(Chamber == "c_3_e0.75",
         gas == "CH₄") %>%
  ggplot(aes(x = TIMESTAMP, y = value, color = keep))+
  geom_point()+
  facet_wrap(~group, scales = "free")

to_plot <- check_these %>%
  left_join(raw %>% filter(gas == "CH₄"),
            by = c("flux_time"))

start_cutoff <- 220 # Buffer of time after flux window
end_cutoff <- 510

ch <- 10
to_plot %>%
  filter(MIU_VALVE == ch,
         change_s >= start_cutoff, 
         change_s <= end_cutoff) %>%
  mutate(label = format(TIMESTAMP, "%b-%d")) %>%
  group_by(label) %>%
  mutate(dup = match(group, unique(group))) %>%
  ungroup() %>%
  mutate(label = paste0(label, " (", dup, ")"),
         label = fct_reorder(label, CH4_se)) %>%
  ggplot(aes(x = TIMESTAMP, y = value, color = keep)) + 
  geom_point()+
  facet_wrap(~label, scales = "free") +
  ggtitle(paste("Chamber", ch))

#### Segmented regression ####

# Check if there is a significant breakpoint - this is flagging too many
segmented_ps <- to_plot %>%
  ungroup() %>%
  filter(
    change_s >= start_cutoff,
    change_s <= end_cutoff,
    !is.na(change_s),
    !is.na(value)
  ) %>%
  nest_by(group) %>%
  mutate(
    seg_p = segmented::davies.test(
      lm(value ~ change_s, data = data),
      seg.Z = ~change_s
    )$p.value
  ) %>%
  select(-data)

# Check if the models with 1-2 breakpoints outperform a linear model
# This works (or worked??), but also flags non-ebullition errors
# Also takes a long time!
test <- to_plot %>%
  ungroup() %>%
  filter(
    MIU_VALVE == 1,
    change_s >= start_cutoff,
    change_s <= end_cutoff,
    !is.na(change_s),
    !is.na(value)
  ) %>%
  nest_by(group) %>%
  mutate(
    m0 = list(lm(value ~ change_s, data = data)),
    m1 = list(
        segmented::segmented(m0, seg.Z = ~change_s, npsi = 1)
    ),
    m2 = list(
        segmented::segmented(m0, seg.Z = ~change_s, npsi = 2)
      ),
    bic0 = BIC(m0),
    bic1 = if (is.null(m1)) Inf else BIC(m1),
    bic2 = if (is.null(m2)) Inf else BIC(m2),
    n_breaks = which.min(c(bic0, bic1, bic2)) - 1,
    breakpoint = (bic1 < (bic0 - 5)) | (bic2 < (bic0 - 5))
  ) %>%
  ungroup()

test_data <- to_plot %>%
  filter(group == 246606,
         change_s >= start_cutoff, 
         change_s <= end_cutoff)

m0 <- lm(value ~ change_s, data = test_data)
m1 <- segmented::segmented(m0, seg.Z = ~change_s, npsi = 1)
m2 <- segmented::segmented(m0, seg.Z = ~change_s, npsi = 2)

plot(test_data %>%
       pull(value))

#### Jonas's version (running variance) ####

eb_by_roll_var <- raw %>%
  filter(!is.na(value),
         change_s >= start_cutoff,
         change_s <= end_cutoff) %>%
  group_by(Chamber, flux_time) %>%
  mutate(
    delta = value - lag(value),
    run_var = RcppRoll::roll_var(value, 5, fill = NA),
    ebullition = run_var > 0.001 & 
      delta > 0
  ) %>%
  summarize(ebullition = sum(ebullition, na.rm = T) > 0,
            CH4_slope_ppm_per_day_ebullition = (last(value) - first(value)) /
              (last(change_s) - first(change_s)) *60*60*24) %>%
  mutate()

chamber_levels <- c(
  "c_1_amb", "c_2_amb", "c_3_e0.75", "c_4_e1.5", "c_5_e2.25",
  "c_6_e2.25", "c_7_e3.0", "c_8_e3.75", "c_9_e3.75",
  "c_10_e4.5", "c_11_e5.25", "c_12_e6.0"
)

ch <- 2
to_plot %>%
  left_join(eb_by_roll_var) %>%
  filter(MIU_VALVE == ch,
         change_s >= start_cutoff, 
         change_s <= end_cutoff) %>%
  mutate(label = format(TIMESTAMP, "%b-%d")) %>%
  group_by(label) %>%
  mutate(dup = match(group, unique(group))) %>%
  ungroup() %>%
  mutate(label = paste0(label, " (", dup, ")"),
         label = fct_reorder(label, CH4_se)) %>%
  ggplot(aes(x = TIMESTAMP, y = value, color = ebullition)) + 
  geom_point()+
  facet_wrap(~label, scales = "free") +
  ggtitle(paste0("Chamber ", ch))

biggest <- raw %>%
  filter(flux_time > "2025-09-05 16:00:00",
         flux_time < "2025-09-05 17:00:00") 

biggest %>%
  mutate(type = ifelse(change_s >= start_cutoff & change_s <= end_cutoff,
                       "yep",
                       "nope")) %>%
  ggplot(aes(x = TIMESTAMP, y = value, color = Chamber, alpha = type))+
  geom_point()

#### 

joined <- target %>%
  mutate(Chamber = factor(MIU_VALVE,
                          levels = 1:12,
                          labels = chamber_levels)) %>%
  left_join(eb_by_roll_var) %>%
  mutate(
    # Some were removed intentionally
    CH4_slope_ppm_per_day_ebullition = ifelse(is.na(CH4_slope_ppm_per_day),
                                              NA,
                                              CH4_slope_ppm_per_day_ebullition),
    #Can't have negative ebullition
    ebullition = ifelse(CH4_slope_ppm_per_day_ebullition < 0,
                        F,
                        ebullition),
    CH4_slope_ppm_per_day_ebullition = ifelse(!ebullition,
                                              NA,
                                              CH4_slope_ppm_per_day_ebullition))

joined %>%
  group_by(Chamber) %>%
  summarize(pct = sum(ebullition, na.rm = T)/n() * 100) %>%
  mutate(Chamber = factor(Chamber, levels = chamber_levels)) %>%
  ggplot(aes(y = Chamber, x = pct))+
  geom_point()

joined %>%
  mutate(Chamber = factor(Chamber, levels = chamber_levels)) %>%
  ggplot(aes(y = CH4_slope_ppm_per_day, x = CH4_slope_ppm_per_day_ebullition))+
  geom_point()+
  geom_smooth(method = "lm")

removal <- filt %>%
  select(MIU_VALVE, flux_time, keep, keep_co2) %>%
  distinct()

df <- joined %>%
  left_join(removal, by = c("MIU_VALVE", "flux_time")) %>%
  mutate(
    CH4_slope_ppm_per_day = ifelse(!keep, NA, CH4_slope_ppm_per_day),
    CH4_slope_ppm_per_day = ifelse(ebullition, 
                                   CH4_slope_ppm_per_day_ebullition, 
                                   CH4_slope_ppm_per_day),
    CO2_slope_ppm_per_day = ifelse(!keep_co2, NA, CO2_slope_ppm_per_day)) %>%
  select(-keep, -keep_co2)

target %>%
  filter(as_date(flux_time) >= START,
         as_date(flux_time) <= END) %>%
  group_by(MIU_VALVE) %>%
  left_join(removal) %>%
  summarize(n_removed = sum(!keep & !is.na(CH4_slope_ppm_per_day), na.rm = T),
            pct = round(n_removed/sum(!is.na(keep) & !is.na(CH4_slope_ppm_per_day))*100, 1),
            n_removed_co2 = sum(!keep_co2 & !is.na(CO2_slope_ppm_per_day), na.rm = T),
            pct_co2 = round(n_removed_co2/sum(!is.na(keep_co2) & !is.na(CO2_slope_ppm_per_day))*100,1)) %>%
  select(MIU_VALVE, pct, pct_co2)

# Load data
driver <- read_csv(here::here("processed_data", "met_2025_L1.csv"))

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

chamber_height = 156 # cm
chamber_radius = 25 # cm
chamber_area = pi*(chamber_radius/100)^2 # m2
chamber_volume = chamber_height/100 * # m
  chamber_area * 1000 #L

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
      (chamber_height - Depth_above_surf) / chamber_height *
      chamber_volume / (0.08206 * (Ta + 273.15)) / (60 * 60 * 24) / chamber_area,
    CH4 = CH4_slope_ppm_per_day * # CONVERT TO umolCH4/m2/s
      (chamber_height - Depth_above_surf) / chamber_height *
      chamber_volume / (0.08206 * (Ta + 273.15)) / (60 * 60 * 24) / chamber_area,
    N2O = N2O_slope_ppm_per_day * # CONVERT TO umolN2O/m2/s
      (chamber_height - Depth_above_surf) / chamber_height *
      chamber_volume / (0.08206 * (Ta + 273.15)) / (60 * 60 * 24) / chamber_area,
    H2O = H2O_slope_ppm_per_day * # CONVERT TO umolH2O/m2/s
      (chamber_height - Depth_above_surf) / chamber_height *
      chamber_volume / (0.08206 * (Ta + 273.15)) / (60 * 60 * 24) / chamber_area,
    LE_W_m2 = H2O / 1E9 * 18.015 * # kg/m2/s
      (2.501 - 0.00237 * Ta) * 10^6 # latent heat of vaporization
  ) %>%
  ungroup() %>%
  select(all_of(c("MIU_VALVE", "DateTime", "flux_time", "NEE", "CH4", "N2O", "LE_W_m2",
                  "PAR", "Ta", "CH4_R2", "CO2_R2", "CH4_se", "CO2_se")))

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
    DateTime = seq(max(g$DateTime),
                       min(g$DateTime),
                    by = "-130 min"
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

merged_export <- merged_grid %>%
  as_tibble()

write_csv(merged_export, here::here("processed_data", "partitioned_co2_variability_MS.csv"))

