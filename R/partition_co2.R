###############################################
# CO2 flux partitioning: NEE → GPP + Reco
###############################################

# Load packages
library(tidyverse)
library(data.table)

# Load data
df <- read_csv("https://raw.githubusercontent.com/abbylewis/GENX_flux_data/refs/heads/main/processed_data/L0_for_dashboard.csv", show_col_types = F)
met <- read_csv("processed_data/met_2025_dashboard.csv")

# Format
df$DateTime <- as.POSIXct(df$TIMESTAMP, tz = "EST")
met$DateTime <- as.POSIXct(met$TIMESTAMP, tz = "EST")
setDT(df) # Convert to data.table
setDT(met)

# Set keys: DateTime is what will be used to join fluxes with met
setkey(df, DateTime)
setkey(met, DateTime)

#Join and format
merged <- met[df, roll = "nearest"] %>% # Rolling join: nearest met to each flux
  rename(Ta = AirTC_Avg,
         PAR = PAR_Den_C_Avg) %>%
  mutate(NEE = CO2_slope_ppm_per_day * #CONVERT TO umolCO2/m2/s
           265.8 / (0.08206*(Ta + 273.15)) * 60*60*24) %>% 
  ungroup() %>%
  select(MIU_VALVE, DateTime, NEE, PAR, Ta)

# Identify nighttime
par_night_thresh <- 5  # µmol m-2 s-1 threshold to define night
merged[, is_night := PAR < par_night_thresh]

# For each chamber, fit Q10 using nighttime points
# We'll fit the log-linear Q10 via lm on log(NEE) with NEE>0 (since Reco positive release).
# Model: log(Reco) = a + b*(Ta - Tref); where b = ln(Q10)/10. We'll use Tref = 10°C.

# helper function to fit Q10 (log-linear)
fit_q10_lm <- function(dt_night, Tref = 10, min_night = 5) {
  # dt_night: data.table with columns NEE, Ta; NEE must be > 0
  if (nrow(dt_night) < min_night) return(NULL)
  dt_night <- dt_night[NEE > 0 & is.finite(Ta)]
  if (nrow(dt_night) < min_night) return(NULL)
  X <- dt_night[, Ta - Tref]
  Y <- log(dt_night$NEE)
  fit <- try(lm(Y ~ X), silent = TRUE)
  if (inherits(fit, "try-error")) return(NULL)
  coef <- coefficients(fit)
  a <- coef[1]; b <- coef[2]
  Rref <- exp(a)
  Q10 <- exp(b * 10)
  return(list(Rref = as.numeric(Rref), Q10 = as.numeric(Q10), n = nrow(dt_night), fit = fit))
}

# Function: moving-window parameter estimation per chamber
estimate_params_moving_window <- function(
    dt_ch, window_days = 30, step_days = 7, 
    par_night_thresh = 5, Tref = 10) {
  # dt_ch: data.table for one chamber
  if (nrow(dt_ch) == 0) return(NULL)
  start_time <- min(dt_ch$DateTime, na.rm = TRUE)
  end_time   <- max(dt_ch$DateTime, na.rm = TRUE)
  centers <- seq(from = start_time, to = end_time, by = paste0(step_days, " days"))
  res_list <- vector("list", length(centers))
  for (i in seq_along(centers)) {
    center <- centers[i]
    wstart <- center - as.difftime(window_days/2, units = "days")
    wend   <- center + as.difftime(window_days/2, units = "days")
    wnd <- dt_ch[DateTime >= wstart & DateTime <= wend]
    # nighttime points (PAR-based)
    wnd_night <- wnd[PAR < par_night_thresh & is.finite(NEE) & NEE > 0 & is.finite(Ta)]
    fit <- fit_q10_lm(wnd_night, Tref = Tref)
    if (!is.null(fit)) {
      res_list[[i]] <- data.table(MIU_VALVE = dt_ch$MIU_VALVE[1],
                                  center = center,
                                  Rref = fit$Rref,
                                  Q10 = fit$Q10,
                                  n_night = fit$n)
    } else {
      res_list[[i]] <- data.table(MIU_VALVE = dt_ch$MIU_VALVE[1],
                                  center = center,
                                  Rref = NA_real_,
                                  Q10 = NA_real_,
                                  n_night = ifelse(is.null(wnd_night), 0, nrow(wnd_night)))
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

for (ch in chambers) {
  pch <- params_dt[MIU_VALVE == ch & !is.na(Rref) & !is.na(Q10)]
  if (nrow(pch) == 0) next
  # ensure unique centers
  pch <- unique(pch, by = "center")
  x <- as.numeric(pch$center)  # seconds since epoch
  yR <- pch$Rref
  yQ <- pch$Q10
  targ_idx <- which(merged$MIU_VALVE == ch)
  xt <- as.numeric(merged$DateTime[targ_idx])
  # approx with rule=2: use nearest outside range
  Rinterp <- approx(x = x, y = yR, xout = xt, rule = 2, ties = "ordered")$y
  Qinterp <- approx(x = x, y = yQ, xout = xt, rule = 2, ties = "ordered")$y
  merged[targ_idx, Rref_t := Rinterp]
  merged[targ_idx, Q10_t  := Qinterp]
}

# Predict Reco_model using time-varying parameters
# Reco = Rref_t * Q10_t ^ ((Ta - Tref)/10)
merged[, Reco_model := NA_real_]
ok_mask <- is.finite(merged$Rref_t) & is.finite(merged$Q10_t) & is.finite(merged$Ta)
merged[ok_mask, Reco_model := Rref_t * (Q10_t ^ ((Ta[ok_mask] - Tref)/10))]

# Compute daytime GPP = Reco - NEE
merged[, is_day := PAR >= par_night_thresh]
merged[, GPP := NA_real_]
day_mask <- merged$is_day & is.finite(merged$Reco_model) & is.finite(merged$NEE)
merged[day_mask, GPP := Reco_model - NEE]
# enforce non-negative GPP if desired
merged[day_mask & GPP < 0, GPP := 0]

merged %>%
  mutate(Date = as.Date(DateTime),
         chamber = factor(MIU_VALVE,
                          levels = 1:12,
                          labels = c("c_1_amb", "c_2_amb", "c_3_e0.75", "c_4_e1.5", "c_5_e2.25",
                                     "c_6_e2.25", "c_7_e3.0", "c_8_e3.75", "c_9_e3.75",
                                     "c_10_e4.5", "c_11_e5.25", "c_12_e6.0"))) %>%
  pivot_longer(c(NEE, GPP, Reco)) %>%
  group_by(name, MIU_VALVE, Date) %>%
  summarize(value = mean(value, na.rm = T)) %>%
  ggplot(aes(x = Date, color = as.factor(MIU_VALVE))) +
  geom_line(aes(y = value)) +
  theme_bw()+
  scale_color_manual(values = c('blue4','blue3','turquoise4','lightseagreen',
                                'mediumseagreen','limegreen','yellowgreen','yellow2',
                                'darkgoldenrod2','darkorange2','orangered1','red2'))+
  facet_wrap(~name)

params_dt %>%
  pivot_longer(c(Rref, Q10)) %>%
  mutate(chamber = factor(MIU_VALVE,
                          levels = 1:12,
                          labels = c("c_1_amb", "c_2_amb", "c_3_e0.75", "c_4_e1.5", "c_5_e2.25",
                                     "c_6_e2.25", "c_7_e3.0", "c_8_e3.75", "c_9_e3.75",
                                     "c_10_e4.5", "c_11_e5.25", "c_12_e6.0"))) %>%
  ggplot(aes(x = center, y = value, color = chamber))+
  facet_wrap(~name, scales = "free")+
  scale_color_manual(values = c('blue4','blue3','turquoise4','lightseagreen',
                                'mediumseagreen','limegreen','yellowgreen','yellow2',
                                'darkgoldenrod2','darkorange2','orangered1','red2'))+
  geom_line()+
  theme_bw()

write_csv(merged, "processed_data/partitioned_co2.csv")
