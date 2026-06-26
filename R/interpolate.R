### GAP FILL GPP AND CH4 ###

# Load packages and data
library(tidyverse)
library(data.table)
library(randomForest)

df <- read_csv(here::here("processed_data", "partitioned_co2_variability_MS.csv")) %>%
  rename(TIMESTAMP = DateTime) %>%
  filter(!is.na(TIMESTAMP)) %>%
  mutate(TIMESTAMP = with_tz(TIMESTAMP, "EST"),
         flux_time = with_tz(flux_time, "EST"),
         driver_time = with_tz(driver_time, "EST"))

evi <- read_csv(here::here("processed_data", "evi.csv")) %>%
  filter(!duplicated(Date))

# Consistent timestamp

setDT(df)
setkey(df, MIU_VALVE, TIMESTAMP)

make_grid <- function(g) {
  data.table(
    TIMESTAMP = seq(min(g$TIMESTAMP),
      max(g$TIMESTAMP),
      by = "130 min"
    ),
    MIU_VALVE = g$MIU_VALVE[1]
  )
}

grid <- df[, make_grid(.SD), by = MIU_VALVE]

setkey(grid, MIU_VALVE, TIMESTAMP)

flux_reg <- df[grid, roll = 7200] # allow 2-hour carry

flux_reg <- flux_reg %>%
  mutate(
    #time_join = round_date(TIMESTAMP, "15 minutes"),
    date_join = as_date(TIMESTAMP)
  )

ch4 <- flux_reg %>%
  #left_join(met %>% rename(time_join = TIMESTAMP), by = "time_join") %>%
  left_join(evi %>% rename(date_join = Date), by = "date_join") %>%
  mutate(is_day = ifelse(is.na(is_day) & !is.na(PAR) & PAR > 5,
                         T,
                         is_day),
         is_day = ifelse(is.na(is_day) & !is.na(PAR) & PAR < 5,
                         F,
                         is_day))

# Confirm CH4 looks reasonable
ch4 %>%
  ggplot(aes(x = TIMESTAMP, y = CH4)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~MIU_VALVE)

# Check GPP params
ch4 %>%
  ggplot(aes(x = TIMESTAMP, y = Rref_t)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~MIU_VALVE)

p <- ch4 %>%
  mutate(MIU_VALVE = factor(MIU_VALVE)) %>%
  ggplot(aes(x = TIMESTAMP, y = Rref_t, 
             color = MIU_VALVE, label = MIU_VALVE)) +
  geom_point()

plotly::ggplotly(p)

ch4 %>%
  ggplot(aes(x = TIMESTAMP, y = Q10_t)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~MIU_VALVE)

# How many are missing?

ch4 %>%
  group_by(MIU_VALVE) %>%
  summarize(
    gpp_nas = sum(is.na(GPP)[!is_night], na.rm = T),
    gpp_pct = gpp_nas / sum(!is.na(is_night) & !is_night) * 100,
    reco_nas = sum(is.na(Reco)),
    reco_pct = reco_nas / n() * 100,
    ch4_nas = sum(is.na(CH4)),
    ch4_pct = ch4_nas / n() * 100
  )


### Gap fill ###

## GPP

train <- ch4[is_day == TRUE & !is.na(GPP)]

rf_gpp_models <- lapply(split(train, train$MIU_VALVE), function(dt_ch) {
  randomForest(
    GPP ~ Ta + PAR + evi_predicted + Depth_cm,
    data = dt_ch,
    na.action = na.omit,
    ntree = 500
  )
})

oob_metrics <- lapply(rf_gpp_models, function(model) {
  data.frame(
    OOB_MSE = tail(model$mse, 1),
    OOB_R2 = tail(model$rsq, 1)
  )
})

oob_metrics <- do.call(rbind, oob_metrics)
oob_metrics$MIU_VALVE <- names(rf_gpp_models)
oob_metrics

for (ch in names(rf_gpp_models)) {
  model <- rf_gpp_models[[ch]]
  idx <- ch4$MIU_VALVE == ch
  ch4[idx, GPP_rf := predict(model, ch4[idx])]
}

ch4 %>%
  ggplot(aes(x = TIMESTAMP, y = GPP)) +
  geom_point() +
  geom_point(aes(y = GPP_rf), color = "red") +
  facet_wrap(~MIU_VALVE)

ch4[, GPP_filled := GPP]
ch4[is.na(GPP_filled) & is_day == TRUE, GPP_filled := GPP_rf]
ch4[is_day == FALSE, GPP_filled := 0]

ch4 %>%
  group_by(MIU_VALVE) %>%
  summarize(nas = sum(is.na(GPP_filled)))

ch4 %>%
  group_by(MIU_VALVE) %>%
  summarize(nas = sum(is.na(Reco)))

ch4 %>%
  ggplot(aes(x = TIMESTAMP, y = Reco))+
  geom_line()+
  facet_wrap(~MIU_VALVE)

## LE

train <- ch4[is_day == TRUE & !is.na(LE_W_m2)]

rf_le_models <- lapply(split(train, train$MIU_VALVE), function(dt_ch) {
  randomForest(
    LE_W_m2 ~ Ta + PAR + evi_predicted + Depth_cm,
    data = dt_ch,
    na.action = na.omit,
    ntree = 500
  )
})

oob_metrics <- lapply(rf_le_models, function(model) {
  data.frame(
    OOB_MSE = tail(model$mse, 1),
    OOB_R2 = tail(model$rsq, 1)
  )
})

oob_metrics <- do.call(rbind, oob_metrics)
oob_metrics$MIU_VALVE <- names(rf_gpp_models)
oob_metrics

for (ch in names(rf_le_models)) {
  model <- rf_le_models[[ch]]
  idx <- ch4$MIU_VALVE == ch
  ch4[idx, LE_rf := predict(model, ch4[idx])]
}

ch4 %>%
  ggplot(aes(x = TIMESTAMP, y = LE_W_m2)) +
  geom_point() +
  geom_point(aes(y = LE_rf), color = "red") +
  facet_wrap(~MIU_VALVE)

ch4[, LE_filled := LE_W_m2]
ch4[is.na(LE_filled) & is_day == TRUE, LE_filled := LE_rf]
ch4[is_day == FALSE, LE_filled := 0]

ch4 %>%
  group_by(MIU_VALVE) %>%
  summarize(nas = sum(is.na(LE_filled)))

### CH4 ###

train <- ch4[!is.na(CH4)]

rf_ch4_models <- lapply(split(train, train$MIU_VALVE), function(dt_ch) {
  randomForest(
    CH4 ~ Ta + PAR + GPP_filled + Reco +
      evi_predicted + Depth_cm + Salinity,
    data = dt_ch,
    na.action = na.omit,
    ntree = 500
  )
})

for (ch in names(rf_ch4_models)) {
  model <- rf_ch4_models[[ch]]
  idx <- ch4$MIU_VALVE == ch
  ch4[idx, CH4_rf := predict(model, ch4[idx])]
}

oob_metrics <- lapply(rf_ch4_models, function(model) {
  data.frame(
    OOB_MSE = tail(model$mse, 1),
    OOB_R2 = tail(model$rsq, 1)
  )
})

oob_metrics <- do.call(rbind, oob_metrics)
oob_metrics$MIU_VALVE <- names(rf_gpp_models)
oob_metrics

ch4 %>%
  ggplot(aes(x = TIMESTAMP, y = CH4)) +
  geom_point() +
  geom_point(aes(y = CH4_rf), color = "red") +
  facet_wrap(~MIU_VALVE)

ch4[, CH4_filled := CH4]
ch4[is.na(CH4), CH4_filled := CH4_rf]

ch4 %>%
  group_by(MIU_VALVE) %>%
  summarize(nas = sum(is.na(CH4_filled)))

ch4 %>%
  filter(MIU_VALVE == 6) %>%
  ggplot(aes(x = TIMESTAMP, y = CH4_filled)) +
  geom_point(aes(color = is.na(CH4))) +
  geom_smooth() +
  facet_wrap(~MIU_VALVE, scales = "free")

ch4 %>%
  ggplot(aes(x = CH4, y = CH4_rf))+
  geom_point() +
  facet_wrap(~MIU_VALVE, scales = "free")

ch4 %>%
  ggplot(aes(x = TIMESTAMP)) +
  geom_point(aes(y = CH4_rf), color = "red")+
  geom_point(aes(y = CH4), shape = 21) +
  facet_wrap(~MIU_VALVE, scales = "free")

ch4 %>%
  ggplot(aes(x = TIMESTAMP)) +
  geom_point(aes(y = CH4_rf), color = "red")+
  facet_wrap(~MIU_VALVE, scales = "free")

write_csv(ch4, here::here("processed_data", "L2- partitioned_and_gap_filled.csv"))

# varImpPlot(rf_ch4_models[[9]])
#
# library(pdp)
#
# vars <- c("Salinity","PAR", "Ta", "evi_predicted", "Depth_cm")
#
# pdp_list <- list()
#
# for (ch in names(rf_ch4_models)) {
#
#  rf_model <- rf_ch4_models[[ch]]
#  train_ch <- train[MIU_VALVE == ch]
#
#  for (v in vars) {
#
#    pdp_obj <- partial(
#      rf_model,
#      pred.var = v,
#      train = train_ch,
#      grid.resolution = 50
#    )
#
#    pdp_df <- as.data.frame(pdp_obj)
#
#    names(pdp_df)[1] <- "x"
#
#    pdp_df$variable <- v
#    pdp_df$MIU_VALVE <- ch
#
#    pdp_list[[paste(ch, v, sep = "_")]] <- pdp_df
#  }
# }
#
# pdp_all <- bind_rows(pdp_list)
#
# ggplot(pdp_all, aes(x = x, y = yhat)) +
#  geom_line(size = 1) +
#  facet_grid(MIU_VALVE ~ variable, scales = "free_x") +
#  theme_bw() +
#  labs(
#    x = "Predictor value",
#    y = "Partial Dependence (Predicted CH4)",
#    title = "Random Forest Partial Dependence by Chamber"
#  )
#
