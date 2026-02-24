
### GAP FILL GPP AND CH4 ### 

# Load packages and data
library(tidyverse)
library(data.table)
library(randomForest)

target <- read_csv(here::here("processed_data","partitioned_co2.csv")) %>%
  rename(TIMESTAMP = DateTime)
met <- read_csv(here::here("processed_data","met_2025_dashboard.csv")) %>%
  mutate(Salinity = ifelse(year(TIMESTAMP) == 2025 & 
                             as.numeric(Salinity) < 6, 
                           NA, Salinity))
wl <- read_csv(here::here("processed_data","water_level_dashboard.csv"))
evi <- read_csv(here::here("processed_data","evi.csv")) %>%
  filter(!duplicated(Date))

# Plot specifications
chamber_levels2 = c("Ch. 1 (+0 ºC)", "Ch. 2 (+0 ºC)", "Ch. 3 (+0.75 ºC)", 
                    "Ch. 4 (+1.5 ºC)", "Ch. 5 (+2.25 ºC)", "Ch. 6 (+2.25 ºC)", 
                    "Ch. 7 (+3.0 ºC)", "Ch. 8 (+3.75 ºC)", "Ch. 9 (+3.75 ºC)",
                    "Ch. 10 (+4.5 ºC)", "Ch. 11 (+5.25 ºC)", "Ch. 12 (+6.0 ºC)")

color.gradient=c('blue4','blue3','turquoise4','lightseagreen',
                 'mediumseagreen','limegreen','yellowgreen','yellow2',
                 'darkgoldenrod2','darkorange2','orangered1','red2')



# QAQC


df <- target %>%
  mutate(
    TIMESTAMP = as.POSIXct(TIMESTAMP),
    CH4 = ifelse(CH4 < 0, NA, CH4)
  ) %>%
  filter(!is.na(TIMESTAMP))

# Consistent timestamp

setDT(df)
setkey(df, MIU_VALVE, TIMESTAMP)

make_grid <- function(g) {
  data.table(
    TIMESTAMP = seq(min(g$TIMESTAMP),
                    max(g$TIMESTAMP),
                    by = "130 min"),
    MIU_VALVE = g$MIU_VALVE[1]
  )
}

grid <- df[, make_grid(.SD), by = MIU_VALVE]

setkey(grid, MIU_VALVE, TIMESTAMP)

flux_reg <- df[grid, roll = 7200]   # allow 2-hour carry

flux_reg <- flux_reg %>%
  mutate(
    time_join = round_date(TIMESTAMP, "15 minutes"),
    date_join = as.Date(TIMESTAMP)
  )

ch4 <- flux_reg %>%
  left_join(met %>% rename(time_join = TIMESTAMP), by = "time_join") %>%
  left_join(wl  %>% rename(time_join = TIMESTAMP), by = "time_join") %>%
  left_join(evi %>% rename(date_join = Date), by = "date_join")

#Confirm CH4 looks reasonable
ch4 %>%
  ggplot(aes(x = TIMESTAMP, y = CH4))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~MIU_VALVE, scales = "free")

#Check GPP params
ch4 %>%
  ggplot(aes(x = TIMESTAMP, y = Rref_t))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~MIU_VALVE)

ch4 %>%
  ggplot(aes(x = TIMESTAMP, y = Q10_t))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~MIU_VALVE)

#How many are missing? 

ch4 %>%
  group_by(MIU_VALVE) %>%
  summarize(gpp_nas = sum(is.na(GPP)[!is_night], na.rm = T),
            gpp_pct = gpp_nas/sum(!is.na(is_night) & !is_night),
            reco_nas = sum(is.na(Reco)),
            reco_pct = reco_nas/n(),
            ch4_nas = sum(is.na(CH4)),
            ch4_pct = ch4_nas/n())


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

for (ch in names(rf_gpp_models)) {
  model <- rf_gpp_models[[ch]]
  idx <- ch4$MIU_VALVE == ch
  ch4[idx, GPP_rf := predict(model, ch4[idx])]
}

ch4 %>%
  ggplot(aes(x = TIMESTAMP, y = GPP))+
  geom_point()+
  geom_point(aes(y = GPP_rf), color = "red")+
  facet_wrap(~MIU_VALVE)

ch4[, GPP_filled := GPP]
ch4[is.na(GPP) & is_day == TRUE, GPP_filled := GPP_rf]
ch4[is_day == FALSE, GPP_filled := 0]

ch4 %>%
  group_by(MIU_VALVE) %>%
  summarize(nas = sum(is.na(GPP_filled)))

### CH4 ###

train <- ch4[!is.na(CH4)]

rf_ch4_models <- lapply(split(train, train$MIU_VALVE), function(dt_ch) {
  randomForest(
    CH4 ~ Ta + PAR + GPP_filled + Reco + evi_predicted + Depth_cm + Salinity,
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

ch4 %>%
  ggplot(aes(x = TIMESTAMP, y = CH4))+
  geom_point()+
  geom_point(aes(y = CH4_rf), color = "red")+
  facet_wrap(~MIU_VALVE)

ch4[, CH4_filled := CH4]
ch4[is.na(CH4), CH4_filled := CH4_rf]

ch4 %>%
  group_by(MIU_VALVE) %>%
  summarize(nas = sum(is.na(CH4_filled)))

ch4 %>%
  filter(MIU_VALVE == 4) %>%
  ggplot(aes(x = TIMESTAMP, y = CH4_filled))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~MIU_VALVE, scales = "free")

write_csv(ch4, here::here("processed_data","L2- partitioned_and_gap_filled.csv"))

varImpPlot(rf_ch4_models[[10]])

library(pdp)

vars <- c("PAR", "Ta", "evi_predicted", "Depth_cm")

pdp_list <- list()

for (ch in names(rf_ch4_models)) {
  
  rf_model <- rf_ch4_models[[ch]]
  train_ch <- train[MIU_VALVE == ch]
  
  for (v in vars) {
    
    pdp_obj <- partial(
      rf_model,
      pred.var = v,
      train = train_ch,
      grid.resolution = 50
    )
    
    pdp_df <- as.data.frame(pdp_obj)
    
    names(pdp_df)[1] <- "x"
    
    pdp_df$variable <- v
    pdp_df$MIU_VALVE <- ch
    
    pdp_list[[paste(ch, v, sep = "_")]] <- pdp_df
  }
}

pdp_all <- bind_rows(pdp_list[2:6])

ggplot(pdp_all, aes(x = x, y = yhat)) +
  geom_line(size = 1) +
  facet_grid(MIU_VALVE ~ variable, scales = "free_x") +
  theme_bw() +
  labs(
    x = "Predictor value",
    y = "Partial Dependence (Predicted CH4)",
    title = "Random Forest Partial Dependence by Chamber"
  )
