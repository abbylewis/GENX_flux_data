raw <- read_csv(here::here("processed_data","raw_2021.csv"))
slopes <- read_csv(here::here("processed_data","L0.csv")) %>%
  select(group, MIU_VALVE, TIMESTAMP, cutoff) 
asl_2021 <- read_csv(here::here("processed_data","L0.csv")) %>%
  rename(chamber = MIU_VALVE) %>%
  mutate(TIMESTAMP = round_date(TIMESTAMP, "minute"))
gln_2021 <- read_csv(here::here("Raw_data","2021-05-05_2021-12-31_CH4_CO2_fluxes.csv"))
gln_cutoffs <- read_csv(here::here("processed_data","chamber_lags_20220627.csv"))

gln_retained <- gln_2021 %>%
  filter(!is.na(CH4_slope_umol_per_day)) %>%
  select(TIMESTAMP, chamber) %>%
  distinct() %>%
  mutate(retained = "yes")

all_obs <- asl_2021 %>%
  bind_rows(gln_2021 %>% #minutes are slightly off sometimes. Try multiple options
              bind_rows(gln_2021 %>% mutate(TIMESTAMP = TIMESTAMP + minutes(1))) %>%
              bind_rows(gln_2021 %>% mutate(TIMESTAMP = TIMESTAMP - minutes(1)))) %>%
  rename(ASL_ch4_slope_ppm = CH4_slope_ppm_per_day,
         GLN_ch4_slope_umol = CH4_slope_umol_per_day)

raw_asl <- raw %>%
  left_join(slopes) %>%
  arrange(TIMESTAMP) %>%
  fill(group, cutoff) %>%
  group_by(group) %>%
  mutate(min_TS = min(TIMESTAMP, na.rm = T),
         change_s = as.numeric(difftime(TIMESTAMP, min_TS, units = "secs")),
         used = ifelse(change_s > cutoff, "yes","no"),
         method = "ASL",
         retained = ifelse(sum(!is.na(CH4d_ppm))>=5,
                           "yes","no"))

raw_gln <- raw %>%
  left_join(gln_cutoffs, by = c("MIU_VALVE" = "Chamber")) %>%
  arrange(TIMESTAMP) %>%
  mutate(group = group_fun(MIU_VALVE)) %>%
  group_by(group) %>%
  mutate(n = row_number(),
         used = ifelse(n > Lag, "yes", "no"),
         method = "GLN") %>%
  mutate(TS_match = round(TIMESTAMP, "mins")) %>%
  left_join(gln_retained, by = c("MIU_VALVE" = "chamber", 
                                 "TS_match" = "TIMESTAMP")) %>%
  group_by(group) %>%
  fill(retained)

raw_comb <- raw_asl %>% 
  bind_rows(raw_gln)  %>% 
  rename(Chamber = MIU_VALVE) %>%
  pivot_longer(matches("CH4d_ppm|N2Od_ppm|CO2d_ppm"), 
               names_to = "gas") %>%
  mutate(gas = case_match(gas,
                          "CH4d_ppm"~"CH₄", 
                          "CO2d_ppm"~"CO₂",
                          "N2Od_ppm"~"N₂O",
                          .default = gas),
         gas = paste(gas, method),
         Chamber = factor(Chamber, 
                          levels = 1:12,
                          labels = chamber_levels)) %>%
  ungroup() %>%
  arrange(TIMESTAMP) %>%
  mutate(group2 = group_fun(Chamber)) %>%
  group_by(group2) %>%
  mutate(label = format(min(TIMESTAMP), "%H:%M"),
         min_TS = min(TIMESTAMP, na.rm = T),
         change_s = as.numeric(difftime(TIMESTAMP, min_TS, units = "secs"))) %>%
  select(-change, -n, -Lag, -TS_match, -min_TS, -cutoff)

write_csv(raw_comb, here::here("processed_data","raw_comb.csv"))
