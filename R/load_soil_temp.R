ch_temp_names <- c(
  `1` = "btemp_avg.1.",
  `2` = "btemp_avg.6.",
  `3` = "btemp_avg.8.",
  `4` = "btemp_avg.10.",
  `5` = "btemp_avg.15.",
  `6` = "btemp_avg.14.",
  `7` = "btemp_avg.16.",
  `8` = "btemp_avg.21.",
  `9` = "btemp_avg.20.",
  `10` = "btemp_avg.22.",
  `11` = "btemp_avg.27.",
  `12` = "btemp_avg.29."
)

soil_temp <- read_csv(here::here("processed_data", "GENX_Export_2025-01.csv")) %>%
  select(all_of(c("timestamp", ch_temp_names))) %>%
  pivot_longer(-timestamp, names_to = "MIU_VALVE", values_to = "Temp_C")

write_csv(soil_temp, here::here("processed_data", "soil_temp.csv"))
