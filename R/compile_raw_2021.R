library(tidyverse)
source(here::here("R","load_data.R"))
source(here::here("R","filter_old_data_20250120.R"))
source(here::here("R","group_fun.R"))

files <- list.files(here::here("Raw_data","dropbox_downloads"), full.names = T)

files_2021 <- files#[grepl("2021", files)|grepl("20220112", files)]

exclude <- c("GENX_INSTRUMENT_FLUX_COMB_20240417020046.dat",
             "GENX_INSTRUMENT_FLUX_COMB_20240403020045.dat",
             "GENX_INSTRUMENT_FLUX_COMB_20240501020048.dat",
             "GENX_LGR_04142021_20210505020005.dat"
)
files_2021 <- files_2021[!grepl(paste0(exclude, collapse = "|"), files_2021)]
message(paste0("Calculating fluxes for ", length(files_2021), " files"))

#Load data
data_small <- files_2021 %>%
  map(load_data) %>% #custom data loading function that deals with multiple file formats
  bind_rows()  %>%
  filter(!TIMESTAMP == "TS") %>%
  mutate(TIMESTAMP = as_datetime(TIMESTAMP, tz = "EST")) %>%
  filter(!is.na(TIMESTAMP),
         year(TIMESTAMP)>=2021) %>%
  distinct()

#Format data
data_numeric <- data_small %>%
  mutate(across(c("CH4d_ppm", "CO2d_ppm", "N2Od_ppb", "Manifold_Timer", "MIU_VALVE"), as.numeric),
         N2Od_ppb = ifelse(N2Od_ppb <=0, NA, N2Od_ppb),
         CH4d_ppm = ifelse(CH4d_ppm <=0, NA, CH4d_ppm),
         CO2d_ppm = ifelse(CO2d_ppm <=0, NA, CO2d_ppm),
         N2Od_ppm = N2Od_ppb / 1000) %>%
  select(-N2Od_ppb) %>%
  filter(!is.na(MIU_VALVE),
         MIU_VALVE %in% 1:12) %>%
  mutate(Flag = "No issues")

#Remove data as specified in maintenance log
googlesheets4::gs4_deauth() # No authentication needed
today <- Sys.time()
maint_log <- googlesheets4::read_sheet("http://docs.google.com/spreadsheets/d/1_uk8-335NDJOdVU6OjLcxWx4MamNJeVEbVkSmdb9oRs/edit?gid=0#gid=0",
                                       col_types = "c") %>%
  mutate(Start_time = as_datetime(Start_time, tz = "America/New_York"),
         End_time = as_datetime(End_time, tz = "America/New_York"),
         End_time = ifelse(is.na(End_time), today, End_time),
         End_time = as_datetime(End_time, tz = "America/New_York"))
for(i in 1:nrow(maint_log)){
  data_numeric <- data_numeric %>%
    mutate(Flag = ifelse(TIMESTAMP <= maint_log$End_time[i] & 
                           TIMESTAMP >= maint_log$Start_time[i] &
                           MIU_VALVE %in% eval(parse(text = maint_log$Chambers[i])),
                         maint_log$Flag[i],
                         Flag),
           CH4d_ppm = ifelse(TIMESTAMP <= maint_log$End_time[i] & 
                               TIMESTAMP >= maint_log$Start_time[i] &
                               maint_log$Remove[i] == "y" &
                               maint_log$Analyzer[i] %in% c("CO2/CH4", "all") &
                               MIU_VALVE %in% eval(parse(text = maint_log$Chambers[i])),
                             NA,
                             CH4d_ppm),
           CO2d_ppm = ifelse(TIMESTAMP <= maint_log$End_time[i] & 
                               TIMESTAMP >= maint_log$Start_time[i] &
                               maint_log$Remove[i] == "y" &
                               maint_log$Analyzer[i] %in% c("CO2/CH4", "all") &
                               MIU_VALVE %in% eval(parse(text = maint_log$Chambers[i])),
                             NA,
                             CO2d_ppm),
           N2Od_ppm = ifelse(TIMESTAMP <= maint_log$End_time[i] & 
                               TIMESTAMP >= maint_log$Start_time[i] &
                               maint_log$Remove[i] == "y" &
                               maint_log$Analyzer[i] %in% c("N2O", "all") &
                               MIU_VALVE %in% eval(parse(text = maint_log$Chambers[i])),
                             NA,
                             N2Od_ppm))
}

#Group flux intervals, prep for slopes
grouped_data <- data_numeric %>%
  #Group flux intervals
  arrange(TIMESTAMP) %>%
  mutate(group = group_fun(MIU_VALVE)) %>%
  group_by(group, MIU_VALVE) %>%
  #Record the amount of time from when chamber closed
  mutate(start = min(TIMESTAMP),
         end = max(TIMESTAMP),
         date = as.Date(start, tz = "America/New_York"),
         change = as.numeric(difftime(TIMESTAMP, start, units = "days")),
         change_s = as.numeric(difftime(TIMESTAMP, start, units = "secs")))

raw_2021 <- grouped_data %>%
  ungroup() %>%
  filter(year(date) == 2021) %>%
  select(TIMESTAMP, change, MIU_VALVE, CH4d_ppm, CO2d_ppm)

write.csv(raw_2021, 
          here::here("processed_data","raw_2021.csv"), 
          row.names = FALSE)


### SLOPES
#Save flags for data that will be removed in the next step
flags <- grouped_data %>%
  ungroup() %>%
  select(start, MIU_VALVE, Flag, date, group) %>%
  distinct() %>%
  group_by(MIU_VALVE, start, date, group) %>%
  filter(n() == 1 | !Flag == "No issues")

library(furrr)
library(progressr)
plan(multisession)
handlers(global = TRUE)

# For testing
#grouped_data <- grouped_data %>%
#  filter((month(TIMESTAMP) == 8) |
#           (month(TIMESTAMP) == 7 & day(TIMESTAMP) > 15))

#Process using old methods
filtered_data <- filter_old_data_2021(
  grouped_data
  )

#Data flags
data_flags <- filtered_data %>%
  group_by(group, MIU_VALVE, date) %>%
  summarize(Flag_CO2_slope = ifelse(sum(!is.na(CO2d_ppm)) > 5, 
                                    "No issues", "Insufficient data"),
            Flag_N2O_slope = ifelse(sum(!is.na(N2Od_ppm)) > 5, 
                                    "No issues", "Insufficient data"),
            Flag_CH4_slope = ifelse(sum(!is.na(CH4d_ppm)) > 5, 
                                    "No issues", "Insufficient data"),
            .groups = "drop") 

#Run lm
slopes <- filtered_data %>%
  #filter(keep) %>%
  pivot_longer(c(CH4d_ppm, CO2d_ppm, N2Od_ppm), names_to = "gas", values_to = "conc") %>%
  group_by(gas, group, MIU_VALVE, date) %>%
  mutate(n = sum(!is.na(conc))) %>%
  filter(!is.na(conc),
         n > 5) %>%
  summarize(model = list(lm(conc ~ change)),
            slope_ppm_per_day = model[[1]]$coefficients[[2]],
            R2 = summary(model[[1]])$r.squared,
            p = summary(model[[1]])$coefficients[,4][2],
            rmse = sqrt(mean(model[[1]]$residuals^2)),
            max = max(conc),
            min = min(conc),
            init = first(conc),
            flux_start = min(TIMESTAMP),
            flux_end = max(TIMESTAMP),
            TIMESTAMP = unique(start),
            n = sum(!is.na(conc)),
            .groups = "drop") %>%
  select(-model) %>%
  mutate(gas = case_match(gas,
                          "CH4d_ppm" ~ "CH4",
                          "CO2d_ppm" ~ "CO2",
                          "N2Od_ppm" ~ "N2O")) %>%
  pivot_wider(names_from = gas, 
              values_from = c(slope_ppm_per_day, R2, p, rmse, init, max, min),
              names_glue = "{gas}_{.value}") %>%
  full_join(data_flags, by = c("group", "MIU_VALVE", "date"))

p <- slopes %>%
  #filter(hour(TIMESTAMP) > 10, hour(TIMESTAMP) < 15) %>%
  ggplot(aes(x = TIMESTAMP, y = CH4_slope_ppm_per_day, 
             color = as.factor(MIU_VALVE)))+
  geom_line()+
  scale_color_manual(values = c('blue4','blue3','turquoise4','lightseagreen',
                                'mediumseagreen','limegreen','yellowgreen','yellow2',
                                'darkgoldenrod2','darkorange2','orangered1','red2'))

plotly::ggplotly(p)

slopes %>%
  ggplot(aes(x = CH4_slope_ppm_per_day, y = CH4_rmse))+
  geom_point(shape = 21)+
  facet_wrap(~MIU_VALVE)

p <- slopes %>%
  filter(CO2_slope_ppm_per_day > 0 | (hour(TIMESTAMP) > 8 & hour(TIMESTAMP) < 19)) %>%
  ggplot(aes(x = TIMESTAMP, y = CH4_slope_ppm_per_day, 
             color = as.factor(MIU_VALVE)))+
  geom_line()+
  scale_color_manual(values = c('blue4','blue3','turquoise4','lightseagreen',
                                'mediumseagreen','limegreen','yellowgreen','yellow2',
                                'darkgoldenrod2','darkorange2','orangered1','red2'))

plotly::ggplotly(p)

write.csv(slopes, 
          here::here("processed_data","L0_bestfits_all.csv"), 
          row.names = FALSE)
