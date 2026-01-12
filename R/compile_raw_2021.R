
source(here::here("R","load_data.R"))
source(here::here("R","filter_old_data.R"))
source(here::here("R","group_fun.R"))

files <- list.files(here::here("Raw_data","dropbox_downloads"), full.names = T)

files_2021 <- files[grepl("2021", files)|grepl("20220112", files)]

exclude <- c("GENX_LGR_04142021_20210505020005.dat"
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
  mutate(date = as.Date(TIMESTAMP, tz = "America/New_York")) %>%
  #Group flux intervals
  arrange(TIMESTAMP) %>%
  mutate(group = group_fun(MIU_VALVE)) %>%
  group_by(group, MIU_VALVE)  %>%
  #Record the amount of time from when chamber closed
  mutate(start = min(TIMESTAMP),
         end = max(TIMESTAMP),
         change = as.numeric(difftime(TIMESTAMP, start, units = "days")),
         change_s = as.numeric(difftime(TIMESTAMP, start, units = "secs")),
         max_s = ifelse(sum(!is.na(CH4d_ppm) > 0),
                        change_s[which.max(CH4d_ppm)],
                        NA),
         min_s = ifelse(sum(!is.na(CH4d_ppm) > 0),
                        change_s[which.min(CH4d_ppm)],
                        NA),)

raw_2021 <- grouped_data %>%
  ungroup() %>%
  filter(year(date) == 2021) %>%
  select(TIMESTAMP, change, MIU_VALVE, CH4d_ppm, CO2d_ppm)

write.csv(raw_2021, 
          here::here("processed_data","raw_2021.csv"), 
          row.names = FALSE)
