source(here::here("R", "drop_dir.R"))
source(here::here("R", "load_file.R"))
source(here::here("R", "get_dropbox_token.R"))
library(tidyverse)

#Save files to computer
files <- drop_dir(path = "Hiremutt Projects/GENX Heating/GENX_data/NormalizedData/genx_export/2025_combined")%>%
  filter(.tag == "file")
all_data <- files$path_display %>%
  map(load_file, output_dir = here::here("Raw_data/soil_temp"))

#Load and format
soil_temp <- list.files(here::here("Raw_data/soil_temp"), 
                        full.names = T,
                        pattern = "Export") %>%
  map(read_csv, show_col_type = F) %>%
  bind_rows() %>%
  mutate(chamber = ifelse(plot_id == "c1", NA, chamber),
         chamber = ifelse(plot_id == "c0", 2, chamber),
         chamber = ifelse(plot_id == "b9", NA, chamber),
         chamber = ifelse(plot_id == "a9", 12, chamber),
         chamber = ifelse(plot_id == "b6", NA, chamber),
         chamber = ifelse(plot_id == "b7", 9, chamber)
         ) %>%
  filter(!is.na(chamber)) %>%
  select(timestamp, chamber, btemp_avg) %>%
  rename(MIU_VALVE = chamber, Temp_C = btemp_avg)

#Save
write_csv(soil_temp, here::here("processed_data", "soil_temp.csv"))

## REPEAT WITH IN-CHAMBER DATA
#Save files to computer
files <- drop_dir(path = "Hiremutt Projects/GENX Heating/GENX_data/Rolling_MSD_Data/yearly_RMSD_genx_ardlog")
all_data <- files$path_display[grepl("2024", files$path_display)] %>%
  map(load_file, output_dir = here::here("Raw_data/soil_temp"))

#Load and format
soil_temp_chamber <- list.files(here::here("Raw_data/soil_temp"), 
                        full.names = T,
                        pattern = "RMSD_norm_GENX_ARD_LOG_2025") %>%
  map(read_csv, show_col_type = F) %>%
  bind_rows() %>%
  filter(chamber %in% 1:12) %>%
  select(timestamp, chamber, new_therm25c_rm) %>%
  rename(MIU_VALVE = chamber, Temp_C = new_therm25c_rm) %>%
  # Every minute is unnecessarily huge
  mutate(timestamp = floor_date(timestamp, "15 minutes")) %>%
  group_by(MIU_VALVE, timestamp) %>%
  summarise(
    Temp_C = mean(Temp_C, na.rm = TRUE),
    .groups = "drop"
  )

#Save
write_csv(soil_temp_chamber, here::here("processed_data", "soil_temp_chamber.csv"))
