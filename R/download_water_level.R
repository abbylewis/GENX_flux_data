#Source
source("R/drop_dir.R")
source("R/get_dropbox_token.R")
source("R/load_file.R")
source("R/load_data.R")
library(tidyverse)

download_water_level <- function(water_level_folder = here::here("Raw_data", "dropbox_water_level")){
  #Identify all files
  files <- drop_dir(path = "GCREW_LOGGERNET_DATA/archive_data")
  relevant_files <- files %>%
    filter(grepl("genx_waterlevel", tolower(name)))
  current <- drop_dir(path = "GCREW_LOGGERNET_DATA/current_data") %>%
    filter(grepl("genx_waterlevel", tolower(name)),
           !grepl("backup", name))
  
  #Remove files that are already loaded
  already_loaded <- list.files(water_level_folder)
  relevant_files <- relevant_files %>%
    filter(!name %in% already_loaded)
  
  #Load current data
  new <- current$path_display %>%
    map(load_file, output_dir = water_level_folder)
  
  if(nrow(relevant_files) == 0){
    message("No new files to download")
  } else {
    message("Downloading ", nrow(relevant_files), " files")
    all_data <- relevant_files$path_display %>%
      map(load_file, output_dir = water_level_folder)
  }
  
  message("Processing and saving all historical water level data")
  
  data <- list.files(water_level_folder, full.names = T) %>%
    map(load_wl) %>%
    bind_rows() %>%
    filter(!TIMESTAMP == "TS") %>%
    mutate(TIMESTAMP = as_datetime(TIMESTAMP)) %>%
    filter(!is.na(TIMESTAMP)) %>%
    distinct()
  
  water_level_output <- data %>%
    select(c(TIMESTAMP, Depth_cm, Temperature_C, Actual_Conductivity_uScm, Salinity_PSU)) %>%
    mutate(Depth_cm = ifelse(TIMESTAMP > as_datetime("2025-09-01"), Depth_cm - 56, Depth_cm - 76))
  
  write.csv(water_level_output, 
            here::here("processed_data", "water_level.csv"), 
            row.names = FALSE)

  write.csv(water_level_output %>%
              filter(TIMESTAMP > as.Date("2025-03-18")), 
            here::here("processed_data", "water_level_dashboard.csv"), 
            row.names = FALSE)
  
  return(T)
}