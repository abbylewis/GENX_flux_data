#Source
source("./R/drop_dir.R")
source("./R/get_dropbox_token.R")
source("./R/load_file.R")
library(tidyverse)

download_tower_met <- function(tower_met_folder = here::here("Raw_data", "dropbox_met")){
  #Identify all files
  files <- drop_dir(path = "SERC_Tower_MET/SERC_Tower_Rawdata_Loggernet/SERC_Tower_Rawdata_Archive")
  relevant_files <- files %>%
    filter(grepl("SERC_TOWER_MET_SERC_TOWER", name))
  current <- drop_dir(path = "SERC_Tower_MET/SERC_Tower_Rawdata_Loggernet/SERC_Tower_current_data") %>%
    filter(grepl("SERC_TOWER_MET_SERC_TOWER", name),
           !grepl("backup|_B", name))
  
  #Remove files that are already loaded
  already_loaded <- list.files(tower_met_folder)
  relevant_files <- relevant_files %>%
    filter(!name %in% already_loaded)
  
  #Load current data
  new <- current$path_display %>%
    map(load_file, output_dir = tower_met_folder)
  
  if(nrow(relevant_files) == 0){
    message("No new files to download")
  } else {
    message("Downloading ", nrow(relevant_files), " files")
    all_data <- relevant_files$path_display %>%
      map(load_file, output_dir = tower_met_folder)
  }
  
  message("Processing and saving all historical met data")
  
  data <- list.files(tower_met_folder, full.names = T) %>%
    map(read.csv, skip = 1) %>%
    bind_rows() %>%
    filter(!TIMESTAMP == "TS") %>%
    mutate(TIMESTAMP = as_datetime(TIMESTAMP)) %>%
    filter(!is.na(TIMESTAMP)) %>%
    distinct()
  
  write_csv(data %>%
              select(all_of(c("TIMESTAMP", "Wind_direction_avg", "Wind_speed_avg", 
                              "Wind_speed_max", "Air_Temperature", "Relative_Humidity", 
                              "Air_Pressure", "Rain_Accumulation", "Rain_Duration", 
                              "Rain_Intensity"))), 
            here::here("processed_data", "met.csv"))
  
  write_csv(data %>%
              filter(TIMESTAMP >= as.Date("2025-03-18")) %>%
              select(all_of(c("TIMESTAMP", "Wind_direction_avg", "Wind_speed_avg", 
                              "Wind_speed_max", "Air_Temperature", "Relative_Humidity", 
                              "Air_Pressure", "Rain_Accumulation", "Rain_Duration", 
                              "Rain_Intensity"))), 
            here::here("processed_data", "met_2025.csv"))
  
  return(T)
}