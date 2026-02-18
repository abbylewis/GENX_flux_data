#Source
source("./R/drop_dir.R")
source("./R/get_dropbox_token.R")
source("./R/load_file.R")
library(tidyverse)

download_gcrew_met <- function(gcrew_met_folder = here::here("Raw_data", "dropbox_met_gcrew")){
  #Identify all files
  files <- drop_dir(path = "GCREW_LOGGERNET_DATA/archive_data")
  relevant_files <- files %>%
    filter(grepl("GCREW_MET_15", name))
  current <- drop_dir(path = "GCREW_LOGGERNET_DATA/current_data") %>%
    filter(grepl("GCREW_MET_15", name))
  
  #Remove files that are already loaded
  already_loaded <- list.files(gcrew_met_folder)
  relevant_files <- relevant_files %>%
    filter(!name %in% already_loaded)
  
  #Load current data
  new <- current$path_display %>%
    map(load_file, output_dir = gcrew_met_folder)
  
  if(nrow(relevant_files) == 0){
    message("No new files to download")
  } else {
    message("Downloading ", nrow(relevant_files), " files")
    all_data <- relevant_files$path_display %>%
      map(load_file, output_dir = gcrew_met_folder)
  }
  
  message("Processing and saving all historical met data")
  
  data <- list.files(gcrew_met_folder, full.names = T) %>%
    map(read.csv, skip = 1) %>%
    bind_rows() %>%
    filter(!TIMESTAMP == "TS") %>%
    mutate(TIMESTAMP = as_datetime(TIMESTAMP)) %>%
    filter(!is.na(TIMESTAMP)) %>%
    distinct()
  
  data %>%
    mutate(WS_ms_WS_Avg = as.numeric(WS_ms_WS_Avg),
           WS_ms_RM_Avg = as.numeric(WS_ms_RM_Avg),
           Salinity = as.numeric(Salinity)) %>%
    ggplot(aes(x = TIMESTAMP, y = Salinity))+
    geom_point()
  
  write_csv(data %>%
              filter(TIMESTAMP >= as.Date("2025-03-18")) %>%
              select(all_of(c("TIMESTAMP", "PAR_Den_C_Avg", "AirTC_Avg", "Rain_cm_Tot", "Barometric_Pressure_PB110B_Avg", "Salinity", "WS_ms_RM_Avg"))) , 
            here::here("processed_data", "met_2025_dashboard.csv"))
  
  write_csv(data %>%
              select(all_of(c("TIMESTAMP", "PAR_Den_C_Avg", "AirTC_Avg"))), 
            here::here("processed_data", "met_all.csv"))
  return(T)
}