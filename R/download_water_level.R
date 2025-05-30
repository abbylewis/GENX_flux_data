#Source
source("./R/drop_dir.R")
source("./R/get_dropbox_token.R")
source("./R/load_file.R")
library(tidyverse)

download_water_level <- function(water_level_folder = here::here("Raw_data", "dropbox_water_level")){
  #Identify all files
  files <- drop_dir(path = "GCREW_LOGGERNET_DATA/archive_data")
  relevant_files <- files %>%
    filter(grepl("GENX_WaterLevel", name))
  current <- drop_dir(path = "GCREW_LOGGERNET_DATA/current_data") %>%
    filter(grepl("GENX_WaterLevel", name),
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
    map(read.csv, skip = 1) %>%
    bind_rows() %>%
    filter(!TIMESTAMP == "TS") %>%
    mutate(TIMESTAMP = as_datetime(TIMESTAMP)) %>%
    filter(!is.na(TIMESTAMP)) %>%
    distinct()
  
  metadata <- read_csv(here::here("Raw_data","chamber_metadata.csv"),
                       show_col_types = F)
  
  water_level_output <- data %>%
    filter(Statname == "GENX") %>%
    select(c(TIMESTAMP, Depth, Temperature, Specific_Conductivity, Salinity, TDS))
  
  write.csv(water_level_output, 
            here::here("processed_data", "water_level.csv"), 
            row.names = FALSE)
  
  return(T)
}