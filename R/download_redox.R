#Source
source("R/drop_dir.R")
source("R/get_dropbox_token.R")
source("R/load_file.R")
source("R/load_data.R")
library(tidyverse)

download_redox <- function(redox_folder = here::here("Raw_data", "dropbox_redox")){
  #Identify all files
  files <- drop_dir(path = "GCREW_LOGGERNET_DATA/archive_data")
  relevant_files <- files %>%
    filter(grepl("genx_redox", tolower(name)))
  current <- drop_dir(path = "GCREW_LOGGERNET_DATA/current_data") %>%
    filter(grepl("genx_redox", tolower(name)),
           !grepl("backup", name))
  
  #Remove files that are already loaded
  already_loaded <- list.files(redox_folder)
  relevant_files <- relevant_files %>%
    filter(!name %in% already_loaded)
  
  #Load current data
  new <- current$path_display %>%
    map(load_file, output_dir = redox_folder)
  
  if(nrow(relevant_files) == 0){
    message("No new files to download")
  } else {
    message("Downloading ", nrow(relevant_files), " files")
    all_data <- relevant_files$path_display %>%
      map(load_file, output_dir = redox_folder)
  }
  
  message("Processing and saving all historical redox data")
  
  file <- list.files(redox_folder, full.names = T)[grepl("SWAP", list.files(redox_folder, full.names = T))][[1]]
  data <- list.files(redox_folder, full.names = T) %>%
    map(load_redox) %>%
    bind_rows() %>%
    filter(!TIMESTAMP == "TS") %>%
    mutate(TIMESTAMP = as_datetime(TIMESTAMP)) %>%
    filter(!is.na(TIMESTAMP)) %>%
    distinct()
  
  redox_output <- data %>%
    select(c(TIMESTAMP, Depth_cm, Temperature_C, Actual_Conductivity_uScm, Salinity_PSU)) %>%
    mutate(Depth_cm = Depth_cm - 76)
  
  write.csv(redox_output, 
            here::here("processed_data", "redox.csv"), 
            row.names = FALSE)

  write.csv(redox_output %>%
              filter(TIMESTAMP > as.Date("2025-03-18")), 
            here::here("processed_data", "redox_dashboard.csv"), 
            row.names = FALSE)
  
  return(T)
}
