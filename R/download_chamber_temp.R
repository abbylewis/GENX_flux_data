# Source
source("./R/drop_dir.R")
source("./R/get_dropbox_token.R")
source("./R/load_file.R")
library(tidyverse)

download_chamber_temp <- function(chamber_temp_folder = here::here("Raw_data", "dropbox_chamber_temp")) {
  # Identify all files
  files <- drop_dir(path = "GCREW_LOGGERNET_DATA/archive_data")
  relevant_files <- files %>%
    filter(grepl("GENX_ARD_LOG", name))
  current <- drop_dir(path = "GCREW_LOGGERNET_DATA/current_data") %>%
    filter(
      grepl("GENX_ARD_LOG", name),
      !grepl("backup", name)
    )

  # Remove files that are already loaded
  already_loaded <- list.files(chamber_temp_folder)
  relevant_files <- relevant_files %>%
    filter(!name %in% already_loaded)

  # Load current data
  new <- current$path_display %>%
    map(load_file, output_dir = chamber_temp_folder)

  if (nrow(relevant_files) == 0) {
    message("No new files to download")
  } else {
    message("Downloading ", nrow(relevant_files), " files")
    all_data <- relevant_files$path_display %>%
      map(load_file, output_dir = chamber_temp_folder)
  }

  message("Processing and saving all historical temp data")

  files <- list.files(chamber_temp_folder, full.names = T)
  files <- files[grepl("2025", files) | grepl("2026", files) | grepl("current", files)]
  
  data <- files %>%
    map(read_csv, skip = 1, show_col_types = F) %>%
    bind_rows() %>%
    filter(!TIMESTAMP == "TS") %>%
    mutate(TIMESTAMP = as_datetime(TIMESTAMP)) %>%
    filter(!is.na(TIMESTAMP)) %>%
    distinct()

  metadata <- read_csv(here::here("Raw_data", "chamber_metadata.csv"),
    show_col_types = F
  )

  temp_output <- data %>%
    select(c(TIMESTAMP, starts_with("BMETemp"))) %>%
    pivot_longer(cols = -TIMESTAMP, names_to = "chamber", values_to = "AirTemp_C") %>%
    mutate(
      chamber = str_extract(sub("BMETemp.", "", chamber), "[0-9]+"),
      AirTemp_C = as.numeric(AirTemp_C),
      Chamber = as.numeric(chamber)
    ) %>%
    filter(
      !AirTemp_C %in% c(0),
      !AirTemp_C < -40,
      !AirTemp_C > 100,
      !(AirTemp_C < 0 & month(TIMESTAMP) %in% c(6, 7, 8))
    ) %>%
    select(Chamber, TIMESTAMP, AirTemp_C) %>%
    filter(Chamber %in% 1:12)
  
  soil_temp_output <- data %>%
    select(c(TIMESTAMP, starts_with("Therm25C"))) %>%
    pivot_longer(cols = -TIMESTAMP, names_to = "chamber", values_to = "SoilTemp_25cm_C") %>%
    mutate(
      chamber = str_extract(sub("Therm25C", "", chamber), "[0-9]+"),
      SoilTemp_25cm_C = as.numeric(SoilTemp_25cm_C),
      Chamber = as.numeric(chamber)
    )  %>%
    select(Chamber, TIMESTAMP, SoilTemp_25cm_C) %>%
    filter(Chamber %in% 1:12)

  temp_output_daily <- temp_output %>%
    mutate(
      Date = as.Date(TIMESTAMP),
      AirTemp_C = as.numeric(AirTemp_C)
    ) %>%
    group_by(Date, Chamber) %>%
    summarise(
      AirTemp_C = mean(AirTemp_C, na.rm = TRUE),
      .groups = "drop"
    )
  
  soil_temp_output_daily <- soil_temp_output %>%
    mutate(
      Date = as.Date(TIMESTAMP),
      SoilTemp_25cm_C = as.numeric(SoilTemp_25cm_C)
    ) %>%
    group_by(Date, Chamber) %>%
    summarise(
      SoilTemp_25cm_C = mean(SoilTemp_25cm_C, na.rm = TRUE),
      .groups = "drop"
    )

  #write.csv(temp_output,
  #  here::here("processed_data", "chamber_temp_1min.csv"),
  #  row.names = FALSE
  #)

  write.csv(temp_output_daily,
    here::here("processed_data", "chamber_temp_daily.csv"),
    row.names = FALSE
  )
  
  write.csv(soil_temp_output_daily,
            here::here("processed_data", "soil_temp_daily.csv"),
            row.names = FALSE
  )

  return(T)
}
