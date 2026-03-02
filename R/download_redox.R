# Source
source("R/drop_dir.R")
source("R/get_dropbox_token.R")
source("R/load_file.R")
source("R/load_data.R")
library(tidyverse)

download_redox <- function(redox_folder = here::here("Raw_data", "dropbox_redox")) {
  # Identify all files
  files <- drop_dir(path = "GCREW_LOGGERNET_DATA/archive_data")
  relevant_files <- files %>%
    filter(grepl("genx_redox", tolower(name)))
  current <- drop_dir(path = "GCREW_LOGGERNET_DATA/current_data") %>%
    filter(
      grepl("genx_redox", tolower(name)),
      !grepl("backup", name)
    )

  # Remove files that are already loaded
  already_loaded <- list.files(redox_folder)
  relevant_files <- relevant_files %>%
    filter(!name %in% already_loaded)

  # Load current data
  new <- current$path_display %>%
    map(load_file, output_dir = redox_folder)

  if (nrow(relevant_files) == 0) {
    message("No new files to download")
  } else {
    message("Downloading ", nrow(relevant_files), " files")
    all_data <- relevant_files$path_display %>%
      map(load_file, output_dir = redox_folder)
  }

  message("Processing and saving all historical redox data")

  # file <- list.files(redox_folder, full.names = T)[!grepl("SWAP", list.files(redox_folder, full.names = T))][[1]]
  data <- list.files(redox_folder, full.names = T)[!grepl("SWAP", list.files(redox_folder, full.names = T))] %>%
    map(load_redox) %>%
    bind_rows() %>%
    filter(!TIMESTAMP == "TS") %>%
    mutate(
      TIMESTAMP = as_datetime(TIMESTAMP),
      REDOX_mV = as.numeric(REDOX_mV)
    ) %>%
    filter(!is.na(TIMESTAMP)) %>%
    distinct()

  redox_output <- data %>%
    select(c(TIMESTAMP, MIU_VALVE, Depth_cm, probe, reference, REDOX_mV)) %>%
    mutate(Date = as.Date(TIMESTAMP)) %>%
    group_by(Date, reference, probe, Depth_cm, MIU_VALVE) %>%
    summarize(REDOX_mV = mean(REDOX_mV, na.rm = T))

  p <- redox_output %>%
    filter(
      Date > as.Date("2025-03-18"),
      Depth_cm == 25,
      reference == "ref1",
      !probe == "P54"
    ) %>%
    group_by(MIU_VALVE, Date) %>%
    summarize(REDOX_mV = mean(REDOX_mV, na.rm = T)) %>%
    mutate(MIU_VALVE = factor(MIU_VALVE,
      levels = 1:12,
      labels = chamber_levels2
    )) %>%
    ggplot(aes(x = Date, y = REDOX_mV, color = MIU_VALVE)) +
    geom_line() +
    scale_color_manual(values = color.gradient)
  plotly::ggplotly(p)

  redox_output %>%
    ggplot(aes(x = Date, y = REDOX_mV, group = paste(reference, probe), color = reference)) +
    geom_line() +
    facet_grid(MIU_VALVE ~ Depth_cm)


  write.csv(redox_output,
    here::here("processed_data", "redox.csv"),
    row.names = FALSE
  )

  write.csv(
    redox_output %>%
      filter(Date > as.Date("2025-03-18")),
    here::here("processed_data", "redox_dashboard.csv"),
    row.names = FALSE
  )

  return(T)
}
