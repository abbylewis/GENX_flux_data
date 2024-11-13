#Source
source(here::here("R","drop_dir.R"))
source(here::here("R","get_dropbox_token.R"))
library(tidyverse)

#' download_new_data
#'
#' @description
#' This function looks for data files on dropbox that are new or have been modified since we last loaded data
#' 
#' @return NULL
#' @export
#'
#' @examples
download_new_data <- function(flux_folder = here::here("Raw_data","dropbox_downloads")){
  #Identify all files
  #GENX flux vs GENX LGR in 2021
  message("Looking for new data files on dropbox")
  relevant_files <- drop_dir(path = "GCREW_LOGGERNET_DATA") %>%
    filter(grepl("GENX_INSTRUMENT_FLUX_COMB", name) |
             grepl("GENX_FLUX_", name) |
             grepl("GENX_LGR_", name))
  current <- drop_dir(path = "GCREW_LOGGERNET_DATA/current_data") %>%
    filter(grepl("GENX_INSTRUMENT_FLUX_COMB", name),
           !grepl("backup", name))
  
  #Remove files that are already loaded and haven't been modified
  already_loaded <- list.files(flux_folder)
  loaded_file_info <- file.info(list.files(flux_folder, full.names = T)) %>%
    mutate(name = basename(row.names(.))) %>%
    select(name, mtime)
  modified <- relevant_files %>%
    select(name, server_modified) %>%
    left_join(loaded_file_info, by = "name") %>%
    filter(server_modified > mtime)
  relevant_files <- relevant_files %>% #Only process files that are new or have been modified on dropbox
    filter(!name %in% already_loaded | name %in% modified$name)
  
  #Load current data file
  new <- current$path_display %>%
    map(load_file, flux_folder = flux_folder) #NOTE: this function is further down in this script
  
  if(nrow(relevant_files) == 0){
    message("No new files to download")
  } else {
    message("Downloading ", nrow(relevant_files), " files")
    all_data <- relevant_files$path_display %>%
      map(load_file, flux_folder = flux_folder)
  }
}

#Load file - helper function
load_file <- function(path_display, flux_folder){
  url <- "https://content.dropboxapi.com/2/files/download"
  name <- sub("/GCREW_LOGGERNET_DATA/", "", path_display)
  if(grepl("current", name)) name <- "current.dat"
  
  httr::POST(
    url = url,
    httr::config(token = get_dropbox_token()),
    httr::add_headers("Dropbox-API-Arg" = jsonlite::toJSON(
      list(
        path = path_display
      ),
      auto_unbox = TRUE
    )),
    httr::write_disk(here::here(flux_folder, name), overwrite = T)
  )
}
