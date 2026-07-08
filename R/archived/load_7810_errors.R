
load_7810_errors <- function(start_date = NULL,
                             end_date = NULL,
                             reprocess = F) {
  
  errors <- autochamber::choose_files(
    input_folder = here::here("Raw_data", "with_error_codes"),
    l0_file_path = here::here("processed_data", "error_codes_7810.csv"),
    reprocess = reprocess,
    start_date = start_date,
    end_date = end_date,
    files_to_exclude = c("GENX_INSTRUMENT_FLUX_7810_20250228235902.dat"))
  
  if (length(errors) == 0) {
    message("No files to process")
    return(read_csv(here::here("processed_data", "error_codes_7810.csv"), show_col_types = F))
  }
  
  data_errors <- errors |>
    purrr::map(load_single_file_error) |> # custom data loading function that deals with multiple file formats
    dplyr::bind_rows() |>
    dplyr::distinct() |>
    dplyr::mutate(Chamber = as.numeric(Chamber)) |>
    filter(year(TIMESTAMP) >= 2021,
           !is.na(Chamber),
           Chamber %in% 1:12
    )
  
  if (!reprocess | !is.null(start_date)) {
    # Load older data
    old_errors <- read_csv(here::here("processed_data", "error_codes_7810.csv"),
                           show_col_types = F
    ) %>%
      mutate(
        TIMESTAMP = force_tz(TIMESTAMP, tz = "EST"))
    #Combine
    errors_comb <- autochamber::combine_slopes(new = data_errors, old = old_errors)
  } else {
    errors_comb <- data_errors
  }
  
  write.csv(errors_comb,
            here::here("processed_data", "error_codes_7810.csv"),
            row.names = FALSE
  )
  
  return(data_errors)
}

load_single_file_error <- function(file) {
  data_raw <- readr::read_csv(
    file,
    col_types = readr::cols(.default = "c"), skip = 1
  ) |>
    dplyr::filter(!TIMESTAMP == "TS") |>
    dplyr::mutate(TIMESTAMP = lubridate::as_datetime(TIMESTAMP, tz = "EST")) |>
    dplyr::filter(
      !is.na(TIMESTAMP),
      lubridate::year(TIMESTAMP) >= 2021
    )
  
  data_small <- data_raw |>
    dplyr::rename(
      Chamber = Fluxing_Chamber
    ) |>
    dplyr::select(
      TIMESTAMP, Chamber, Diag_7810
    ) |>
    dplyr::mutate(
      Diag_7810 = as.numeric(na_if(Diag_7810, "NAN"))
    ) |>
    dplyr::filter(!is.na(Chamber), Chamber %in% 1:12)
  
  return(data_small)
}

#data_errors <- load_7810_errors(reprocess = F)

#data_errors %>%
#  filter(TIMESTAMP > as_date(Sys.Date() - days(100) - years(1))) %>%
#  ggplot(aes(x = TIMESTAMP, y = Diag_7810, color = Chamber))+
#  geom_point()
#