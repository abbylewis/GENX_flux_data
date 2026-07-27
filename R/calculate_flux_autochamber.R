#' calculate_flux
#'
#' @description
#' This function calculates the raw CH4 fluxes for all files in the dropbox_downloads folder
#'
#' @param start_date earliest file to process (based on file name)
#' @param end_date latest file to process
#' @param modif_start_date only run files that have been modified/created since this date
#'
#' @return L0 slopes

calculate_flux <- function(start_date = NULL,
                           end_date = NULL,
                           reprocess = F) {
  
  ### Load files ###
  files <- autochamber::choose_files(
    input_folder = here::here("Raw_data", "dropbox_downloads"),
    l0_file_path = here::here("processed_data", "L0.csv"),
    reprocess = reprocess,
    start_date = start_date,
    end_date = end_date,
    files_to_exclude = c(
      "GENX_INSTRUMENT_FLUX_COMB_20240417020046.dat",
      "GENX_INSTRUMENT_FLUX_COMB_20240403020045.dat",
      "GENX_INSTRUMENT_FLUX_COMB_20240501020048.dat",
      "GENX_LGR_04142021_20210505020005.dat",
      "GENX_INSTRUMENT_FLUX_COMB_missing.dat"
    ))
  
  if (length(files) == 0) {
    message("No files to process")
    return(readr::read_csv(here::here("processed_data", "L0.csv"), show_col_types = F))
  }
  
  message(paste0("Calculating fluxes for ", length(files), " files"))
  
  # Load data
  data_small <- autochamber::load_loggernet_flux_data(
    files,
    format = "GENX"
  ) |>
    dplyr::filter(lubridate::year(TIMESTAMP) >= 2021,
                  !is.na(Chamber),
                  Chamber %in% 1:12
    )
  
  if("OLD" %in% data_small$Format){
    stop("Old data format detected. Cannot use autochamber package")
  }
  
  slopes <- autochamber::calculate_flux(
    data_small,
    cutoff_start = 240,
    cutoff_end = 510
  )
  
  if (!reprocess | !is.null(start_date)) {
    # Load previously calculated slopes
    old_slopes <- readr::read_csv(here::here("processed_data", "L0.csv"),
                           show_col_types = F
    ) |>
      dplyr::mutate(
        TIMESTAMP = lubridate::force_tz(TIMESTAMP, tz = "EST"),
        flux_start = lubridate::force_tz(flux_start, tz = "EST"),
        flux_end = lubridate::force_tz(flux_end, tz = "EST")
      ) |>
      dplyr::rename(Chamber = MIU_VALVE)
    #Combine
    slopes_comb <- autochamber::combine_slopes(new = slopes, old = old_slopes)
  } else {
    slopes_comb <- slopes
  }
  
  slopes_out <- autochamber::add_maintenance_log(
    slopes = slopes_comb,
    gs_url = "http://docs.google.com/spreadsheets/d/1_uk8-335NDJOdVU6OjLcxWx4MamNJeVEbVkSmdb9oRs/edit?gid=0#gid=0"
  ) |>
    dplyr::rename(MIU_VALVE = Chamber) #for compatibility downstream
  
  # Export errors
  if("Diag_7810" %in% colnames(data_small)) {
    data_errors <- data_small |>
      dplyr::select(TIMESTAMP, Chamber, Diag_7810, Diag_7820) |>
      dplyr::mutate(
        Diag_7810 = as.integer(dplyr::na_if(Diag_7810, "NAN")),
        Diag_7820 = as.integer(dplyr::na_if(Diag_7820, "NAN")),
        Chamber = as.integer(Chamber)
      )
    if (!reprocess | !is.null(start_date)) {
      # Load older data
      old_errors <- readr::read_csv(here::here("processed_data", "error_codes.csv"),
                             show_col_types = F
      ) |>
        dplyr::mutate(
          TIMESTAMP = lubridate::force_tz(TIMESTAMP, tz = "EST"),
          Diag_7810 = as.integer(Diag_7810),
          Diag_7820 = as.integer(Diag_7820),
          Chamber = as.integer(Chamber))
      #Combine
      errors_comb <- autochamber::combine_slopes(new = data_errors, old = old_errors)
    } else {
      errors_comb <- data_errors
    }
    
    errors_small <- errors_comb |>
      dplyr::filter(lubridate::second(TIMESTAMP) == 0)
    
    write.csv(errors_small,
              here::here("processed_data", "error_codes.csv"),
              row.names = FALSE
    )
  }
  
  # Output
  write.csv(
    slopes_out |>
      dplyr::mutate(
        dplyr::across(where(is.numeric), 
                      signif,
                      digits = 3)), #Trim file size
    here::here("processed_data", "L0.csv"),
    row.names = FALSE
  )

  write.csv(
    slopes_out |>
      dplyr::filter(TIMESTAMP > as.Date("2025-03-18")),
    here::here("processed_data", "L0_for_dashboard.csv"),
    row.names = FALSE
  )

  recent_raw <- autochamber::generate_recent_raw(data_small)

  write.csv(recent_raw,
    here::here("processed_data", "raw_for_dashboard.csv"),
    row.names = FALSE
  )

  return(slopes_out)
}


#calculate_flux(reprocess = T,
#              start_date = as.Date("2025-03-01"),
#              end_date = Sys.Date())
#
#test <- calculate_flux(reprocess = T,
#                       start_date = as.Date("2025-03-01"),
#                       end_date = as.Date("2025-12-01"))
#
# calculate_flux(reprocess = F)
