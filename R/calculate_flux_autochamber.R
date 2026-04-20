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
      "GENX_LGR_04142021_20210505020005.dat"
    ))

  if (length(files) == 0) {
    message("No files to process")
    return(read_csv(here::here("processed_data", "L0.csv"), show_col_types = F))
  }

  message(paste0("Calculating fluxes for ", length(files), " files"))

  # Load data
  data_small <- autochamber::load_loggernet_flux_data(
    files,
    format = "GENX"
    ) %>%
    filter(year(TIMESTAMP) >= 2021,
           !is.na(Chamber),
           Chamber %in% 1:12
    )
  
  if("OLD" %in% data_small$Format){
    stop("Old data format detected. Cannot use autochamber package")
  }
  
  slopes <- autochamber::calculate_flux(
    data_small,
    cutoff_start = 220,
    cutoff_end = 510
    )

  if (!reprocess | !is.null(start_date)) {
    # Load previously calculated slopes
    old_slopes <- read_csv(here::here("processed_data", "L0.csv"),
      show_col_types = F
    ) %>%
      mutate(
        TIMESTAMP = force_tz(TIMESTAMP, tz = "EST"),
        flux_start = force_tz(flux_start, tz = "EST"),
        flux_end = force_tz(flux_end, tz = "EST")
      ) %>%
      rename(Chamber = MIU_VALVE)
    #Combine
    slopes_comb <- autochamber::combine_slopes(old_slopes, slopes)
  } else {
    slopes_comb <- slopes
  }
  
  slopes_out <- autochamber::add_maintenance_log(
    slopes = slopes_comb,
    gs_url = "http://docs.google.com/spreadsheets/d/1_uk8-335NDJOdVU6OjLcxWx4MamNJeVEbVkSmdb9oRs/edit?gid=0#gid=0"
  ) %>%
    rename(MIU_VALVE = Chamber) #for compatibility downstream
  
  # Output
  write.csv(slopes_out %>%
              mutate(across(where(is.numeric), 
                            signif,
                            digits = 3)), #Trim file size
    here::here("processed_data", "L0.csv"),
    row.names = FALSE
  )

  write.csv(
    slopes_out %>%
      filter(TIMESTAMP > as.Date("2025-03-18")),
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


# calculate_flux(reprocess = T,
#               start_date = as.Date("2025-03-01"),
#               end_date = Sys.Date())


# calculate_flux(reprocess = F)
