source(here::here("R", "load_data.R"))
source(here::here("R", "filter_old_data.R"))
source(here::here("R", "group_fun.R"))
# requires zoo, tidyverse

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
                           reprocess = F,
                           plot = reprocess) {
  
  ### Load files ###
  files <- autochamber::choose_files(
    input_folder = here::here("Raw_data", "dropbox_downloads"),
    l0_file_path = here::here("processed_data", "L0.csv"),
    reprocess = F,
    start_date = NULL,
    end_date = NULL,
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

  exclude <- c(
    "GENX_INSTRUMENT_FLUX_COMB_20240417020046.dat",
    "GENX_INSTRUMENT_FLUX_COMB_20240403020045.dat",
    "GENX_INSTRUMENT_FLUX_COMB_20240501020048.dat",
    "GENX_LGR_04142021_20210505020005.dat"
  )
  files <- files[!grepl(paste0(exclude, collapse = "|"), files)]
  
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
      ) 
    #Combine
    slopes_comb <- autochamber::combine_slopes(old_slopes, slopes)
  } else {
    slopes_comb <- slopes
  }
  
  slopes_comb <- autochamber::add_maintenance_log(
    slopes = genx_fluxes,
    gs_url = "http://docs.google.com/spreadsheets/d/1_uk8-335NDJOdVU6OjLcxWx4MamNJeVEbVkSmdb9oRs/edit?gid=0#gid=0"
  )
  
  # Output
  write.csv(slopes_comb %>% select(-max_s), 
    here::here("processed_data", "L0.csv"),
    row.names = FALSE
  )

  write.csv(
    slopes_comb %>%
      select(-max_s) %>%
      filter(TIMESTAMP > as.Date("2025-03-18")),
    here::here("processed_data", "L0_for_dashboard.csv"),
    row.names = FALSE
  )

  recent_raw <- autochamber::generate_recent_raw(data_small)

  write.csv(recent_raw,
    here::here("processed_data", "raw_for_dashboard.csv"),
    row.names = FALSE
  )

  if (plot) {
    for (year_i in unique(year(slopes$TIMESTAMP))) {
      p <- slopes %>%
        filter(
          max_s <= 1000,
          month(date) %in% c(1:12)
        ) %>%
        mutate(MIU_VALVE = factor(MIU_VALVE,
          levels = c(
            1, 4, 7, 10,
            3, 6, 9, 12,
            2, 5, 8, 11
          )
        )) %>%
        filter(year(TIMESTAMP) == year_i) %>%
        ggplot(aes(x = TIMESTAMP, y = max_s)) +
        geom_point(alpha = 0.02) +
        geom_line(aes(y = cutoff, x = as.POSIXct(date)), color = "red") +
        facet_wrap(~MIU_VALVE) +
        ggtitle(year_i) +
        xlab("Date") +
        ylab("Time to peak (s)") +
        theme_bw()
      jpeg(here::here("figures", paste0("TimeToPeak_", year_i, ".jpeg")), width = 6, height = 5, units = "in", res = 300)
      print(p)
      dev.off()
    }
  }

  return(slopes_comb)
}


# calculate_flux(reprocess = T,
#               start_date = as.Date("2025-03-01"),
#               end_date = Sys.Date())


# calculate_flux(reprocess = F)
