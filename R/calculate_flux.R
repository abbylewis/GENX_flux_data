
source(here::here("R","load_data.R"))
source(here::here("R","filter_old_data.R"))
source(here::here("R","group_fun.R"))

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
                           plot = reprocess){
  ### Load files ###
  files <- list.files(here::here("Raw_data","dropbox_downloads"), full.names = T)
  #By default, only calculate slopes for files that have been modified/created since the last time we ran the script
  if(!reprocess){
    modif_start_date = file.info(here::here("processed_data","L0.csv"))$mtime
    files <- files[file.info(files)$mtime > modif_start_date]
  }
  #If a start and end date are provided, look for files that match these dates
  if(!is.null(start_date) & !is.null(end_date)){
    possible_file_names <- seq(as.Date(start_date), 
                               as.Date(end_date), 
                               by = "1 day") %>%
      format("%Y%m%d")
    if(as.Date(end_date) >= Sys.Date()) {possible_file_names <- c(possible_file_names, "current.dat")}
    files <- files[grepl(paste0(possible_file_names, collapse = "|"), files)]
  } else if (!is.null(start_date) | !is.null(end_date)){
    stop("If you provide a start or end date, you must provide both")
  } 
  
  if(length(files) == 0){
    message("No files to process")
    return(read_csv(here::here("processed_data","L0.csv"), show_col_types = F))
  }
  
  exclude <- c("GENX_INSTRUMENT_FLUX_COMB_20240417020046.dat",
               "GENX_INSTRUMENT_FLUX_COMB_20240403020045.dat",
               "GENX_INSTRUMENT_FLUX_COMB_20240501020048.dat",
               "GENX_LGR_04142021_20210505020005.dat"
  )
  files <- files[!grepl(paste0(exclude, collapse = "|"), files)]
  message(paste0("Calculating fluxes for ", length(files), " files"))
  
  #Load data
  data_small <- files %>%
    map(load_data) %>% #custom data loading function that deals with multiple file formats
    bind_rows()  %>%
    filter(!TIMESTAMP == "TS") %>%
    mutate(TIMESTAMP = as_datetime(TIMESTAMP, tz = "EST")) %>%
    filter(!is.na(TIMESTAMP),
           year(TIMESTAMP)>=2021) %>%
    distinct()
  
  #Format data
  data_numeric <- data_small %>%
    mutate(across(c("CH4d_ppm", "CO2d_ppm", "N2Od_ppb", "Manifold_Timer", "MIU_VALVE"), as.numeric),
           N2Od_ppb = ifelse(N2Od_ppb <=0, NA, N2Od_ppb),
           CH4d_ppm = ifelse(CH4d_ppm <=0, NA, CH4d_ppm),
           CO2d_ppm = ifelse(CO2d_ppm <=0, NA, CO2d_ppm),
           N2Od_ppm = N2Od_ppb / 1000) %>%
    select(-N2Od_ppb) %>%
    filter(!is.na(MIU_VALVE),
           MIU_VALVE %in% 1:12) %>%
    mutate(Flag = "No issues")
  
  #Remove data as specified in maintenance log
  googlesheets4::gs4_deauth() # No authentication needed
  today <- Sys.time()
  maint_log <- googlesheets4::read_sheet("http://docs.google.com/spreadsheets/d/1_uk8-335NDJOdVU6OjLcxWx4MamNJeVEbVkSmdb9oRs/edit?gid=0#gid=0",
                                         col_types = "c") %>%
    mutate(Start_time = as_datetime(Start_time, tz = "America/New_York"),
           End_time = as_datetime(End_time, tz = "America/New_York"),
           End_time = ifelse(is.na(End_time), today, End_time),
           End_time = as_datetime(End_time, tz = "America/New_York"))
  for(i in 1:nrow(maint_log)){
    data_numeric <- data_numeric %>%
      mutate(Flag = ifelse(TIMESTAMP <= maint_log$End_time[i] & 
                             TIMESTAMP >= maint_log$Start_time[i] &
                             MIU_VALVE %in% eval(parse(text = maint_log$Chambers[i])),
                           maint_log$Flag[i],
                           Flag),
             CH4d_ppm = ifelse(TIMESTAMP <= maint_log$End_time[i] & 
                                 TIMESTAMP >= maint_log$Start_time[i] &
                                 maint_log$Remove[i] == "y" &
                                 maint_log$Analyzer[i] %in% c("CO2/CH4", "all") &
                                 MIU_VALVE %in% eval(parse(text = maint_log$Chambers[i])),
                               NA,
                               CH4d_ppm),
             CO2d_ppm = ifelse(TIMESTAMP <= maint_log$End_time[i] & 
                                 TIMESTAMP >= maint_log$Start_time[i] &
                                 maint_log$Remove[i] == "y" &
                                 maint_log$Analyzer[i] %in% c("CO2/CH4", "all") &
                                 MIU_VALVE %in% eval(parse(text = maint_log$Chambers[i])),
                               NA,
                               CO2d_ppm),
             N2Od_ppm = ifelse(TIMESTAMP <= maint_log$End_time[i] & 
                                 TIMESTAMP >= maint_log$Start_time[i] &
                                 maint_log$Remove[i] == "y" &
                                 maint_log$Analyzer[i] %in% c("N2O", "all") &
                                 MIU_VALVE %in% eval(parse(text = maint_log$Chambers[i])),
                               NA,
                               N2Od_ppm))
  }
  
  #Group flux intervals, prep for slopes
  grouped_data <- data_numeric %>%
    #Group flux intervals
    arrange(TIMESTAMP) %>%
    mutate(group = group_fun(MIU_VALVE)) %>%
    group_by(group, MIU_VALVE)  %>%
    #Record the amount of time from when chamber closed
    mutate(start = min(TIMESTAMP),
           end = max(TIMESTAMP),
           date = as.Date(start, tz = "America/New_York"),
           change = as.numeric(difftime(TIMESTAMP, start, units = "days")),
           change_s = as.numeric(difftime(TIMESTAMP, start, units = "secs")),
           max_s = ifelse(sum(!is.na(CH4d_ppm) > 0),
                          change_s[which.max(CH4d_ppm)],
                          NA),
           min_s = ifelse(sum(!is.na(CH4d_ppm) > 0),
                          change_s[which.min(CH4d_ppm)],
                          NA),)
  
  #Save flags for data that will be removed in the next step
  flags <- grouped_data %>%
    ungroup() %>%
    select(start, MIU_VALVE, Flag, date, group) %>%
    distinct() %>%
    group_by(MIU_VALVE, start, date, group) %>%
    filter(n() == 1 | !Flag == "No issues")
  
  #Set aside data after the system switched to process differently
  time_split <- split(grouped_data, grouped_data$Format)
  #Process new format
  start_cutoff <- 200 #Buffer of time after flux window
  end_cutoff <- 540
  filtered_data_new <- time_split$NEW %>%
    group_by(group, MIU_VALVE)  %>%
    mutate(n = sum(Manifold_Timer >= start_cutoff &
                     Manifold_Timer <= end_cutoff),
           cutoff = NA) %>%
    #Remove earlier measurements
    filter(Manifold_Timer >= start_cutoff,
           Manifold_Timer <= end_cutoff,
           max(change_s) < 1000, #After ~15 min there is probably a problem
           n < 200 #probably some issue if this many measurements are taken
    ) 
  
  #Process using old methods
  filtered_data_old <- filter_old_data(time_split$OLD)
  
  #Combine
  filtered_data <- rbind(filtered_data_old, filtered_data_new)
  
  #Data flags
  data_flags <- filtered_data %>%
    group_by(group, MIU_VALVE, date) %>%
    summarize(Flag_CO2_slope = ifelse(sum(!is.na(CO2d_ppm)) > 5, 
                                      "No issues", "Insufficient data"),
              Flag_N2O_slope = ifelse(sum(!is.na(N2Od_ppm)) > 5, 
                                      "No issues", "Insufficient data"),
              Flag_CH4_slope = ifelse(sum(!is.na(CH4d_ppm)) > 5, 
                                      "No issues", "Insufficient data"),
              cutoff_removed = unique(cutoff),
              n_removed = unique(n),
              .groups = "drop") 
  
  #Run lm
  slopes <- filtered_data %>%
    pivot_longer(c(CH4d_ppm, CO2d_ppm, N2Od_ppm), names_to = "gas", values_to = "conc") %>%
    group_by(gas, group, MIU_VALVE, date) %>%
    mutate(n = sum(!is.na(conc))) %>%
    filter(!is.na(conc),
           n > 5) %>%
    summarize(model = list(lm(conc ~ change)),
              slope_ppm_per_day = model[[1]]$coefficients[[2]],
              R2 = summary(model[[1]])$r.squared,
              p = summary(model[[1]])$coefficients[,4][2],
              rmse = sqrt(mean(model[[1]]$residuals^2)),
              max = max(conc),
              min = min(conc),
              init = first(conc),
              max_s = unique(max_s),
              flux_start = min(TIMESTAMP),
              flux_end = max(TIMESTAMP),
              TIMESTAMP = unique(start),
              n = sum(!is.na(conc)),
              cutoff = unique(cutoff),
              .groups = "drop") %>%
    select(-model) %>%
    mutate(gas = case_match(gas,
                            "CH4d_ppm" ~ "CH4",
                            "CO2d_ppm" ~ "CO2",
                            "N2Od_ppm" ~ "N2O")) %>%
    pivot_wider(names_from = gas, 
                values_from = c(slope_ppm_per_day, R2, p, rmse, init, max, min),
                names_glue = "{gas}_{.value}") %>%
    full_join(flags, by = c("TIMESTAMP" = "start", "MIU_VALVE", "date", "group")) %>%
    full_join(data_flags, by = c("group", "MIU_VALVE", "date")) %>%
    mutate(cutoff = ifelse(is.na(cutoff), cutoff_removed, cutoff),
           n = ifelse(is.na(n), n_removed, n)) %>%
    select(-cutoff_removed, -n_removed)
  
  if(!reprocess){
    #Load previously calculated slopes
    old_slopes <- read_csv(here::here("processed_data","L0.csv"), 
                           col_types = "nnDcccnnnnnnnnnnnnnnnnnnnnnnncccc",
                           show_col_types = F) %>%
      mutate(TIMESTAMP = as_datetime(TIMESTAMP, tz = "EST"),
             flux_start = as_datetime(flux_start, tz = "EST"),
             flux_end = as_datetime(flux_end, tz = "EST")) %>%
      filter(TIMESTAMP < min(slopes$TIMESTAMP) | 
               TIMESTAMP > max(slopes$TIMESTAMP))
    slopes_comb <- bind_rows(old_slopes, slopes)
  } else {
    slopes_comb <- slopes
    #Whenever we reprocess everything, save the raw output for QAQC efforts
    #Update: this file is insanely big and I don't want it any more
    #round_comb <- function(x){round(as.numeric(x), 2)}
    #write.csv(data_small %>%
    #            mutate(across(c(CO2d_ppm), round_comb)),
    #          here::here("processed_data","raw_small.csv"), row.names = FALSE)
  }
  
  #Output
  write.csv(slopes_comb %>% select(-max_s), 
            here::here("processed_data","L0.csv"), 
            row.names = FALSE)
  
  write.csv(slopes_comb %>% 
              select(-max_s) %>%
              filter(TIMESTAMP > as.Date("2025-03-18")), 
            here::here("processed_data","L0_for_dashboard.csv"), 
            row.names = FALSE)
  
  recent_raw <- grouped_data %>%
    filter(date >= Sys.Date()-days(7)) %>%
    select(TIMESTAMP, Manifold_Timer, change, MIU_VALVE, group, CH4d_ppm, CO2d_ppm, N2Od_ppm)
  
  write.csv(recent_raw, 
            here::here("processed_data","raw_for_dashboard.csv"), 
            row.names = FALSE)
  
  if(plot){
    for(year_i in unique(year(slopes$TIMESTAMP))){
      p <- slopes %>%
        filter(max_s <=1000,
               month(date) %in% c(1:12)) %>%
        mutate(MIU_VALVE = factor(MIU_VALVE, 
                                  levels = c(1,4,7,10,
                                             3,6,9,12,
                                             2,5,8,11))) %>%
        filter(year(TIMESTAMP) == year_i) %>%
        ggplot(aes(x = TIMESTAMP, y = max_s)) +
        geom_point(alpha = 0.02)+
        geom_line(aes(y = cutoff, x = as.POSIXct(date)), color = "red")+
        facet_wrap(~MIU_VALVE)+
        ggtitle(year_i)+
        xlab("Date")+
        ylab("Time to peak (s)")+
        theme_bw()
      jpeg(here::here("figures", paste0("TimeToPeak_", year_i, ".jpeg")), width = 6, height = 5, units = "in", res = 300)
      print(p)
      dev.off()
    }
  }
  
  return(slopes_comb)
}


calculate_flux(reprocess = T)
