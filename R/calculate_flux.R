
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
                           modif_start_date = file.info(here::here("processed_data","L0.csv"))$mtime,
                           reprocess = F,
                           plot = reprocess){
  ### Load files ###
  files <- list.files(here::here("Raw_data","dropbox_downloads"), full.names = T)
  #By default, only calculate slopes for files that have been modified/created since the last time we ran the script
  if(!is.null(modif_start_date)){
    files <- files[file.info(files)$mtime > modif_start_date]
  }
  #If a start and end date are provided, look for files that match these dates
  if(!is.null(start_date) & !is.null(end_date)){
    possible_file_names <- seq(as.Date(start_date), 
                               as.Date(end_date), 
                               by = "1 day") %>%
      format("%Y%m%d")
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
               "GENX_LGR_04142021_20210505020005.dat")
  files <- files[!grepl(paste0(exclude, collapse = "|"), files)]
  message(paste0("Calculating fluxes for ", length(files), " files"))
  
  #Load data
  data_raw <- files %>%
    map(read_csv, col_types = cols(.default = "c"), skip = 1)  %>%
    bind_rows() %>%
    filter(!TIMESTAMP == "TS") %>%
    mutate(TIMESTAMP = as_datetime(TIMESTAMP, tz = "America/New_York")) %>%
    filter(!is.na(TIMESTAMP),
           year(TIMESTAMP)>=2021) %>%
    distinct() 
  
  #Account for different formatting among files
  if("LGR_Time" %in% colnames(data_raw)){
    data_small <- data_raw %>%
      filter(is.na(LGR_Time) | !duplicated(LGR_Time) | 
               !duplicated(CH4d_ppm)) %>% #I've spent some time looking into this and there are some duplicated LGR rows
      select(TIMESTAMP, CH4d_ppm, CO2d_ppm, MIU_VALVE, GasT_C)
  } else {
    #NOTE GENX_FLUX_20210602020004.dat has issues with timestamp
    data_small <- data_raw %>%
      select(TIMESTAMP, CH4d_ppm, CO2d_ppm, MIU_VALVE, GasT_C)
  }
  rm(data_raw) #Save memory
  
  #Format data
  data_numeric <- data_small %>%
    mutate(CH4d_ppm = as.numeric(CH4d_ppm),
           CO2d_ppm = as.numeric(CO2d_ppm),
           GasT_C = as.numeric(GasT_C),
           MIU_VALVE = as.numeric(MIU_VALVE),
           CH4d_ppm = ifelse(CH4d_ppm<=0, NA, CH4d_ppm),
           CO2d_ppm = ifelse(CO2d_ppm<=0, NA, CO2d_ppm)) %>%
    filter(!is.na(MIU_VALVE),
           MIU_VALVE %in% 1:12) %>%
    mutate(Flag = "No issues")
  
  #Remove data as specified in maintenance log
  googlesheets4::gs4_deauth() # No authentication needed
  today <- Sys.time()
  maint_log <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1-fcWU3TK936cR0kPvLTy6CUF_GTvHTwGbiAKDCIE05s/edit?gid=0#gid=0") %>%
    mutate(Start_time = as_datetime(Start_time, tz = "America/New_York"),
           End_time = as_datetime(End_time, tz = "America/New_York"),
           End_time = ifelse(is.na(End_time), today, End_time),
           End_time = as_datetime(End_time, tz = "America/New_York"))
  for(i in 1:nrow(maint_log)){
    data_numeric <- data_numeric %>%
      mutate(Flag = ifelse(TIMESTAMP < maint_log$End_time[i] & 
                             TIMESTAMP > maint_log$Start_time[i] &
                             MIU_VALVE %in% eval(parse(text = maint_log$Chambers[i])),
                           maint_log$Flag[i],
                           Flag),
             CH4d_ppm = ifelse(TIMESTAMP < maint_log$End_time[i] & 
                                 TIMESTAMP > maint_log$Start_time[i] &
                                 maint_log$Remove[i] == "y" &
                                 MIU_VALVE %in% eval(parse(text = maint_log$Chambers[i])),
                               NA,
                               CH4d_ppm),
             CO2d_ppm = ifelse(TIMESTAMP < maint_log$End_time[i] & 
                                 TIMESTAMP > maint_log$Start_time[i] &
                                 maint_log$Remove[i] == "y" &
                                 MIU_VALVE %in% eval(parse(text = maint_log$Chambers[i])),
                               NA,
                               CO2d_ppm))
  }
  
  #Group flux intervals, prep for slopes
  grouped_data <- data_numeric %>%
    mutate(date = as.Date(TIMESTAMP)) %>%
    #Group flux intervals
    arrange(TIMESTAMP) %>%
    mutate(group = group_fun(MIU_VALVE)) %>%
    group_by(group, MIU_VALVE)  %>%
    #Record the amount of time from when chamber closed
    mutate(start = min(TIMESTAMP),
           end = max(TIMESTAMP),
           change = as.numeric(difftime(TIMESTAMP, start, units = "days")),
           change_s = as.numeric(difftime(TIMESTAMP, start, units = "secs")),
           max_s = ifelse(sum(!is.na(CH4d_ppm) > 0),
                          change_s[which.max(CH4d_ppm)],
                          NA),
           min_s = ifelse(sum(!is.na(CH4d_ppm) > 0),
                          change_s[which.min(CH4d_ppm)],
                          NA),)
  
  buffer <- 60 #Buffer of time after peak (s)
  rolling_window = 11 #days
  peaks <- grouped_data %>%
    group_by(group, MIU_VALVE)  %>%
    filter(max(change_s) < 1000, #After ~15 min there is probably a problem
    ) %>%
    group_by(MIU_VALVE, date, group, end, start) %>%
    summarize(max_s = unique(max_s), .groups = "drop") %>%
    filter((difftime(end, start, units = "secs") - max_s) > 20) %>%
    group_by(MIU_VALVE, date) %>%
    summarize(cutoff = density(max_s, bw = 10)$x[which.max(density(max_s, bw = 10)$y)],
              cutoff = round(cutoff) + buffer,
              .groups = "drop") %>%
    group_by(MIU_VALVE) %>%
    arrange(date) %>%
    mutate(cutoff = zoo::rollmax(cutoff, 
                                 rolling_window, 
                                 align = "center", 
                                 fill = "extend"))
  
  #Save flags for data that will be removed in the next step
  flags <- grouped_data %>%
    ungroup() %>%
    select(start, MIU_VALVE, Flag) %>%
    distinct() %>%
    group_by(MIU_VALVE, start) %>%
    filter(n() == 1 | !Flag == "No issues")
  
  filtered_data <- grouped_data %>%
    left_join(peaks) %>%
    group_by(group, MIU_VALVE)  %>%
    mutate(n = sum(change_s >= cutoff)) %>%
    #Remove earlier measurements
    filter(change_s >= cutoff,
           max(change_s) < 1000, #After ~15 min there is probably a problem
           n < 200 #probably some issue if this many measurements are taken
    ) 
  
  #Run lm
  slopes <- filtered_data %>%
    pivot_longer(c(CH4d_ppm, CO2d_ppm), names_to = "gas", values_to = "conc") %>%
    group_by(gas, group, MIU_VALVE, date) %>%
    filter(!is.na(conc)) %>%
    mutate(n = n()) %>%
    filter(n > 5) %>%
    summarize(model = list(lm(conc ~ change)),
              slope_ppm_per_day = model[[1]]$coefficients[[2]],
              Temp_init = first(GasT_C),
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
    mutate(gas = ifelse(gas == "CH4d_ppm", "CH4", "CO2")) %>%
    pivot_wider(names_from = gas, 
                values_from = c(slope_ppm_per_day, R2, p, rmse, init, max, min),
                names_glue = "{gas}_{.value}") %>%
    full_join(flags, by = c("TIMESTAMP" = "start", "MIU_VALVE"))
  
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
  
  if(!reprocess){
    #Load previously calculated slopes
    old_slopes <- read_csv(here::here("processed_data","L0.csv"), show_col_types = F) %>%
      filter(!TIMESTAMP %in% slopes$TIMESTAMP)
    slopes_comb <- bind_rows(old_slopes, slopes)
  } else {
    slopes_comb <- slopes
    #Whenever we reprocess everything, save the raw output for QAQC efforts
    round_comb <- function(x){round(as.numeric(x), 2)}
    write.csv(data_small %>%
                mutate(across(c(CO2d_ppm, GasT_C), round_comb)),
              here::here("processed_data","raw_small.csv"), row.names = FALSE)
    write_csv(filtered_data, 
              here::here("processed_data","processed_GENX_LGR_data.csv"))
  }
  
  #Output
  write.csv(slopes_comb %>% select(-max_s), 
            here::here("processed_data","L0.csv"), 
            row.names = FALSE)
  return(slopes_comb)
}

#Create function to assign groups for separate readings
group_fun <- function(MIU_VALVE) {
  group <- rep(1, length(MIU_VALVE))
  for (i in 2:length(MIU_VALVE)) {
    if(MIU_VALVE[i] == MIU_VALVE[i - 1]) {
      group[i] <- group[i - 1]
    } else {
      group[i] <- group[i - 1] + 1
    }
  }
  return(group)
}
#calculate_flux(start_date = "2021-01-01", 
#               end_date = Sys.Date()+1,
#               modif_start_date = NULL,
#               reprocess = TRUE)
