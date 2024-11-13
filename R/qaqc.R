
qaqc <- function(L0_file = "L0.csv"){
  ### LOAD DATA
  metadata <- read_csv(here::here("Raw_data","chamber_metadata.csv"),
                       show_col_types = F)
  bme_temp <- read_csv(here::here("processed_data","chamber_temp_1min.csv"),
                       show_col_types = F) %>%
    left_join(metadata, by = "chamber_treatment") %>% #find miu valve
    select(TIMESTAMP, AirTemp_C, miu_valve) %>%
    rename(time_1min = TIMESTAMP) #prep to join with slopes
  
  slopes <- read_csv(L0_file, show_col_types = F) 
  
  #Identify when measurements were taken
  measurement_times <- slopes %>%
    mutate(time_1min = round_date(TIMESTAMP, unit="1 mins")) %>%
    rename(miu_valve_flux = MIU_VALVE) %>%
    select(miu_valve_flux, time_1min) %>%
    distinct()
    
  ### STUCK OPEN/CLOSED
  # Use temperature data to filter out fluxes
  stuck_removed <- bme_temp %>%
    ## Format data
    #Can only use daytime data for this
    filter(hour(time_1min) >= 8 & hour(time_1min) <= 12)  %>%
    rename(miu_valve_bme = miu_valve) %>%
    inner_join(measurement_times, by = c("time_1min")) %>%
    arrange(miu_valve_bme, time_1min) %>%
    ## Calculate rate of change in temp during the flux period
    group_by(miu_valve_bme) %>%
    mutate(temp_change = lead(AirTemp_C) - AirTemp_C) %>%
    group_by(time_1min) %>%
    filter(!is.na(temp_change)) %>%
    mutate(temp_change_quantile = ecdf(temp_change)(temp_change)) %>%
    #Now only work with the chamber being measured
    filter(miu_valve_bme == miu_valve_flux) %>%
    mutate(stuck = ifelse(temp_change_quantile < 0.5, T, F),
           date = as.Date(time_1min)) %>%
    filter(!is.na(stuck)) %>%
    ## Remove the full day if two measurements seem stuck
    group_by(date, miu_valve_flux) %>%
    summarize(stuck = sum(stuck)>=2, .groups = "drop") %>%
    rename(MIU_VALVE = miu_valve_flux) %>%
    ## Re-join all slopes
    left_join(slopes %>%
                mutate(date = as.Date(TIMESTAMP)),
              by = c("date", "MIU_VALVE")) %>%
    mutate(across(contains("CO2|CH4"), ~ifelse(stuck, NA, .)),
           Flag_stuck = ifelse(is.na(stuck), "No temp data",
                               ifelse(stuck, "Stuck",
                                      "No issues"))) %>%
    select(-date, -stuck)
  
  ### CALCULATE SLOPE IN µMOL
  #Add temp
  slopes_with_bme <- stuck_removed %>%
    mutate(time_slope = TIMESTAMP,
           time_1min = round_date(TIMESTAMP, unit="1 mins")) %>%
    left_join(bme_temp %>%
                rename(MIU_VALVE = miu_valve),
              by = c("MIU_VALVE", "time_1min")) 
  
  #Fill in temperature with a linear regression (LGR vs BME)
  lm_temp <- lm(AirTemp_C ~ Temp_init, data = slopes_with_bme)
  slopes_umol <- slopes_with_bme %>%
    mutate(Flag_AirTemp_C = ifelse(is.na(AirTemp_C), "Filled from LGR", "No issues"),
           AirTemp_C = ifelse(is.na(AirTemp_C), 
                              predict(lm_temp, #Fill
                                      newdata = data.frame(Temp_init)), 
                              AirTemp_C),
           CH4_slope_umol_per_day = CH4_slope_ppm_per_day * 
             265.8 / (0.08206*(AirTemp_C + 273.15)),
           CO2_slope_umol_per_day = CO2_slope_ppm_per_day * 
             265.8 / (0.08206*(AirTemp_C + 273.15)))
    
  #Add full metadata
  slopes_metadata <- metadata %>%
    left_join(slopes_umol %>%
                mutate(MIU_VALVE = as.numeric(MIU_VALVE)), 
              by = c("miu_valve" = "MIU_VALVE"))
  
  slopes_final <- slopes_metadata %>%
    filter(CH4_max < 1000) %>%
    rename(Flag_QAQC_log = Flag) %>%
    select(all_of(c("chamber_treatment", "miu_valve", "TIMESTAMP", "n", 
                    "CH4_slope_ppm_per_day", "CO2_slope_ppm_per_day",
                    "CH4_slope_umol_per_day", "CO2_slope_umol_per_day",
                    "CH4_R2", "CO2_R2", "CH4_p", "CO2_p", "CH4_rmse", "CO2_rmse", 
                    "CH4_init", "CO2_init", "CH4_max", "CO2_max", 
                    "CH4_min", "CO2_min", "AirTemp_C", "Flag_stuck", 
                    "Flag_QAQC_log", "Flag_AirTemp_C")))
  
  #Output
  write.csv(slopes_final, here::here("L1.csv"), row.names = FALSE)
  return(slopes_metadata)
}
