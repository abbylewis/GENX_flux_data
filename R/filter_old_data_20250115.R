filter_old_data_2021 <- function(grouped_data){
  
  if(is.null(grouped_data)){
    #Return empty dataframe
    return(setNames(data.frame(matrix(ncol = 17, nrow = 0)), 
               c("TIMESTAMP", "CH4d_ppm", "CO2d_ppm", "N2Od_ppb", "MIU_VALVE", "Manifold_Timer", 
                 "Flag", "date", "group", "start", "end", "change", "change_s", 
                 "max_s", "min_s", "n", "cutoff")))
  }
  
  default = 100
  range_thresh = 0.01
  buffer = 30 #If there is a peak but no min (negative flux)
  near_end_buffer = 40
  
  # STEP 1: Identify peak. Remove everything before this
  
  correct_duration <- grouped_data %>%
    group_by(group, MIU_VALVE) %>%
    filter(max(change_s) < 1000 #After ~15 min there is probably a problem
    )
  
  sufficient_range <- correct_duration %>%
    filter(!is.na(CH4d_ppm),
           change_s > 60) %>% #All chambers need to remove first 60 seconds
    group_by(MIU_VALVE, date, group, end, start) %>%
    mutate(range = max(CH4d_ppm, na.rm = T)-min(CH4d_ppm, na.rm = T)) %>%
    filter(range > range_thresh)
  
  peaks_raw <- sufficient_range %>%
    group_by(MIU_VALVE, date, group, end, start) %>%
    #Doesn't count as a "first peak" if it is in the last minute and a half
    #Could still be a second peak below
    filter(change_s < (max(change_s) - 90)) %>% 
    summarize(first_peak = change_s[which.max(CH4d_ppm)],
              .groups = "drop") %>%
    filter(#Doesn't count as a peak if it's at the end
      (difftime(end, start, units = "secs") - first_peak) > near_end_buffer) 
  
  # Calculate min after this peak
  mins <- sufficient_range %>%
    left_join(peaks_raw) %>%
    filter(change_s > first_peak) %>%
    #Still sufficient range?
    group_by(MIU_VALVE, date, group, end, start) %>%
    mutate(range = max(CH4d_ppm, na.rm = T)-min(CH4d_ppm, na.rm = T)) %>%
    #Find min
    summarize(first_min = change_s[which.min(CH4d_ppm)],
              first_peak = unique(first_peak),
              range = unique(range)) %>%
    #Doesn't count as the min if its at the end, substitute buffer
    mutate(first_min = ifelse(
      (difftime(end, start, units = "secs") - first_min) > near_end_buffer &
        range > range_thresh,
      first_min,
      first_peak + buffer)) %>%
    select(-range)
  
  second_peaks_raw <- sufficient_range %>%
    #Remove data before first min
    left_join(mins) %>%
    filter(change_s > first_min) %>%
    #Still sufficient range?
    group_by(MIU_VALVE, date, group, end, start) %>%
    mutate(range = max(CH4d_ppm, na.rm = T)-min(CH4d_ppm, na.rm = T)) %>%
    filter(range > range_thresh) %>%
    group_by(MIU_VALVE, date, group, end, start) %>%
    #Find max
    summarize(second_peak = change_s[which.max(CH4d_ppm)],
              .groups = "drop") %>%
    filter(#Doesn't count as a peak if it's at the end
      (difftime(end, start, units = "secs") - second_peak) > near_end_buffer)
  
  second_mins <- sufficient_range %>%
    inner_join(second_peaks_raw) %>%
    filter(change_s > second_peak) %>%
    #Still sufficient range?
    group_by(MIU_VALVE, date, group, end, start) %>%
    mutate(range = max(CH4d_ppm, na.rm = T)-min(CH4d_ppm, na.rm = T)) %>%
    group_by(MIU_VALVE, date, group, end, start) %>%
    #Find min
    summarize(second_min = change_s[which.min(CH4d_ppm)],
              second_peak = unique(second_peak),
              range = unique(range)) %>%
    #Doesn't count as the min if its at the end
    mutate(second_min = ifelse((difftime(end, start, units = "secs") - second_min) > near_end_buffer &
                                 range > range_thresh,
                              second_min,
                              second_peak + buffer)) %>%
    select(-range)
  
  combined_cutoff <- grouped_data %>%
    select(MIU_VALVE, date, group, end, start) %>%
    distinct() %>%
    left_join(mins) %>%
    left_join(second_mins) %>%
    mutate(comb_peak = ifelse(is.na(second_peak), first_peak, second_peak),
           comb_min = ifelse(is.na(second_min), first_min, second_min))
  
  cutoff_daily <- combined_cutoff %>%
    group_by(MIU_VALVE, date) %>%
    filter(!is.na(comb_min)) %>%
    summarize(cutoff = density(comb_min, bw = 10)$x[which.max(density(comb_min, bw = 10)$y)],
              cutoff = round(cutoff),
              .groups = "drop") 
  
  cutoff_filled <- grouped_data %>%
    group_by(MIU_VALVE, date, group, end, start) %>%
    summarize(range = ifelse(sum(!is.na(CH4d_ppm) & change_s > 60) >= 5,
                             max(CH4d_ppm[change_s > 60], na.rm = T)-min(CH4d_ppm[change_s > 60], na.rm = T),
                             NA)) %>%
    left_join(combined_cutoff) %>%
    left_join(cutoff_daily) %>%
    mutate(cutoff = ifelse(range < range_thresh,
                           cutoff, 
                           comb_min))
  
  cutoff_filled %>%
    ggplot(aes(x = date, y = cutoff))+
    geom_point(shape = 21, alpha = 0.5)+
    facet_wrap(~MIU_VALVE)
  
  look <- correct_duration  %>%
    filter(MIU_VALVE == 2, date %in% c("2021-08-25"))
  
  look %>%
    left_join(cutoff_filled) %>%
    group_by(group, MIU_VALVE)  %>%
    mutate(n = sum(change_s >= cutoff),
           cutoff = ifelse(n >= 5,
                             cutoff, 
                             NA),
           color = ifelse(change_s > cutoff, "use","don't")) %>%
    ggplot(aes(x = TIMESTAMP, y = CH4d_ppm, color = color))+
    geom_point()+
    facet_wrap(~group, scales = "free_x")
  
  filtered_data <- grouped_data %>%
    left_join(cutoff_filled) %>%
    group_by(group, MIU_VALVE)  %>%
    mutate(n = sum(change_s >= cutoff)) %>%
    filter(change_s >= cutoff)
}
