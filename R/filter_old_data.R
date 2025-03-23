filter_old_data <- function(grouped_data){
  
  if(nrow(grouped_data) == 0){
    #Return empty dataframe
    return(setNames(data.frame(matrix(ncol = 15, nrow = 0)), 
               c("TIMESTAMP", "CH4d_ppm", "CO2d_ppm", "N2Od_ppb", "MIU_VALVE", "Manifold_Timer", 
                 "Flag", "date", "group", "start", "end", "change", "change_s", 
                 "max_s", "min_s", "n")))
  }
  
  buffer <- 60 #Buffer of time after peak (s)
  rolling_window = 15 #days
  default = 60
  range_thresh = 0.02
  peaks_raw <- grouped_data %>%
    group_by(group, MIU_VALVE)  %>%
    filter(max(change_s) < 1000, #After ~15 min there is probably a problem
    ) %>%
    group_by(MIU_VALVE, date, group, end, start) %>%
    summarize(max_s = unique(max_s), .groups = "drop",
              range = max(CH4d_ppm, na.rm = T)-min(CH4d_ppm, na.rm = T)) %>%
    filter((difftime(end, start, units = "secs") - max_s) > 20) %>%
    mutate(max_s = ifelse(range < range_thresh, default, max_s)) %>%
    group_by(MIU_VALVE, date) %>%
    summarize(cutoff = density(max_s, bw = 10)$x[which.max(density(max_s, bw = 10)$y)],
              cutoff = round(cutoff) + buffer,
              .groups = "drop") 
  old_peaks <- read_csv(here::here("processed_data","peaks_raw.csv"), show_col_types = F)
  peaks_comb <- old_peaks %>%
    rename(old = cutoff) %>%
    full_join(peaks_raw) %>%
    mutate(cutoff = ifelse(is.na(cutoff), old, cutoff)) %>%
    select(-old)
  write_csv(peaks_comb, here::here("processed_data","peaks_raw.csv"))
  
  #remove outliers, then calculate rolling max
  peaks <- peaks_comb %>%
    group_by(MIU_VALVE) %>%
    arrange(date) %>%
    mutate(sd = zoo::rollapply(cutoff, 
                               width = rolling_window, 
                               FUN = sd,
                               na.rm = T,
                               fill = "expand",
                               partial = T,
                               align = "center"),
           mean = zoo::rollapply(cutoff, 
                                 width = rolling_window, 
                                 FUN = mean,
                                 na.rm = T,
                                 fill = "expand",
                                 partial = T,
                                 align = "center")) %>%
    filter(!cutoff > (mean + 2*sd)) %>%
    select(-mean, -sd) %>%
    mutate(cutoff = zoo::rollapply(cutoff, 
                                   width = rolling_window, 
                                   FUN = max,
                                   na.rm = T,
                                   fill = "expand",
                                   partial = T,
                                   align = "center")) %>%
    #Manually reset these because there are two peaks
    mutate(cutoff = ifelse(date > as.Date("2024-09-25") &
                             date < as.Date("2024-10-15"),
                           300,
                           cutoff))
  
  #Save flags for data that will be removed in the next step
  flags <- grouped_data %>%
    ungroup() %>%
    select(start, MIU_VALVE, Flag, date, group) %>%
    distinct() %>%
    group_by(MIU_VALVE, start, date, group) %>%
    filter(n() == 1 | !Flag == "No issues")
  
  filtered_data <- grouped_data %>%
    left_join(peaks, by = c("MIU_VALVE", "date")) %>%
    group_by(group, MIU_VALVE)  %>%
    mutate(n = sum(change_s >= cutoff)) %>%
    #Remove earlier measurements
    filter(change_s >= cutoff,
           max(change_s) < 1000, #After ~15 min there is probably a problem
           n < 200 #probably some issue if this many measurements are taken
    )
}
