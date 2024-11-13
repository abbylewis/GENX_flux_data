generate_target() <- function(data){
  #Format as target data
  target <- data %>%
    mutate(time2 = with_tz(time2, tzone = "America/New_York"),
           project_id = "gcrew",
           duration = "P1D",
           time2 = as.Date(time2)) %>%
    rename(site_id = chamber_treatment,
           datetime = time2) %>%
    select(project_id, site_id, datetime, duration, CH4_slope_umol_per_day) %>%
    pivot_longer(cols = CH4_slope_umol_per_day, 
                 names_to = "variable", values_to = "observation") %>%
    mutate(datetime = as.Date(datetime)) %>%
    group_by(project_id, site_id, datetime, duration, variable) %>%
    summarise(observation = mean(observation, na.rm = TRUE), .groups = "drop")
  
  write.csv(target, here::here("L1_target.csv"), row.names = FALSE)
  return(target)
}