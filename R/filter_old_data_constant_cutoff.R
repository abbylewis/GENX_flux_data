filter_old_data_2021 <- function(grouped_data){
  
  if(is.null(grouped_data)){
    #Return empty dataframe
    return(setNames(data.frame(matrix(ncol = 17, nrow = 0)), 
               c("TIMESTAMP", "CH4d_ppm", "CO2d_ppm", "N2Od_ppb", "MIU_VALVE", "Manifold_Timer", 
                 "Flag", "date", "group", "start", "end", "change", "change_s", 
                 "max_s", "min_s", "n", "cutoff")))
  }
  
  lags <- read_csv(here::here("processed_data", "chamber_lags_asl.csv")) %>%
    rename(cutoff = Lag)

  #Save flags for data that will be removed in the next step
  flags <- grouped_data %>%
    ungroup() %>%
    select(start, MIU_VALVE, Flag, date, group) %>%
    distinct() %>%
    group_by(MIU_VALVE, start, date, group) %>%
    filter(n() == 1 | !Flag == "No issues")
  
  filtered_data <- grouped_data %>%
    left_join(lags, by = c("MIU_VALVE" = "Chamber")) %>%
    group_by(group, MIU_VALVE)  %>%
    mutate(n = sum(change_s >= cutoff*10)) %>%
    #Remove earlier measurements
    filter(change_s >= cutoff*10,
           max(change_s) < 1000, #After ~15 min there is probably a problem
           n < 200 #probably some issue if this many measurements are taken
    )
}
