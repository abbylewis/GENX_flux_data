library(furrr)

filter_old_data_2021 <- function(grouped_data){
  
  if(is.null(grouped_data)){
    #Return empty dataframe
    return(setNames(data.frame(matrix(ncol = 17, nrow = 0)), 
               c("TIMESTAMP", "CH4d_ppm", "CO2d_ppm", "N2Od_ppb", "MIU_VALVE", "Manifold_Timer", 
                 "Flag", "date", "group", "start", "end", "change", "change_s", 
                 "max_s", "min_s", "n", "cutoff")))
  }
  
  correct_duration <- grouped_data %>%
    group_by(group, MIU_VALVE) %>%
    filter(max(change_s) < 1000, #After ~15 min there is probably a problem
           change_s > 90) #All chambers need to remove first 60 seconds
  
  plan(multisession, workers = 8)
  
  labeled_data <- correct_duration %>%
    filter(!is.na(CH4d_ppm),
           !is.na(CO2d_ppm)) %>%
    group_by(MIU_VALVE, date, group, end, start) %>%
    arrange(change_s) %>%
    mutate(index = row_number()) %>%
    nest() %>%
    mutate(
      keep = furrr::future_map(data, ~ {
        df <- .
        if (nrow(df) < 10) {
          rep(FALSE, nrow(df))
        } else {
          #Function is defined below
          w <- find_best_linear_window(df$change_s, df$CH4d_ppm, df$CO2d_ppm, min_n = 10)
          df$index >= w$start & df$index <= w$end
        }
      })
    ) %>%
    unnest(c(data, keep))
  
  
  return(labeled_data)
}

find_best_linear_window_dt <- function(time, ch4, co2, min_n = 10) {
  n <- length(time)
  
  ch4_z <- as.numeric(scale(ch4))
  co2_z <- as.numeric(scale(co2))
  
  best_error <- Inf
  best_start <- NA_integer_
  best_end   <- NA_integer_
  
  for (s in 1:(n - min_n + 1)) {
    for (e in (s + min_n - 1):n) {
      
      fit_ch4 <- lm(ch4_z[s:e] ~ time[s:e])
      fit_co2 <- lm(co2_z[s:e] ~ time[s:e])
      
      error <- summary(fit_ch4)$sigma +
        summary(fit_co2)$sigma
      
      if (error < best_error) {
        best_error <- error
        best_start <- s
        best_end   <- e
      }
    }
  }
  
  list(start = best_start, end = best_end)
}
