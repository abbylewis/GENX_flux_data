
filter_old_data_2021 <- function(grouped_data){
  
  if(is.null(grouped_data)){
    #Return empty dataframe
    return(setNames(data.frame(matrix(ncol = 17, nrow = 0)), 
               c("TIMESTAMP", "CH4d_ppm", "CO2d_ppm", "N2Od_ppb", "MIU_VALVE", "Manifold_Timer", 
                 "Flag", "date", "group", "start", "end", "change", "change_s", 
                 "max_s", "min_s", "n", "cutoff")))
  }
  
  correct_duration <- grouped_data %>%
    #filter(as.Date(TIMESTAMP) %in% c("2021-06-30", "2021-06-29")) %>%
    group_by(group, MIU_VALVE) %>%
    filter(max(change_s) < 1000, #After ~15 min there is probably a problem
           change_s > 90) #All chambers need to remove first 90 seconds
  
  labeled_data <- with_progress({
    nested <- correct_duration %>%
      filter(!is.na(CH4d_ppm),
             !is.na(CO2d_ppm)) %>%
      group_by(MIU_VALVE, date, group, end, start) %>%
      filter(n() >= 10) %>%
      mutate(CH4_z = as.numeric(scale(CH4d_ppm)),
             CO2_z = as.numeric(scale(CO2d_ppm))) %>%
      arrange(change_s) %>%
      mutate(index = row_number()) %>%
      group_by(MIU_VALVE, date, group, end, start) %>%
      nest()
    
    p <- progressor(along = seq_len(nrow(nested)))
    
    nested %>%
      mutate(
        keep = future_map(data, ~ {
          p()
          df <- .
          w <- find_best_linear_window_fast(
            df$change_s,
            df$CH4_z,
            df$CO2_z,
            min_n = 9,
            max_n = 9 #In reality, this algorithm never picks >12 regardless
          )
          df$index >= w$start & df$index <= w$end
        })
      )
  })
  
  filtered_data <- labeled_data %>%
    unnest(c(data, keep))
  
  filtered_data %>%
    filter(TIMESTAMP > as_datetime("2021-06-29 19:00:00"),
           TIMESTAMP > as_datetime("2021-06-30 1:00:00"),
           MIU_VALVE == 7) %>%
    ggplot(aes(x = TIMESTAMP, y = CH4d_ppm, color = keep))+
    geom_point()+
    facet_wrap(~start, scales = "free_x")
  
  return(filtered_data)
}

#Functionally doing linear regressions, but without using lm. Hopefully a bit faster
find_best_linear_window_fast <- function(time, ch4_z, co2_z, min_n = 10, max_n = 18) {
  
  n <- length(time)
  
  # Precompute cumulative sums
  Sx   <- cumsum(time)
  Sxx  <- cumsum(time^2)
  
  Sy1  <- cumsum(ch4_z)
  Syy1 <- cumsum(ch4_z^2)
  Sxy1 <- cumsum(time * ch4_z)
  
  Sy2  <- cumsum(co2_z)
  Syy2 <- cumsum(co2_z^2)
  Sxy2 <- cumsum(time * co2_z)
  
  best_error <- Inf
  best_start <- NA_integer_
  best_end   <- NA_integer_
  
  for (s in 1:(n - min_n + 1)) {
    for (e in (s + min_n - 1):min(n, s + max_n - 1)) {
      
      k <- e - s + 1
      
      sx  <- Sx[e]  - if (s > 1) Sx[s-1]  else 0
      sxx <- Sxx[e] - if (s > 1) Sxx[s-1] else 0
      
      # ---- CH4 ----
      sy  <- Sy1[e]  - if (s > 1) Sy1[s-1]  else 0
      syy <- Syy1[e] - if (s > 1) Syy1[s-1] else 0
      sxy <- Sxy1[e] - if (s > 1) Sxy1[s-1] else 0
      
      denom <- k * sxx - sx^2
      if (denom <= 0) next
      
      rss1 <- syy - (k * sxy^2 - 2 * sx * sy * sxy + sy^2 * sxx) / denom
      
      # ---- CO2 ----
      sy  <- Sy2[e]  - if (s > 1) Sy2[s-1]  else 0
      syy <- Syy2[e] - if (s > 1) Syy2[s-1] else 0
      sxy <- Sxy2[e] - if (s > 1) Sxy2[s-1] else 0
      
      rss2 <- syy - (k * sxy^2 - 2 * sx * sy * sxy + sy^2 * sxx) / denom
      
      error <- sqrt(rss1 / k) + sqrt(rss2 / k)
      
      if (error < best_error) {
        best_error <- error
        best_start <- s
        best_end   <- e
      }
    }
  }
  
  list(start = best_start, end = best_end)
}
