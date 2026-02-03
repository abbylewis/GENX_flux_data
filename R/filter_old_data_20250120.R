
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
  
  n <- nrow(nested)
  results <- vector("list", n)
  
  for (i in seq_len(n)) {
    results[[i]] <- find_best_linear_window_fast(
      nested$data[[i]]$change_s,
      nested$data[[i]]$CH4_z,
      nested$data[[i]]$CO2_z,
      nested$group[[i]],
      min_n = 9,
      max_n = 9
    )
  }
  
  final <- dplyr::bind_rows(results)
  
  filtered_data <- correct_duration %>%
    left_join(final) %>%
    filter(change_s >= start_i,
           change_s <= end_i)
  
  return(filtered_data)
}

#Functionally doing linear regressions, but without using lm. 
#Hopefully a bit faster than lm
find_best_linear_window_fast <- function(time, ch4_z, co2_z, group, 
                                         min_n = 10, max_n = 18) {
  
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
    #for (e in (s + min_n - 1):min(n, s + max_n - 1)) {
      e <- s + min_n - 1
      
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
      
      if (!is.na(error) & error < best_error) {
        best_error <- error
        best_start <- s
        best_end   <- e
      }
    #}
  }
  
  data.frame(start_i = time[best_start], end_i = time[best_end], group = group)
}
