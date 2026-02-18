entropy <- function(target) {
  freq <- table(target)/length(target)
  # vectorize
  vec <- as.data.frame(freq)[,2]
  #compute entropy
  -sum(vec * log2(vec))
}

bin <- function(x, n_breaks) {
  highest <- quantile(x, 0.99)
  lowest <- quantile(x, 0.01)
  findInterval(x, seq(lowest, highest, length.out = n_breaks))
}

bin_ranked <- function(x, n_breaks) {
  # Rank values (ties resolved randomly)
  r <- rank(x, ties.method = "first")
  bins <- as.numeric(cut(r, breaks = n_breaks, include.lowest = TRUE))
  return(bins)
}

analyze_wavelets <- function(ch4, treatment, var_name, timestep_s) {
  #Format data (dealing with irregularly spaced data)
  test_data <- ch4 %>%
    filter(MIU_VALVE == treatment) %>%
    filter(!is.na(!!sym(var_name)))
  
  #Run wavelet transformation
  data <- zoo::na.approx(test_data[[var_name]])
  wavelet <- waveslim::modwt(data, "la8", n.levels = 11)
  
  meshed <- data.frame(TIMESTAMP = test_data$TIMESTAMP,
                       data,
                       l_1 = wavelet[[1]],
                       l_2 = wavelet[[2]],
                       l_3 = wavelet[[3]],
                       l_4 = wavelet[[4]],
                       l_5 = wavelet[[5]],
                       l_6 = wavelet[[6]],
                       l_7 = wavelet[[7]],
                       l_8 = wavelet[[8]],
                       l_9 = wavelet[[9]],
                       l_10 = wavelet[[10]],
                       l_11 = wavelet[[11]]) %>%
    pivot_longer(cols = -c(data, TIMESTAMP)) %>%
    mutate(name = as.numeric(sub("l_", "", name)),
           name = factor(name, 
                         levels = 1:11,
                         labels = parse_time(2^(1:11)*timestep_s)
           ))
  
  df <- meshed %>% 
    mutate(MIU_VALVE = treatment,
           var_name = var_name)
  
  return(df)
}

parse_time <- function(s_list){
  out <- numeric(length(s_list))
  for(i in 1:length(s_list)){
    if(s_list[i] < 1.5*86400){
      out[i] <- paste(round(s_list[i]/60/60), "hours")
    } else {
      out[i] <- paste(round(s_list[i]/60/60/24), "days")
    }
  }
  return(out)
}