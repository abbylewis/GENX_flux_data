load_data <- function(file){
  data_raw <- read_csv(file, col_types = cols(.default = "c"), skip = 1)
  
  #Account for different formatting among files
  if("GENX_CH4ppm" %in% colnames(data_raw)){
    data_small <- data_raw %>%
      rename(CH4d_ppm = GENX_CH4ppm,
             CO2d_ppm = GENX_CO2ppm,
             N2Od_ppb = GENX_N20ppb,
             MIU_VALVE = Fluxing_Chamber,
             format = "NEW") %>% 
      select(TIMESTAMP, CH4d_ppm, CO2d_ppm, N2Od_ppb, MIU_VALVE, Manifold_Timer, Format)
  } else if("LGR_Time" %in% colnames(data_raw)){
    data_small <- data_raw %>%
      filter(is.na(LGR_Time) | !duplicated(LGR_Time) | 
               !duplicated(CH4d_ppm)) %>% #I've spent some time looking into this and there are some duplicated LGR rows
      mutate(Manifold_Timer = NA,
             N2Od_ppb = NA,
             format = "OLD") %>%
      select(TIMESTAMP, CH4d_ppm, CO2d_ppm, N2Od_ppb, MIU_VALVE, Manifold_Timer, Format)
  } else {
    data_small <- data_raw %>%
      mutate(Manifold_Timer = NA,
             N2Od_ppb = NA,
             format = "OLD") %>%
      select(TIMESTAMP, CH4d_ppm, CO2d_ppm, N2Od_ppb, MIU_VALVE, Manifold_Timer, Format)
  }
  
  return(data_small)
}
