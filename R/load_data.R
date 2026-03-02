load_data <- function(file) {
  data_raw <- read_csv(file, col_types = cols(.default = "c"), skip = 1)

  # Account for different formatting among files
  if ("GENX_CH4ppm" %in% colnames(data_raw)) {
    data_small <- data_raw %>%
      rename(
        CH4d_ppm = GENX_CH4ppm,
        CO2d_ppm = GENX_CO2ppm,
        N2Od_ppb = GENX_N20ppb,
        MIU_VALVE = Fluxing_Chamber
      ) %>%
      mutate(Format = "NEW") %>%
      select(TIMESTAMP, CH4d_ppm, CO2d_ppm, N2Od_ppb, MIU_VALVE, Manifold_Timer, Format)
  } else if ("GENX_CH4ppb" %in% colnames(data_raw)) {
    message(file, " has CH4ppb")
    data_small <- data_raw %>%
      mutate(
        CH4d_ppm = as.numeric(GENX_CH4ppb),
        CH4d_ppm = as.character(ifelse(CH4d_ppm > 1000,
          CH4d_ppm / 1000, CH4d_ppm
        )),
        N2Od_ppb = as.numeric(GENX_N20ppm),
        N2Od_ppb = as.character(ifelse(as.numeric(N2Od_ppb) > 10000,
          N2Od_ppb,
          N2Od_ppb * 1000
        ))
      ) %>%
      rename(
        CO2d_ppm = GENX_CO2ppm,
        MIU_VALVE = Fluxing_Chamber
      ) %>%
      mutate(Format = "NEW") %>%
      select(TIMESTAMP, CH4d_ppm, CO2d_ppm, N2Od_ppb, MIU_VALVE, Manifold_Timer, Format)
  } else if ("LGR_Time" %in% colnames(data_raw)) {
    data_small <- data_raw %>%
      filter(is.na(LGR_Time) | !duplicated(LGR_Time) |
        !duplicated(CH4d_ppm)) %>% # I've spent some time looking into this and there are some duplicated LGR rows
      mutate(
        Manifold_Timer = NA,
        N2Od_ppb = NA
      ) %>%
      mutate(Format = "OLD") %>%
      select(TIMESTAMP, CH4d_ppm, CO2d_ppm, N2Od_ppb, MIU_VALVE, Manifold_Timer, Format)
  } else {
    data_small <- data_raw %>%
      mutate(
        Manifold_Timer = NA,
        N2Od_ppb = NA
      ) %>%
      mutate(Format = "OLD") %>%
      select(TIMESTAMP, CH4d_ppm, CO2d_ppm, N2Od_ppb, MIU_VALVE, Manifold_Timer, Format)
  }

  return(data_small)
}

load_wl <- function(file) {
  data_raw <- read_csv(file, col_types = cols(.default = "c"), skip = 1)

  # Account for different formatting among files
  if ("H21_Depth" %in% colnames(data_raw)) {
    data_small <- data_raw %>%
      rename(
        Depth_cm = H21_Depth,
        Temperature_C = H21_Temperature,
        Salinity_PSU = Derived_Salinity,
        Actual_Conductivity_uScm = H21_Electrical_Conductivity,
        ID = `Hydros_ID(2)`
      ) %>%
      filter(!Depth_cm %in% c("mm", "Smp")) %>%
      mutate(Depth_cm = as.numeric(Depth_cm) / 10) %>% # convert mm to cm
      select(-RECORD, -Statname)
  } else {
    data_small <- data_raw %>%
      rename(
        ID = `Aquatroll_ID(2)`,
        Temperature_C = Temperature,
        Salinity_PSU = Salinity,
        Actual_Conductivity_uScm = Actual_Conductivity,
        Depth_cm = Depth
      ) %>%
      select(-RECORD, -Statname) %>%
      filter(!Depth_cm %in% c("cm", "Smp")) %>%
      mutate(Depth_cm = as.numeric(Depth_cm))
  }

  return(data_small)
}

load_redox <- function(file) {
  if (grepl("SWAP", file)) {
    stop("Wrong file format")
  }
  raw_data <- read_csv(file, col_types = cols(.default = "c"), skip = 1) %>%
    filter(
      !is.na(RECORD),
      !RECORD == "RN"
    )

  # Rename the 10cm variables
  raw_data <- raw_data %>%
    rename(
      "Ch01_10cm_P45_ref1_trA_B" = "Volt_Ref1(1)",
      "Ch01_10cm_P46_ref1_trA_B" = "Volt_Ref1(3)",
      "Ch04_10cm_P17_ref1_trA_G" = "Volt_Ref1(5)",
      "Ch04_10cm_P19_ref1_trA_G" = "Volt_Ref1(7)",
      "Ch07_10cm_P00_ref1_trA_Y" = "Volt_Ref1(9)",
      "Ch07_10cm_P01_ref1_trA_Y" = "Volt_Ref1(11)",
      "Ch10_10cm_P22_ref1_trA_R" = "Volt_Ref1(13)",
      "Ch10_10cm_P23_ref1_trA_R" = "Volt_Ref1(15)",
      "Ch03_10cm_P54_ref1_trB_B" = "Volt_Ref1(17)",
      "Ch03_10cm_P47_ref1_trB_B" = "Volt_Ref1(19)",
      "Ch06_10cm_P56_ref1_trB_G" = "Volt_Ref1(21)",
      "Ch06_10cm_P57_ref1_trB_G" = "Volt_Ref1(23)",
      "Ch09_10cm_P49_ref1_trB_Y" = "Volt_Ref1(25)",
      "Ch09_10cm_P48_ref1_trB_Y" = "Volt_Ref1(27)",
      "Ch12_10cm_P26_ref1_trB_R" = "Volt_Ref1(29)",
      "Ch12_10cm_P34_ref1_trB_R" = "Volt_Ref1(31)",
      "Ch02_10cm_P16_ref1_trC_B" = "Volt_Ref1(33)",
      "Ch02_10cm_P59_ref1_trC_B" = "Volt_Ref1(35)",
      "Ch05_10cm_P58_ref1_trC_G" = "Volt_Ref1(37)",
      "Ch05_10cm_P43_ref1_trC_G" = "Volt_Ref1(39)",
      "Ch08_10cm_P18_ref1_trC_Y" = "Volt_Ref1(41)",
      "Ch08_10cm_P40_ref1_trC_Y" = "Volt_Ref1(43)",
      "Ch11_10cm_P51_ref1_trC_R" = "Volt_Ref1(45)",
      "Ch11_10cm_P50_ref1_trC_R" = "Volt_Ref1(47)"
    )
  # Rename the 10cm variables ref2
  raw_data <- raw_data %>%
    rename(
      "Ch01_10cm_P45_ref2_trA_B" = "Volt_Ref2(1)",
      "Ch01_10cm_P46_ref2_trA_B" = "Volt_Ref2(3)",
      "Ch04_10cm_P17_ref2_trA_G" = "Volt_Ref2(5)",
      "Ch04_10cm_P19_ref2_trA_G" = "Volt_Ref2(7)",
      "Ch07_10cm_P00_ref2_trA_Y" = "Volt_Ref2(9)",
      "Ch07_10cm_P01_ref2_trA_Y" = "Volt_Ref2(11)",
      "Ch10_10cm_P22_ref2_trA_R" = "Volt_Ref2(13)",
      "Ch10_10cm_P23_ref2_trA_R" = "Volt_Ref2(15)",
      "Ch03_10cm_P54_ref2_trB_B" = "Volt_Ref2(17)",
      "Ch03_10cm_P47_ref2_trB_B" = "Volt_Ref2(19)",
      "Ch06_10cm_P56_ref2_trB_G" = "Volt_Ref2(21)",
      "Ch06_10cm_P57_ref2_trB_G" = "Volt_Ref2(23)",
      "Ch09_10cm_P49_ref2_trB_Y" = "Volt_Ref2(25)",
      "Ch09_10cm_P48_ref2_trB_Y" = "Volt_Ref2(27)",
      "Ch12_10cm_P26_ref2_trB_R" = "Volt_Ref2(29)",
      "Ch12_10cm_P34_ref2_trB_R" = "Volt_Ref2(31)",
      "Ch02_10cm_P16_ref2_trC_B" = "Volt_Ref2(33)",
      "Ch02_10cm_P59_ref2_trC_B" = "Volt_Ref2(35)",
      "Ch05_10cm_P58_ref2_trC_G" = "Volt_Ref2(37)",
      "Ch05_10cm_P43_ref2_trC_G" = "Volt_Ref2(39)",
      "Ch08_10cm_P18_ref2_trC_Y" = "Volt_Ref2(41)",
      "Ch08_10cm_P40_ref2_trC_Y" = "Volt_Ref2(43)",
      "Ch11_10cm_P51_ref2_trC_R" = "Volt_Ref2(45)",
      "Ch11_10cm_P50_ref2_trC_R" = "Volt_Ref2(47)"
    )
  # Rename the 25cm variables
  raw_data <- raw_data %>%
    rename(
      "Ch01_25cm_P45_ref1_trA_B" = "Volt_Ref1(2)",
      "Ch01_25cm_P46_ref1_trA_B" = "Volt_Ref1(4)",
      "Ch04_25cm_P17_ref1_trA_G" = "Volt_Ref1(6)",
      "Ch04_25cm_P19_ref1_trA_G" = "Volt_Ref1(8)",
      "Ch07_25cm_P00_ref1_trA_Y" = "Volt_Ref1(10)",
      "Ch07_25cm_P01_ref1_trA_Y" = "Volt_Ref1(12)",
      "Ch10_25cm_P22_ref1_trA_R" = "Volt_Ref1(14)",
      "Ch10_25cm_P23_ref1_trA_R" = "Volt_Ref1(16)",
      "Ch03_25cm_P54_ref1_trB_B" = "Volt_Ref1(18)",
      "Ch03_25cm_P47_ref1_trB_B" = "Volt_Ref1(20)",
      "Ch06_25cm_P56_ref1_trB_G" = "Volt_Ref1(22)",
      "Ch06_25cm_P57_ref1_trB_G" = "Volt_Ref1(24)",
      "Ch09_25cm_P49_ref1_trB_Y" = "Volt_Ref1(26)",
      "Ch09_25cm_P48_ref1_trB_Y" = "Volt_Ref1(28)",
      "Ch12_25cm_P26_ref1_trB_R" = "Volt_Ref1(30)",
      "Ch12_25cm_P34_ref1_trB_R" = "Volt_Ref1(32)",
      "Ch02_25cm_P16_ref1_trC_B" = "Volt_Ref1(34)",
      "Ch02_25cm_P59_ref1_trC_B" = "Volt_Ref1(36)",
      "Ch05_25cm_P58_ref1_trC_G" = "Volt_Ref1(38)",
      "Ch05_25cm_P43_ref1_trC_G" = "Volt_Ref1(40)",
      "Ch08_25cm_P18_ref1_trC_Y" = "Volt_Ref1(42)",
      "Ch08_25cm_P40_ref1_trC_Y" = "Volt_Ref1(44)",
      "Ch11_25cm_P51_ref1_trC_R" = "Volt_Ref1(46)",
      "Ch11_25cm_P50_ref1_trC_R" = "Volt_Ref1(48)"
    )
  # Rename the 25cm variables
  raw_data <- raw_data %>%
    rename(
      "Ch01_25cm_P45_ref2_trA_B" = "Volt_Ref2(2)",
      "Ch01_25cm_P46_ref2_trA_B" = "Volt_Ref2(4)",
      "Ch04_25cm_P17_ref2_trA_G" = "Volt_Ref2(6)",
      "Ch04_25cm_P19_ref2_trA_G" = "Volt_Ref2(8)",
      "Ch07_25cm_P00_ref2_trA_Y" = "Volt_Ref2(10)",
      "Ch07_25cm_P01_ref2_trA_Y" = "Volt_Ref2(12)",
      "Ch10_25cm_P22_ref2_trA_R" = "Volt_Ref2(14)",
      "Ch10_25cm_P23_ref2_trA_R" = "Volt_Ref2(16)",
      "Ch03_25cm_P54_ref2_trB_B" = "Volt_Ref2(18)",
      "Ch03_25cm_P47_ref2_trB_B" = "Volt_Ref2(20)",
      "Ch06_25cm_P56_ref2_trB_G" = "Volt_Ref2(22)",
      "Ch06_25cm_P57_ref2_trB_G" = "Volt_Ref2(24)",
      "Ch09_25cm_P49_ref2_trB_Y" = "Volt_Ref2(26)",
      "Ch09_25cm_P48_ref2_trB_Y" = "Volt_Ref2(28)",
      "Ch12_25cm_P26_ref2_trB_R" = "Volt_Ref2(30)",
      "Ch12_25cm_P34_ref2_trB_R" = "Volt_Ref2(32)",
      "Ch02_25cm_P16_ref2_trC_B" = "Volt_Ref2(34)",
      "Ch02_25cm_P59_ref2_trC_B" = "Volt_Ref2(36)",
      "Ch05_25cm_P58_ref2_trC_G" = "Volt_Ref2(38)",
      "Ch05_25cm_P43_ref2_trC_G" = "Volt_Ref2(40)",
      "Ch08_25cm_P18_ref2_trC_Y" = "Volt_Ref2(42)",
      "Ch08_25cm_P40_ref2_trC_Y" = "Volt_Ref2(44)",
      "Ch11_25cm_P51_ref2_trC_R" = "Volt_Ref2(46)",
      "Ch11_25cm_P50_ref2_trC_R" = "Volt_Ref2(48)"
    )

  raw_data_long <- pivot_longer(raw_data,
    cols = (4:ncol(raw_data)),
    names_to = "parameter", values_to = "REDOX_mV"
  )
  raw_data_long$MIU_VALVE <- as.numeric(substr(raw_data_long$parameter, 3, 4))
  raw_data_long$reference <- substr(raw_data_long$parameter, 15, 18)
  raw_data_long$Depth_cm <- substr(raw_data_long$parameter, 6, 7)
  raw_data_long$probe <- substr(raw_data_long$parameter, 11, 13)
  raw_data_long$transect <- substr(raw_data_long$parameter, 20, 22)
  raw_data_long$color <- substr(raw_data_long$parameter, 24, 24)

  return(raw_data_long)
}
