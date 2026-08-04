plot_flux <- function(partitioned, 
                      compare_years, 
                      today, 
                      days_to_plot, 
                      gases, 
                      smooth, 
                      daily,
                      chamber_levels, 
                      color.gradient) {
  
  #Which columns do we need?
  potential_gas_cols <- c("CH4", "NEE", "N2O", "H2O", "NEE", "GPP", "Reco")
  names(potential_gas_cols) <- c("CH₄", "CO₂", "N₂O", "H₂O", "NEE", "GPP", "Reco")
  gas_cols <- potential_gas_cols[gases]
  
  if(compare_years == "Compare years"){
    part_recent <- partitioned %>%
      filter(
        yday(as_date(DateTime)) <= yday(today),
        yday(DateTime) > (yday(today) - days_to_plot)
      )
  } else {
    part_recent <- partitioned %>%
      filter(
        as_date(DateTime) <= today,
        DateTime > (today - days(days_to_plot))
      )
  }
  
  part_recent2 <- part_recent %>%
    select(any_of(c(gas_cols, "DateTime", "Chamber"))) %>%
    pivot_longer(matches(paste0(gases, collapse = "|")),
                 names_to = "gas"
    ) %>%
    mutate(year = year(DateTime)) %>%
    filter(!gas == "Flag")
  
  if (daily == "Daily mean") {
    part_recent3 <- part_recent2 %>%
      mutate(
        DateTime = as_date(DateTime),
        DateTime = as.POSIXct(DateTime)
      ) %>%
      group_by(DateTime, Chamber, gas, year) %>%
      summarize(
        value = mean(value, na.rm = T),
        .groups = "drop"
      )
  } else {
    part_recent3 <- part_recent2
  }
  
  p1 <- part_recent3 %>%
    mutate(
      Chamber = factor(Chamber,
                       levels = 1:12,
                       labels = chamber_levels
      )
    ) %>%
    filter(gas %in% gases) %>%
    ggplot(aes(x = DateTime, y = value, color = Chamber)) +
    geom_hline(yintercept = 0, color = "grey70") +
    geom_point(size = 0.5) +
    {
      if (smooth == "Smoothed") {
        geom_smooth(se = FALSE, method = "gam")
      } else {
        geom_line()
      }
    } +
    ylab("Flux (µmol/m²/s)") +
    {
      if(compare_years == "Compare years"){
        facet_grid(gas~year, scales = "free")
      } else {
        facet_wrap(~gas, scales = "free_y")
      }
    } +
    scale_color_manual(
      values = color.gradient,
      breaks = chamber_levels
    ) +
    theme_bw() +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_text(angle = 30, vjust = 1.0, hjust = 1.0),
      strip.background = element_rect(fill = "grey95", color = "grey")
    )
}
