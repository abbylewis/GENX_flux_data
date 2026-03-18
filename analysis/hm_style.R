soil_temps <- read_csv(here::here("processed_data", "soil_temp.csv")) %>%
  group_by(MIU_VALVE) %>%
  summarize(mean = mean(Temp_C, na.rm = T), .groups = "drop") %>%
  arrange(#mean,
          MIU_VALVE) %>%
  mutate(#new_name = row_number(),
    new_name = MIU_VALVE,
         dif = mean - mean[new_name == 1],
         label = paste0("Ch. ", new_name, " (+", round(dif,1), " ºC)"))

chamber_labels2 <- soil_temps$label
chamber_levels <- soil_temps$MIU_VALVE

# Colorblind friendly pride palette
# https://www.reddit.com/r/vexillology/comments/v2luae/the_6colour_pride_flag_but_colourblindfriendly/
color.vals <- c("#D60303", "#FF790B", "#EAEE03", "#06D68B", "#017EFF", "blue4")
color.gradient <- colorRampPalette(rev(color.vals))(12)

theme_hm <- egg::theme_article() +
  theme(
    panel.grid.major = element_line(color = "grey93", linewidth = 0.5),
    panel.grid.minor = element_line(color = "grey95", linewidth = 0.25),
    legend.position = "bottom",
    legend.title.position = "top",
    plot.background = element_rect(fill = "white"),
    legend.text = element_text(size = 8),
    legend.key.spacing.y = unit(0, "cm"),
    legend.key.spacing.x = unit(0.5, "cm"),
    legend.justification = "left",
    legend.location = "plot"
  )
