# Load soil data to format labels in the right order
soil_labs <- read_csv(here::here("processed_data", "for_HM_MS", "Soil_temp.csv")) %>%
  mutate(ch = as.numeric(trimws(substr(Chamber, 5, 6)))) %>%
  select(ch, Chamber) %>%
  arrange(ch) %>%
  distinct()
chamber_labels <- soil_labs$Chamber

# Colorblind friendly pride palette
# https://www.reddit.com/r/vexillology/comments/v2luae/the_6colour_pride_flag_but_colourblindfriendly/
color.vals <- c("#D60303", "#FF790B", "#EAEE03", "#06D68B", "#017EFF", "blue4")
color.gradient <- colorRampPalette(rev(color.vals))(12)

# Theme for all plots
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

# Themes for Fig 2
panel_theme <- theme(
  plot.margin = margin(0.1, 1, 0.1, 1, unit = "mm")
)
top_theme <- theme(
  axis.text.x = element_blank(),
  axis.ticks.x = element_blank()
)
