chamber_levels2 <- c(
  "Ch. 1 (+0 ºC)", "Ch. 2 (+0 ºC)", "Ch. 3 (+0.75 ºC)",
  "Ch. 4 (+1.5 ºC)", "Ch. 5 (+2.25 ºC)", "Ch. 6 (+2.25 ºC)",
  "Ch. 7 (+3.0 ºC)", "Ch. 8 (+3.75 ºC)", "Ch. 9 (+3.75 ºC)",
  "Ch. 10 (+4.5 ºC)", "Ch. 11 (+5.25 ºC)", "Ch. 12 (+6.0 ºC)"
)

color.gradient <- c(
  "blue4", "blue3", "turquoise4", "lightseagreen",
  "mediumseagreen", "limegreen", "yellowgreen", "yellow2",
  "darkgoldenrod2", "darkorange2", "orangered1", "red2"
)

# Colorblind friendly pride palette
# https://www.reddit.com/r/vexillology/comments/v2luae/the_6colour_pride_flag_but_colourblindfriendly/
color.vals <- c("#D60303", "#FF790B", "#EAEE03", "#06D68B", "#017EFF", "#6B0ECC")
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
