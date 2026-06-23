library(tidyverse)

color.gradient <- c(
  "blue4", "blue3", "turquoise4", "lightseagreen",
  "mediumseagreen", "limegreen", "yellowgreen", "yellow2",
  "darkgoldenrod2", "darkorange2", "orangered1", "red2"
)

partitioned <- read_csv("https://raw.githubusercontent.com/abbylewis/GENX_flux_data/refs/heads/master/processed_data/partitioned_co2.csv", show_col_types = F)

to_plot <- partitioned %>%
  filter(as.Date(DateTime) >= (Sys.Date() - days(3)),
         !is.na(CH4)) %>%
  mutate(DateTime = with_tz(DateTime, "EST"),
         Chamber = factor(MIU_VALVE, 
                          levels = 1:12,
                          labels = c(
                            "c_1_amb", "c_2_amb", "c_3_e0.75", "c_4_e1.5", 
                            "c_5_e2.25", "c_6_e2.25", "c_7_e3.0", "c_8_e3.75", 
                            "c_9_e3.75", "c_10_e4.5", "c_11_e5.25", "c_12_e6.0"
                          )))

p <- to_plot %>%
  ggplot(aes(x = DateTime, y = CH4, color = Chamber)) +
  geom_hline(yintercept = 0, color = "grey70") +
  geom_point(size = 0.5) +
  geom_line() +
  ylab("Flux (µmol/m²/s)") +
  scale_color_manual(
    values = color.gradient
  ) +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 30, vjust = 1.0, hjust = 1.0),
    strip.background = element_rect(fill = "grey95", color = "grey")
  )
p

plotly::ggplotly(p)
