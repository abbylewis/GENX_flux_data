install.packages("rsconnect")
install.packages("quarto")
install.packages("shiny")
install.packages("plotly")
install.packages("here")
install.packages("slackr")
library(quarto)
library(tidyverse)

# Check for errors
data <- read.csv(here::here("processed_data","L0_for_dashboard.csv"))
error_check <- data %>%
  filter(!is.na(flux_start)) %>%
  group_by(MIU_VALVE) %>%
  arrange(rev(TIMESTAMP)) %>%
  summarize(last_timestamp = max(TIMESTAMP),
            r2_check_ch4 = mean(CH4_R2[1:3]),
            r2_check_co2 = mean(CO2_R2[1:3])) %>%
  filter(r2_check_ch4 < 0.7 & r2_check_co2 < 0.7)

if(nrow(error_check) > 0) {
  slackr::slackr_setup()
  slackr::slackr_msg(paste0("Hi team! I noticed that CO2 and CH4 R2 values have been low recently for the following chamber(s):\n",
                   paste(error_check$MIU_VALVE, collapse = ", "),
                   "\nYou might want to take a quick look at the dashboard and make sure things look okay:\n",
                   "https://aslewis.shinyapps.io/dashboard/",
                   "\nThanks! -genx bot"))
}
