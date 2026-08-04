install.packages("rsconnect")
install.packages("here")
install.packages("slackr")
library(tidyverse)

# Check for errors
data <- read_csv(here::here("processed_data", "L0_for_dashboard.csv"))
error_check <- data %>%
  mutate(TIMESTAMP = force_tz(TIMESTAMP, "EST")) %>%
  filter(!is.na(flux_start)) %>%
  group_by(MIU_VALVE) %>%
  filter(TIMESTAMP > (force_tz(Sys.time(), "EST") - hours(24))) %>%
  summarize(
    last_timestamp = max(TIMESTAMP),
    r2_check_ch4 = median(CH4_R2, na.rm = T),
    r2_check_co2 = median(CO2_R2, na.rm = T)
  ) %>%
  filter(r2_check_ch4 < 0.7 & r2_check_co2 < 0.7)

if (nrow(error_check) > 0) {
  slackr::slackr_setup(token = Sys.getenv("SLACKRTOKEN"),
                       incoming_webhook_url = Sys.getenv("SLACKRURL"))
  slackr::slackr_msg(
    channel = "#genx-flux-data",
    username = "GENX QAQC bot",
    txt = paste0(
    "Hi team! I noticed that CO2 and CH4 R2 values have been low recently for the following chamber(s):\n",
    paste(error_check$MIU_VALVE, collapse = ", "),
    "\nYou might want to take a quick look at the dashboard and make sure things look okay:\n",
    "https://aslewis.shinyapps.io/dashboard/",
    "\nThanks! -genx bot"
  ))
}

# Check for licor errors
data <- read.csv(here::here("processed_data", "error_codes.csv"))
error_check <- data %>%
  filter((!is.na(Diag_7810) & Diag_7810 > 0) | (!is.na(Diag_7820) & Diag_7820 > 0)) %>%
  filter(TIMESTAMP >= (force_tz(Sys.time(), "EST") - hours(24)))

if (nrow(error_check) > 0) {
  slackr::slackr_setup(token = Sys.getenv("SLACKRTOKEN"),
                       incoming_webhook_url = Sys.getenv("SLACKRURL"))
  
  unique_7810 <- unique(error_check$Diag_7810)
  unique_7810 <- unique_7810[!is.na(unique_7810)]
  unique_7820 <- unique(error_check$Diag_7820)
  unique_7820 <- unique_7820[!is.na(unique_7820)]
  both <- length(unique_7810) > 1 & length(unique_7820) > 1
  
  text = if(both){"both of the licors are"} else {"one of the licors is"}
  
  slackr::slackr_msg(
    channel = "#genx-flux-data",
    username = "GENX QAQC bot",
    txt = paste0(
    "Hi team- it looks like ", text, " unhappy. \n",
    "LI-7810 error codes today: ", paste(unique_7810, collapse = ", "), "\n",
    "LI-7820 error codes today: ", paste(unique_7820, collapse = ", "), "\n",
    "Thanks! -genx bot"
  ))
}
