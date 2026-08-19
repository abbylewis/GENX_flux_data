install.packages("rsconnect")
install.packages("here")
install.packages("slackr")
library(tidyverse)

Sys.setenv(TZ = "EST")
current_time <- Sys.time()

# Check for errors
data <- read_csv(here::here("processed_data", "L0_for_dashboard.csv"))
error_check <- data %>%
  mutate(TIMESTAMP = force_tz(TIMESTAMP, "EST")) %>%
  filter(!is.na(flux_start)) %>%
  group_by(MIU_VALVE) %>%
  filter(TIMESTAMP > (force_tz(current_time, "EST") - hours(24))) %>%
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

# Check for licor errors
diag_dict <- c(
  `0`   = "Normal operation (measuring)",
  `1`   = "Start frequency adjustment; measurements may be noisy",
  `2`   = "Laser temperature adjustment; measurements may be noisy",
  `4`   = "Incomplete scan resulted in missing cavity modes; measurements may be noisy",
  `8`   = "Start-up mode finished; measurements may be noisy",
  `16`  = "Start-up mode initializing; measurements may be noisy",
  `32`  = "Spectral fit residual RMS too high; measurements are invalid",
  `64`  = "Unregulated pressures or temperatures; measurements are invalid",
  `128` = "Inlet clogged; instrument enters sleep mode",
  `256` = "Instrument not ready; measurements are invalid"
)

decode_diag <- function(x, dict) {
  if (is.na(x)) {
    return("Missing data")
  }
  
  if (x == 0) {
    return("0 (Normal operation)")
  }
  
  codes <- as.numeric(names(dict))
  codes <- codes[codes > 0]
  
  matched <- codes[bitwAnd(as.integer(x), codes) == codes]
  matched <- matched[rev(order(matched))]
  
  paste0(
    matched, " (", unname(dict[as.character(matched)]), ")",
    collapse = " and "
  )
}

data <- read_csv(here::here("processed_data", "error_codes.csv")) %>%
  mutate(TIMESTAMP = with_tz(TIMESTAMP, tzone = "EST"))
error_check <- data %>%
  filter(TIMESTAMP >= (force_tz(current_time, "EST") - hours(24))) %>%
  select(Diag_7810, Diag_7820) %>%
  distinct()

if (sum(error_check$Diag_7810[!is.na(error_check$Diag_7810)]) > 0 |
    sum(error_check$Diag_7820[!is.na(error_check$Diag_7820)]) > 0) {
  
  slackr::slackr_setup(token = Sys.getenv("SLACKRTOKEN"),
                       incoming_webhook_url = Sys.getenv("SLACKRURL"))
  
  unique_7810 <- unique(error_check$Diag_7810)
  unique_7820 <- unique(error_check$Diag_7820)
  
  text_7810 <- paste(
    paste0("*   ",unique_7810, ": ", vapply(unique_7810, decode_diag, character(1), dict = diag_dict)),
    collapse = "\n"
  )
  
  text_7820 <- paste(
    paste0("*   ",unique_7820, ": ", vapply(unique_7820, decode_diag, character(1), dict = diag_dict)),
    collapse = "\n"
  )
  
  text = if(sum(error_check$Diag_7810, na.rm = T) > 0 &
            sum(error_check$Diag_7820, na.rm = T) > 0) {
    "both of the licors are"
    } else {"one of the licors is"}
  
  slackr::slackr_msg(
    channel = "#genx-flux-data",
    username = "GENX QAQC bot",
    txt = paste0(
      "Hi team- it looks like ", text, " unhappy. \n\n",
      "LI-7810 error codes today:\n", text_7810, "\n",
      "LI-7820 error codes today:\n", text_7820, "\n",
      "\nYou can visualize when the errors happened on the dashboard:\n",
      "https://aslewis.shinyapps.io/dashboard/",
      "\n\nThanks! -genx bot"
    ))
}
