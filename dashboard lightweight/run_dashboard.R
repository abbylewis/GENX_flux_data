install.packages("rsconnect")
install.packages("quarto")
install.packages("shiny")
install.packages("plotly")
install.packages("here")
install.packages("slackr")
library(quarto)

unlink(here::here("dashboard lightweight","data","*"))
copy_target <- file.copy(here::here("processed_data","L0.csv"), here::here("dashboard lightweight", "data"), overwrite = T)
#copy_met <- file.copy(here::here("processed_data","met_2025.csv"), here::here("dashboard lightweight", "data"), overwrite = T)

# Deploy
quarto_publish_app(input = here::here("dashboard lightweight"), 
                   server = "shinyapps.io")

slackr::slackr('test message')
