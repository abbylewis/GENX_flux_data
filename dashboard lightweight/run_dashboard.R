install.packages("rsconnect")
install.packages("quarto")
install.packages("shiny")
install.packages("here")
library(quarto)

# Deploy
quarto_publish_app(input = here::here("dashboard lightweight"), 
                   server = "shinyapps.io")
