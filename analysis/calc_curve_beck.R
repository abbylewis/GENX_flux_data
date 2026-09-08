source("https://raw.githubusercontent.com/abbylewis/pecan/refs/heads/evi_helpers/modules/data.remote/R/FitDoubleLogBeck.R")

#' Wrapper function for double log calculation
#'
#' @param df data frame
#'
#' @returns modeled evi on each date
#' @export
calc_curve_beck <- function(df) {
  year_to_run <- unique(lubridate::year(df$Date))
  if(length(year_to_run) >1){
    stop("EVI has to be calculated one year at a time")
  }
  # Add explicit NAs
  x <- df |>
    dplyr::mutate(Date = as.Date(Date),
                  img_doy = lubridate::yday(Date)) |>
    dplyr::group_by(img_doy) |>
    dplyr::summarise(
      Date = min(Date),
      evi = mean(evi, na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::complete(img_doy = 1:365)
  
  # Run double log function
  fit <- FitDoubleLogBeck(x$evi, t = x$img_doy, hessian = T, ninit = 100)
  
  # Format output
  out <- data.frame(
    param_name = names(fit$params),
    param_value = fit$params,
    stdError = fit$stdError
  )
  rownames(out) <- NULL
  
  pred_df_beck <- data.frame(doy = rep(1:365)) |>
    dplyr::cross_join(out |>
                        tidyr::pivot_wider(names_from = param_name,
                                           values_from = c(param_value, stdError))) |>
    dplyr::mutate(pred = param_value_mn + (param_value_mx - param_value_mn) *
                    (1/(1 + exp(-param_value_rsp * (doy - param_value_sos))) +
                       1/(1 + exp(param_value_rau * (doy - param_value_eos))))) |>
    dplyr::left_join(df |> dplyr::rename(doy = img_doy)) |>
    dplyr::mutate(method = "Beck",
                  Date = as.Date(paste0(year_to_run,"-01-01"))+ lubridate::days(doy-1))
  
  return(pred_df_beck)
}