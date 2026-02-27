make_lm_label <- function(data, xvar, yvar, group_var = NULL) {
  
  if (!is.null(group_var)) {
    
    data %>%
      group_by(.data[[group_var]]) %>%
      summarise(
        model = list(lm(reformulate(xvar, yvar), data = cur_data())),
        intercept = coef(model[[1]])[1],
        slope     = coef(model[[1]])[2],
        r2        = summary(model[[1]])$r.squared,
        p         = summary(model[[1]])$coefficients[2, 4],
        x_pos     = min(.data[[xvar]], na.rm = TRUE),
        y_pos     = (max(.data[[yvar]], na.rm = TRUE) - min(.data[[yvar]], na.rm = TRUE)) * 0.95,
        .groups = "drop"
      ) %>%
      mutate(
        label = paste0(
          "  y = ",
          round(intercept, 2),
          ifelse(slope >= 0, " + ", " - "),
          round(abs(slope), 2), "x",
          "\n  R² = ", round(r2, 2),
          "\n  p ", ifelse(p < 0.001, "< 0.001", paste0("= ", round(p, 3)))
        )
      )
    
  } else {
    
    model <- lm(reformulate(xvar, yvar), data = data)
    
    data.frame(
      x_pos = min(data[[xvar]], na.rm = TRUE),
      y_pos = (max(data[[yvar]], na.rm = TRUE) - min(data[[yvar]], na.rm = TRUE)) * 0.95,
      label = paste0(
        "  y = ",
        round(coef(model)[1], 2),
        ifelse(coef(model)[2] >= 0, " + ", " - "),
        round(abs(coef(model)[2]), 2), "x",
        "\n  R² = ", round(summary(model)$r.squared, 2),
        "\n  p ", ifelse(summary(model)$coefficients[2,4] < 0.001, 
                        "< 0.001", 
                        paste0("= ", round(summary(model)$coefficients[2,4], 3)))
      )
    )
  }
}
