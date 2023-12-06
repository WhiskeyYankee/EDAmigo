

#' Plot Highest Correlated Variables
#'
#'\strong{amigoPlot} plots the n most highly correlated variables.
#'
#' @param df A clean dataframe.
#' @param n_top The number of variable comparisons to plot.
#' @param line_color The regression line color.
#'
#' @return \strong{auto_plots} A dataframe containing the correlation, slope, and R^2 values for the highest correlated  variables.
#'
#' @export
#'
#' @examples
#' # Clean the dataframe, fires
#' cleaned <- autoClean(fires, factor_tol = 10, drop_user_level = 0,
#'                   impute_user_level = 0, impute_factors = TRUE)
#'
#' # Plot the highest correlated variables
#' plot_results <- amigoPlot(cleaned$clean_df)
#'
#' plot_results
amigoPlot <- function(df, n_top = 4, line_color = 'dodgerblue') {
  # Keep only numeric columns
  df <- dplyr::`%>%`(df, dplyr::select_if(is.numeric))

  # Calculate correlation
  correlation <- stats::cor(df,method="pearson")
  correlation_df <- as.data.frame(as.table(correlation))
  names(correlation_df) <- c("VarA", "VarB", "Correlation")

  # Type corrections for filtering
  correlation_df$VarA <- as.character(correlation_df$VarA)
  correlation_df$VarB <- as.character(correlation_df$VarB)

  # Remove correlation with itself and duplicate combinations
  correlation_df <- dplyr::`%>%`(correlation_df, dplyr::filter(VarA != VarB))
  correlation_df <-  dplyr::`%>%`(correlation_df, dplyr::filter(VarA < VarB))

  # Check here to make sure right counts of columns and pair keys are kept
  if (nrow(correlation_df) != choose(ncol(df), 2)) {
    print("The number of rows in correlation matrix does not match the number of combinations of variable columns in df.")
  }

  # Reset n_top if greater than available values
  if (n_top > nrow(correlation_df)){
    n_top = nrow(correlation_df)
  }

  auto_plots <- data.frame(VarA = character(n_top),
                           VarB = character(n_top),
                           Correlation = numeric(n_top),
                           Slope = numeric(n_top),
                           Intercept = numeric(n_top),
                           R2 = numeric(n_top),
                           stringsAsFactors = FALSE)

  # Get the top correlated pairs of variables.
  ordered_correlation_df <- correlation_df[order(-abs(correlation_df$Correlation)),]
  top_correlations <- ordered_correlation_df[1:n_top,]
  auto_plots$VarA <- top_correlations$VarA
  auto_plots$VarB <- top_correlations$VarB
  auto_plots$Correlation <- top_correlations$Correlation

  for (i in 1:n_top){
    var1 <- top_correlations$VarA[i]
    var2 <- top_correlations$VarB[i]
    fit <- stats::lm(df[[var2]] ~ df[[var1]])
    auto_plots[i, 'Slope'] <- round(stats::coef(fit)[2], 2)
    auto_plots[i, 'Intercept'] <- round(stats::coef(fit)[1], 2)
    auto_plots[i, 'R2'] <- round(summary(fit)$r.squared, 2)
  }



  # Create scatter plots with trend lines
  plots <- lapply(1:nrow(top_correlations), function(i) {
    var1 <- top_correlations$VarA[i]
    var2 <- top_correlations$VarB[i]

    # Define Equation
    eq <- paste("y = ", auto_plots[i, 'Slope'],
                "x + ", auto_plots[i, 'Intercept'],
                ",       R^2 = ", auto_plots[i, 'R2'])
    p <- ggplot2::ggplot(df, ggplot2::aes_string(x = var1, y = var2)) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(method = "lm", se = FALSE, color = line_color,
                  formula = y ~ x) +
      ggplot2::labs(title = paste(var1, "vs.", var2),
                    subtitle = eq)

    return(p)
  })

  # Combine plots
  combined_plot <- do.call(gridExtra::grid.arrange, plots)

  return(auto_plots)
}
