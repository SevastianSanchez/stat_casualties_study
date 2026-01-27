setwd("~/Documents/GitHub/stat_casualties_study")

#load libraries/packages
source("packages.R")

# load data
source("Comp2_panel_wrangling.R")

# Function to plot trends for specified countries and variables
plot_trends <- function(data, 
                        countries,        # vector of country codes
                        variables,        # vector of variable names
                        use_zscore = FALSE,
                        start_year = 2015,
                        end_year = max(data$year, na.rm = TRUE),
                        facet_by = "country") {  # "country" or "variable"
  
  # Filter data
  plot_data <- data %>%
    filter(country_code %in% countries,
           year >= start_year,
           year <= end_year) %>%
    select(year, country_code, country_name, all_of(variables))
  
  # Check if data exists
  if (nrow(plot_data) == 0) {
    stop("No data available for specified countries/variables/years")
  }
  
  # Apply z-score transformation if requested
  if (use_zscore) {
    plot_data <- plot_data %>%
      group_by(country_code) %>%
      mutate(across(all_of(variables), 
                    ~ scale(.) %>% as.vector(), 
                    .names = "{.col}_z")) %>%
      ungroup() %>%
      select(-all_of(variables)) %>%
      rename_with(~ str_remove(., "_z$"), ends_with("_z"))
  }
  
  # Reshape to long format
  plot_data_long <- plot_data %>%
    pivot_longer(
      cols = all_of(variables),
      names_to = "variable",
      values_to = "value"
    )
  
  # Create plot
  y_label <- if (use_zscore) "Z-Score" else "Value"
  
  p <- ggplot(plot_data_long, aes(x = year, y = value, color = variable)) +
    geom_line(linewidth = 0.75, na.rm = TRUE) +
    geom_vline(xintercept = 2020, linetype = "dashed", color = "gray50", alpha = 0.5) +
    labs(x = "Year", y = y_label, color = "Variable") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Add horizontal line at 0 for z-scores
  if (use_zscore) {
    p <- p + geom_hline(yintercept = 0, linetype = "dashed", color = "gray40")
  }
  
  # Add faceting
  if (facet_by == "country" && length(countries) > 1) {
    p <- p + facet_wrap(~ country_name, scales = "free_y")
  } else if (facet_by == "variable" && length(variables) > 1) {
    p <- p + facet_wrap(~ variable, scales = "free_y")
  }
  
  return(p)
}

# Example usage:
countries_to_plot <- "USA" #c("TUR", "HUN", "POL")
variables_to_plot <- c("spi_comp", "sdg_overall", "di_score", "log_gdppc")
trend_plot <- plot_trends(panel_data, countries_to_plot, variables_to_plot, use_zscore = TRUE, facet_by = "country")
print(trend_plot)
