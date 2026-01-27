#---------------------------------------------------------------------
# ROBUSTNESS CHECKS WITH ALTERNATIVE DEMOCRACY MEASURES
#---------------------------------------------------------------------

# Function to re-run key models with alternative democracy measures
democracy_measure_robustness <- function(data, dem_vars) {
  # List to store models
  robust_models <- list()
  
  # For each democracy measure
  for (dem_var in dem_vars) {
    # Base model
    formula <- as.formula(paste("spi_comp ~", dem_var, 
                               "+ log_gdppc + income_level_recoded"))
    
    model <- plm(
      formula,
      data = data,
      index = c("country_code", "year"),
      model = "within",
      effect = "twoways"
    )
    
    robust_models[[paste0("Base_", dem_var)]] <- model
    
    # Event study with this democracy measure
    # This assumes you've created event variables using this measure too
    event_var <- paste0(gsub("di_score", "eiu", dem_var), "_aut_event_time")
    
    # Check if event variable exists
    if (event_var %in% names(data)) {
      event_formula <- as.formula(paste("spi_comp ~", dem_var, "* factor(", 
                                       event_var, ") + log_gdppc + income_level_recoded"))
      
      event_model <- plm(
        event_formula,
        data = data,
        index = c("country_code", "year"),
        model = "within",
        effect = "twoways"
      )
      
      robust_models[[paste0("Event_", dem_var)]] <- event_model
    }
  }
  
  return(robust_models)
}

# Example usage with alternative democracy measures
alternative_dem_vars <- c("di_score", "v2x_polyarchy", "fh_total")
robustness_models <- democracy_measure_robustness(panel_data_analysis, alternative_dem_vars)

# Compare coefficients across measures
stargazer(
  robustness_models,
  type = "text",
  title = "Robustness to Alternative Democracy Measures",
  column.labels = names(robustness_models),
  model.numbers = FALSE
)