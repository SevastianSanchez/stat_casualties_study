#---------------------------------------------------------------------
# ADDITIONAL ANALYSES FOR YOUR THESIS
#---------------------------------------------------------------------

# 1. HETEROGENEITY ANALYSIS BY REGION OR INCOME GROUP
# Examine if effects differ by region or income level

region_heterogeneity <- function(data, region_var = "region") {
  regions <- unique(data[[region_var]])
  
  region_models <- list()
  for (r in regions) {
    region_data <- data %>% filter(get(region_var) == r)
    
    # Skip regions with insufficient data
    if (nrow(region_data) < 30) next
    
    model <- feols(
      spi_comp ~ i(eiu_regch_event, ref = "No Transition") + 
                di_score + log_gdppc + income_level_recoded | 
                country_code + year,
      data = region_data,
      cluster = "country_code"
    )
    
    region_models[[r]] <- model
  }
  
  return(region_models)
}

# 2. DOSE-RESPONSE ANALYSIS
# Examine if larger democracy changes have proportionally larger effects

dose_response <- function(data) {
  # Create change magnitude variable
  data <- data %>%
    group_by(country_code) %>%
    mutate(
      di_change_magnitude = abs(di_score - lag(di_score)),
      change_quartile = ntile(di_change_magnitude, 4)
    ) %>%
    ungroup()
  
  # Separate models by change magnitude quartile
  quartile_models <- list()
  for (q in 1:4) {
    q_data <- data %>% filter(change_quartile == q)
    
    model <- plm(
      spi_comp ~ di_score + log_gdppc + income_level_recoded,
      data = q_data,
      index = c("country_code", "year"),
      model = "within",
      effect = "twoways"
    )
    
    quartile_models[[paste("Quartile", q)]] <- model
  }
  
  return(quartile_models)
}

# 3. COMPONENT ANALYSIS
# Examine if regime changes affect different SPI components differently

component_analysis <- function(data) {
  # Assuming you have component variables like spi_basic, spi_opportunity, etc.
  components <- c("spi_basic", "spi_foundations", "spi_opportunity")
  
  component_models <- list()
  for (comp in components) {
    formula <- as.formula(paste(comp, "~ di_score + log_gdppc + income_level_recoded"))
    
    model <- plm(
      formula,
      data = data,
      index = c("country_code", "year"),
      model = "within",
      effect = "twoways"
    )
    
    component_models[[comp]] <- model
  }
  
  return(component_models)
}

# 4. MECHANISM ANALYSIS
# Investigate potential mediating variables

mechanism_analysis <- function(data, mediator_var) {
  # Step 1: Regime change -> Mediator
  mediator_formula <- as.formula(paste(mediator_var, "~ di_score + log_gdppc + income_level_recoded"))
  mediator_model <- plm(
    mediator_formula,
    data = data,
    index = c("country_code", "year"),
    model = "within",
    effect = "twoways"
  )
  
  # Step 2: Mediator -> Outcome
  outcome_with_mediator <- plm(
    spi_comp ~ di_score + get(mediator_var) + log_gdppc + income_level_recoded,
    data = data,
    index = c("country_code", "year"),
    model = "within",
    effect = "twoways"
  )
  
  # Step 3: Direct effect (for comparison)
  direct_effect <- plm(
    spi_comp ~ di_score + log_gdppc + income_level_recoded,
    data = data,
    index = c("country_code", "year"),
    model = "within",
    effect = "twoways"
  )
  
  return(list(
    "Mediator Effect" = mediator_model,
    "Outcome with Mediator" = outcome_with_mediator,
    "Direct Effect" = direct_effect
  ))
}