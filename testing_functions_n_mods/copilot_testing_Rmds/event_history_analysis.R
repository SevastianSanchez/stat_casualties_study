# Event History Analysis for Regime Change Effects on Social Progress
# This script analyzes how democratization and autocratization events affect social progress
# across different country subgroups

# Load required packages
library(dplyr)
library(ggplot2)
library(plm)
library(lmtest)
library(sandwich)
library(gridExtra)
library(fixest)
library(kableExtra)
library(stargazer)

#---------------------------------------------------------------------
# 1. DATA PREPARATION
#---------------------------------------------------------------------

# Apply our improved regime change identification function to the panel data
panel_data_regch <- eiu_identify_regime_changes(panel_data_with_regch)

# Select only the relevant columns for analysis
panel_data_analysis <- panel_data_regch %>%
  select(
    # Identifiers
    country_name, country_code, year,
    
    # Outcome variable
    spi_comp,
    
    # Key predictors
    di_score, log_gdppc, income_level_recoded,
    
    # Regime change variables
    eiu_regime_type, eiu_regch_event, 
    eiu_aut_ep, eiu_dem_ep, eiu_regch_ep,
    
    # Event timing variables
    eiu_aut_event_time, eiu_dem_event_time,
    eiu_aut_ep_time, eiu_dem_ep_time,
    
    # Pre-event indicators
    eiu_pre_aut_ep, eiu_pre_dem_ep, eiu_pre_regch,
    
    # Country-level indicators for subgrouping
    eiu_stable, eiu_has_neither, 
    eiu_has_aut_ep, eiu_has_dem_ep, eiu_has_both,
    eiu_autocratized, eiu_democratized
  ) %>%
  arrange(country_code, year)

#---------------------------------------------------------------------
# 2. CREATE SUBGROUPS FOR ANALYSIS
#---------------------------------------------------------------------

# 1. All countries (full dataset)
all_countries <- panel_data_analysis

# 2. Stable countries (no threshold transitions)
stable_countries <- panel_data_analysis %>%
  filter(eiu_stable == 1)

# 3. Countries with no episodes
no_episode_countries <- panel_data_analysis %>%
  filter(eiu_has_neither == 1)

# 4. Countries with autocratization episodes
autocratization_countries <- panel_data_analysis %>%
  filter(eiu_has_aut_ep == 1)

# 5. Countries with democratization episodes
democratization_countries <- panel_data_analysis %>%
  filter(eiu_has_dem_ep == 1)

# 6. Countries with both types of episodes
both_episode_countries <- panel_data_analysis %>%
  filter(eiu_has_both == 1)

# 7. Countries that transitioned to autocracies
autocratized_countries <- panel_data_analysis %>%
  filter(eiu_autocratized == 1)

# 8. Countries that transitioned to democracies
democratized_countries <- panel_data_analysis %>%
  filter(eiu_democratized == 1)

#---------------------------------------------------------------------
# 3. EVENT STUDY MODELS BY SUBGROUP
#---------------------------------------------------------------------

# Function to prepare data for event study analysis
prepare_event_data <- function(data, event_time_var, min_time = -5, max_time = 5) {
  # Filter to countries that have the event
  # Ensure event time is numeric by converting from factor if needed
  data %>%
    mutate(event_time = as.numeric(as.character(get(event_time_var)))) %>%
    filter(!is.na(event_time)) %>%
    # Restrict to time window around event
    filter(event_time >= min_time & event_time <= max_time) %>%
    # Create event time factor (for plotting coefficients)
    mutate(event_time_factor = factor(event_time, levels = min_time:max_time))
}

#---------------------------------------------------------------------
# 4. AUTOCRATIZATION TRANSITION EVENT STUDY
#---------------------------------------------------------------------

# Prepare data for autocratization event study
aut_event_data <- prepare_event_data(autocratized_countries, "eiu_aut_event_time")

# Run the event study model
aut_event_model <- plm(
  spi_comp ~ di_score * factor(eiu_aut_event_time) + log_gdppc + income_level_recoded,
  data = aut_event_data,
  index = c("country_code", "year"),
  model = "within",
  effect = "twoways"
)

# Model with robust standard errors
aut_event_robust <- coeftest(
  aut_event_model, 
  vcov = vcovHC(aut_event_model, cluster = "group", type = "HC1")
)

#---------------------------------------------------------------------
# 5. DEMOCRATIZATION TRANSITION EVENT STUDY
#---------------------------------------------------------------------

# Prepare data for democratization event study
dem_event_data <- prepare_event_data(democratized_countries, "eiu_dem_event_time")

# Run the event study model
dem_event_model <- plm(
  spi_comp ~ di_score * factor(eiu_dem_event_time) + log_gdppc + income_level_recoded,
  data = dem_event_data,
  index = c("country_code", "year"),
  model = "within",
  effect = "twoways"
)

# Model with robust standard errors
dem_event_robust <- coeftest(
  dem_event_model, 
  vcov = vcovHC(dem_event_model, cluster = "group", type = "HC1")
)

#---------------------------------------------------------------------
# 6. AUTOCRATIZATION EPISODE EVENT STUDY
#---------------------------------------------------------------------

# Prepare data for autocratization episode study
aut_ep_data <- prepare_event_data(autocratization_countries, "eiu_aut_ep_time")

# Run the episode event study model
aut_ep_model <- plm(
  spi_comp ~ di_score * factor(eiu_aut_ep_time) + log_gdppc + income_level_recoded,
  data = aut_ep_data,
  index = c("country_code", "year"),
  model = "within",
  effect = "twoways"
)

# Model with robust standard errors
aut_ep_robust <- coeftest(
  aut_ep_model, 
  vcov = vcovHC(aut_ep_model, cluster = "group", type = "HC1")
)

#---------------------------------------------------------------------
# 7. DEMOCRATIZATION EPISODE EVENT STUDY
#---------------------------------------------------------------------

# Prepare data for democratization episode study
dem_ep_data <- prepare_event_data(democratization_countries, "eiu_dem_ep_time")

# Run the episode event study model
dem_ep_model <- plm(
  spi_comp ~ di_score * factor(eiu_dem_ep_time) + log_gdppc + income_level_recoded,
  data = dem_ep_data,
  index = c("country_code", "year"),
  model = "within",
  effect = "twoways"
)

# Model with robust standard errors
dem_ep_robust <- coeftest(
  dem_ep_model, 
  vcov = vcovHC(dem_ep_model, cluster = "group", type = "HC1")
)

#---------------------------------------------------------------------
# 8. COMPARING EFFECTS ACROSS SUBGROUPS
#---------------------------------------------------------------------

# Function to extract event time coefficients from a model
extract_event_coefficients <- function(model_robust, event_prefix, interaction = FALSE) {
  coefs <- coef(model_robust)
  se <- sqrt(diag(vcov(model_robust)))
  
  # Identify event time variables
  if (interaction) {
    pattern <- paste0("di_score:factor\\(", event_prefix, "_event_time\\)")
  } else {
    pattern <- paste0("factor\\(", event_prefix, "_event_time\\)")
  }
  
  event_vars <- grep(pattern, names(coefs), value = TRUE)
  
  # Extract coefficients and standard errors
  result <- data.frame(
    variable = event_vars,
    coefficient = coefs[event_vars],
    se = se[event_vars],
    time = as.numeric(gsub(".*\\)([0-9-]+)$", "\\1", event_vars))
  )
  
  # Add confidence intervals
  result$lower_ci <- result$coefficient - 1.96 * result$se
  result$upper_ci <- result$coefficient + 1.96 * result$se
  
  # Sort by time
  result <- result[order(result$time), ]
  result$event_type <- event_prefix
  
  return(result)
}

# Extract coefficients for plotting
aut_event_coefs <- extract_event_coefficients(aut_event_robust, "eiu_aut")
dem_event_coefs <- extract_event_coefficients(dem_event_robust, "eiu_dem")
aut_ep_coefs <- extract_event_coefficients(aut_ep_robust, "eiu_aut_ep")
dem_ep_coefs <- extract_event_coefficients(dem_ep_robust, "eiu_dem_ep")

# Also extract interaction coefficients
aut_event_int_coefs <- extract_event_coefficients(aut_event_robust, "eiu_aut", interaction = TRUE)
dem_event_int_coefs <- extract_event_coefficients(dem_event_robust, "eiu_dem", interaction = TRUE)

#---------------------------------------------------------------------
# 9. VISUALIZING EVENT STUDY RESULTS
#---------------------------------------------------------------------

# Plot autocratization transition effects
plot_aut_event <- ggplot(aut_event_coefs, aes(x = time, y = coefficient)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "red") +
  theme_minimal() +
  labs(
    title = "Effect of Autocratization Transitions on Social Progress",
    subtitle = "Event study relative to transition year (t=0)",
    x = "Years relative to autocratization",
    y = "Effect on SPI Composite Score"
  )

# Plot democratization transition effects
plot_dem_event <- ggplot(dem_event_coefs, aes(x = time, y = coefficient)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "red") +
  theme_minimal() +
  labs(
    title = "Effect of Democratization Transitions on Social Progress",
    subtitle = "Event study relative to transition year (t=0)",
    x = "Years relative to democratization",
    y = "Effect on SPI Composite Score"
  )

# Plot autocratization episode effects
plot_aut_ep <- ggplot(aut_ep_coefs, aes(x = time, y = coefficient)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "red") +
  theme_minimal() +
  labs(
    title = "Effect of Autocratization Episodes on Social Progress",
    subtitle = "Event study relative to episode start year (t=0)",
    x = "Years relative to autocratization episode",
    y = "Effect on SPI Composite Score"
  )

# Plot democratization episode effects
plot_dem_ep <- ggplot(dem_ep_coefs, aes(x = time, y = coefficient)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "red") +
  theme_minimal() +
  labs(
    title = "Effect of Democratization Episodes on Social Progress",
    subtitle = "Event study relative to episode start year (t=0)",
    x = "Years relative to democratization episode",
    y = "Effect on SPI Composite Score"
  )

#---------------------------------------------------------------------
# 10. ALTERNATIVE ESTIMATION WITH FIXEST PACKAGE
#---------------------------------------------------------------------

# More modern approach using fixest package for event studies
# This provides better handling of time-relative effects and standard errors

# Autocratization transition event study with fixest
aut_event_feols <- feols(
  spi_comp ~ i(eiu_aut_event_time, ref = -0) + di_score + log_gdppc + income_level_recoded |
             country_code + year,
  data = aut_event_data,
  cluster = "country_code"
)

# Plot with coefplot from fixest
aut_event_plot <- coefplot(aut_event_feols, 
                          var = "i\\(eiu_aut_event_time.*\\)",
                          main = "Effect of Autocratization on Statistical Capacity",
                          xlab = "Years relative to transition (t=0)",
                          ylab = "Coefficient estimate")

# Democratization transition event study with fixest
dem_event_feols <- feols(
  spi_comp ~ i(eiu_dem_event_time, ref = -1) + di_score + log_gdppc + income_level_recoded |
             country_code + year,
  data = dem_event_data,
  cluster = "country_code"
)

# Plot with coefplot from fixest
dem_event_plot <- coefplot(dem_event_feols, 
                          var = "i\\(eiu_dem_event_time.*\\)",
                          main = "Effect of Democratization on Social Progress",
                          xlab = "Years relative to transition (t=0)",
                          ylab = "Coefficient estimate")

#---------------------------------------------------------------------
# 11. COMPARING MODELS ACROSS SUBGROUPS
#---------------------------------------------------------------------

# Function to run standard models across different subgroups
run_subgroup_models <- function(data_list, subgroup_names) {
  models_list <- list()
  
  for (i in seq_along(data_list)) {
    data <- data_list[[i]]
    name <- subgroup_names[i]
    
    # Skip if insufficient data
    if (nrow(data) < 30) {
      message(paste("Skipping", name, "due to insufficient data"))
      models_list[[name]] <- NULL
      next
    }
    
    # Run fixed effects model
    model <- plm(
      spi_comp ~ di_score + log_gdppc + income_level_recoded,
      data = data,
      index = c("country_code", "year"),
      model = "within",
      effect = "twoways"
    )
    
    models_list[[name]] <- model
  }
  
  return(models_list)
}

# Create list of datasets
data_list <- list(
  all_countries,
  stable_countries,
  no_episode_countries,
  autocratization_countries,
  democratization_countries,
  both_episode_countries,
  autocratized_countries,
  democratized_countries
)

# Names for subgroups
subgroup_names <- c(
  "All Countries",
  "Stable Regimes",
  "No Episodes",
  "Autocratization Episodes",
  "Democratization Episodes",
  "Both Episode Types",
  "Autocratized Transitions",
  "Democratized Transitions"
)

# Run models across subgroups
subgroup_models <- run_subgroup_models(data_list, subgroup_names)

# Generate model comparison table
stargazer(
  subgroup_models,
  type = "text",
  title = "Democracy and Social Progress Across Country Subgroups",
  column.labels = names(subgroup_models),
  model.numbers = FALSE,
  dep.var.labels = "Social Progress Index",
  covariate.labels = c("Democracy Index", "Log GDP per capita", "Income Level"),
  add.lines = list(c("Country FE", rep("Yes", length(subgroup_models))),
                  c("Year FE", rep("Yes", length(subgroup_models))))
)

#---------------------------------------------------------------------
# 12. PRE-EVENT ANALYSIS (for causal identification)
#---------------------------------------------------------------------

# Analyze periods before regime changes for potential anticipation effects
# This helps address pre-trends and validate causal identification

# Pre-autocratization analysis
pre_aut_data <- panel_data_analysis %>%
  filter(eiu_pre_aut_ep == 1)

pre_aut_model <- plm(
  spi_comp ~ di_score + log_gdppc + income_level_recoded,
  data = pre_aut_data,
  index = c("country_code", "year"),
  model = "within",
  effect = "twoways"
)

# Pre-democratization analysis
pre_dem_data <- panel_data_analysis %>%
  filter(eiu_pre_dem_ep == 1)

pre_dem_model <- plm(
  spi_comp ~ di_score + log_gdppc + income_level_recoded,
  data = pre_dem_data,
  index = c("country_code", "year"),
  model = "within",
  effect = "twoways"
)

# Compare regular periods to pre-event periods
pre_event_models <- list(
  "All Periods" = subgroup_models[["All Countries"]],
  "Pre-Autocratization" = pre_aut_model,
  "Pre-Democratization" = pre_dem_model
)

stargazer(
  pre_event_models,
  type = "text",
  title = "Comparing Pre-Event Periods to Regular Periods",
  column.labels = names(pre_event_models),
  model.numbers = FALSE,
  dep.var.labels = "Social Progress Index",
  covariate.labels = c("Democracy Index", "Log GDP per capita", "Income Level"),
  add.lines = list(c("Country FE", rep("Yes", length(pre_event_models))),
                  c("Year FE", rep("Yes", length(pre_event_models))))
)
