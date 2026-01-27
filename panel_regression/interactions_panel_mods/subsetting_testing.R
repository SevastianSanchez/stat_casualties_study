# set working directory
setwd("~/Documents/GitHub/QMSS_Thesis_Sanchez")

#load libraries/packages
source("packages.R")

# load data
source("Comp2_panel_wrangling.R")
panel_data1 <- panel_data %>% 
  dplyr::select(country_name, country_code, year, spi_comp, di_score, 
                log_gdppc, sdg_overall, income_level_recoded, starts_with("goal")) %>%
  dplyr::arrange(country_code, year)

## subsetting by regime types and stability ##

panel_data_test <- panel_data1 %>%
  arrange(country_code, year) %>%
  group_by(country_code) %>%
  mutate(
    regime_type_eiu = case_when(
      di_score >= 8.0 ~ "Full democracy",
      di_score >= 6.0 & di_score < 8.0 ~ "Flawed democracy",
      di_score >= 4.0 & di_score < 6.0 ~ "Hybrid regime",
      di_score < 4.0 ~ "Authoritarian",
      TRUE ~ NA_character_
    ),
    regime_transition_eiu = regime_type_eiu != dplyr::lag(regime_type_eiu),  # Did regime change?
    regime_stable_eiu = !regime_transition_eiu | is.na(regime_transition_eiu)  # Stable years
  ) %>%
  ungroup()

# Function that counts number of unique countries by regime type
get_regime_counts <- function(regime_type, stable = NULL, 
                                   transition = NULL, return_names = FALSE) {
  
  result <- panel_data_test
  
  # Filter by regime type
  result <- result %>%
    filter(regime_type_eiu == regime_type)
  
  # Filter by stability if specified
  if (!is.null(stable)) {
    result <- result %>%
      filter(regime_stable_eiu == stable)
  }
  
  # Filter by transition if specified
  if (!is.null(transition)) {
    result <- result %>%
      filter(regime_transition_eiu == transition)
  }
  
  # Get distinct countries
  countries <- result %>%
    distinct(country_code) %>%
    pull(country_code)
  
  # Return based on argument
  if (return_names) {
    list(
      count = length(countries),
      countries = countries
    )
  } else {
    length(countries)
  }
}
# Example usage:
countries_testing <- get_regime_counts("Full democracy", stable = FALSE, return_names = TRUE)

# New function: focuses on subsetting
get_regime_data <- function(regime_type, stable = NULL, transition = NULL) {
  
  # Reuse the original function to get countries
  matching <- get_regime_counts(
    regime_type, 
    stable = stable, 
    transition = transition, 
    return_names = TRUE
  )$countries
  
  # Return filtered data
  panel_data_test %>%
    filter(country_code %in% matching)
}

# Example usage:
regime_data_testing <- get_regime_data(c("Full democracy", "Flawed democracy", "Hybrid regime", "Authoritarian"), transition = TRUE)

### DIFFERENT VARIATIONS ###
# Democracy - Full democracies only
# regime_data <- get_regime_data("Full democracy")

# Flawed democracies only
# regime_data <- get_regime_data("Flawed democracy")

# Hybrid regimes only
# regime_data <- get_regime_data("Hybrid regime")

# Authoritarian regimes only
# regime_data <- get_regime_data("Authoritarian")

# Democracies with regime transitions only
# regime_data <- get_regime_data("Full democracy", stable = FALSE)

# Flawed democracies with regime transitions only
# regime_data <- get_regime_data("Flawed democracy", stable = FALSE)

# Hybrid regimes with regime transitions only
# regime_data <- get_regime_data("Hybrid regime", stable = FALSE)

# Authoritarian regimes with regime transitions only
# regime_data <- get_regime_data("Authoritarian", stable = FALSE)

# All regimes
regime_data <- get_regime_data(c("Full democracy", "Flawed democracy", "Hybrid regime", "Authoritarian"))

=================================================================================
##### TESTING MODERATOR TERMS ####
regime_data_plm <- pdata.frame(regime_data, index = c("country_code", "year"))

# FE interaction model
sdg_spixdi_fe <- plm(formula = sdg_overall ~ spi_comp*di_score + log_gdppc + factor(year),
                      model = "within", 
                      data = regime_data_plm)
summary(sdg_spixdi_fe, vcov = vcovHC(sdg_spixdi_fe, cluster = "group", type = "HC1"))

# RE interaction model
sdg_spixdi_re <- plm(formula = sdg_overall ~ spi_comp*di_score + log_gdppc,
                      model = "random", 
                      data = regime_data_plm)
summary(sdg_spixdi_re, vcov = vcovHC(sdg_spixdi_re, cluster = "group", type = "HC1"))

# hausman test 
phtest(sdg_spixdi_fe, sdg_spixdi_re)

# FE interaction model with regime type as moderator
sdg_spixregime_fe <- plm(formula = sdg_overall ~ spi_comp*factor(regime_type_eiu) + log_gdppc + factor(year),
                     model = "within", 
                     data = regime_data_plm)
summary(sdg_spixregime_fe, vcov = vcovHC(sdg_spixregime_fe, cluster = "group", type = "HC1"))
