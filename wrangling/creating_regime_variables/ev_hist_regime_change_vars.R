# R Code for identifying years before significant backsliding
library(dplyr)

# Load your panel data
#panel_data <- read.csv("panel_data_perp_again.csv")

# Sort data by country and year
panel_data1 <- panel_data %>%
  arrange(country_code, year) %>%
  group_by(country_code) %>%
  mutate(
    # Calculate change from current year to next year (negative = drop)
    di_score_diff_next = dplyr::lead(di_score) - di_score,
    
    # Calculate cumulative changes over next 3 and 4 years
    di_score_change_2yr = dplyr::lead(di_score, 2) - di_score,
    di_score_change_3yr = dplyr::lead(di_score, 3) - di_score,
    di_score_change_4yr = dplyr::lead(di_score, 4) - di_score,
    di_score_change_5yr = dplyr::lead(di_score, 5) - di_score,
    
    # Autocratization: Set thresholds for significant drops (adjust these as needed)
    abrupt_drop_threshold = -0.5,    # Single year drop threshold
    cumulative_drop_threshold = -0.75, # Multi-year drop threshold
    
    # Autocratization: Identify different types of drops
    abrupt_drop_next = di_score_diff_next <= abrupt_drop_threshold, # Abrupt drop next year
    cumulative_drop_2yr = di_score_change_2yr <= cumulative_drop_threshold, # Cumulative drop over 2 years
    cumulative_drop_3yr = di_score_change_3yr <= cumulative_drop_threshold, # Cumulative drop over 3 years
    cumulative_drop_4yr = di_score_change_4yr <= cumulative_drop_threshold, # Cumulative drop over 4 years
    cumulative_drop_5yr = di_score_change_5yr <= cumulative_drop_threshold, # Cumulative drop over 5 years
    
    # Autocratization: Create main indicator: year before significant backsliding
    pre_backsliding_year = case_when(
      abrupt_drop_next ~ 1,           # Year before abrupt drop
      cumulative_drop_2yr ~ 1,        # Year before 2-year cumulative drop
      cumulative_drop_3yr ~ 1,        # Year before 3-year cumulative drop
      cumulative_drop_4yr ~ 1,        # Year before 4-year cumulative drop
      cumulative_drop_5yr ~ 1,        # Year before 5-year cumulative drop
      TRUE ~ 0
    ),
    
    # Democratization: Set thresholds for significant drops (adjust these as needed)
    abrupt_rise_threshold = 0.5,     # Single year rise threshold
    cumulative_rise_threshold = 0.75,  # Multi-year rise threshold
    
    # Democratization: Identify different types of rises
    abrupt_rise_next = di_score_diff_next >= abrupt_rise_threshold, # Abrupt rise next year
    cumulative_rise_2yr = di_score_change_2yr >= cumulative_rise_threshold, # Cumulative rise over 2 years
    cumulative_rise_3yr = di_score_change_3yr >= cumulative_rise_threshold, # Cumulative rise over 3 years
    cumulative_rise_4yr = di_score_change_4yr >= cumulative_rise_threshold, # Cumulative rise over 4 years
    cumulative_rise_5yr = di_score_change_5yr >= cumulative_rise_threshold, # Cumulative rise over 5 years
    
    # Democratization: Create main indicator: year before significant democratization
    pre_democratization_year = case_when(
      abrupt_rise_next ~ 1,           # Year before abrupt rise
      cumulative_rise_2yr ~ 1,        # Year before 2-year cumulative rise
      cumulative_rise_3yr ~ 1,        # Year before 3-year cumulative rise
      cumulative_rise_4yr ~ 1,        # Year before 4-year cumulative rise
      cumulative_rise_5yr ~ 1,        # Year before 5-year cumulative rise
      TRUE ~ 0
    )
  ) %>%
  ungroup()

# View results for a specific country
panel_data1 %>%
  filter(country_code == "HUN") %>%  # Hungary as example
  select(country_name, year, di_score, di_score_diff_next, di_score_change_2yr,
         di_score_change_3yr, di_score_change_4yr, di_score_change_5yr, pre_backsliding_year) %>%
  print()
