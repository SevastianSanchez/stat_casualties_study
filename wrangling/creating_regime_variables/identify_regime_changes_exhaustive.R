# Regime change identification for discrete event study analysis -- Component 3
# Combines best elements from both methodologies with consistent naming

# loading packages
source("packages.R")

# Function to identify regime changes and episodes using EIU data
eiu_identify_regime_changes <- function(data) {
  # Validate all columns exist in DF
  required_cols <- c("country_code", "year", "di_score", "regime_type_eiu")
  missing_cols <- setdiff(required_cols, names(data)) 
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  result <- data %>%
    arrange(country_code, year) %>% 
    group_by(country_code) %>%
    mutate(
      # Lag variables for regime type and score
      regime_type_eiu_lag1 = dplyr::lag(regime_type_eiu, 1),
      di_score_lag1 = dplyr::lag(di_score, 1),
      
      #------------------------------------------------------------
      # 1. IDENTIFY FUTURE CHANGES (event study pre-periods)
      #------------------------------------------------------------
      # Calculate changes over multiple future time horizons
      di_score_diff_next = dplyr::lead(di_score) - di_score,
      di_score_change_2yr = dplyr::lead(di_score, 2) - di_score,
      di_score_change_3yr = dplyr::lead(di_score, 3) - di_score,
      di_score_change_4yr = dplyr::lead(di_score, 4) - di_score,
      di_score_change_5yr = dplyr::lead(di_score, 5) - di_score,
      
      # Set thresholds for significant changes
      abrupt_change_threshold = 0.5,    # Single year change threshold
      cumulative_change_threshold = 0.5, # Multi-year change threshold
      
      # Identify pre-autocratization years (di_score decreases)
      eiu_pre_aut_ep = case_when(
        dplyr::lead(di_score) - di_score <= -abrupt_change_threshold ~ 1,
        di_score_change_2yr <= -cumulative_change_threshold ~ 1,
        di_score_change_3yr <= -cumulative_change_threshold ~ 1,
        di_score_change_4yr <= -cumulative_change_threshold ~ 1,
        di_score_change_5yr <= -cumulative_change_threshold ~ 1,
        TRUE ~ 0
      ),
      
      # Identify pre-democratization years (di_score increases)
      eiu_pre_dem_ep = case_when(
        dplyr::lead(di_score) - di_score >= abrupt_change_threshold ~ 1,
        di_score_change_2yr >= cumulative_change_threshold ~ 1,
        di_score_change_3yr >= cumulative_change_threshold ~ 1,
        di_score_change_4yr >= cumulative_change_threshold ~ 1,
        di_score_change_5yr >= cumulative_change_threshold ~ 1,
        TRUE ~ 0
      ),
      
      # Pre-change indicator for any significant change 
      eiu_pre_regch = case_when(
        eiu_pre_aut_ep == 1 | eiu_pre_dem_ep == 1 ~ 1,
        TRUE ~ 0
      ),
      
      # Identify direction of the future change
      eiu_future_chg_dir = case_when(
        eiu_pre_aut_ep == 1 ~ -1,  # Autocratization coming
        eiu_pre_dem_ep == 1 ~ 1,   # Democratization coming
        TRUE ~ 0                   # No significant change coming
      ),
      
      #------------------------------------------------------------
      # 2. IDENTIFY ACTUAL REGIME TRANSITIONS (threshold crossings)
      #------------------------------------------------------------
      # A regime transition occurs when a country crosses the democracy threshold (5.0)
      # and maintains the new regime type for at least 3 years
      
      eiu_regch_event = case_when(
        # Democratization event (crossing 5.0 threshold upward)
        regime_type_eiu == 1 & regime_type_eiu_lag1 == 0 &
          dplyr::lead(regime_type_eiu) == 1 & dplyr::lead(regime_type_eiu, 2) == 1 ~ 1,
        
        # Autocratization event (crossing 5.0 threshold downward)
        regime_type_eiu == 0 & regime_type_eiu_lag1 == 1 &
          dplyr::lead(regime_type_eiu) == 0 & dplyr::lead(regime_type_eiu, 2) == 0 ~ -1,
        
        # No regime transition
        TRUE ~ 0
      ),
      
      #------------------------------------------------------------
      # 3. IDENTIFY EPISODES OF CHANGE (significant changes in score)
      #------------------------------------------------------------
      # Calculate past changes to identify episodes
      di_score_diff = di_score - di_score_lag1,
      di_score_diff_2yr = di_score - lag(di_score, 2),
      di_score_diff_3yr = di_score - lag(di_score, 3),
      di_score_diff_4yr = di_score - lag(di_score, 4),
      di_score_diff_5yr = di_score - lag(di_score, 5),
      
      # Identify autocratization episodes (significant negative changes)
      eiu_aut_ep = case_when(
        di_score_diff <= -abrupt_change_threshold ~ 1,
        di_score_diff_2yr <= -cumulative_change_threshold & di_score_diff < 0 ~ 1,
        di_score_diff_3yr <= -cumulative_change_threshold & di_score_diff < 0 ~ 1,
        di_score_diff_4yr <= -cumulative_change_threshold & di_score_diff < 0 ~ 1,
        di_score_diff_5yr <= -cumulative_change_threshold & di_score_diff < 0 ~ 1,
        TRUE ~ 0
      ),
      
      # Identify democratization episodes (significant positive changes)
      eiu_dem_ep = case_when(
        di_score_diff >= abrupt_change_threshold ~ 1,
        di_score_diff_2yr >= cumulative_change_threshold & di_score_diff > 0 ~ 1,
        di_score_diff_3yr >= cumulative_change_threshold & di_score_diff > 0 ~ 1,
        di_score_diff_4yr >= cumulative_change_threshold & di_score_diff > 0 ~ 1,
        di_score_diff_5yr >= cumulative_change_threshold & di_score_diff > 0 ~ 1,
        TRUE ~ 0
      ),
      
      # Combined episode indicator
      eiu_regch_ep = case_when(
        eiu_aut_ep == 1 ~ -1,    # Autocratization episode
        eiu_dem_ep == 1 ~ 1,     # Democratization episode
        TRUE ~ 0                 # No episode
      )
    ) %>%
    
    #------------------------------------------------------------
    # 4. COUNTRY-LEVEL IDENTIFIERS (for sample selection)
    #------------------------------------------------------------
    mutate(
      # Total episodes by country
      eiu_total_aut_ep = sum(eiu_aut_ep, na.rm = TRUE),
      eiu_total_dem_ep = sum(eiu_dem_ep, na.rm = TRUE),
      
      # Binary indicators for country classification
      eiu_has_aut_ep = ifelse(eiu_total_aut_ep > 0, 1, 0),
      eiu_has_dem_ep = ifelse(eiu_total_dem_ep > 0, 1, 0),
      eiu_has_both = ifelse(eiu_has_aut_ep == 1 & eiu_has_dem_ep == 1, 1, 0),
      eiu_has_neither = ifelse(eiu_has_aut_ep == 0 & eiu_has_dem_ep == 0, 1, 0),
      
      # Regime transition indicators
      eiu_democratized = ifelse(any(eiu_regch_event == 1, na.rm = TRUE), 1, 0),
      eiu_autocratized = ifelse(any(eiu_regch_event == -1, na.rm = TRUE), 1, 0),
      eiu_stable = ifelse(eiu_democratized == 0 & eiu_autocratized == 0, 1, 0),
      
      # First transition years
      first_dem_year = if(any(eiu_regch_event == 1, na.rm = TRUE)) {
        min(year[eiu_regch_event == 1], na.rm = TRUE)
      } else {
        NA_integer_
      },
      
      first_aut_year = if(any(eiu_regch_event == -1, na.rm = TRUE)) {
        min(year[eiu_regch_event == -1], na.rm = TRUE)
      } else {
        NA_integer_
      },
      
      # First episode years
      first_dem_ep_year = if(any(eiu_dem_ep == 1, na.rm = TRUE)) {
        min(year[eiu_dem_ep == 1], na.rm = TRUE)
      } else {
        NA_integer_
      },
      
      first_aut_ep_year = if(any(eiu_aut_ep == 1, na.rm = TRUE)) {
        min(year[eiu_aut_ep == 1], na.rm = TRUE)
      } else {
        NA_integer_
      }
    ) %>%
    
    # Create event time variables (for event study)
    mutate(
      # Years relative to transitions
      eiu_dem_event_time = if_else(!is.na(first_dem_year), year - first_dem_year, NA_integer_),
      eiu_aut_event_time = if_else(!is.na(first_aut_year), year - first_aut_year, NA_integer_),
      
      # Years relative to episodes
      eiu_dem_ep_time = if_else(!is.na(first_dem_ep_year), year - first_dem_ep_year, NA_integer_),
      eiu_aut_ep_time = if_else(!is.na(first_aut_ep_year), year - first_aut_ep_year, NA_integer_)
    ) %>%
    
    # Clean up temporary variables
    select(-first_dem_year, -first_aut_year, -first_dem_ep_year, -first_aut_ep_year) %>%
    ungroup() %>%
    
    # Convert to factors for categorical analysis
    mutate(
      eiu_regime_type = factor(eiu_regime_type, levels = c(0, 1), labels = c("Autocracy", "Democracy")),
      eiu_pre_regch = factor(eiu_pre_regch),
      eiu_pre_aut_ep = factor(eiu_pre_aut_ep),
      eiu_pre_dem_ep = factor(eiu_pre_dem_ep),
      eiu_future_chg_dir = factor(eiu_future_chg_dir, levels = c(-1, 0, 1), 
                                  labels = c("Autocratization", "No Change", "Democratization")),
      eiu_regch_event = factor(eiu_regch_event, levels = c(-1, 0, 1), 
                               labels = c("Autocratized", "No Transition", "Democratized")),
      eiu_regch_ep = factor(eiu_regch_ep, levels = c(-1, 0, 1), 
                            labels = c("Autocratization", "No Episode", "Democratization")),
      eiu_aut_ep = factor(eiu_aut_ep),
      eiu_dem_ep = factor(eiu_dem_ep),
      eiu_has_aut_ep = factor(eiu_has_aut_ep),
      eiu_has_dem_ep = factor(eiu_has_dem_ep),
      eiu_has_both = factor(eiu_has_both),
      eiu_has_neither = factor(eiu_has_neither),
      eiu_democratized = factor(eiu_democratized),
      eiu_autocratized = factor(eiu_autocratized),
      eiu_stable = factor(eiu_stable)
    )
  
  return(result)
}

# testing function
source("Comp2_panel_wrangling.R") # to get panel_data
testing_df <- eiu_identify_regime_changes(panel_data)
# rm(panel_data) # to free up memory


