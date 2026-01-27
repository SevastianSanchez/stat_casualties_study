## provide list of countries in merged_final_df for reference

merged_final_df <- read_csv("data/Main CSV Outputs/merged_final_df.csv")
# List of countries in merged_final_sdg
countries_in_merged_final_df <- merged_final_df %>%
  distinct(country_code, country_name) %>%
  arrange(country_name)
View(countries_in_merged_final_df)

# saving countries_in_merged_final_sdg
#write_csv(countries_in_merged_final_df, "wrangling/adjust_outputs_diagnostics/merged_final_df_alignment/countries_in_merged_final_df.csv")

# load all_countries_studied_comparison.csv diagnostic file 
all_countries <- read_csv('wrangling/adjust_outputs_diagnostics/all_countries_studied_comparison.csv')

# extracting country sets
merged_final <- countries_in_merged_final_df$country_code[!is.na(countries_in_merged_final_df$country_code)]

# creating list of datasets to check alignment
datasets <- list(
  "merged_cleaned_spi" = all_countries$country_code[all_countries$in_merged_cleaned_spi == 1],
  "merged_cleaned_sdg" = all_countries$country_code[all_countries$in_merged_cleaned_sdg == 1],
  "merged_exclusive" = all_countries$country_code[all_countries$in_merged_exclusive == 1],
  "merged_inclusive" = all_countries$country_code[all_countries$in_merged_inclusive == 1]
)

# checking alignment 
for (name in names(datasets)) {
  if (setequal(datasets[[name]], merged_final)) {
    cat("✓", name, "dataset is IDENTICAL to merged_final\n")
  } else {
    cat("✗", name, "dataset is NOT identical to merged_final\n")
  }
}

# CONCLUDSION: merged_cleaned_sdg is the only dataset that IS identical 
# to merged_final_df
