# create a codebook of all my panel_data variables, their data-type and descriptions

source("packages.R")

# Load the panel data
source("Comp2_panel_wrangling.R")

# Create a codebook
panel_codebook <- data.frame(
  Variable = names(panel_data),
  Data_Type = sapply(panel_data, class)
  )
