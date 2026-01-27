# Function that extracts model data for plm models

extract_plm_data <- function(plm_model) {
  # Get index and model frame
  idx <- index(plm_model)
  mf <- model.frame(plm_model)
  
  # Combine with index first
  result <- cbind(
    data.frame(country_code = idx[,1], year = idx[,2]),
    mf
  )
  
  rownames(result) <- NULL
  return(result)
}
