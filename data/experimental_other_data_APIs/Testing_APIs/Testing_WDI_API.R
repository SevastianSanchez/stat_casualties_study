# Install and load the WDI package
if (!require(WDI)) install.packages("WDI")
library(WDI)

# Define the indicator codes for SPI overall and the 5 pillars
spi_indicators <- c(
  overall = "IQ.SPI.OVRL",
  p1_use = "IQ.SPI.PIL1",
  p2_services = "IQ.SPI.PIL2",
  p3_products = "IQ.SPI.PIL3",
  p4_sources = "IQ.SPI.PIL4",
  p5_infra = "IQ.SPI.PIL5"
)

# Download the data for all available years and countries
spi_data <- WDI(
  country = "all",
  indicator = spi_indicators,
  start = 2016,  # or your preferred start year
  end = as.numeric(format(Sys.Date(), "%Y"))  # current year
)

# View the first few rows
head(spi_data)


# Define SCI indicators (overall and subscores)
sci_indicators <- c(
  overall = "IQ.SCI.OVRL",           # Overall SCI score (0-100)
  availability = "IQ.SCI.AVAIL",     # Data availability subscore
  collection = "IQ.SCI.COLLECT",     # Data collection subscore
  practice = "IQ.SCI.PRACTICE"       # Statistical practice subscore
)

# Fetch SCI data (available up to 2020)
sci_data <- WDI(
  country = "all",
  indicator = sci_indicators,
  start = 2004,   # SCI starts in 2004
  end = 2020      # Last year of SCI data
)

# View the first few rows
head(sci_data)


