library(jsonlite)

# Source from online: https://ourworldindata.org/grapher/democracy-index-eiu?tab=chart%20%20;%20%20https://www.eiu.com/n/campaigns/democracy-index-2022/
# "The indices are normalized to range from 0 to 1. The data is still not directly comparable because the sources
# have different definitions of democracy. The number of available sources differs by country or region."

# Fetch the data
df <- read.csv("https://ourworldindata.org/grapher/democracy-index-by-source.csv?v=1&csvType=full&useColumnShortNames=true")

# Fetch the metadata
metadata <- fromJSON("https://ourworldindata.org/grapher/democracy-index-by-source.metadata.json?v=1&csvType=full&useColumnShortNames=true")
