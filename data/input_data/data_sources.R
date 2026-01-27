library(tidyverse)
library(readxl)
library(devtools)
#devtools::install_github("vdeminstitute/vdemdata")
library(vdemdata) # call vdem package 
devtools::install_github("vdeminstitute/ERT")
#library(ERT) # call ERT package [NOT USED, IN DEVELOPMENT STAGES]
library(WDI) # call WDI package for GINI coefficient

#calls packages 
source("packages.R")

# set working directory 
setwd("~/Documents/GitHub/QMSS_Thesis_Sanchez")
##### SOURCES #####

#Vdem package from github [API]
vdem <- vdemdata::vdem %>% 
  filter(year >= 2000)
#write.csv(vdem, "data/misc/vdem.csv")

#ERT package
ert <- read.csv("data/input_data/ert.csv") %>% 
  filter(year >= 2000)
#write.csv(ert, "data/misc/ert.csv")

#spi csv from github [API]
url <- "https://raw.githubusercontent.com/worldbank/SPI/refs/heads/master/03_output_data/SPI_index.csv"
spi <- read_csv(url) 
#write.csv(spi, "data/misc/spi.csv")

#sdg excel from directory 
sdg <- read_excel("data/input_data/SDR2024-data.xlsx", sheet = "Backdated SDG Index") %>% 
  filter(year >= 2000)
#write.csv(sdg, "data/misc/sdg.csv")

#sci csv from directory 
sci_df <- read_csv("data/input_data/SCI_All_Dim_TS.csv")
#write.csv(sci_df, "data/misc/sci.csv")

#GDP per capita
gdppc_df <- read_csv("data/input_data/gdppc_df_long.csv") %>% 
  filter(year >= 2000)
#write.csv(gdppc_df, "data/misc/gdppc.csv")

#Information Capacity 
info_cap <- read_csv("data/input_data/information_capacity.csv") %>% 
  filter(year >= 2000)
#write.csv(info_cap, "data/misc/info_cap.csv")

#WB GNI Classifications 
gni_class <- read_csv("data/input_data/world_bank_income_classifications.csv") %>% 
  filter(year >= 2000)
#write.csv(gni_class, "data/misc/gni_class.csv")

#EIU Democracy Index 
di <- read_csv("data/input_data/democracy-index-eiu.csv") %>% 
  filter(year >= 2000)
#write.csv(di, "data/misc/di.csv")

#GINI Coefficient - Income [API]
gini <- WDI(country = "all", indicator = "SI.POV.GINI", start = 2000, end = NULL)
