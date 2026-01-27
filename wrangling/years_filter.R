library(tidyverse)

#calls sources
source("data/input_data/data_sources.R")

#function to extract data from specified years 
years_filter <- function(start_yr = 2005, end_yr = 2023, 
                          df1=vdem, #vdem data ONLY
                          df2=spi, #spi data ONLY
                          df3=sdg, #sdg data ONLY
                          df4=sci_df, #sci data ONLY
                          df5=ert, #ert data ONLY
                          df6=gdppc_df, #gdppc_dta data ONLY
                          #df7=info_cap, #info_cap data ONLY
                          df8=gni_class, #gni_class data ONLY
                          df9=di, # di_score data ONLY
                          df10 = gini # di data ONLY 
                          #df10=odin, odin_yr=yr1 #odin data ONLY
                          ){ 
  
  #VDEM DATASET
  
  name1 <- df1 %>%
    dplyr::select(country_name, country_text_id, year, v2x_regime, 
                  v2x_regime_amb, v2x_polyarchy, v2x_libdem, v2x_partipdem, 
                  v2x_delibdem, v2x_egaldem, v2xel_frefair, v2x_accountability, 
                  v2x_veracc, v2x_horacc, v2x_diagacc, v2xca_academ, 
                  v2x_freexp_altinf, e_wb_pop) %>%
    rename(regime_type_4 = v2x_regime, # MAIN RoW Regime Type Variable 
           regime_type_10 = v2x_regime_amb,
           elect_dem = v2x_polyarchy,
           lib_dem = v2x_libdem,
           part_dem = v2x_partipdem,
           delib_dem = v2x_delibdem,
           egal_dem = v2x_egaldem,
           freefair = v2xel_frefair,
           #freefair_ord = e_v2xel_frefair,
           accountability = v2x_accountability,
           vt_account = v2x_veracc,
           hz_account = v2x_horacc,
           diag_account = v2x_diagacc,
           academ_free = v2xca_academ,
           freexp_altinfo = v2x_freexp_altinf,
           population = e_wb_pop) %>% 
    dplyr::mutate(log_pop = log(population)) %>% 
    dplyr::filter(year >= start_yr, year <= end_yr)
  
  #SPI DATASET
  name2 <- df2 %>% 
    dplyr::select(country, iso3c, date, SPI.INDEX, SPI.INDEX.PIL1, 
                  SPI.INDEX.PIL2, SPI.INDEX.PIL3, SPI.INDEX.PIL4, 
                  SPI.INDEX.PIL5, income, region, weights) %>% 
    #rename(country_code = iso3c) %>% 
    rename(year = date,
           spi_comp = SPI.INDEX,
           p1_use = SPI.INDEX.PIL1, 
           p2_services = SPI.INDEX.PIL2,
           p3_products = SPI.INDEX.PIL3,
           p4_sources = SPI.INDEX.PIL4,
           p5_infra = SPI.INDEX.PIL5,
           income_spi = income,
           region_spi = region,
           weights_spi = weights) %>% 
    dplyr::filter(year >= start_yr, year <= end_yr)
  
  #SDG DATASET
  name3 <- df3 %>% 
    dplyr::select(country_name, country_code, year, sdg_overall, goal1, goal2, 
                  goal3, goal4, goal5, goal6, goal7, goal8, goal9, goal10, 
                  goal11, goal12, goal13, goal14, goal15, goal16, goal17) %>% 
    dplyr::filter(year >= start_yr, year <= end_yr)
  
  #SCI DATASET
  name4 <- df4 %>% 
    dplyr::select(country_name, country_code, Year, IQ.SCI.OVRL, IQ.SCI.MTHD, 
                  IQ.SCI.PRDC, IQ.SCI.SRCE) %>% 
    # Replacing ".." with NA explicitly
    dplyr::mutate(across(c(IQ.SCI.OVRL, IQ.SCI.MTHD, IQ.SCI.PRDC, IQ.SCI.SRCE), 
                         ~ ifelse(. == "..", NA_character_, .))) %>%
    # converting to numeric
    dplyr::mutate(
      year = as.numeric(Year),
      sci_overall = as.numeric(IQ.SCI.OVRL), 
      sci_method = as.numeric(IQ.SCI.MTHD), 
      sci_periodicity = as.numeric(IQ.SCI.PRDC),
      sci_source = as.numeric(IQ.SCI.SRCE)) %>% 
  # Selecting relevant columns
  dplyr::select(country_name, country_code, year, sci_overall, sci_method, 
                sci_periodicity, sci_source) %>%
  dplyr::filter(year >= start_yr, year <= end_yr)

  #ERT DATASET 
  name5 <- df5 %>%
    dplyr::select(country_name, country_text_id, country_id, year, reg_type, 
                  v2x_polyarchy, row_regch_event, reg_trans, dem_ep, 
                  dem_pre_ep_year, dem_ep_start_year, dem_ep_end_year, aut_ep, 
                  aut_pre_ep_year, aut_ep_start_year, aut_ep_end_year) %>%
    dplyr::filter(year >= start_yr, year <= end_yr) %>% 
    rename(regime_type_2 = reg_type,
           elect_dem_ert = v2x_polyarchy,
           regch_event = row_regch_event,
           regch_genuine = reg_trans,
           dem_ep_pre_yr = dem_pre_ep_year,
           dem_ep_start_yr = dem_ep_start_year,
           dem_ep_end_yr = dem_ep_end_year,
           aut_ep_pre_yr = aut_pre_ep_year,
           aut_ep_start_yr = aut_ep_start_year,
           aut_ep_end_yr = aut_ep_end_year)
  
  #GDP Per Capita
  name6 <- df6 %>% 
    dplyr::select(country_name, country_code, year, gdp_pc) %>% 
    dplyr::mutate(log_gdppc = log(gdp_pc)) %>% 
    dplyr::filter(year >= start_yr, year <= end_yr)
   
  #INFO CAPACITY
  #name7 <- df7 %>% dplyr::select(country_id, year, infcap_irt, infcap_pca, 
  #everything()) %>% filter(year >= start_yr, year <= end_yr)
  
 #WB Income Classifications 
  name8 <- df8 %>% 
    dplyr::mutate(income_level = na_if(income_level, "..")) %>% 
    dplyr::mutate(income_level_lab = factor(income_level, 
      # H (higher income) as the reference category 
      levels = c("H", "UM", "LM", "L"),  # Desired order
      labels = c("High Income Countries", 
                 "Upper-Middle Income Countries", 
                 "Lower-Middle Income Countries", 
                 "Low Income Countries")  # Full descriptive labels
    )) %>% 
    filter(year >= start_yr, year <= end_yr)
  
  #EIU Democracy Index  
  name9 <- df9 %>% 
    filter(year >= start_yr, year <= end_yr)
  
  #GINI Coefficient 
  name10 <- df10 %>% 
    dplyr::select(country, iso3c, year, SI.POV.GINI) %>% 
    dplyr::rename(country_name = country, 
                  country_code = iso3c, 
                  gini_score = SI.POV.GINI) %>%
    dplyr::mutate(year = as.numeric(year)) %>% 
    dplyr::filter(year >= start_yr, year <= end_yr)
  
  return(list(vdem = name1, 
              spi = name2, 
              sdg = name3, 
              sci_df = name4, 
              ert = name5, 
              gdppc_df = name6, #info_cap = name7, 
              gni_class = name8, 
              di = name9, 
              gini = name10))
  
} 

#testing_years_filter <- years_filter(start_yr = 2014, end_yr = 2015) # Example usage to test the function
