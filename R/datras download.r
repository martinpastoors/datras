# -------------------------------------------------------------------------------
# Download all Datras and tidy it up
#
# http://www.hafro.is/~einarhj/datrasdoodle/import.html#data-download
#
# 14/08/2018 first coding
# 12/03/2019 small update in the coding of survey names
# 19/07/2019 removed gisland package; very minor updates; new download of files after losing HD!!
# -------------------------------------------------------------------------------

rm(list=ls())

library(tidyverse)
library(lubridate)
library(sf)
library(icesDatras)  # install.packages("icesDatras")
# library(tidyices)    # devtools::install_github("fishvice/tidyices", dependencies = FALSE)
# library(data.table)
# library(gisland)     # devtools::install_github("einarhjorleifsson/gisland", dependencies = FALSE); requires geo package that no longer exists

# Load utils code
source("../prf/r/my utils.r")

# Data path
datapath <- "D:/ICES/DATRAS"

# Get all objects in icesDatras
# list_all_objects_in_package("icesDatras")
# list_all_objects_in_package("tidyices")

# A messy data structure
# dtrs <- icesDatras::getDatrasDataOverview()

# Function doing the same as above but returns a tidy dataframe TAKES A LONG TIME!
get_datras_data_overview <- function() {
  d <- 
    # dplyr::data_frame(survey =  icesDatras::getSurveyList()) %>% 
    dplyr::tibble(survey =  icesDatras::getSurveyList()) %>% 
    dplyr::mutate(year = purrr::map(survey, icesDatras::getSurveyYearList)) %>% 
    tidyr::unnest(cols=c(year)) %>% 
    dplyr::mutate(quarter = purrr::map2(survey, year, icesDatras::getSurveyYearQuarterList)) %>% 
    tidyr::unnest(cols=c(quarter))
  return(d)
}

dtrs    <- get_datras_data_overview()

surveys <- distinct(dtrs, survey)

years   <- distinct(dtrs, year)
years   <- 2021:2022

# loop trough years, loop through each survey, download and save ----------------------------------
# Error in sp-north 2008;  match.names(clabs, names(xi)) : names do not match previous names 

# y <- 2008
# s <- 20

for(s in 1:nrow(surveys) ) { 
    
  sur <- filter(surveys, row_number() == s)$survey
  print(sur)
  
  for(y in years) {
    
    yrs <- y
    print(yrs)
    
    qts <- c(1:4)
    # A error occurs in the NS-IBTS if all quarters are requested
    if(sur == "NS-IBTS") qts <- c(1, 3)
  
    if (y %in% getSurveyYearList(sur)) {
      hh_raw <- 
        icesDatras::getDATRAS(record = "HH", survey = sur, years = yrs, quarters = qts)
      hl_raw <- 
        icesDatras::getDATRAS(record = "HL", survey = sur, years = yrs, quarters = qts)
      ca_raw <- 
        icesDatras::getDATRAS(record = "CA", survey = sur, years = yrs, quarters = qts)
      
      raw <- list()
      
      if(exists("hh_raw")) {
        raw[["hh"]] <- hh_raw; rm(hh_raw)
      }
      if(exists("hl_raw")) {
        raw[["hl"]] <- hl_raw; rm(hl_raw)
      }
      if(exists("ca_raw")) {
        raw[["ca"]] <- ca_raw; rm(ca_raw)
      }
      
      write_rds(raw, file = paste0(datapath, "/raw/", tolower(sur),y, "_raw.rds"))
      
    } # year exists loop
    
  } # survey loop
  
} # year loop


