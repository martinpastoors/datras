# -------------------------------------------------------------------------------
# Datras tidy it up
#
# 14/08/2018 first coding
# 15/03/2019 coding during HAWG
# 19/07/2019 small change in mutate_at
# 02/02/2023 full recoding of the original codes from Einar
#
# -------------------------------------------------------------------------------

rm(list=ls())

library(tidyverse)
library(lubridate)
library(sf)
library(icesDatras)  # install.packages("icesDatras")
library(tidyices)    
library(data.table)

library(gisland)     # devtools::install_github("einarhjorleifsson/gisland", dependencies = FALSE); requires geo package that no longer exists
# source("../gisland/R/read_sf_ftp.R")
source("R/geo_inside.R")

library(tidyices)     # requires tidyices package that is not updated with recent tibble structure
source("R/tidy_hh.R")
source("R/tidy_hl.R")
source("R/tidy_ca.R")
source("R/id_unite.R")

# Load utils code
source("../prf/r/my utils.r")

# Data path
datapath <- "D:/ICES/DATRAS"

fao     <- read_sf_ftp("FAO_AREAS_CWP_NOCOASTLINE")
ns_area <- read_sf_ftp("NS_IBTS_RF") %>% as("Spatial")
species <- suppressMessages(readr::read_csv("ftp://ftp.hafro.is/pub/reiknid/einar/datras_worms.csv"))  %>% 
  mutate(aphia = as.character(aphia))
afsis <- suppressMessages(read_rds(file = file.path("rdata", "afsis.rds")))

# Load excel file format descriptions
hh_desc <- readxl::read_excel(path  = "excel/DATRAS_Field_descriptions_and_example_file_May2022.xlsx", sheet = "HH") 
  
  hh_int  <- hh_desc %>% filter(DataType == "int")      %>% mutate(Field=tolower(Field)) %>% 
    dplyr::select(Field)  %>% unlist() %>% as.character()
  hh_num  <- hh_desc %>% filter(grepl("dec", DataType)) %>% mutate(Field=tolower(Field)) %>%
    dplyr::select(Field)  %>% unlist() %>% as.character()

hl_desc <- readxl::read_excel(path  = "excel/DATRAS_Field_descriptions_and_example_file_May2022.xlsx", sheet = "HL") 
  
  hl_int  <- hl_desc %>% filter(DataType == "int")      %>% mutate(Field=tolower(Field)) %>% 
    dplyr::select(Field)  %>% unlist() %>% as.character()
  hl_num  <- hl_desc %>% filter(grepl("dec", DataType)) %>% mutate(Field=tolower(Field)) %>%
    dplyr::select(Field)  %>% unlist() %>% as.character()

ca_desc <- readxl::read_excel(path  = "excel/DATRAS_Field_descriptions_and_example_file_May2022.xlsx", sheet = "CA") 
  
  ca_int  <- ca_desc %>% filter(DataType == "int")      %>% mutate(Field=tolower(Field)) %>% 
    dplyr::select(Field)  %>% unlist() %>% as.character()
  ca_num  <- ca_desc %>% filter(grepl("dec", DataType)) %>% mutate(Field=tolower(Field)) %>%
    dplyr::select(Field)  %>% unlist() %>% as.character()


# ---------------------------------------------------------------------------------------------
# Selections
# ---------------------------------------------------------------------------------------------

surveys <- c("NS-IBTS", "FR-CGFS")
yrs <- 2020:2022  # years
qs <- c(1,2,3,4)   # quarters

hh <- hl <- ca <- data.frame(stringsAsFactors = FALSE)

for (sur in c(surveys)) {
  
  print(paste("HH", sur, paste(min(yrs),max(yrs),sep="-")))
  
  hh <-
    bind_rows(
      hh,
      suppressMessages(icesDatras::getDATRAS(record   = "HH",
                                             survey   = sur,
                                             years    = yrs,
                                             quarters = qs))  %>% 
        tidy_hh(hh_int=hh_int, hh_num=hh_num, all_variables = FALSE)
    )
}

# rm(hl)

for (sur in c(surveys)) {
  
  print(paste("HL", sur, paste(min(yrs),max(yrs),sep="-")))
  
  hl <-
    bind_rows(
      hl,
        suppressMessages(icesDatras::getDATRAS(record   = "HL",
                                             survey   = sur,
                                             years    = yrs,
                                             quarters = qs))   %>%
        tidy_hl(., hh=hh, species=species, afsis=afsis, hl_int=hl_int, hl_num=hl_num, all_variables = FALSE)
    )
}


for (sur in c(surveys)) {
  
  print(paste("CA", sur, paste(min(yrs),max(yrs),sep="-")))
  
  ca <-
    bind_rows(
      ca,
      suppressMessages(icesDatras::getDATRAS(record   = "CA",
                                             survey   = sur,
                                             years    = yrs,
                                             quarters = qs))    %>% 
        tidy_ca(., species=species, afsis=afsis, ca_num=ca_num, ca_int=ca_int, all_variables=FALSE)
    )
}

#construct filenames
tt <- 
  paste(
    paste(unique(hh$survey), collapse ="_"),
    paste(min(hh$year, na.rm=TRUE), max(hh$year, na.rm=TRUE), sep="-")
  ) %>% 
  as.character()

# save files
hh %>% write_rds(file = paste0(datapath, "/tidy/", tt, "_hh.rds"))
hl %>% write_rds(file = paste0(datapath, "/tidy/", tt, "_hl.rds"))
ca %>% write_rds(file = paste0(datapath, "/tidy/", tt, "_ca.rds"))
    







