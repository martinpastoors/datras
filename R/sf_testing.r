# sf join testing
library(icesDatras)
library(tidyverse)
library(sf)

fao     <- 
  gisland::read_sf_ftp("FAO_AREAS_CWP_NOCOASTLINE") %>% 
  filter(F_LEVEL == "DIVISION") %>% 
  dplyr::select(division = F_CODE) %>% 
  sf::st_as_sf()

ns_area <- 
  gisland::read_sf_ftp("NS_IBTS_RF") %>% 
  dplyr::select(rfarea = AreaName) %>% 
  sf::st_make_valid() %>% 
  sf::st_as_sf()

hh <-
  suppressMessages(icesDatras::getDATRAS(record   = "HH",
                                         survey   = "NS-IBTS",
                                         years    = 2021,
                                         quarters = 3))  %>%
  dplyr::rename_all(tolower) %>% 
  
  # do spatial allocations
  sf::st_as_sf(coords = c("shootlong", "shootlat"), crs = 4326, remove = FALSE)  %>%  
  
  sf::st_join(fao) %>%
  sf::st_join(ns_area) %>%
  
  sf::st_drop_geometry() 
  