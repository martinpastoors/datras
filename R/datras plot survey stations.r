# -----------------------------------------------------------------------------------------------
# Datras plot.r 
#
# R version: 3.5
# 
# Plot simple survey plots
#
# 05/09/2017 First version based on code from Einar
# 13/08/2018 Updated on the basis of Datras doodle website
# 18/09/2019 Corrected error in calculation of B (used N instead of B before)
# -----------------------------------------------------------------------------------------------

rm(list=ls())

library(icesDatras)   # install.packages("icesDatras")
library(tidyverse)    # tidying packages
library(lubridate)
library(sf)
library(data.table)

library(maps)
library(mapdata)
library(lubridate)
library(viridis)

# source my utils
source("../prf/R/my utils.r")

# onedrive <- get_onedrive()
onedrive <- "C:/DATA/RDATA"

# Data path
datapath <- "D:/ICES/DATRAS"

# -----------------------------------------------------------------------------------------------
# Load the tidy and combined datras data
# -----------------------------------------------------------------------------------------------

if(!exists("hh")) hh <- read_rds(file = file.path(datapath, "comb", "hh.rds"))
if(!exists("hl")) hl <- read_rds(file = file.path(datapath, "comb", "hl.rds"))
if(!exists("ca")) ca <- read_rds(file = file.path(datapath, "comb", "ca.rds"))


# -----------------------------------------------------------------------------------------------
# Survey station plots
# -----------------------------------------------------------------------------------------------
plot_survey_stations <- function(mysurvey  = "FR-CGFS",
                                 myspecies = "bss",
                                 myyears   = 2015:2022,
                                 mylengths = 10:70,
                                 ncol      = 5,
                                 plotvar   = "N") {

  
  if (!plotvar %in% c("N","B")) {stop("plotvar needs to be N or B")}
  
  le <- 
    hl %>%
    dplyr::filter(survey  %in% mysurvey, 
                  species %in% myspecies) %>% 
    group_by(survey, id, latin, species, length) %>% 
    summarise(n = n())
  
  # stations
  st <-
    hh %>%
    filter(
      survey  %in% mysurvey, 
      year %in% myyears) %>%
    select(id, survey, year, quarter, date, lon = shootlong, lat = shootlat)
  
  # summary(st)
  
  xlim <- range(st$lon)
  ylim <- range(st$lat)
  
  # generate final data frame
  df <-
    le %>%
    filter(length %in% mylengths) %>%
    
    mutate(b = n * 0.01 * length^3) %>%
    
    group_by(id, latin, species) %>%
    summarise(N = sum(n),
              b = sum(b)) %>%
    ungroup() %>%
    
    right_join(st) %>%
    
    # filter(quarter %in% 1) %>%
    filter(year %in% myyears) %>%
    
    # only use points with rc smaller than myline
    # filter((lat-myline$y[1])/(lon-myline$x[1]) < (myline$y[2]-myline$y[1])/(myline$x[2]-myline$x[1])) %>%
    
    mutate(
      # year = year(date),
      N    = ifelse(is.na(N), 0, N),
      B    = ifelse(is.na(b), 0, b),
      sq   = encode_zchords(lon, lat, dx = 1)) %>%
    
    # Special treatment of CGFS; one year later
    # mutate(year = ifelse(survey == "FR-CGFS", year+1, year)) %>%
    # mutate(year = paste(year-1,year,sep="-")) %>%
    
    group_by(lat, lon, year, latin, species) %>%
    summarise(N = mean(N),
              B = mean(B)) %>%
    filter(!is.na(latin)) 
  
  # Generate title
  tt <- paste0(unique(df$latin), 
              " (",
              toupper(unique(df$species)), 
              "). Lengths:",
              min(mylengths),
              "-",
              max(mylengths))

  # Create final plot
  map_data("worldHires", xlim = xlim, ylim = ylim) %>%
    ggplot() +
    theme_bw() +
    geom_polygon(aes(long, lat, group = group), fill = "grey") +
    coord_quickmap(xlim = xlim, ylim = ylim) +
    # coord_quickmap(xlim = c(xlim[1],4), ylim = ylim) +
    scale_x_continuous(expand = expansion(add=c(1,1))) +
    scale_y_continuous(expand = expansion(add=c(1,1))) + 
    
    geom_point(data = st, aes(lon, lat), colour="red", shape=3, size=0.5) +
    {if(plotvar == "N") {
      geom_point(data = df, aes(lon, lat, size = N), shape=21, fill=NA)
    } else {
      geom_point(data = df, aes(lon, lat, size = B), shape=21, fill=NA)
    }} +
    
    labs(title=tt, x="", y="") +
    facet_wrap(~ year, ncol = ncol)
  
}

plot_survey_stations (mysurvey  = "FR-CGFS",
                      myspecies = "bss",
                      myyears   = 2010:2022,
                      mylengths = 10:70,
                      ncol      = 5,
                      plotvar   = "B")
  


