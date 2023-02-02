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
source("r/cutter.r")

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

# Plot data aggregated by years and lengths
plot_datras_aggregated <- function(mysurvey, myyear, myquarter, myspecies, mylength, cutyear, cutlength) {
  
  # mylatin <- filter(afsis, species %in% myspecies) %>% dplyr::select(latin)
  
  # numbers at length
  le <- 
    hl %>%
    dplyr::filter(survey  %in% mysurvey, species %in% myspecies) %>% 
    dplyr::filter(length >= mylength[1] & length < mylength[2]) %>%
    mutate(length2 = cutter(length, upper=50, by=cutlength)) %>% 
    group_by(survey, id, latin, species, length, length2) %>% 
    summarise(n = n()) %>% 
    
    mutate(b = n * 0.01 * length^3) %>%
    group_by(id, latin, species, length2) %>%
    summarise(N = sum(n),
              b = sum(b)) %>%
    ungroup()
    
  # stations
  st <-
    hh %>%
    filter(survey  %in% mysurvey, 
           year    %in% myyear, 
           quarter %in% myquarter) %>%
    select(id, survey, year, quarter, date, lon = shootlong, lat = shootlat) %>% 
    mutate(year2 = cutter(year, upper=2022, by=cutyear))  
    

  # summary(st)
  
  xlim <- range(st$lon)
  ylim <- range(st$lat)
  
  # plot map
  map <-
    map_data("worldHires", xlim = xlim, ylim = ylim) %>%
    ggplot() +
    theme_bw() +
    geom_polygon(aes(long, lat, group = group), fill = "grey") +
    coord_quickmap(xlim = xlim, ylim = ylim, expand = FALSE) +
    scale_x_continuous(NULL, NULL) +
    scale_y_continuous(NULL, NULL)
  
  # generate final data frame
  df <-
    le %>%
    
    right_join(st) %>%
    
    filter(quarter %in% myquarter) %>%
    filter(year %in% myyear) %>%
    
    mutate(
      N    = ifelse(is.na(N), 0, N),
      B    = ifelse(is.na(b), 0, b),
      sq   = encode_zchords(lon, lat, dx = 1)) %>%
    
    # Special treatment of CGFS; one year later
    # mutate(year = ifelse(survey == "FR-CGFS", year+1, year)) %>%
    # mutate(year = paste(year-1,year,sep="-")) %>%
    
    group_by(sq, year2, length2, latin, species) %>%
    summarise(N = mean(N),
              B = mean(N)) %>%
    separate(sq, c("lon", "lat"), sep = ":", convert = TRUE) %>%
    filter(!is.na(latin)) 
  
  # unique(le$length2)
  
  # df2 <-
  #   df %>% 
  #   group_by(year, latin, species) %>% 
  #   summarize(lat = weighted.mean(lat, w=B, na.rm=TRUE),
  #             lon = weighted.mean(lon, w=B, na.rm=TRUE))
  
  df3 <- 
    df %>% 
    group_by(latin, species, year2, length2) %>% 
    summarize(B = mean(B, na.rm=TRUE))
  
  # Create final plot
  map +
    geom_raster(data = df, aes(lon, lat, fill = B)) +
    scale_fill_viridis(option = "B", direction = -1) +
    ggtitle(paste0(paste(myspecies, collapse=" "),
                   paste(" ("),
                   paste(toupper(myspecies), collapse=" "),
                   paste(") "),
                   paste(mysurvey, collapse=","),
                   paste(" "),
                   "quarter:",
                   paste(myquarter, collapse=";"),
                   paste(" "))) +
    facet_grid(length2 ~ year2)
}

plot_datras_aggregated(mysurvey=c("NS-IBTS"), myyear=1992:2018, myquarter=c(3), myspecies=c("cod"), mylength=c(10,50), cutlength=10, cutyear=3)
plot_datras_aggregated(mysurvey=c("NS-IBTS"), myyear=1992:2018, myquarter=c(1), myspecies=c("aru","ary","arg"), mylength=c(10,50), cutlength=10, cutyear=3)
plot_datras_aggregated(mysurvey=c("NS-IBTS"), myyear=1992:2018, myquarter=c(3), myspecies=c("aru","ary","arg"), mylength=c(10,50), cutlength=10, cutyear=3)
plot_datras_aggregated(mysurvey=c("BTS"), myyear=1992:2018, myquarter=c(3), myspecies=c("sol"), mylength=c(10,50), cutlength=10, cutyear=3)
