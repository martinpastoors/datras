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

# -----------------------------------------------------------------------------------------------
# Datras index by length
# -----------------------------------------------------------------------------------------------

mysurvey="FR-CGFS"; myyear=1990:2022; myquarter=4; myspecies="bss"; mylength=10:70; plotvar="N"
  
datras_index <- function(mysurvey, myyear, myquarter, myspecies, mylength, plotvar="N") {
  
  # numbers at length
  le <- 
    hl %>%
    dplyr::filter(survey  %in% mysurvey, 
                  species %in% myspecies,
                  length  %in% mylength) %>% 
    group_by(survey, id, latin, species, length) %>% 
    summarise(n = n())
  
  # stations
  st <-
    hh %>%
    dplyr::filter(survey  %in% mysurvey, 
                  year    %in% myyear, 
                  quarter %in% myquarter) %>%
    select(id, survey, year, quarter, date, lon = shootlong, lat = shootlat)
  
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
    filter(length >= mylength[1] & length < mylength[2]) %>%
    
    mutate(b = n * 0.01 * length^3) %>%

    group_by(id, latin, species) %>%
    summarise(N = sum(n),
              b = sum(b)) %>%
    ungroup() %>%

    right_join(st) %>%

    filter(quarter %in% myquarter) %>%
    filter(year %in% myyear) %>%

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

    group_by(sq, year, latin, species) %>%
    summarise(N = mean(N),
              B = mean(B)) %>%
    separate(sq, c("lon", "lat"), sep = ":", convert = TRUE) %>%
    filter(!is.na(latin)) 
  
  df2 <-
    df %>% 
    group_by(year, latin, species) %>% 
    summarize(lat = weighted.mean(lat, w=B, na.rm=TRUE),
              lon = weighted.mean(lon, w=B, na.rm=TRUE))
  
  df3 <- 
    df %>% 
    group_by(latin, species, year) %>% 
    summarize(B = mean(B, na.rm=TRUE))
  
  # Create final plot
  map +
    geom_raster(data = df, aes(lon, lat, fill = B)) +
    scale_fill_viridis(option = "B", direction = -1) +
    
    {if(midpoint) geom_point(data=df2, aes(lon, lat), shape=3, size=2, stroke=2, colour="green")} +
    
    labs(
      title    = paste0(paste(mylatin, collapse=" "),
                        paste(" ("),
                        paste(toupper(myspecies), collapse=" "),
                        paste(") ")),
      subtitle = paste0("Length:",
                        paste(mylength, collapse="-")),
      caption  = paste0(paste(unique(st$survey), collapse=","),
                        " quarter:",
                        paste(myquarter, collapse=";") ) ) +
    facet_wrap(~ year, ncol = 6)

  
  # df3 %>% 
  #   ggplot(aes(year, B)) +
  #   theme_bw() +
  #   geom_line()
  
}


# sort(unique(hl$latin))
plot_datras(mysurvey=c("NS-IBTS"), myyear=2003:2019, myquarter=c(1), myspecies=c("her"), mylength=c(10,20))
plot_datras(mysurvey=c("NS-IBTS"), myyear=2003:2019, myquarter=c(3), myspecies=c("her"), mylength=c(10,20))

plot_datras(mysurvey=c("BTS"),     myyear=2003:2020, myquarter=c(3), myspecies=c("ple"), mylength=c(30,40))
plot_datras(mysurvey=c("BTS"),     myyear=2003:2020, myquarter=c(3), myspecies=c("ple"), mylength=c(5,15))

plot_datras(mysurvey=c("NS-IBTS"), myyear=2003:2022, myquarter=c(1), myspecies=c("ple"), mylength=c(30,40))
plot_datras(mysurvey=c("NS-IBTS"), myyear=2003:2020, myquarter=c(3), myspecies=c("ple"), mylength=c(25,30))
plot_datras(mysurvey=c("NS-IBTS"), myyear=2005:2022, myquarter=c(3), myspecies=c("ple"), mylength=c(30,40))

plot_datras(mysurvey=c("NS-IBTS"), myyear=2003:2019, myquarter=c(1), myspecies=c("spr"), mylength=c(10,30))
plot_datras(mysurvey=c("NS-IBTS"), myyear=2003:2019, myquarter=c(1), myspecies=c("pil"), mylength=c(10,30))
plot_datras(mysurvey=c("NS-IBTS"), myyear=2003:2019, myquarter=c(1), myspecies=c("ane"), mylength=c(10,30))

plot_datras(mysurvey=c("NS-IBTS"), myyear=2003:2021, myquarter=c(1), myspecies=c("mac"), mylength=c(10,40))
plot_datras(mysurvey=c("NS-IBTS"), myyear=2003:2021, myquarter=c(1), myspecies=c("hom"), mylength=c(10,30))
plot_datras(mysurvey=c("NS-IBTS"), myyear=2003:2019, myquarter=c(1), myspecies=c("cod"), mylength=c(10,30))
plot_datras(mysurvey=c("NS-IBTS"), myyear=2003:2019, myquarter=c(1), myspecies=c("whg"), mylength=c(10,30))
plot_datras(mysurvey=c("NS-IBTS"), myyear=2003:2019, myquarter=c(1), myspecies=c("had"), mylength=c(10,30))
plot_datras(mysurvey=c("NS-IBTS"), myyear=2003:2019, myquarter=c(1), myspecies=c("rjc"), mylength=c(10,50))

plot_datras(mysurvey=c("NS-IBTS"), myyear=2003:2019, myquarter=c(1), myspecies=c("aru","ary","arg"), mylength=c(10,40))
plot_datras(mysurvey=c("NS-IBTS"), myyear=2003:2019, myquarter=c(3), myspecies=c("aru","ary","arg"), mylength=c(10,40))

plot_datras(mysurvey=c("NS-IBTS"), myyear=2003:2019, myquarter=c(1), myspecies=c("ary"), mylength=c(10,40))
plot_datras(mysurvey=c("NS-IBTS"), myyear=2003:2019, myquarter=c(1), myspecies=c("aru"), mylength=c(10,40))
plot_datras(mysurvey=c("NS-IBTS"), myyear=2003:2019, myquarter=c(1), myspecies=c("arg"), mylength=c(10,40))

plot_datras(mysurvey=c("NS-IBTS"), myyear=1991:2018, myquarter=c(1), myspecies=c("ple"), mylength=c(10,40))
plot_datras(mysurvey=c("NS-IBTS"), myyear=1991:2018, myquarter=c(1), myspecies=c("sol"), mylength=c(10,40))

plot_datras(mysurvey=c("BTS"), myyear=1991:2018, myquarter=c(3), myspecies=c("ple"), mylength=c(10,40))
plot_datras(mysurvey=c("NS-IBTS"), myyear=1991:2018, myquarter=c(3), myspecies=c("ple"), mylength=c(10,40))

plot_datras(mysurvey=c("FR-CGFS"), myyear=1991:2021, myquarter=c(4), myspecies=c("ple"), mylength=c(10,40))
plot_datras(mysurvey=c("FR-CGFS"), myyear=1991:2021, myquarter=c(4), myspecies=c("mac"), mylength=c(10,40))
plot_datras(mysurvey=c("FR-CGFS"), myyear=1991:2021, myquarter=c(4), myspecies=c("hom"), mylength=c(10,40))
plot_datras(mysurvey=c("FR-CGFS"), myyear=1991:2021, myquarter=c(4), myspecies=c("bss"), mylength=c(10,70))
plot_datras(mysurvey=c("FR-CGFS"), myyear=1991:2021, myquarter=c(4), myspecies=c("bib"), mylength=c(10,50))
plot_datras(mysurvey=c("FR-CGFS"), myyear=1991:2021, myquarter=c(4), myspecies=c("ctc"), mylength=c(10,50))
plot_datras(mysurvey=c("FR-CGFS"), myyear=1991:2021, myquarter=c(4), myspecies=c("gur"), mylength=c(10,50))
plot_datras(mysurvey=c("FR-CGFS"), myyear=1991:2021, myquarter=c(4), myspecies=c("guu"), mylength=c(10,50))
plot_datras(mysurvey=c("FR-CGFS"), myyear=1991:2021, myquarter=c(4), myspecies=c("mur"), mylength=c(10,50))
plot_datras(mysurvey=c("FR-CGFS"), myyear=1991:2021, myquarter=c(4), myspecies=c("sqr"), mylength=c(10,50))

plot_datras(mysurvey=c("NS-IBTS","SCOWCGFS","SWC-IBTS"), 
            myyear=c(2000:2018,2020:2021), 
            # myquarter=c(1,3, 4), 
            myquarter=c(3,4), 
            myspecies=c("ple"), 
            mylength=c(30,40))

plot_datras(mysurvey=c("SCOWCGFS"), 
            myyear=c(2012:2021), 
            myquarter=c(4), 
            myspecies=c("ple"), 
            mylength=c(20,40))

plot_datras(mysurvey=c("EVHOE","FR-CGFS","IE-IGFS","NIGFS",
                        "NS-IBTS","ROCKALL","SCOROC","SCOWCGFS",
                        "SP-PORC","SWC-IBTS"), 
            myyear=2000:2021, 
            myquarter=c(2), 
            myspecies=c("mac"), 
            mylength=c(10,40))



