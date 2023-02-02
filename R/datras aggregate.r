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
# Load the tidy datras data
# -----------------------------------------------------------------------------------------------

# names(hh)
# unique(hh$hauldur)
# afsis
afsis <- 
  get(load(file.path(onedrive, "afsis.RData"))) %>% 
  rename(latin = scientific_name)

hh <- hl <- ca <- data.frame(stringsAsFactors = FALSE)

# hh files
list.hh <- list.files(file.path(datapath, "tidy"),
                      pattern = "_hh", 
                      full.names=TRUE) 

for (i in 1:length(list.hh)) {
  
  print(paste(i, list.hh[i], sep = " - "))
  
  if (nrow(read_rds(file=list.hh[i])) > 0) {
    
    hh <- 
      bind_rows(hh, 
                read_rds(file = list.hh[i]) %>% 
                  mutate(year = as.integer(year))
      ) 
  } # end of if statement 
} # end of loop

# hl files
list.hl <- list.files(file.path(datapath, "tidy"),
                      pattern = "_hl", 
                      full.names=TRUE) 

for (i in 1:length(list.hl)) {
  
  print(paste(i, list.hl[i], sep = " - "))
  
  if (nrow(read_rds(file=list.hl[i])) > 0) {
    
    hl <- 
      bind_rows(hl, 
                read_rds(file = list.hl[i]) %>% 
                  left_join(dplyr::select(afsis, 
                                          species, latin, english_name, dutch_name), 
                            by="latin")
      ) 
  } # end of if statement 
} # end of loop


# ca files
list.ca <- list.files(file.path(datapath, "tidy"),
                      pattern = "_ca", 
                      full.names=TRUE) 

for (i in 1:length(list.ca)) {
  
  print(paste(i, list.ca[i], sep = " - "))
  
  
  if (nrow(read_rds(file=list.ca[i])) > 0) {
    
    ca <- 
      bind_rows(ca, 
                read_rds(file = list.ca[i]) %>% 
                  left_join(dplyr::select(afsis, 
                                          species, latin, english_name, dutch_name), 
                            by="latin")
      ) 
  } # end of if statement 
} # end of loop



write_rds(hh, file = file.path(datapath, "comb", "hh.rds"))
write_rds(hl, file = file.path(datapath, "comb", "hl.rds"))
write_rds(ca, file = file.path(datapath, "comb", "ca.rds"))
