#' tidy_hh
#'
#' @param df A datras haul dataframe
#' @param all_variables A TRUE/FALSE flag. Should all variables
#' (default FALSE) or all variable (TRUE) be returned
#'
#' @return data.frame
#' @export
#'
#'
tidy_hh <- function(hh, hh_int, hh_num, fao, ns_area, all_variables = FALSE) {
  
  if(missing(fao)) fao         <- read_sf_ftp("FAO_AREAS_CWP_NOCOASTLINE")
  
  if(missing(ns_area)) ns_area <- read_sf_ftp("NS_IBTS_RF")
  
  hh <-
    hh %>% 
    # suppressMessages(icesDatras::getDATRAS(record   = "HH",
    #                                        survey   = sur,
    #                                        years    = yrs,
    #                                        quarters = qs))  %>% 
    
    dplyr::rename_all(tolower) %>% 
    mutate(across(everything(),   as.character)) %>% 
    mutate(across(any_of(hh_int), as.integer)) %>% 
    mutate(across(any_of(hh_num), as.numeric)) %>% 
    
    # create unique id
    id_unite(remove = FALSE) %>%
    
    # get proper date
    dplyr::mutate(timeshot = stringr::str_pad(timeshot, width = 4, side = "left", pad = "0"),
                  timeshot = paste0(stringr::str_sub(timeshot, 1, 2),
                                    ":",
                                    stringr::str_sub(timeshot, 3, 4)),
                  datetime = lubridate::ymd_hm(paste(year, month, day, timeshot))) %>% 
    
    # do spatial allocations
    # mutate(., division = geo_inside(shootlong, shootlat, fao, "F_DIVISION")) %>% 
    # mutate(ns_area  = geo_inside(shootlong, shootlat, ns_area, "AreaName")) %>% 
    
    {if(all_variables) {
      
      dplyr::select(., -recordtype) %>%
        return()
      
    } else {
      dplyr::select(., 
                    survey, id, year, quarter, survey, ship, gear, haulno,
                    date = datetime, country, depth, haulval, hauldur,
                    shootlat, shootlong,
                    haullat, haullong,
                    statrec = statrec,
                    daynight,
                    datatype,
                    stdspecreccode,
                    byspecreccode) %>%
        return()
    }}
  
}