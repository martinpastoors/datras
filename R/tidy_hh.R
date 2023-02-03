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
  
  if(missing(fao)) 
    fao         <- 
      gisland::read_sf_ftp("FAO_AREAS_CWP_NOCOASTLINE") %>% 
      filter(F_LEVEL == "DIVISION") %>% 
      dplyr::select(division = F_CODE) %>% 
      st_as_sf()
  
  if(missing(ns_area)) 
    ns_area <- 
      gisland::read_sf_ftp("NS_IBTS_RF") %>% 
      dplyr::select(rfarea = AreaName) %>% 
      sf::st_make_valid() %>% 
      sf::st_as_sf()
  
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
    sf::st_as_sf(coords = c("shootlong", "shootlat"), crs = 4326, remove = FALSE)  %>%  
    
    sf::st_join(fao) %>%  
    sf::st_join(ns_area) %>%  
    sf::st_drop_geometry() %>% 
    
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
                    division, 
                    daynight,
                    datatype,
                    stdspecreccode,
                    byspecreccode) %>%
        return()
    }}
  
}