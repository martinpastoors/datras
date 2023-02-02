#' tidy_hh
#'
#' @param df A datras haul dataframe
#' @param all_variables A TRUE/FALSE flag. Should all variables
#' (default FALSE) or all variable (TRUE) be returned
#'
#' @return data.frame
#' @export
#'
tidy_hh <- function(hh, hh_int, hh_num, all_variables = FALSE) {
  
  hh <-
    hh %>%
    dplyr::rename_all(tolower) %>% 
    mutate(across(everything(),   as.character)) %>% 
    mutate(across(any_of(hh_int), as.integer)) %>% 
    mutate(across(any_of(hh_num), as.numeric)) %>% 
    
    # create unique id
    tidyices::id_unite(remove = FALSE) %>%
    
    # get proper date
    dplyr::mutate(timeshot = stringr::str_pad(timeshot, width = 4, side = "left", pad = "0"),
                  timeshot = paste0(stringr::str_sub(timeshot, 1, 2),
                                    ":",
                                    stringr::str_sub(timeshot, 3, 4)),
                  datetime = lubridate::ymd_hm(paste(year, month, day, timeshot))) %>% 
    
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