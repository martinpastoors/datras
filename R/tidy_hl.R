#' tidy_hl
#'
#' @param hl datras untidy length dataframe, as obtained via the function
#' icesDatras::getDATRAS
#' @param hh datras tidy haul data
#' @param species dataframe with species code, of the form obtained via function
#' get_species. Required column names are valid_aphia and latin. If dataframe
#' not supplied in the function argument, it will be automatically obtained.
#' @param all_variables A TRUE/FALSE flag. Should all variables
#' (default FALSE) or all variable (TRUE) be returned
#'
#' @return A tibble with 6 variables:
#'
#' \describe{
#'   \item{id}{haul id}
#'   \item{latin}{Latin name of species}
#'   \item{species}{English name of species}
#'   \item{sex}{Sex of fish}
#'   \item{length}{Length in centimeters}
#'   \item{n}{Number of fish per standardized to 60 minute haul}
#' }
#'
#' @export
#'
tidy_hl <- function(hl, hh, species, afsis, hl_int, hl_num, all_variables = FALSE) {
  
  if(missing(species)) 
    species <- 
      suppressMessages(readr::read_csv("ftp://ftp.hafro.is/pub/reiknid/einar/datras_worms.csv"))  %>% 
      mutate(aphia = as.character(aphia))
  
  if(missing(afsis)) 
    afsis <- 
      suppressMessages(read_rds(file = file.path("rdata", "afsis.rds"))) 
  
  hl <-
    hl %>%
    dplyr::rename_all(tolower) %>% 
    mutate(across(everything(),         as.character)) %>% 
    mutate(across(any_of(hl_int),       as.integer)) %>% 
    mutate(across(any_of(hl_num),       as.numeric)) %>% 
    
    tidyices::id_unite(remove=FALSE) %>%
    
    # only stations that are in the station table (north sea ibts)
    dplyr::filter(id %in% hh$id) %>%
    
    # length class to cm
    dplyr::mutate(length = ifelse(lngtcode %in% c(".", "0"), lngtclass / 10, lngtclass),
                  hlnoatlngt = hlnoatlngt * subfactor) %>%
    
    # get the data type and hauldur
    dplyr::left_join(hh %>% dplyr::select(id, 
                                     datatype, hauldur), 
                     by = "id") %>%
    
    # catch per hour
    dplyr::mutate(n = ifelse(datatype == "R",
                             hlnoatlngt * 60 / hauldur,
                             hlnoatlngt)) %>% 
  
    # add species codea
    mutate(aphia = valid_aphia,
           aphia = ifelse(is.na(aphia) & speccodetype == "W",
                          speccode,
                          aphia)) %>%
    left_join(species, by = "aphia") %>%
    dplyr::select(-aphia) %>% 
    
    # add afsis species data
    left_join(dplyr::select(afsis,
                            latin, species, english_name, dutch_name), 
              by="latin") %>% 
    
    # select subset if required
    {if (!all_variables) {
      dplyr::select(.,
                    survey, id, species, latin, english_name, dutch_name,
                    sex, length, n)
    }} %>% 
    
    as_tibble()
  
  return(hl)
  
}
