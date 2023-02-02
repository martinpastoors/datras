#' tidy_ca
#'
#' @param ca datras untidy age dataframe, as obtained via the function
#' icesDatras::getDATRAS
#' @param species dataframe with species code, of the form obtained via function
#' get_species. Required column names are valid_aphia and latin. If dataframe
#' not supplied in the function argument, it will be automatically obtained.
#' @param all_variables A TRUE/FALSE flag. Should all variables
#' (default FALSE) or all variable (TRUE) be returned
#'
#' @return A tibble with
#'
#' \describe{
#'   \item{id}{haul id}
#'   \item{latin}{Latin name of species}
#'   \item{species}{English name of species}
#'   \item{length}{Length in centimeters}
#'   \item{sex}{Sex of fish}
#'   \item{maturity}{The maturity scale}
#'   \item{age}{Age in years}
#'   \item{wgt}{Weight in grammes}
#'   \item{n}{Number of fish}
#' }
#'
#' @export
#'
tidy_ca <- function(ca, species, afsis, ca_int, ca_num, all_variables = FALSE) {
  
  
  if(missing(species)) species <- suppressMessages(readr::read_csv("ftp://ftp.hafro.is/pub/reiknid/einar/datras_worms.csv"))  %>% 
      mutate(aphia = as.character(aphia))
  
  if(missing(afsis)) afsis <- suppressMessages(read_rds(file = file.path("rdata", "afsis.rds"))) 
  
  ca <-
    ca %>% 
    
    dplyr::rename_all(tolower) %>% 
    mutate(across(everything(), as.character)) %>% 
    mutate(across(any_of(ca_int),       as.integer)) %>% 
    mutate(across(any_of(ca_num),       as.numeric)) %>% 
    
    # create unique id
    tidyices::id_unite(remove=FALSE) %>% 
    
    # turn everything to cm
    dplyr::mutate(length = ifelse(lngtcode %in% c(".", "0"), lngtclass / 10, lngtclass),
                  indwgt = ifelse(indwgt <= 0, NA, indwgt)) %>% 
    # create unique id
    tidyices::id_unite(remove=FALSE) %>% 
    
    # turn everything to cm
    dplyr::mutate(length = ifelse(lngtcode %in% c(".", "0"), lngtclass / 10, lngtclass),
                  indwgt = ifelse(indwgt <= 0, NA, indwgt)) %>% 
    
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
                    length, sex, maturity, age, wgt = indwgt, n = canoatlngt)
    }} %>% 
    
    as_tibble()
  
  return(ca)
  
}
