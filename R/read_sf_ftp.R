#' Read shapefile from ftp
#'
#' @param name The name of the shapefile
#' @param url The ftp directory
#'
#' @return a sf-tibble
#' @export
#'
read_sf_ftp <- function(name, url = "ftp://ftp.hafro.is/pub/data/shapes") {

  sf::read_sf(paste0(url, "/", name, ".gpkg"))

}

#' Read Icelandic harbours
#'
#' @param trim Boolean(default TRUE) filters out some smaller harbours, cause a nuisance downstream
#'
#' @return a tibble with harbour polygons
#' @export
#'
gl_read_is_harbours <- function(trim = TRUE) {

  d <- read_sf_ftp("harbours")
  if(trim) {
    d <-
      d |>
      dplyr::filter(!hid %in% c("HRI", "ASS", "HAU", "GRE", "MJH", "MJO", "AED", "HJA"))
  }
  return(d)
}

gl_get_grunnpunktar <- function() {

  read_sf_ftp("grunnpunktar")

}

gl_get_grunnflaki <- function() {

  d <- gl_get_grunnpunktar()

  dplyr::bind_rows(d %>%
                     # not sker because they are just points
                     dplyr::filter(region %in% c("meginland", "grimsey")) %>%
                     dplyr::group_by(region) %>%
                     dplyr::summarise(do_union = FALSE) %>%
                     sf::st_cast("LINESTRING") %>%
                     sf::st_cast("POLYGON"),
                   d %>%
                     dplyr::filter(region %in% c("kolbeinsey", "hvalbakur")) %>%
                     sf::st_transform(crs = 9040) %>%
                     sf::st_buffer(dist = 200) %>%
                     sf::st_transform(crs = 4326) %>%
                     dplyr::select(region))

}

gl_get_grunnlina <- function() {

  gl_get_grunnflaki() %>%
    sf::st_cast("LINESTRING")

}

gl_get_landhelgi <- function() {
  read_sf_ftp("landhelgi")
}

gl_get_eez_iceland <- function() {
  read_sf_ftp("eez_iceland")
}
gl_get_eez_noshoreline <- function() {
  read_sf_ftp("eez_no-shoreline")
}
gl_get_eez <- function() {
  read_sf_ftp("eez")
}


gl_get_vidmidunarpunktar <- function() {

  read_sf_ftp("vidmidunarpunktar")

}

gl_get_vidmidunarlina <- function() {

  gl_get_vidmidunarpunktar() %>%
    dplyr::summarise(do_union = FALSE) %>%
    sf::st_cast("LINESTRING") %>%
    dplyr::mutate(nafn = "viðmiðunarlína") %>%
    dplyr::select(nafn)

}

gl_get_vidmidunarflaki <- function() {

  gl_get_vidmidunarlina() %>%
    sf::st_cast("POLYGON")

}

gl_get_bormicon <- function() {
  read_sf_ftp("bormicon")
}

gl_get_fao_area <- function() {
  read_sf_ftp("fao")
}

gl_get_fao_area_full <- function() {
  read_sf_ftp("FAO_AREAS_CWP_NOCOASTLINE")
}


gl_get_ices_areas <- function() {
  read_sf_ftp("ices_areas")
}

gl_get_ices_ecoregions <- function() {
  read_sf_ftp("ices_ecoregions")
}

gl_get_ices_rectangles <- function() {
  read_sf_ftp("ices_rectangles")
}

gl_get_ices_subrectangles <- function() {
  read_sf_ftp("ices_subrectangles")
}


