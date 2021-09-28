#' Retrieve REST layer data in WGS84 coordinate format
#' @param url character
#' @param feature character
#' @param layer character
#' @param id_var_name character
#' @return sf
#' @export
retrieveREST <- function(url, feature, layer, id_var_name) {
  url <- httr::parse_url(url)
  url$path <- paste(url$path, feature, 'FeatureServer', layer,'query', sep = '/')
  url$query <- list(where = paste(id_var_name, " > 0", sep = ''),
                    outFields = "*",
                    returnGeometry = "true",
                    f = "geojson")
  request <- httr::build_url(url)
  return(sf::st_read(request))
}
