#' Retrieve REST layer data in WGS84 coordinate format
#' @param url character
#' @param id_var_name character
#' @return sf
#' @export
retrieveREST <- function(url, id_var_name) {
  tmp <- httr::parse_url(url)
  tmp$path <- paste(tmp$path,'query', sep = '/')
  tmp$query <- list(where = paste(id_var_name, " > 0", sep = ''),
                    outFields = "*",
                    returnGeometry = "true",
                    f = "geojson")
  request <- httr::build_url(tmp)
  return(sf::st_read(request))
}


retrieveWFS <- function(url, bounding_data, layer, crs) {
  bbox <- attr(sf::st_transform(bounding_data, crs)$geometry, 'bbox')[c(2,1,4,3)] %>% as.vector() %>% paste(collapse = ',')
  url <- httr::parse_url(url)
  url$query <- list(service = "WFS",
                    version = "2.0.0",
                    request = "GetFeature",
                    typeNames = layer,
                    BBOX = bbox
  )
  request <- httr::build_url(url)
  temp <- tempfile()
  utils::download.file(request,temp)
  return(sf::st_transform(sf::st_set_crs(sf::st_as_sf(as.data.frame(sf::read_sf(temp))),crs),4326))
}
