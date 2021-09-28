#' Retrieve WFS layer data in WGS84 coordinate format
#' @param url character
#' @param bounding_data data.frame
#' @param layer character
#' @param crs integer
#' @return sf
#' @export
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
