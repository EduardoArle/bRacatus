#' availableCountries
#'
#' List of countries and entities names for checklists
#'
#' @import rgdal
#' @import raster
#' @import rworldmap
#' @return This function provides a list of countries and entities names available with rworldmaps for checklists
#' @export
availableCountries <- function(){
  world <- rworldmap::getMap()
  list <- sort(world$NAME)
  return(list)
}


