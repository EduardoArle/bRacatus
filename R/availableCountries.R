#' availableCountries
#'
#' List of countries and entities names for checklists
#'
#' @importFrom rworldmap getMap
#' @return This function provides a list of countries and entities names 
#' available with rworldmaps for checklists
#' @examples
#' country_list <- availableCountries()
#' @export
availableCountries <- function() {
  world <- getMap()
  list <- sort(world$NAME)
  return(list)
}


