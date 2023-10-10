#' availableCountries
#'
#' List of countries and entities names for checklists
#'
#' @importFrom rnaturalearth ne_countries
#' @return This function provides a list of countries and entities names 
#' available with rworldmaps for checklists
#' @examples
#' country_list <- availableCountries()
#' @export
availableCountries <- function() {
  world <- ne_countries(returnclass = "sf")
  list <- sort(world$name_sort)
  return(list)
}